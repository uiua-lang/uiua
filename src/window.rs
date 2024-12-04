use std::{
    collections::HashMap,
    env::current_exe,
    fs,
    io::{ErrorKind, Read, Write},
    net::{SocketAddr, TcpListener, TcpStream},
    path::Path,
    process::{exit, Command, Stdio},
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
    thread::{self, sleep},
    time::{Duration, Instant},
};

use crossbeam_channel::Receiver;
use eframe::egui::*;
use image::{GenericImageView, ImageFormat};
use load::SizedTexture;
use serde::*;

use crate::encode::SmartOutput;

static USE_WINDOW: AtomicBool = AtomicBool::new(false);
pub fn use_window() -> bool {
    USE_WINDOW.load(atomic::Ordering::Relaxed)
}
pub fn set_use_window(use_window: bool) {
    if use_window {
        if Request::Ping.send().is_err() {
            return;
        }
        USE_WINDOW.store(true, atomic::Ordering::Relaxed);
    }
}

const PORT: u16 = 8482;

/// A request to the window process
#[derive(Clone, Serialize, Deserialize)]
pub enum Request {
    Ping,
    ShowText(String),
    Show(SmartOutput),
    ShowAll(Vec<SmartOutput>),
    Separator,
    ClearBeforeNext,
    Shutdown,
}

const RETRIES: usize = 20;

impl Request {
    /// Send the request
    pub fn send(self) -> Result<(), String> {
        self.send_impl(RETRIES)
    }
    fn send_impl(self, retries: usize) -> Result<(), String> {
        let socket_addr = ([127, 0, 0, 1], PORT).into();
        let timeout = Duration::from_secs_f32(0.1);
        let mut stream = match TcpStream::connect_timeout(&socket_addr, timeout) {
            Ok(stream) => stream,
            Err(e)
                if retries > 0
                    && [ErrorKind::TimedOut, ErrorKind::ConnectionRefused].contains(&e.kind()) =>
            {
                if let Request::Shutdown = self {
                    return Ok(());
                }
                if retries + 1 == RETRIES {
                    if cfg!(debug_assertions) {
                        eprintln!("Uiua window not found, creating...");
                    }
                    Command::new(current_exe().unwrap())
                        .arg("window")
                        .stdout(if cfg!(debug_assertions) {
                            Stdio::inherit()
                        } else {
                            Stdio::null()
                        })
                        .spawn()
                        .unwrap();
                    if cfg!(debug_assertions) {
                        eprintln!("Uiua window created, waiting for connection...");
                    }
                }
                if e.kind() != ErrorKind::TimedOut {
                    sleep(timeout);
                }
                return self.send_impl(retries - 1);
            }
            Err(e) => {
                return Err(format!("Failed to connect to window: {e}"));
            }
        };
        let mut serializer = rmp_serde::Serializer::new(Vec::new())
            .with_struct_map()
            .with_human_readable();
        self.serialize(&mut serializer).unwrap();
        let bin = serializer.into_inner();
        stream.write_all(&bin).map_err(|e| e.to_string())?;
        stream.flush().map_err(|e| e.to_string())?;
        Ok(())
    }
}

pub fn run_window() {
    let (send, recv) = crossbeam_channel::unbounded();
    thread::spawn(move || {
        let addr = SocketAddr::from(([0u8; 4], PORT));
        let listener = match TcpListener::bind(addr) {
            Ok(listener) => listener,
            Err(e) => {
                eprintln!("Failed to bind to port {PORT}: {e}");
                exit(1);
            }
        };
        if cfg!(debug_assertions) {
            eprintln!("Listening on port {PORT}");
        }
        loop {
            match listener.accept() {
                Ok((mut stream, _)) => {
                    let mut buffer = Vec::new();
                    stream.read_to_end(&mut buffer).unwrap();
                    let mut deserializer =
                        rmp_serde::Deserializer::new(buffer.as_slice()).with_human_readable();
                    match Request::deserialize(&mut deserializer) {
                        Ok(req) => send.send(req).unwrap(),
                        Err(e) => {
                            eprintln!("Failed to decode request: {e}")
                        }
                    }
                }
                // Break if the window is closed
                Err(e) if e.to_string().is_empty() => break,
                Err(e) => {
                    eprintln!("Failed to accept connection: {e}")
                }
            };
        }
    });
    let icon =
        image::load_from_memory(include_bytes!("primitive/assets/uiua-logo-512.png")).unwrap();
    let icon = IconData {
        width: icon.width(),
        height: icon.height(),
        rgba: icon.into_rgba8().into_raw(),
    };
    eframe::run_native(
        "Uiua",
        eframe::NativeOptions {
            viewport: ViewportBuilder::default().with_icon(icon),
            ..Default::default()
        },
        Box::new(|cc| {
            cc.egui_ctx.set_theme(Theme::Dark);
            let mut fonts = FontDefinitions::default();
            fonts.font_data.insert(
                "Uiua386".into(),
                FontData::from_static(include_bytes!("algorithm/Uiua386.ttf")),
            );
            (fonts.families)
                .entry(FontFamily::Monospace)
                .or_default()
                .insert(0, "Uiua386".into());
            cc.egui_ctx.set_fonts(fonts);
            cc.egui_ctx
                .send_viewport_cmd(ViewportCommand::RequestUserAttention(
                    UserAttentionType::Informational,
                ));
            Ok(Box::new(App::new(recv, &cc.egui_ctx)))
        }),
    )
    .unwrap();
}

struct App {
    items: Vec<OutputItem>,
    recv: Receiver<Request>,
    cache: Cache,
    clear: bool,
    clear_before_next: bool,
    #[cfg(feature = "audio")]
    audio_output: Option<hodaun::OutputDeviceMixer<hodaun::Stereo>>,
}

struct Cache {
    ppp: f32,
    next_id: u64,
    size_map: HashMap<[u32; 2], Vec2>,
    #[cfg(feature = "audio")]
    samples_map: HashMap<usize, bool>,
    last_frame: Instant,
    errors: Vec<String>,
}

impl Cache {
    fn next_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

enum OutputItem {
    Text(String),
    Code(String),
    #[allow(dead_code)]
    Error(String),
    Image {
        tex_id: TextureId,
        state: ImageState,
        bytes: Vec<u8>,
    },
    Gif {
        frames: Vec<(TextureId, f32)>,
        curr: f32,
        play: bool,
        state: ImageState,
        bytes: Vec<u8>,
    },
    #[cfg(feature = "audio")]
    Audio {
        controls: audio::AudioControls,
        play: bool,
        label: Option<String>,
        total_time: f64,
        sample_count: usize,
        bytes: Vec<u8>,
    },
    Separator,
}

struct ImageState {
    id: u64,
    true_size: [u32; 2],
    resize: Vec2,
    label: Option<String>,
    changing: bool,
}

impl ImageState {
    fn new(cache: &mut Cache, true_size: [u32; 2], label: Option<String>) -> Self {
        Self {
            id: cache.next_id(),
            true_size,
            resize: cache
                .size_map
                .get(&true_size)
                .copied()
                .unwrap_or(vec2(true_size[0] as f32, true_size[1] as f32)),
            label,
            changing: false,
        }
    }
    fn show_reset(&mut self, cache: &mut Cache, ui: &mut Ui) {
        if (self.true_size[0] as f32 != self.resize.x || self.true_size[1] as f32 != self.resize.y)
            && ui.button("↻").on_hover_text("Reset size").clicked()
        {
            self.id = cache.next_id();
            self.resize = vec2(self.true_size[0] as f32, self.true_size[1] as f32);
            cache.size_map.remove(&self.true_size);
        }
    }
    fn handle_resize(&mut self, cache: &mut Cache, ui: &mut Ui, rect: Rect) {
        if self.changing && !ui.input(|i| i.pointer.primary_down()) {
            self.resize = rect.size() * cache.ppp;
            cache.size_map.insert(self.true_size, self.resize);
            self.id = cache.next_id();
            self.changing = false;
        }
    }
}

impl App {
    fn new(recv: Receiver<Request>, ctx: &Context) -> Self {
        let (ppp, clear) = ctx.memory_mut(|mem| {
            (
                mem.data.get_persisted(Id::new("ppp")).unwrap_or(1.5),
                mem.data.get_persisted(Id::new("clear")).unwrap_or(true),
            )
        });
        ctx.set_pixels_per_point(ppp);
        App {
            items: Vec::new(),
            recv,
            cache: Cache {
                ppp,
                next_id: 0,
                size_map: HashMap::new(),
                #[cfg(feature = "audio")]
                samples_map: HashMap::new(),
                last_frame: Instant::now(),
                errors: Vec::new(),
            },
            clear,
            clear_before_next: true,
            #[cfg(feature = "audio")]
            audio_output: hodaun::default_output().ok(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        while let Ok(req) = self.recv.try_recv() {
            if self.clear_before_next {
                self.clear_before_next = false;
                for item in self.items.drain(..) {
                    match item {
                        OutputItem::Image { tex_id, .. } => ctx.tex_manager().write().free(tex_id),
                        OutputItem::Gif { frames, .. } => {
                            for (tex_id, _) in frames {
                                ctx.tex_manager().write().free(tex_id);
                            }
                        }
                        _ => {}
                    }
                }
                #[cfg(feature = "audio")]
                {
                    self.audio_output = hodaun::default_output().ok();
                }
            }
            match req {
                Request::Ping => {}
                Request::ShowText(text) => self.items.push(OutputItem::Text(text)),
                Request::Show(so) => {
                    let item = self.convert_smart_output(so, ctx);
                    self.items.push(item);
                    self.clear_before_next = false;
                }
                Request::ShowAll(sos) => {
                    for so in sos.into_iter().rev() {
                        let item = self.convert_smart_output(so, ctx);
                        self.items.push(item);
                    }
                    self.clear_before_next = false;
                }
                Request::Separator => {
                    if !self.clear {
                        self.items.push(OutputItem::Separator)
                    }
                }
                Request::ClearBeforeNext => self.clear_before_next = self.clear,
                Request::Shutdown => ctx.send_viewport_cmd(ViewportCommand::Close),
            }
        }

        // Top bar
        TopBottomPanel::top("top bar").show(ctx, |ui| {
            ui.horizontal_wrapped(|ui| {
                ComboBox::new("ppp", "🔍")
                    .width(60.0)
                    .selected_text(format!("{:.0}%", self.cache.ppp * 100.0))
                    .show_ui(ui, |ui| {
                        for ppp in [0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0] {
                            let label = format!("{:.0}%", ppp * 100.0);
                            if ui
                                .selectable_value(&mut self.cache.ppp, ppp, label)
                                .clicked()
                            {
                                ui.ctx().set_pixels_per_point(ppp);
                            }
                        }
                    })
                    .response
                    .on_hover_text("UI Scale");
                ui.add_space(10.0);
                global_theme_preference_switch(ui);
                ui.add_space(10.0);
                Checkbox::new(&mut self.clear, "Clear")
                    .ui(ui)
                    .on_hover_text("Clear on each run");
                if !self.clear && ui.button("Clear All").clicked() {
                    self.items.clear();
                }
            })
        });

        // Main content
        CentralPanel::default().show(ctx, |ui| {
            ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| self.inner(ui))
        });

        // Error window
        if !self.cache.errors.is_empty() {
            let mut open = true;
            Window::new(format!(
                "Error{}",
                if self.cache.errors.len() > 1 { "s" } else { "" }
            ))
            .open(&mut open)
            .show(ctx, |ui| {
                for error in &self.cache.errors {
                    ui.label(RichText::new(error.as_str()).color(Color32::RED));
                }
            });
            if !open {
                self.cache.errors.clear();
            }
        }

        if ctx.input(|input| input.viewport().close_requested()) {
            ctx.data_mut(|data| {
                data.clear();
                data.insert_persisted(Id::new("ppp"), self.cache.ppp);
                data.insert_persisted(Id::new("clear"), self.clear);
            });
        }
        ctx.request_repaint_after_secs(0.1);
        self.cache.last_frame = Instant::now();
    }
}

fn save_name(ovrride: Option<&str>, name: &str, ext: &str) -> String {
    if let Some(ovr) = ovrride {
        return format!("{ovr}.{ext}");
    }
    let mut num: Option<u64> = None;
    loop {
        let path = if let Some(n) = num {
            format!("{name}({n}).{ext}")
        } else {
            format!("{name}.{ext}")
        };
        if !Path::new(path.as_str()).exists() {
            return path;
        }
        *num.get_or_insert(0) += 1;
    }
}

impl App {
    fn inner(&mut self, ui: &mut Ui) {
        for (i, item) in self.items.iter_mut().enumerate().rev() {
            ScrollArea::horizontal().id_salt(i).show(ui, |ui| {
                Self::item(&mut self.cache, item, ui);
                ui.add_space(3.0);
            });
        }
    }
    fn item(cache: &mut Cache, item: &mut OutputItem, ui: &mut Ui) {
        match item {
            OutputItem::Text(text) => {
                ui.label(&*text);
            }
            OutputItem::Code(code) => {
                ui.label(RichText::new(code.as_str()).font(FontId::monospace(14.0)));
            }
            OutputItem::Error(error) => {
                ui.label(RichText::new(error.as_str()).color(Color32::RED));
            }
            OutputItem::Separator => {
                ui.separator();
            }
            OutputItem::Image {
                tex_id,
                state,
                bytes,
            } => {
                if let Some(label) = &state.label {
                    ui.label(RichText::new(format!("{label}:")).font(FontId::monospace(14.0)));
                }
                ui.horizontal_wrapped(|ui| {
                    // Image
                    let render_size = state.resize / cache.ppp;
                    let resp = (Resize::default()
                        .id_salt(state.id)
                        .with_stroke(false)
                        .default_size(render_size))
                    .show(ui, |ui| {
                        let available_width = ui.available_width();
                        let available_height = ui.available_height();
                        let aspect_ratio = state.true_size[0] as f32 / state.true_size[1] as f32;
                        let use_height = (available_width / aspect_ratio).max(available_height);
                        let use_width = (use_height * aspect_ratio).max(available_width);
                        ui.image(SizedTexture {
                            id: *tex_id,
                            size: vec2(use_width, use_height),
                        })
                    });
                    ui.vertical(|ui| {
                        ui.label(format!("{}×{}", state.true_size[0], state.true_size[1]));
                        // Save
                        if ui.button("Save").clicked() {
                            match native_dialog::FileDialog::new()
                                .set_title("Save Image")
                                .set_filename(&save_name(state.label.as_deref(), "image", "png"))
                                .add_filter("PNG Image", &["png"])
                                .add_filter("JPEG Image", &["jpg", "jpeg"])
                                .add_filter("BMP Image", &["bmp"])
                                .add_filter("WebP Image", &["webp"])
                                .add_filter("QOI Image", &["qoi"])
                                .add_filter("All Files", &["*"])
                                .show_save_single_file()
                            {
                                Ok(Some(path)) => {
                                    let res = match path.extension().and_then(|ext| ext.to_str()) {
                                        Some("png") | None => {
                                            fs::write(path, bytes).map_err(|e| e.to_string())
                                        }
                                        Some(ext) => {
                                            if let Some((_, format)) = [
                                                ("jpg", image::ImageFormat::Jpeg),
                                                ("jpeg", image::ImageFormat::Jpeg),
                                                ("bmp", image::ImageFormat::Bmp),
                                                ("webp", image::ImageFormat::WebP),
                                                ("qoi", image::ImageFormat::Qoi),
                                            ]
                                            .into_iter()
                                            .find(|(s, _)| ext == *s)
                                            {
                                                image::load_from_memory(bytes)
                                                    .unwrap()
                                                    .save_with_format(path, format)
                                                    .map_err(|e| e.to_string())
                                            } else {
                                                Err(format!("Unsupported image format: {ext}"))
                                            }
                                        }
                                    };
                                    if let Err(e) = res {
                                        cache.errors.push(e);
                                    }
                                }
                                Ok(None) => {}
                                Err(e) => cache.errors.push(e.to_string()),
                            }
                        }
                        // Reset
                        state.show_reset(cache, ui);
                        // Resize
                        let chng = resp.rect.width() != render_size.x
                            && resp.rect.height() != render_size.y;
                        state.changing |= chng;
                        state.handle_resize(cache, ui, resp.rect);
                    });
                });
            }
            OutputItem::Gif {
                frames,
                curr,
                play,
                state,
                bytes,
            } => {
                if let Some(label) = &state.label {
                    ui.label(RichText::new(format!("{label}:")).font(FontId::monospace(14.0)));
                }
                let total_time: f32 = frames.iter().map(|(_, d)| d).sum();
                ui.horizontal_wrapped(|ui| {
                    // Frames
                    let render_size = state.resize / cache.ppp;
                    let resp = (Resize::default()
                        .id_salt(state.id)
                        .with_stroke(false)
                        .default_size(render_size))
                    .show(ui, |ui| {
                        let available_width = ui.available_width();
                        let available_height = ui.available_height();
                        let aspect_ratio = state.true_size[0] as f32 / state.true_size[1] as f32;
                        let use_height = (available_width / aspect_ratio).max(available_height);
                        let use_width = (use_height * aspect_ratio).max(available_width);
                        let mut t = 0.0;
                        for (tex_id, delay) in &*frames {
                            if t < *curr {
                                t += delay;
                                continue;
                            }
                            ui.ctx()
                                .request_repaint_after(Duration::from_secs_f32(*delay));
                            return ui.image(SizedTexture {
                                id: *tex_id,
                                size: vec2(use_width, use_height),
                            });
                        }
                        let (text_id, delay) = frames.last().unwrap();
                        ui.ctx()
                            .request_repaint_after(Duration::from_secs_f32(*delay));
                        ui.image(SizedTexture {
                            id: *text_id,
                            size: vec2(use_width, use_height),
                        })
                    });
                    let chng =
                        resp.rect.width() != render_size.x && resp.rect.height() != render_size.y;
                    state.changing |= chng;
                    state.handle_resize(cache, ui, resp.rect);
                    ui.vertical(|ui| {
                        ui.label(format!(
                            "{}×{}, {} frames",
                            state.true_size[0],
                            state.true_size[1],
                            frames.len()
                        ));
                        ui.horizontal_wrapped(|ui| {
                            // Play/pause
                            let play_text = if *play { "⏸" } else { "▶" };
                            ui.toggle_value(play, play_text).on_hover_text(if *play {
                                "Pause"
                            } else {
                                "Play"
                            });
                            // Time slider
                            Slider::new(curr, 0.0..=total_time)
                                .min_decimals(2)
                                .max_decimals(2)
                                .suffix(format!("/{total_time:.2}"))
                                .ui(ui);
                        });
                        // Save
                        if ui.button("Save").clicked() {
                            match native_dialog::FileDialog::new()
                                .set_title("Save Gif")
                                .set_filename(&save_name(state.label.as_deref(), "gif", "gif"))
                                .add_filter("GIF Image", &["gif"])
                                .add_filter("All Files", &["*"])
                                .show_save_single_file()
                            {
                                Ok(Some(path)) => {
                                    if let Err(e) = fs::write(path, bytes) {
                                        cache.errors.push(e.to_string());
                                    }
                                }
                                Ok(None) => {}
                                Err(e) => cache.errors.push(e.to_string()),
                            }
                        }
                        // Reset
                        state.show_reset(cache, ui);
                    });
                });
                if *play {
                    *curr += cache.last_frame.elapsed().as_secs_f32();
                    *curr %= total_time;
                }
            }
            #[cfg(feature = "audio")]
            OutputItem::Audio {
                controls,
                play,
                label,
                total_time,
                sample_count,
                bytes,
            } => {
                ui.horizontal_wrapped(|ui| {
                    if let Some(label) = label {
                        ui.label(RichText::new(format!("{label}:")).font(FontId::monospace(14.0)));
                    }
                    // Play/pause
                    let mut done = !controls.play.get() && controls.curr.get() == 0.0;
                    if done {
                        *play = false;
                    }
                    let play_text = if *play { "⏸" } else { "▶" };
                    let play_hint = if *play { "Pause" } else { "Play" };
                    if ui
                        .toggle_value(play, play_text)
                        .on_hover_text(play_hint)
                        .clicked()
                        && *play
                    {
                        done = false;
                    }
                    // Time slider
                    let mut t = controls.curr.get();
                    let orig_t = t;
                    let dragged = Slider::new(&mut t, 0.0..=*total_time)
                        .min_decimals(2)
                        .max_decimals(2)
                        .suffix(format!("/{total_time:.2}"))
                        .ui(ui)
                        .dragged;
                    done |= dragged;
                    if t != orig_t {
                        controls.curr.set(t);
                    }
                    let should_play = *play && !done && (!dragged || t != orig_t);
                    controls.play.set(should_play);
                    if should_play {
                        ui.ctx().request_repaint();
                    }
                    // Repeat
                    if ui
                        .selectable_label(controls.repeat.get(), "🔁")
                        .on_hover_text("Repeat")
                        .clicked()
                    {
                        controls.repeat.with(|r| *r = !*r);
                        cache
                            .samples_map
                            .insert(*sample_count, controls.repeat.get());
                    }
                    // Save
                    if ui.button("Save").clicked() {
                        match native_dialog::FileDialog::new()
                            .set_title("Save Audio")
                            .set_filename(&save_name(label.as_deref(), "audio", "wav"))
                            .add_filter("WAV Audio", &["wav"])
                            .add_filter("All Files", &["*"])
                            .show_save_single_file()
                        {
                            Ok(Some(path)) => {
                                if let Err(e) = fs::write(path, bytes) {
                                    cache.errors.push(e.to_string());
                                }
                            }
                            Ok(None) => {}
                            Err(e) => cache.errors.push(e.to_string()),
                        }
                    }
                });
            }
        }
    }
    fn convert_smart_output(&mut self, output: SmartOutput, ctx: &Context) -> OutputItem {
        const TEXTURE_OPTIONS: TextureOptions = TextureOptions {
            magnification: TextureFilter::Nearest,
            ..TextureOptions::LINEAR
        };
        match output {
            SmartOutput::Normal(value) => OutputItem::Code(value.show()),
            #[cfg(feature = "image")]
            SmartOutput::Png(bytes, label) => {
                let img = image::load_from_memory_with_format(&bytes, ImageFormat::Png).unwrap();
                let (width, height) = img.dimensions();
                let pixels: Vec<Color32> = img
                    .into_rgba8()
                    .into_raw()
                    .chunks_exact(4)
                    .map(|w| Color32::from_rgba_unmultiplied(w[0], w[1], w[2], w[3]))
                    .collect();
                let color_image = ColorImage {
                    size: [width as usize, height as usize],
                    pixels,
                };
                let text_id = ctx.tex_manager().write().alloc(
                    String::new(),
                    ImageData::Color(Arc::new(color_image)),
                    TEXTURE_OPTIONS,
                );
                OutputItem::Image {
                    tex_id: text_id,
                    state: ImageState::new(&mut self.cache, [width, height], label),
                    bytes,
                }
            }
            #[cfg(not(feature = "image"))]
            SmartOutput::Png(..) => {
                OutputItem::Error("Images are not supported in this environment".into())
            }
            #[cfg(feature = "gif")]
            SmartOutput::Gif(bytes, label) => {
                let mut decoder = gif::DecodeOptions::new();
                decoder.set_color_output(gif::ColorOutput::RGBA);
                let mut decoder = decoder.read_info(bytes.as_slice()).unwrap();
                let first_frame = decoder.read_next_frame().unwrap().unwrap();
                let gif_width = first_frame.width as u32;
                let gif_height = first_frame.height as u32;
                // Init frame data with the first frame
                let mut tex_ids = Vec::new();
                tex_ids.push((
                    ctx.tex_manager().write().alloc(
                        String::new(),
                        ImageData::Color(Arc::new(ColorImage {
                            size: [gif_width as usize, gif_height as usize],
                            pixels: first_frame
                                .buffer
                                .chunks(4)
                                .map(|w| Color32::from_rgba_unmultiplied(w[0], w[1], w[2], w[3]))
                                .collect(),
                        })),
                        TEXTURE_OPTIONS,
                    ),
                    first_frame.delay as f32 / 100.0,
                ));
                for frame in decoder {
                    let frame = frame.unwrap();
                    tex_ids.push((
                        ctx.tex_manager().write().alloc(
                            String::new(),
                            ImageData::Color(Arc::new(ColorImage {
                                size: [gif_width as usize, gif_height as usize],
                                pixels: frame
                                    .buffer
                                    .chunks(4)
                                    .map(|w| {
                                        Color32::from_rgba_unmultiplied(w[0], w[1], w[2], w[3])
                                    })
                                    .collect(),
                            })),
                            TEXTURE_OPTIONS,
                        ),
                        frame.delay as f32 / 100.0,
                    ));
                }
                OutputItem::Gif {
                    frames: tex_ids,
                    curr: 0.0,
                    play: true,
                    state: ImageState::new(&mut self.cache, [gif_width, gif_height], label),
                    bytes,
                }
            }
            #[cfg(not(feature = "gif"))]
            SmartOutput::Gif(..) => {
                OutputItem::Error("Gifs are not supported in this environment".into())
            }
            #[cfg(feature = "audio")]
            SmartOutput::Wav(bytes, label) => {
                let (src, sample_count) = audio::SeekBufferSource::from_wav_bytes(&bytes);
                let mut controls = src.controls.clone();
                let total_time = src.total_time;
                if let Some(output) = &self.audio_output {
                    output.add(src);
                }
                if let Some(repeat) = self.cache.samples_map.get(&sample_count) {
                    controls.repeat.set(*repeat);
                }
                OutputItem::Audio {
                    play: controls.play.get(),
                    controls,
                    label,
                    total_time,
                    sample_count,
                    bytes,
                }
            }
            #[cfg(all(not(feature = "audio"), feature = "audio_encode"))]
            SmartOutput::Wav(bytes, label) => {
                let mut value =
                    crate::Value::from(crate::encode::array_from_wav_bytes(&bytes).unwrap().0);
                value.set_label(label.map(Into::into));
                OutputItem::Code(value.show())
            }
            #[cfg(not(any(feature = "audio", feature = "audio_encode")))]
            SmartOutput::Wav(..) => {
                OutputItem::Error("Audio is not supported in this environment".into())
            }
        }
    }
}

#[cfg(feature = "audio")]
mod audio {
    use hodaun::*;
    pub struct SeekBufferSource {
        pub buffer: Vec<Stereo>,
        pub controls: AudioControls,
        pub total_time: f64,
    }

    #[derive(Clone)]
    pub struct AudioControls {
        pub play: Shared<bool>,
        pub curr: Shared<f64>,
        pub repeat: Shared<bool>,
    }

    impl SeekBufferSource {
        pub fn from_wav_bytes(bytes: &[u8]) -> (Self, usize) {
            let mut wav_source = wav::WavSource::new(bytes).unwrap().resample::<Stereo>();
            let sr = crate::SysBackend::audio_sample_rate(&crate::NativeSys) as f64;
            let mut buffer = Vec::new();
            while let Some(frame) = wav_source.next(sr) {
                buffer.push(frame);
            }
            let sample_count = buffer.len();
            let src = Self {
                total_time: buffer.len() as f64 / sr,
                buffer,
                controls: AudioControls {
                    curr: Shared::new(0.0),
                    play: Shared::new(true),
                    repeat: Shared::new(false),
                },
            };
            (src, sample_count)
        }
    }

    impl Source for SeekBufferSource {
        type Frame = Stereo;
        fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
            Some(if !self.controls.play.get() {
                0.0.into()
            } else if let Some(sample) = self
                .buffer
                .get((self.controls.curr.get() * sample_rate) as usize)
            {
                self.controls.curr.with(|curr| {
                    *curr += 1.0 / sample_rate;
                    if self.controls.repeat.get() {
                        *curr %= self.total_time;
                    }
                });
                *sample
            } else if self.controls.repeat.get() && !self.buffer.is_empty() {
                self.controls.curr.set(0.0);
                return self.next(sample_rate);
            } else {
                self.controls.play.set(false);
                self.controls.curr.set(0.0);
                0.0.into()
            })
        }
    }
}
