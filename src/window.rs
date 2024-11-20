use std::{
    collections::HashMap,
    env::current_exe,
    io::{ErrorKind, Read, Write},
    net::{SocketAddr, TcpListener, TcpStream},
    process::{exit, Command, Stdio},
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
    thread,
    time::Duration,
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
    USE_WINDOW.store(use_window, atomic::Ordering::Relaxed);
}

const PORT: u16 = 8482;

#[derive(Clone, Serialize, Deserialize)]
pub enum Request {
    ShowText(String),
    Show(SmartOutput),
    ShowAll(Vec<SmartOutput>),
    Separator,
    ClearBeforeNext,
    Shutdown,
}

const RETRIES: usize = 10;

impl Request {
    pub fn send(self) -> Result<(), String> {
        self.send_impl(RETRIES)
    }
    fn send_impl(self, retries: usize) -> Result<(), String> {
        let socket_addr = ([127, 0, 0, 1], PORT).into();
        let timeout = Duration::from_secs_f32(if retries + 1 == RETRIES { 1.0 } else { 0.1 });
        let mut stream = match TcpStream::connect_timeout(&socket_addr, timeout) {
            Ok(stream) => stream,
            Err(e) if retries > 0 && e.kind() == ErrorKind::TimedOut => {
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
                return self.send_impl(retries - 1);
            }
            Err(e) => {
                return Err(format!("Failed to connect to window: {e}"));
            }
        };
        let bin = rmp_serde::to_vec(&self).unwrap();
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
                Ok((mut stream, addr)) => {
                    if cfg!(debug_assertions) {
                        eprintln!("Accepted connection from {addr}");
                    }
                    let mut buffer = Vec::new();
                    stream.read_to_end(&mut buffer).unwrap();
                    match rmp_serde::from_slice(&buffer) {
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
            Ok(Box::new(App::new(recv, &cc.egui_ctx)))
        }),
    )
    .unwrap();
}

struct App {
    items: Vec<OutputItem>,
    recv: Receiver<Request>,
    next_id: u64,
    ppp: f32,
    clear: bool,
    clear_before_next: bool,
    size_map: HashMap<[u32; 2], Vec2>,
}

enum OutputItem {
    Text(String),
    Code(String),
    Error(String),
    Image {
        id: u64,
        tex_id: TextureId,
        true_size: [u32; 2],
        resize: Vec2,
        label: Option<String>,
    },
    Separator,
}

impl App {
    fn new(recv: Receiver<Request>, ctx: &Context) -> Self {
        let (ppp, clear) = ctx.memory_mut(|mem| {
            (
                mem.data.get_persisted(Id::new("ppp")).unwrap_or(1.5),
                mem.data.get_persisted(Id::new("clear")).unwrap_or(false),
            )
        });
        ctx.set_pixels_per_point(ppp);
        App {
            items: Vec::new(),
            recv,
            next_id: 0,
            ppp,
            clear,
            clear_before_next: false,
            size_map: HashMap::new(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        while let Ok(req) = self.recv.try_recv() {
            if self.clear_before_next {
                self.clear_before_next = false;
                self.items.clear();
            }
            match req {
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
                Request::Separator => self.items.push(OutputItem::Separator),
                Request::ClearBeforeNext => self.clear_before_next = self.clear,
                Request::Shutdown => ctx.send_viewport_cmd(ViewportCommand::Close),
            }
        }
        TopBottomPanel::top("top bar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ComboBox::new("ppp", "🔍")
                    .selected_text(format!("{:.0}%", self.ppp * 100.0))
                    .show_ui(ui, |ui| {
                        for ppp in [0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0] {
                            let label = format!("{:.0}%", ppp * 100.0);
                            if ui.selectable_value(&mut self.ppp, ppp, label).clicked() {
                                ui.ctx().set_pixels_per_point(ppp);
                            }
                        }
                    });
                Checkbox::new(&mut self.clear, "Clear")
                    .ui(ui)
                    .on_hover_text("Clear on each run");
                if !self.clear && ui.button("Clear All").clicked() {
                    self.items.clear();
                }
            })
        });
        CentralPanel::default().show(ctx, |ui| {
            ScrollArea::both()
                .auto_shrink([false; 2])
                .show(ui, |ui| self.inner(ui))
        });
        if ctx.input(|input| input.viewport().close_requested()) {
            ctx.data_mut(|data| {
                data.clear();
                data.insert_persisted(Id::new("ppp"), self.ppp);
                data.insert_persisted(Id::new("clear"), self.clear);
            });
        }
        ctx.request_repaint_after_secs(0.1);
    }
}

impl App {
    fn inner(&mut self, ui: &mut Ui) {
        for item in self.items.iter_mut().rev() {
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
                    id,
                    tex_id,
                    true_size,
                    resize,
                    label,
                } => {
                    if let Some(label) = label {
                        ui.code(format!("{label}:"));
                    }
                    ui.horizontal(|ui| {
                        let render_size = *resize / self.ppp;
                        let resp = (Resize::default().id_salt(*id).default_size(render_size)).show(
                            ui,
                            |ui| {
                                let available_width = ui.available_width();
                                let available_height = ui.available_height();
                                let aspect_ratio = true_size[0] as f32 / true_size[1] as f32;
                                let use_height =
                                    (available_width / aspect_ratio).min(available_height);
                                let use_width = (use_height * aspect_ratio).min(available_width);
                                ui.image(SizedTexture {
                                    id: *tex_id,
                                    size: vec2(use_width, use_height),
                                })
                            },
                        );
                        let changed = resp.rect.width() != render_size.x
                            && resp.rect.height() != render_size.y;
                        if changed {
                            *resize = resp.rect.size() * self.ppp;
                            self.size_map.insert(*true_size, *resize);
                        }
                        if ui.button("↻").on_hover_text("Reset size").clicked() {
                            *id = self.next_id;
                            self.next_id += 1;
                            *resize = vec2(true_size[0] as f32, true_size[1] as f32);
                            self.size_map.remove(true_size);
                        }
                    });
                }
            }
        }
    }
    fn convert_smart_output(&mut self, output: SmartOutput, ctx: &Context) -> OutputItem {
        match output {
            SmartOutput::Normal(value) => OutputItem::Code(value.show()),
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
                    Default::default(),
                );
                let unique = self.next_id;
                self.next_id += 1;
                OutputItem::Image {
                    id: unique,
                    tex_id: text_id,
                    true_size: [width, height],
                    resize: self
                        .size_map
                        .get(&[width, height])
                        .copied()
                        .unwrap_or(vec2(width as f32, height as f32)),
                    label,
                }
            }
            SmartOutput::Gif(..) => OutputItem::Error("Gif not yet implemented".into()),
            SmartOutput::Wav(..) => OutputItem::Error("Audio not yet implemented".into()),
        }
    }
}
