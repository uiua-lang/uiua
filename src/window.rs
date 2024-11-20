use std::{
    env::current_exe,
    io::{ErrorKind, Read, Write},
    net::{SocketAddr, TcpListener, TcpStream},
    process::{exit, Command, Stdio},
    thread::{self, sleep},
    time::Duration,
};

use crossbeam_channel::Receiver;
use eframe::egui::*;
use serde::*;

use crate::Value;

const PORT: u16 = 8482;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Request {
    Show(Vec<Value>),
}

impl Request {
    pub fn send(self) {
        self.send_impl(true);
    }
    fn send_impl(self, allow_creation: bool) {
        let socket_addr = ([127, 0, 0, 1], PORT).into();
        let mut stream =
            match TcpStream::connect_timeout(&socket_addr, Duration::from_secs_f32(0.5)) {
                Ok(stream) => stream,
                Err(e) if allow_creation && e.kind() == ErrorKind::TimedOut => {
                    Command::new(current_exe().unwrap())
                        .arg("window")
                        .stdout(if cfg!(debug_assertions) {
                            Stdio::inherit()
                        } else {
                            Stdio::null()
                        })
                        .spawn()
                        .unwrap();
                    sleep(Duration::from_secs_f32(0.5));
                    return self.send_impl(false);
                }
                Err(e) => {
                    eprintln!("Failed to connect to window: {e}");
                    exit(1);
                }
            };
        let json = serde_json::to_string(&self).unwrap();
        stream.write_all(json.as_bytes()).unwrap();
        stream.flush().unwrap();
    }
}

pub fn run_window() {
    eframe::run_native(
        "Uiua",
        eframe::NativeOptions::default(),
        Box::new(|cc| {
            cc.egui_ctx.set_visuals(Visuals::dark());
            Ok(Box::new(App::default()))
        }),
    )
    .unwrap();
}

struct App {
    stack: Vec<Value>,
    recv: Receiver<Request>,
}

impl Default for App {
    fn default() -> Self {
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
            eprintln!("Listening on port {PORT}");
            loop {
                match listener.accept() {
                    Ok((mut stream, _)) => {
                        let mut buffer = String::new();
                        stream.read_to_string(&mut buffer).unwrap();
                        let req: Request = serde_json::from_str(&buffer).unwrap();
                        send.send(req).unwrap();
                    }
                    // Break if the window is closed
                    Err(e) if e.kind() == ErrorKind::ConnectionReset => break,
                    Err(e) => {
                        eprintln!("Failed to accept connection: {e}");
                        continue;
                    }
                };
            }
        });
        App {
            stack: Vec::new(),
            recv,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        for req in self.recv.try_iter() {
            match req {
                Request::Show(stack) => self.stack = stack,
            }
        }
        CentralPanel::default().show(ctx, |_ui| {});
    }
}
