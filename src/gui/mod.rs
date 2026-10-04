//! The window: a winit application that owns the machine. Each redraw runs
//! the guest for `CPU_SLICE` of host time (`pacer`), draws the frame
//! (`renderer`) and asks for the next redraw; key presses and releases go
//! to the keyboard controller as scancodes (`keys`), which are all released
//! when the window loses the keyboard, and F12 asks for a VGA dump.
//!
//! The invariant: the guest runs only inside `RedrawRequested`, so a redraw
//! must always be pending. The loop asks for one after every frame and
//! whenever it is about to wait; a path that stops asking stops the
//! machine.

mod keys;
mod pacer;
mod renderer;

use std::sync::Arc;
use std::sync::mpsc;
use std::time::Duration;

use winit::application::ApplicationHandler;
use winit::event::{ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::PhysicalKey;
use winit::window::{Window, WindowId};

use emu8086_core::io::keyboard::EmulatorEvent;
use emu8086_core::vm::runtime::Runtime;
use keys::GuestKeys;
use pacer::Pacer;
use renderer::{DEFAULT_WINDOW_HEIGHT, DEFAULT_WINDOW_WIDTH, Renderer};

/// Host time the CPU runs for before each frame is drawn. The rest of the
/// frame period is left for rasterising and for waiting on vsync.
const CPU_SLICE: Duration = Duration::from_millis(10);

/// The file the VGA dump is written to, in the working directory.
const VGA_DUMP_FILE: &str = "vga_dump.txt";

struct EmulatorApp {
    runtime: Runtime,
    pacer: Pacer,
    renderer: Option<Renderer>,
    window: Option<Arc<Window>>,
    keys: GuestKeys,
}

impl EmulatorApp {
    /// Writes the machine's VGA dump to `VGA_DUMP_FILE` and says on stderr
    /// what became of it. The machine only gives the text: where it goes is
    /// the app's business.
    fn dump_vga(&self) {
        let Some(dump) = self.runtime.vga_dump() else {
            return;
        };
        match std::fs::write(VGA_DUMP_FILE, dump) {
            Ok(()) => eprintln!("VGA dumped to {VGA_DUMP_FILE}"),
            Err(error) => eprintln!("VGA dump failed: {error}"),
        }
    }
}

impl ApplicationHandler for EmulatorApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }

        let window = Arc::new(
            event_loop
                .create_window(
                    Window::default_attributes()
                        .with_title("Emulator 8086")
                        .with_inner_size(winit::dpi::PhysicalSize::new(
                            DEFAULT_WINDOW_WIDTH,
                            DEFAULT_WINDOW_HEIGHT,
                        )),
                )
                .expect("Failed to create window"),
        );

        let renderer = Renderer::new(window.clone(), event_loop.owned_display_handle());
        self.window = Some(window);
        self.renderer = Some(renderer);
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                if let Some(renderer) = &mut self.renderer {
                    renderer.resize(size.width, size.height);
                }
            }

            WindowEvent::KeyboardInput { event, .. } => {
                // F12 asks for a VGA dump and is not passed to the guest.
                if let PhysicalKey::Code(winit::keyboard::KeyCode::F12) = event.physical_key
                    && event.state == ElementState::Pressed
                {
                    self.dump_vga();
                    return;
                }

                self.keys.key(event.physical_key, event.state);
            }

            // The releases of the keys held now go to whoever has the
            // keyboard next, not to this window.
            WindowEvent::Focused(false) => self.keys.release_all(),

            WindowEvent::RedrawRequested => {
                self.pacer.run_slice(&mut self.runtime, CPU_SLICE);

                if let (Some(renderer), Some(vga)) = (&mut self.renderer, &self.runtime.vga) {
                    let vga = vga.borrow();
                    renderer.render(&vga, &self.runtime.cpu.memory);
                }

                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }

            _ => {}
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }
}

/// Opens the window and runs `runtime` in it until the window is closed.
/// `kbd_tx` feeds the keyboard controller the machine was built with.
pub fn run(runtime: Runtime, kbd_tx: mpsc::Sender<EmulatorEvent>) {
    log::debug!(
        "Starting emulator with boot order: {:02X?}",
        runtime.disks.borrow().boot_order()
    );

    let event_loop = EventLoop::new().expect("Failed to create event loop");
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = EmulatorApp {
        runtime,
        pacer: Pacer::new(),
        renderer: None,
        window: None,
        keys: GuestKeys::new(kbd_tx),
    };

    event_loop.run_app(&mut app).expect("Event loop failed");
}
