use std::sync::Arc;
use std::sync::mpsc;
use std::time::Duration;

use winit::application::ApplicationHandler;
use winit::event::{ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::PhysicalKey;
use winit::window::{Window, WindowId};

use crate::io::keyboard::EmulatorEvent;
use crate::io::keyboard_winit::physical_key_to_scancode;
use crate::io::renderer_wgpu::{DEFAULT_WINDOW_HEIGHT, DEFAULT_WINDOW_WIDTH, WgpuRenderer};
use crate::vm::runtime::Runtime;

/// Host time the CPU runs for before each frame is drawn. The rest of the
/// frame period is left for rasterising and for waiting on vsync.
const CPU_SLICE: Duration = Duration::from_millis(10);

struct EmulatorApp {
    runtime: Runtime,
    renderer: Option<WgpuRenderer>,
    window: Option<Arc<Window>>,
    kbd_tx: mpsc::Sender<EmulatorEvent>,
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

        let renderer = WgpuRenderer::new(window.clone());
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
                // F12: dump VGA (CLI monitor not supported in GUI mode)
                if let PhysicalKey::Code(winit::keyboard::KeyCode::F12) = event.physical_key
                    && event.state == ElementState::Pressed
                {
                    let _ = self.kbd_tx.send(EmulatorEvent::DumpVga);
                    return;
                }

                if let Some(scancode) = physical_key_to_scancode(event.physical_key) {
                    match event.state {
                        ElementState::Pressed => {
                            let _ = self.kbd_tx.send(EmulatorEvent::Scancode(scancode));
                        }
                        ElementState::Released => {
                            let _ = self
                                .kbd_tx
                                .send(EmulatorEvent::ScanRelease(scancode | 0x80));
                        }
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                // Run the CPU for one time slice
                if !self.runtime.run_slice(CPU_SLICE) {
                    event_loop.exit();
                    return;
                }

                // Render VGA
                if let (Some(renderer), Some(vga)) = (&mut self.renderer, &self.runtime.vga) {
                    let vga = vga.borrow();
                    renderer.render(&vga, &self.runtime.cpu.memory);
                }

                // Request next frame
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

impl Runtime {
    /// GUI-mode main loop using winit + wgpu. `kbd_tx` feeds the keyboard
    /// controller the machine was built with.
    pub fn run_gui(self, kbd_tx: mpsc::Sender<EmulatorEvent>) {
        log::debug!(
            "Starting emulator (GUI) with boot order: {:02X?}",
            self.disks.borrow().boot_order()
        );

        let event_loop = EventLoop::new().expect("Failed to create event loop");
        event_loop.set_control_flow(ControlFlow::Poll);

        let mut app = EmulatorApp {
            runtime: self,
            renderer: None,
            window: None,
            kbd_tx,
        };

        event_loop.run_app(&mut app).expect("Event loop failed");
    }
}
