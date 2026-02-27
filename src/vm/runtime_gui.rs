#![cfg(feature = "gui")]

use std::sync::Arc;
use std::sync::mpsc;

use winit::application::ApplicationHandler;
use winit::event::{ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::PhysicalKey;
use winit::window::{Window, WindowId};

use crate::io::keyboard::EmulatorEvent;
use crate::io::keyboard_winit::physical_key_to_scancode;
use crate::io::renderer_wgpu::WgpuRenderer;
use crate::vm::runtime::Runtime;

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
                        .with_inner_size(winit::dpi::LogicalSize::new(960, 600)),
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
                if let PhysicalKey::Code(winit::keyboard::KeyCode::F12) = event.physical_key {
                    if event.state == ElementState::Pressed {
                        let _ = self.kbd_tx.send(EmulatorEvent::DumpVga);
                        return;
                    }
                }

                if let Some(scancode) = physical_key_to_scancode(event.physical_key) {
                    match event.state {
                        ElementState::Pressed => {
                            let _ = self.kbd_tx.send(EmulatorEvent::Scancode(scancode));
                        }
                        ElementState::Released => {
                            let _ =
                                self.kbd_tx.send(EmulatorEvent::ScanRelease(scancode | 0x80));
                        }
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                // Run CPU for one frame
                if !self.runtime.step_frame() {
                    event_loop.exit();
                    return;
                }

                // Render VGA
                if let (Some(renderer), Some(vga)) =
                    (&mut self.renderer, &self.runtime.vga)
                {
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
    /// GUI-mode main loop using winit + wgpu.
    pub fn run_gui(mut self) {
        log::debug!(
            "Starting emulator (GUI) with boot order: {:02X?}",
            self.boot_order
        );

        let event_loop = EventLoop::new().expect("Failed to create event loop");
        event_loop.set_control_flow(ControlFlow::Poll);

        // Create keyboard channel and replace the controller
        let (kbd_tx, kbd_rx) = mpsc::channel();
        let keyboard =
            crate::io::keyboard::KeyboardController::with_receiver(kbd_rx);
        use std::cell::RefCell;
        use std::rc::Rc;
        let keyboard = Rc::new(RefCell::new(keyboard));
        keyboard
            .borrow_mut()
            .set_pit(Rc::clone(self.pit_ref()));
        // Re-register keyboard on IO bus (port range 0x60-0x63)
        self.replace_keyboard(keyboard);

        let mut app = EmulatorApp {
            runtime: self,
            renderer: None,
            window: None,
            kbd_tx,
        };

        event_loop.run_app(&mut app).expect("Event loop failed");
    }
}
