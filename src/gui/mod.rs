//! The window: a winit application around a `Session`, which is the
//! machine, its pacer, the guest's keyboard and the overlay. This file is
//! the glue that needs the window: it hands the window's events to egui and
//! to the session, and each redraw runs a frame of the session and draws it
//! (`renderer`): the emulator's picture, and over it the overlay.
//!
//! The invariants:
//!
//! - The guest runs only inside `RedrawRequested`, so a redraw must always
//!   be pending. The loop asks for one after every frame and whenever it is
//!   about to wait; a path that stops asking stops the machine.
//! - The overlay's key is the window's: neither egui nor the guest sees it.
//!   Every other event goes to egui first, and a key goes on to the guest
//!   unless egui kept it or the overlay has the keyboard (`Session::key`).
//! - A hidden overlay costs a frame nothing: no input is gathered for it,
//!   no pass run, nothing painted. Events still go through egui's window
//!   state, so that it knows where the pointer is and which modifiers are
//!   down when the overlay is shown, and the input they leave behind is
//!   dropped on the spot.

mod keys;
mod overlay;
mod pacer;
mod renderer;
mod session;

use std::sync::Arc;
use std::sync::mpsc;

use winit::application::ApplicationHandler;
use winit::event::{ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowId};

use emu8086_core::io::keyboard::EmulatorEvent;
use emu8086_core::vm::runtime::Runtime;
use renderer::{DEFAULT_WINDOW_HEIGHT, DEFAULT_WINDOW_WIDTH, Renderer};
use session::Session;

/// The key that shows and hides the overlay.
const OVERLAY_KEY: PhysicalKey = PhysicalKey::Code(KeyCode::F12);

/// The window's title, and its title while the machine is paused: the mark
/// in the overlay's menu bar is not there to see when the overlay is hidden.
const TITLE: &str = "Emulator 8086";
const TITLE_PAUSED: &str = "Emulator 8086 (paused)";

/// What there is from the moment the window is: the window, what draws
/// into it, and egui's side of it.
struct Front {
    window: Arc<Window>,
    renderer: Renderer,
    /// The window's events as egui's input, and what egui asks of the
    /// window in return: the cursor, the IME.
    egui: egui_winit::State,
    /// Whether the title says that the machine is paused.
    titled_paused: bool,
}

struct EmulatorApp {
    session: Session,
    front: Option<Front>,
}

impl EmulatorApp {
    /// The overlay's key went down or came up.
    fn overlay_key(&mut self, state: ElementState, repeat: bool) {
        let shown = self.session.overlay.visible();
        self.session.overlay_key(state, repeat);
        // A hidden overlay asks nothing of the window anymore: the cursor
        // and the IME go back to what a window without egui has.
        if shown
            && !self.session.overlay.visible()
            && let Some(front) = &mut self.front
        {
            front
                .egui
                .handle_platform_output(&front.window, egui::PlatformOutput::default());
        }
    }

    /// Hands `event` to egui. Returns whether egui kept it for itself, which
    /// only a shown overlay can.
    fn offer_to_overlay(&mut self, event: &WindowEvent) -> bool {
        let Some(front) = &mut self.front else {
            return false;
        };
        let response = front.egui.on_window_event(&front.window, event);
        if !self.session.overlay.draws() {
            // No pass will take the input this event became: left where
            // it is, it would pile up and be replayed when the overlay is
            // shown.
            let input = front.egui.egui_input_mut();
            input.events.clear();
            input.hovered_files.clear();
            input.dropped_files.clear();
        }
        self.session.overlay.visible() && response.consumed
    }

    /// One frame: the session runs the guest and the overlay, the renderer
    /// draws both, and the next redraw is asked for.
    fn redraw(&mut self) {
        let Some(front) = &mut self.front else {
            return;
        };
        let passed = self
            .session
            .frame(|| front.egui.take_egui_input(&front.window));
        if passed {
            let wishes = self.session.overlay.take_platform_output();
            front.egui.handle_platform_output(&front.window, wishes);
        }

        if let Some(vga) = &self.session.runtime.vga {
            let overlay = passed.then_some(&mut self.session.overlay);
            let memory = &self.session.runtime.cpu.memory;
            front.renderer.render(&vga.borrow(), memory, overlay);
        }

        let paused = self.session.paused();
        if front.titled_paused != paused {
            front.titled_paused = paused;
            front
                .window
                .set_title(if paused { TITLE_PAUSED } else { TITLE });
        }

        front.window.request_redraw();
    }
}

impl ApplicationHandler for EmulatorApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.front.is_some() {
            return;
        }

        let window = Arc::new(
            event_loop
                .create_window(
                    Window::default_attributes()
                        .with_title(TITLE)
                        .with_inner_size(winit::dpi::PhysicalSize::new(
                            DEFAULT_WINDOW_WIDTH,
                            DEFAULT_WINDOW_HEIGHT,
                        )),
                )
                .expect("Failed to create window"),
        );

        let renderer = Renderer::new(window.clone(), event_loop.owned_display_handle());
        let egui = egui_winit::State::new(
            self.session.overlay.context().clone(),
            egui::ViewportId::ROOT,
            window.as_ref(),
            Some(window.scale_factor() as f32),
            window.theme(),
            Some(renderer.max_texture_side()),
        );
        self.front = Some(Front {
            window,
            renderer,
            egui,
            titled_paused: false,
        });
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        if let WindowEvent::KeyboardInput {
            event: key,
            is_synthetic,
            ..
        } = &event
            && key.physical_key == OVERLAY_KEY
        {
            // A key the host reports as down because the window got the
            // keyboard was not pressed for this window.
            if !is_synthetic {
                self.overlay_key(key.state, key.repeat);
            }
            return;
        }

        let kept = self.offer_to_overlay(&event);

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                if let Some(front) = &mut self.front {
                    front.renderer.resize(size.width, size.height);
                }
            }

            WindowEvent::KeyboardInput { event, .. } => {
                self.session.key(event.physical_key, event.state, kept);
            }

            WindowEvent::Focused(false) => self.session.focus_lost(),

            WindowEvent::RedrawRequested => self.redraw(),

            _ => {}
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        if let Some(front) = &self.front {
            front.window.request_redraw();
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
        session: Session::new(runtime, kbd_tx),
        front: None,
    };

    event_loop.run_app(&mut app).expect("Event loop failed");
}
