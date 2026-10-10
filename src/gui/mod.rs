//! The window: a winit application around a `Session`, which is the
//! machine, its pacer and meter, the guest's keyboard and the overlay. This
//! file is the glue that needs the window: it hands the window's events to
//! egui and to the session, and each redraw runs a frame of the session and
//! draws it (`renderer`): the emulator's picture, and over it the overlay.
//!
//! The invariants:
//!
//! - The guest runs only inside `RedrawRequested`, so a redraw must always
//!   be pending. The loop asks for one after every frame and whenever it is
//!   about to wait; a path that stops asking stops the machine.
//! - The window comes first and the renderer when its start-up is over,
//!   which on some hosts is after the event loop has turned
//!   (`platform::spawn`). The renderer is taken from where its start-up
//!   puts it, never assumed to be there: until it is, a redraw runs no
//!   frame, so the machine starts with its picture.
//! - The overlay's switch is the host's (`platform::OVERLAY_SWITCH`). A key
//!   that is the switch is the window's: neither egui nor the guest sees
//!   it. Every other event goes to egui first: a key goes on to the guest
//!   unless egui kept it or the overlay has the keyboard (`Session::key`),
//!   and a button of the pointer that is the switch works it unless egui
//!   kept the press (`Session::overlay_button`). On every host a finger
//!   held still on the picture works the switch as well: the session is
//!   told of every finger (`Session::touch`), and asked at every frame
//!   whether one's time is up (`Session::hold`), since a finger that rests
//!   sends no event.
//! - A hidden overlay costs a frame nothing: no input is gathered for it,
//!   no pass run, nothing painted. Events still go through egui's window
//!   state, so that it knows where the pointer is and which modifiers are
//!   down when the overlay is shown, and the input they leave behind is
//!   dropped on the spot.

mod disk_tools;
mod hold;
mod host;
mod keys;
mod meter;
mod overlay;
mod pacer;
mod renderer;
mod session;

use std::sync::Arc;
use std::sync::mpsc::{self, Receiver};

use winit::application::ApplicationHandler;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::WindowEvent;
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::PhysicalKey;
use winit::window::{Window, WindowId};

use crate::platform::{self, OVERLAY_SWITCH, OverlayInput};
use emu8086_core::io::keyboard::EmulatorEvent;
use emu8086_core::vm::runtime::Runtime;
use renderer::{DEFAULT_WINDOW_HEIGHT, DEFAULT_WINDOW_WIDTH, Renderer};
use session::Session;

/// The window's title, and its title while the machine is paused: the mark
/// in the overlay's menu bar is not there to see when the overlay is hidden.
const TITLE: &str = "Emulator 8086";
const TITLE_PAUSED: &str = "Emulator 8086 (paused)";

/// What there is from the moment the window is: the window, egui's side of
/// it, and what draws into it once that has started up.
struct Front {
    window: Arc<Window>,
    /// What draws into the window. `None` until its start-up is over.
    renderer: Option<Renderer>,
    /// Where the renderer's start-up puts it.
    starting: Receiver<Renderer>,
    /// The window's events as egui's input, and what egui asks of the
    /// window in return: the cursor, the IME.
    egui: OverlayInput,
    /// Whether the title says that the machine is paused.
    titled_paused: bool,
}

impl Front {
    /// Takes the renderer if its start-up is over, and tells it and egui
    /// what neither could know before: the size the window has by now, and
    /// how large a texture the device takes.
    fn adopt_renderer(&mut self) {
        if self.renderer.is_some() {
            return;
        }
        let Ok(mut renderer) = self.starting.try_recv() else {
            return;
        };
        // A resize that came while the renderer was on its way found no
        // renderer to tell.
        let size = self.window.inner_size();
        if renderer.size() != (size.width, size.height) {
            renderer.resize(size.width, size.height);
        }
        self.egui.set_max_texture_side(renderer.max_texture_side());
        self.renderer = Some(renderer);
    }
}

struct EmulatorApp {
    session: Session,
    front: Option<Front>,
}

impl EmulatorApp {
    /// The overlay's switch was worked: `switch` is what the session makes
    /// of it, which may show the overlay or hide it.
    fn switch_overlay(&mut self, switch: impl FnOnce(&mut Session)) {
        let shown = self.session.overlay.visible();
        switch(&mut self.session);
        // A hidden overlay asks nothing of the window anymore: the cursor
        // and the IME go back to what a window without egui has.
        if shown
            && !self.session.overlay.visible()
            && let Some(front) = &mut self.front
        {
            front
                .egui
                .handle_output(&front.window, egui::PlatformOutput::default());
        }
    }

    /// `position`, in the window's physical pixels, in egui's points: the
    /// pixels per point are the window's scale factor times egui's zoom.
    /// `None` before there is a window.
    fn points(&self, position: PhysicalPosition<f64>) -> Option<egui::Pos2> {
        let front = self.front.as_ref()?;
        let zoom = self.session.overlay.context().zoom_factor();
        let pixels_per_point = front.window.scale_factor() as f32 * zoom;
        Some(egui::pos2(
            position.x as f32 / pixels_per_point,
            position.y as f32 / pixels_per_point,
        ))
    }

    /// Hands `event` to egui. Returns whether egui kept it for itself, which
    /// only a shown overlay can.
    fn offer_to_overlay(&mut self, event: &WindowEvent) -> bool {
        let Some(front) = &mut self.front else {
            return false;
        };
        let kept = front.egui.on_window_event(&front.window, event);
        if !self.session.overlay.draws() {
            // No pass will take the input this event became: left where
            // it is, it would pile up and be replayed when the overlay is
            // shown.
            front.egui.discard();
        }
        self.session.overlay.visible() && kept
    }

    /// One frame: the session runs the guest and the overlay, the renderer
    /// draws both, and the next redraw is asked for. Without a renderer yet
    /// there is no frame, only the asking. First, a finger held still on
    /// the picture may have worked the overlay's switch since the last one.
    fn redraw(&mut self) {
        let now = platform::Instant::now();
        self.switch_overlay(|session| session.hold(now));

        let Some(front) = &mut self.front else {
            return;
        };
        front.adopt_renderer();
        let Some(renderer) = &mut front.renderer else {
            front.window.request_redraw();
            return;
        };

        let passed = self.session.frame(|| front.egui.take(&front.window));
        if passed {
            let wishes = self.session.overlay.take_platform_output();
            front.egui.handle_output(&front.window, wishes);
        }

        if let Some(vga) = &self.session.runtime.vga {
            let overlay = passed.then_some(&mut self.session.overlay);
            let memory = &self.session.runtime.cpu.memory;
            let drawn = renderer.render(&vga.borrow(), memory, overlay);
            self.session.drawn(drawn);
        }

        let paused = self.session.paused();
        if front.titled_paused != paused {
            front.titled_paused = paused;
            platform::set_title(&front.window, if paused { TITLE_PAUSED } else { TITLE });
        }

        front.window.request_redraw();
    }
}

impl ApplicationHandler for EmulatorApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.front.is_some() {
            return;
        }

        let window = Arc::new(platform::create_window(
            event_loop,
            TITLE,
            PhysicalSize::new(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT),
        ));
        let egui = OverlayInput::new(self.session.overlay.context().clone(), &window);
        self.session.shown_in(&window);

        // The renderer's start-up ends when the host lets it, and leaves
        // the renderer in this channel: `adopt_renderer` looks there.
        let (ready, starting) = mpsc::channel();
        let start_up = Renderer::new(window.clone(), event_loop.owned_display_handle());
        platform::spawn(async move {
            // The window may be gone by then, and nobody waiting.
            let _ = ready.send(start_up.await);
        });

        let mut front = Front {
            window,
            renderer: None,
            starting,
            egui,
            titled_paused: false,
        };
        // On a host that may block, the start-up is over already.
        front.adopt_renderer();
        self.front = Some(front);
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
            && OVERLAY_SWITCH
                .key
                .is_some_and(|switch| key.physical_key == PhysicalKey::Code(switch))
        {
            // A key the host reports as down because the window got the
            // keyboard was not pressed for this window.
            if !is_synthetic {
                let (state, repeat) = (key.state, key.repeat);
                self.switch_overlay(|session| session.overlay_key(state, repeat));
            }
            return;
        }

        let kept = self.offer_to_overlay(&event);

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                if let Some(front) = &mut self.front
                    && let Some(renderer) = &mut front.renderer
                {
                    renderer.resize(size.width, size.height);
                }
            }

            WindowEvent::KeyboardInput { event, .. } => {
                self.session.key(event.physical_key, event.state, kept);
            }

            WindowEvent::MouseInput { state, button, .. }
                if OVERLAY_SWITCH.button == Some(button) =>
            {
                self.switch_overlay(|session| session.overlay_button(state, kept));
            }

            WindowEvent::Touch(touch) => {
                if let Some(at) = self.points(touch.location) {
                    let now = platform::Instant::now();
                    self.session.touch(touch.id, touch.phase, at, now);
                }
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

/// Opens the window and runs `runtime` in it, in the host's event loop
/// (`platform::run_app`). `kbd_tx` feeds the keyboard controller the
/// machine was built with.
pub fn run(runtime: Runtime, kbd_tx: mpsc::Sender<EmulatorEvent>) {
    log::debug!(
        "Starting emulator with boot order: {:02X?}",
        runtime.disks.borrow().boot_order()
    );

    platform::run_app(EmulatorApp {
        session: Session::new(runtime, kbd_tx),
        front: None,
    });
}
