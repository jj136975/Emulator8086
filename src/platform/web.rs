//! The browser side of `platform`: a page, reached through wasm-bindgen.
//! It has no command line, no files and no stderr, and one thread, which is
//! the page's own.
//!
//! The invariant: nothing here blocks. Whatever takes time (the GPU's
//! start-up, the wait for the next frame) is a task or a callback of the
//! page's event loop, and `wait` says that it cannot wait: a call that did
//! not return would freeze the page with the machine in it.

use std::future::Future;
use std::path::Path;
use std::time::Duration;

use wasm_bindgen::closure::Closure;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::console;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::{MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::platform::web::{EventLoopExtWebSys, WindowAttributesExtWebSys, WindowExtWebSys};
use winit::window::Window;

use super::OverlaySwitch;
use super::egui_events::{CursorWish, EguiEvents};
use super::fat_stamp;
use crate::Options;

/// The most detailed level of the log that reaches the console. The console
/// has a filter of its own; what is cut here is the cost of formatting, on
/// every frame, the debug lines of the GPU stack and the machine's trace.
const CONSOLE_LEVEL: log::LevelFilter = log::LevelFilter::Info;

/// How the canvas lies in the page: over the whole viewport, whatever else
/// the page holds, and without the ring a browser draws around what has the
/// keyboard. Its size is the style's alone: winit reports the size the
/// style gives it as the window's, and the renderer makes the canvas's
/// drawing buffer as large.
const CANVAS_STYLE: [(&str, &str); 6] = [
    ("position", "fixed"),
    ("left", "0"),
    ("top", "0"),
    ("width", "100%"),
    ("height", "100%"),
    ("outline", "none"),
];

/// How long the browser has to start reading a download before the text is
/// given up, in milliseconds.
const DOWNLOAD_KEPT_MS: i32 = 60_000;

/// How the overlay is shown and hidden: with the secondary button of the
/// pointer, on the picture. A page has no key for it: F12, the desktop's,
/// opens the browser's developer tools and never reaches the page, and the
/// other keys a browser leaves alone are the guest's. The pointer is free,
/// since the machine has no mouse.
pub const OVERLAY_SWITCH: OverlaySwitch = OverlaySwitch {
    key: None,
    button: Some(MouseButton::Right),
    name: "Right-click",
};

/// Sends the log to the page's console, and has a panic told there too.
pub fn init() {
    std::panic::set_hook(Box::new(report_panic));
    // A logger can be set once, and `init` is called once.
    if log::set_logger(&ConsoleLogger).is_ok() {
        log::set_max_level(CONSOLE_LEVEL);
    }
}

/// The log on the console. Each record goes to the console's method for its
/// level, so that the console's own filter and colours apply to it.
struct ConsoleLogger;

impl log::Log for ConsoleLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        metadata.level() <= CONSOLE_LEVEL
    }

    fn log(&self, record: &log::Record<'_>) {
        if !self.enabled(record.metadata()) {
            return;
        }
        let line = JsValue::from_str(&format!("{}: {}", record.target(), record.args()));
        match record.level() {
            log::Level::Error => console::error_1(&line),
            log::Level::Warn => console::warn_1(&line),
            log::Level::Info => console::info_1(&line),
            log::Level::Debug => console::log_1(&line),
            log::Level::Trace => console::debug_1(&line),
        }
    }

    fn flush(&self) {}
}

/// Tells a panic on the console: its message and where it was raised, then
/// the stack of the page's script at that moment. A panic of wasm32 ends in
/// a trap, and without this the console would show the trap and nothing of
/// the panic.
fn report_panic(info: &std::panic::PanicHookInfo<'_>) {
    let mut report = info.to_string();
    // `stack` is in no standard, and in every browser.
    let stack = js_sys::Reflect::get(&js_sys::Error::new("panic"), &JsValue::from_str("stack"));
    if let Some(stack) = stack.ok().and_then(|stack| stack.as_string()) {
        report.push_str("\n\nStack:\n");
        report.push_str(&stack);
    }
    console::error_1(&JsValue::from_str(&report));
}

/// What the start asks of the machine: nothing, since a page has no command
/// line. The machine starts with the built-in BIOS and no disk.
pub fn options() -> Options {
    Options::default()
}

/// Tells the user why the app cannot start: on the console, which is where
/// a page tells its errors. There is no process to end.
pub fn fail(message: &str) {
    console::error_1(&JsValue::from_str(&format!("error: {message}")));
}

/// Runs `app` in the page's event loop and returns to the page at once: the
/// loop goes on for as long as the page is open.
pub fn run_app(app: impl ApplicationHandler + 'static) {
    let event_loop = EventLoop::new().expect("Failed to create event loop");
    // The loop waits for the page's events. What keeps the frames coming is
    // the redraw each frame asks for, which the browser answers before its
    // next repaint. Polling instead would have winit post itself a task
    // after every turn, and keep the page's thread busy between two frames.
    event_loop.set_control_flow(ControlFlow::Wait);
    event_loop.spawn_app(app);
}

/// Opens the window the machine is shown in: a canvas, put into the page
/// and as large as its viewport, which is why `_size` is not heeded.
pub fn create_window(
    event_loop: &ActiveEventLoop,
    title: &str,
    _size: PhysicalSize<u32>,
) -> Window {
    let window = event_loop
        .create_window(
            Window::default_attributes()
                .with_title(title)
                .with_append(true),
        )
        .expect("Failed to create window");
    if let Some(canvas) = window.canvas() {
        let style = canvas.style();
        for (property, value) in CANVAS_STYLE {
            // A canvas the browser refuses a style for keeps its own size,
            // and still shows the machine.
            let _ = style.set_property(property, value);
        }
    }
    set_title(&window, title);
    window
}

/// Changes the title of the page, which is what a browser shows of it: on
/// its tab.
pub fn set_title(_window: &Window, title: &str) {
    if let Some(document) = web_sys::window().and_then(|window| window.document()) {
        document.set_title(title);
    }
}

/// Runs `task` to its end, later: as a task of the page's event loop, to
/// which this call returns first. What the task makes is not there when
/// `spawn` returns.
pub fn spawn(task: impl Future<Output = ()> + 'static) {
    wasm_bindgen_futures::spawn_local(task);
}

/// The window's events as egui's input, and what a pass of egui asks of the
/// window in return: here, this crate's own translation (`EguiEvents`),
/// since egui-winit does not build for a browser.
pub struct OverlayInput(EguiEvents);

impl OverlayInput {
    /// The input of the passes of `ctx`, gathered from `window`.
    pub fn new(ctx: egui::Context, window: &Window) -> Self {
        Self(EguiEvents::new(ctx, window.scale_factor()))
    }

    /// The longest side a texture of the overlay may have: the limit of
    /// the device it is drawn with, known once there is one.
    pub fn set_max_texture_side(&mut self, side: usize) {
        self.0.set_max_texture_side(side);
    }

    /// Makes `event` part of the next pass's input. Returns whether egui
    /// keeps the event for itself.
    pub fn on_window_event(&mut self, _window: &Window, event: &WindowEvent) -> bool {
        self.0.on_window_event(event)
    }

    /// Drops what was gathered: no pass will take it.
    pub fn discard(&mut self) {
        self.0.discard();
    }

    /// The input of a pass that is run now: all that was gathered since
    /// the last call.
    pub fn take(&mut self, window: &Window) -> egui::RawInput {
        self.0.take(window.inner_size(), window.scale_factor())
    }

    /// Heeds what a pass asks of the window: the cursor.
    pub fn handle_output(&mut self, window: &Window, output: egui::PlatformOutput) {
        match self.0.cursor_wish(output.cursor_icon) {
            Some(CursorWish::Show(icon)) => {
                window.set_cursor_visible(true);
                window.set_cursor(icon);
            }
            Some(CursorWish::Hide) => window.set_cursor_visible(false),
            None => {}
        }
    }
}

/// Lets no time go by, and says so: a page's one thread cannot wait. The
/// caller comes back with the page's next frame.
pub fn wait(_duration: Duration) -> bool {
    false
}

/// The options a FAT volume is mounted with so that its files are stamped
/// with the local time of the browser.
pub fn fat_options() -> fatfs::FsOptions {
    fatfs::FsOptions::new().time_provider(&PageClock)
}

/// The page's `Date` as fatfs asks for the time: the local date and time of
/// the browser. fatfs's own provider is not built here: it reads chrono's
/// clock, which this side leaves out.
#[derive(Debug)]
struct PageClock;

impl fatfs::TimeProvider for PageClock {
    fn get_current_date(&self) -> fatfs::Date {
        self.get_current_date_time().date
    }

    fn get_current_date_time(&self) -> fatfs::DateTime {
        let now = js_sys::Date::new_0();
        fat_stamp::date_time(fat_stamp::Local {
            year: now.get_full_year(),
            month: now.get_month(),
            day: now.get_date(),
            hour: now.get_hours(),
            minute: now.get_minutes(),
            second: now.get_seconds(),
            millisecond: now.get_milliseconds(),
        })
    }
}

/// Puts `text` where the user can read it: a page has no files, so the
/// browser is handed the text as a download, named as `file` is. Returns
/// what was done with it, for a message to the user, or why it could not
/// be done.
pub fn save_text(file: &Path, text: &str) -> Result<String, String> {
    let name = file
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("download.txt");
    match download(name, text) {
        Ok(()) => Ok(format!("offered as the download {name}")),
        Err(error) => Err(format!("Cannot offer {name} as a download: {error:?}")),
    }
}

/// Has the browser download `text` as a file called `name`: a link to the
/// text, with the name, that the page follows itself.
fn download(name: &str, text: &str) -> Result<(), JsValue> {
    let window = web_sys::window().ok_or("the page has no window")?;
    let document = window.document().ok_or("the page has no document")?;
    let body = document.body().ok_or("the page has no body")?;

    let parts = js_sys::Array::of1(&JsValue::from_str(text));
    let options = web_sys::BlobPropertyBag::new();
    options.set_type("text/plain;charset=utf-8");
    let blob = web_sys::Blob::new_with_str_sequence_and_options(&parts, &options)?;
    let url = web_sys::Url::create_object_url_with_blob(&blob)?;

    let link: web_sys::HtmlAnchorElement = document.create_element("a")?.unchecked_into();
    link.set_href(&url);
    link.set_download(name);
    // Not every browser follows a link that is not in the page.
    body.append_child(&link)?;
    link.click();
    link.remove();

    // The browser reads the text after this call has returned: the text is
    // given up later, not here.
    let give_up = Closure::once_into_js(move || {
        let _ = web_sys::Url::revoke_object_url(&url);
    });
    window.set_timeout_with_callback_and_timeout_and_arguments_0(
        give_up.unchecked_ref(),
        DOWNLOAD_KEPT_MS,
    )?;
    Ok(())
}
