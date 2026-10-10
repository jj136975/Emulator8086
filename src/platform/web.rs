//! The browser side of `platform`: a page, reached through wasm-bindgen.
//! It has no command line, no files and no stderr, and one thread, which is
//! the page's own; it has the site that served it, whose files it fetches.
//!
//! The invariant: nothing here blocks. Whatever takes time (the GPU's
//! start-up, the wait for the next frame, the user at a file picker, a file
//! on its way from the site) is a task or a callback of the page's event
//! loop, and `wait` says that it cannot wait: a call that did not return
//! would freeze the page with the machine in it.

use std::future::Future;
use std::path::Path;
use std::sync::Arc;
use std::sync::mpsc::{self, Receiver};
use std::time::Duration;

use wasm_bindgen::closure::Closure;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_futures::JsFuture;
use web_sys::console;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::{MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::platform::web::{EventLoopExtWebSys, WindowAttributesExtWebSys, WindowExtWebSys};
use winit::window::Window;

use super::egui_events::{CursorWish, EguiEvents};
use super::{Fetched, FileAccess, FileQuestion, OverlaySwitch, Wanted};
use super::{fat_stamp, site_answer};
use crate::Options;
use emu8086_core::io::disk_ops::HostFile;

/// The most detailed level of the log that reaches the console. The console
/// has a filter of its own; what is cut here is the cost of formatting, on
/// every frame, the debug lines of the GPU stack and the machine's trace.
const CONSOLE_LEVEL: log::LevelFilter = log::LevelFilter::Info;

/// How the canvas lies in the page: over the whole viewport, whatever else
/// the page holds, and without the ring a browser draws around what has the
/// keyboard. Its size is the style's alone: winit reports the size the
/// style gives it as the window's, and the renderer makes the canvas's
/// drawing buffer as large. A finger on it is the page's alone too: a
/// browser that took it for a scroll or a zoom of its own would cancel it.
const CANVAS_STYLE: [(&str, &str); 7] = [
    ("position", "fixed"),
    ("left", "0"),
    ("top", "0"),
    ("width", "100%"),
    ("height", "100%"),
    ("outline", "none"),
    ("touch-action", "none"),
];

/// How long the browser has to start reading a download before it is given
/// up, in milliseconds.
const DOWNLOAD_KEPT_MS: i32 = 60_000;

/// The keys that stay the browser's own, by the `code` of their events: its
/// full screen and its developer tools. No XT keyboard has them, so the
/// guest misses nothing.
const BROWSER_KEYS: [&str; 2] = ["F11", "F12"];

/// How the overlay is shown and hidden: with the secondary button of the
/// pointer, on the picture. A page has no key for it: F12, the desktop's,
/// is the browser's own (`BROWSER_KEYS`), and the keys a browser has no use
/// for are the guest's. The pointer is free, since the machine has no
/// mouse. A phone or a tablet has no secondary button: there a finger held
/// still on the picture does it, which works on every host and which the
/// name tells of here, since a phone reaches the app as a page.
pub const OVERLAY_SWITCH: OverlaySwitch = OverlaySwitch {
    key: None,
    button: Some(MouseButton::Right),
    name: "Right-click or touch and hold",
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

/// Tells the user what a start did that they did not ask for in so many
/// words: on the console, where `fail` tells its errors, whatever the
/// level of the log.
pub fn notice(message: &str) {
    console::warn_1(&JsValue::from_str(message));
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
/// and as large as its viewport, which is why `_size` is not heeded. The
/// canvas takes the keyboard from the page, but for the keys that stay the
/// browser's.
pub fn create_window(
    event_loop: &ActiveEventLoop,
    title: &str,
    _size: PhysicalSize<u32>,
) -> Window {
    leave_the_browser_its_keys();
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

/// Keeps the keys of `BROWSER_KEYS` from the canvas, so that they do in
/// this page what they do in any other. winit cancels what the browser
/// would do with a key of the canvas, as it has to: Tab would walk out of
/// the canvas, and F5 reload the page under the guest. A listener on the
/// window sees a key on its way down to the canvas, before the canvas does,
/// and stops these keys there: winit never sees them and cancels nothing.
fn leave_the_browser_its_keys() {
    let Some(window) = web_sys::window() else {
        return;
    };
    let stop = Closure::<dyn Fn(web_sys::KeyboardEvent)>::new(|event: web_sys::KeyboardEvent| {
        if BROWSER_KEYS.contains(&event.code().as_str()) {
            event.stop_propagation();
        }
    });
    for kind in ["keydown", "keyup"] {
        // `true`: on the way down to the canvas, not on the way back up.
        let _ = window.add_event_listener_with_callback_and_bool(
            kind,
            stop.as_ref().unchecked_ref(),
            true,
        );
    }
    // The listener stays for as long as the page does.
    stop.forget();
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
    let parts = js_sys::Array::of1(&JsValue::from_str(text));
    let options = web_sys::BlobPropertyBag::new();
    options.set_type("text/plain;charset=utf-8");
    let blob = web_sys::Blob::new_with_str_sequence_and_options(&parts, &options);
    offer(file, "download.txt", blob)
}

/// Puts `bytes` where the user can get at them: the browser is handed them
/// as a download, named as `file` is. Returns what was done with them, for
/// a message to the user, or why it could not be done.
pub fn save_bytes(file: &Path, bytes: &[u8]) -> Result<String, String> {
    // The page's copy of the bytes: the download is read after this call
    // has returned, when the caller's may be gone.
    let parts = js_sys::Array::of1(&js_sys::Uint8Array::from(bytes));
    let options = web_sys::BlobPropertyBag::new();
    options.set_type("application/octet-stream");
    let blob = web_sys::Blob::new_with_u8_array_sequence_and_options(&parts, &options);
    offer(file, "download.bin", blob)
}

/// Offers `blob` as a download named as `file` is, or `unnamed` if `file`
/// has no name a page can use, and says what was done, or why not.
fn offer(
    file: &Path,
    unnamed: &str,
    blob: Result<web_sys::Blob, JsValue>,
) -> Result<String, String> {
    let name = file
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(unnamed);
    match blob.and_then(|blob| download(name, &blob)) {
        Ok(()) => Ok(format!("offered as the download {name}")),
        Err(error) => Err(format!("Cannot offer {name} as a download: {error:?}")),
    }
}

/// How the user's files reach the app: by their contents. A page is given
/// no path to a file of its user, and can write none.
pub const FILE_ACCESS: FileAccess = FileAccess::Contents;

/// The page's file picker. It belongs to the page, whatever the window: a
/// browser shows it over the tab.
pub struct FileDialogs;

impl FileDialogs {
    /// Dialogs that belong to no window: the page's, as all are.
    pub fn windowless() -> Self {
        Self
    }

    /// The dialogs of `_window`, which are the page's.
    pub fn new(_window: &Arc<Window>) -> Self {
        Self
    }

    /// Asks the user `question` in the browser's file picker, and returns
    /// at once. The answer arrives later through the channel returned: the
    /// files chosen, each by its name and its contents, or none if the user
    /// chose nothing. What a page cannot ask for, a folder or a file to
    /// write, is answered with none.
    ///
    /// A browser opens its picker only for a page its user has just
    /// clicked in or typed at. Asked later than that, the picker's library
    /// puts a small form of its own into the page, with the button the
    /// browser wants pressed.
    pub fn ask(&self, question: FileQuestion) -> Receiver<Vec<HostFile>> {
        let (answer, receiver) = mpsc::channel();
        spawn(async move {
            let dialog = rfd::AsyncFileDialog::new().set_title(question.title);
            let picked: Vec<rfd::FileHandle> = match question.wanted {
                Wanted::Image => dialog.pick_file().await.into_iter().collect(),
                Wanted::Files => dialog.pick_files().await.unwrap_or_default(),
                Wanted::NewImage { .. } | Wanted::Folder => Vec::new(),
            };
            let mut files = Vec::with_capacity(picked.len());
            for file in picked {
                files.push(HostFile::Contents {
                    name: file.file_name(),
                    bytes: file.read().await,
                });
            }
            // Whoever asked may have gone since.
            let _ = answer.send(files);
        });
        receiver
    }
}

/// The app came from the site that served the page.
pub const HAS_SITE: bool = true;

/// Asks the site that served the page for the file at `path`, relative to
/// the page, and returns at once: the browser fetches it in a task of the
/// page, and its bytes, or why none came, arrive later through the channel
/// returned. Relative to the page, the path is the same wherever the site
/// serves the page: at its root, or below a path as GitHub Pages does.
pub fn fetch(path: &str) -> Receiver<Fetched> {
    let (answer, receiver) = mpsc::channel();
    let path = path.to_string();
    spawn(async move {
        // Whoever asked may have gone since.
        let _ = answer.send(fetched(&path).await);
    });
    receiver
}

/// The file at `path` of the site, fetched: its bytes, once the site's
/// answer says that it came (`site_answer::verdict`).
async fn fetched(path: &str) -> Fetched {
    let cannot = |error: JsValue| format!("Cannot fetch {path}: {}", reason(&error));
    let window =
        web_sys::window().ok_or_else(|| format!("Cannot fetch {path}: the page has no window"))?;
    let response = JsFuture::from(window.fetch_with_str(path))
        .await
        .map_err(cannot)?;
    let response: web_sys::Response = response.dyn_into().map_err(cannot)?;
    let content_type = response.headers().get("content-type").ok().flatten();
    site_answer::verdict(
        path,
        response.status(),
        &response.status_text(),
        content_type.as_deref(),
    )?;
    let body = response.array_buffer().map_err(cannot)?;
    let body = JsFuture::from(body).await.map_err(cannot)?;
    Ok(js_sys::Uint8Array::new(&body).to_vec())
}

/// Why the page did not keep a promise: the message of the error the
/// browser rejected it with.
fn reason(error: &JsValue) -> String {
    match error.dyn_ref::<js_sys::Error>() {
        Some(error) => String::from(error.message()),
        None => error.as_string().unwrap_or_else(|| format!("{error:?}")),
    }
}

/// Has the browser download `blob` as a file called `name`: a link to it,
/// with the name, that the page follows itself.
fn download(name: &str, blob: &web_sys::Blob) -> Result<(), JsValue> {
    let window = web_sys::window().ok_or("the page has no window")?;
    let document = window.document().ok_or("the page has no document")?;
    let body = document.body().ok_or("the page has no body")?;

    let url = web_sys::Url::create_object_url_with_blob(blob)?;

    let link: web_sys::HtmlAnchorElement = document.create_element("a")?.unchecked_into();
    link.set_href(&url);
    link.set_download(name);
    // Not every browser follows a link that is not in the page.
    body.append_child(&link)?;
    link.click();
    link.remove();

    // The browser reads the download after this call has returned: it is
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
