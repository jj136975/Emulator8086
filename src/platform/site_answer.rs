//! What a site's answer to a fetch says of the file asked for, apart from
//! the browser that hands the answer over: whether the file came, and if
//! not, why, in words for the user (`verdict`). It knows nothing of a
//! browser, and is built on the desktop for its tests.
//!
//! The invariant: a file of the site comes only in an answer that says it
//! is there, a status of success that is not a page. A server that answers
//! every path it has no file for with the page, as `trunk serve` does, says
//! as surely as one that answers 404 that it has no such file: what the app
//! fetches is an image, never a page.

/// HTTP's statuses for a file the site does not have: Not Found, Gone.
const NO_SUCH_FILE: [u16; 2] = [404, 410];

/// What an answer of HTTP status `status`, which the site calls
/// `status_text`, with a body of `content_type`, says of the file at `path`:
/// that it came, or why it did not.
pub fn verdict(
    path: &str,
    status: u16,
    status_text: &str,
    content_type: Option<&str>,
) -> Result<(), String> {
    let page = content_type.is_some_and(|kind| {
        kind.trim_start()
            .to_ascii_lowercase()
            .starts_with("text/html")
    });
    let success = (200..300).contains(&status);
    if NO_SUCH_FILE.contains(&status) || (success && page) {
        return Err(format!("The site has no {path}"));
    }
    if !success {
        let answered = match status_text.trim() {
            "" => status.to_string(),
            text => format!("{status} {text}"),
        };
        return Err(format!("The site answered {answered} for {path}"));
    }
    Ok(())
}

#[cfg(test)]
#[path = "site_answer_tests.rs"]
mod tests;
