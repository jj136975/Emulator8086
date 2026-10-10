//! The verdict on a site's answers, as the servers of the page give them:
//! GitHub Pages, `trunk serve`, any static server.

use super::*;

const ROM: &str = "rom/bios.bin";

/// A file the site serves comes, whatever it is served as, but for a page.
#[test]
fn a_file_served_with_success_comes() {
    for content_type in [
        Some("application/octet-stream"),
        Some("application/x-binary"),
        None,
    ] {
        assert_eq!(
            verdict(ROM, 200, "OK", content_type),
            Ok(()),
            "{content_type:?}"
        );
    }
    assert_eq!(verdict(ROM, 203, "", None), Ok(()));
}

/// A site that says it has no such file has none, Not Found or Gone, and
/// the words say so of the file rather than give the site's status.
#[test]
fn a_site_that_says_it_has_no_such_file_has_none() {
    let none = Err("The site has no rom/bios.bin".to_string());
    assert_eq!(verdict(ROM, 404, "Not Found", Some("text/html")), none);
    assert_eq!(verdict(ROM, 410, "Gone", None), none);
}

/// A site that answers with a page where a file was asked for has no such
/// file: `trunk serve` answers every path it has no file for with the page,
/// and says that it succeeded.
#[test]
fn a_page_in_the_place_of_a_file_says_there_is_no_such_file() {
    for content_type in ["text/html", "text/html; charset=utf-8", " Text/HTML"] {
        assert_eq!(
            verdict(ROM, 200, "OK", Some(content_type)),
            Err("The site has no rom/bios.bin".to_string()),
            "{content_type:?}"
        );
    }
}

/// Any other answer is a failure, told with its status and the site's words
/// for it where it has some (HTTP/2 has none), and with the file's path.
#[test]
fn any_other_answer_is_a_failure_with_its_status() {
    assert_eq!(
        verdict(ROM, 500, "Internal Server Error", Some("text/html")),
        Err("The site answered 500 Internal Server Error for rom/bios.bin".to_string())
    );
    assert_eq!(
        verdict(ROM, 403, "", None),
        Err("The site answered 403 for rom/bios.bin".to_string())
    );
}
