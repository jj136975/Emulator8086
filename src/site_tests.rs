//! The table of the site against what publishes its files, read as sources:
//! the page trunk builds (`index.html`) and the workflow that builds it and
//! publishes what trunk made (`.github/workflows/pages.yml`). A file added
//! to one of the three and not to the others fails here.

use std::path::Path;

use super::*;

/// The page trunk builds, and the workflow that publishes it, from the root
/// of the package.
const PAGE: &str = "index.html";
const WORKFLOW: &str = ".github/workflows/pages.yml";

fn source(path: &str) -> String {
    let file = Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
    std::fs::read_to_string(&file).unwrap_or_else(|error| panic!("{}: {error}", file.display()))
}

/// `html` without its comments.
fn uncommented_html(html: &str) -> String {
    let mut kept = String::new();
    let mut rest = html;
    while let Some(start) = rest.find("<!--") {
        kept.push_str(&rest[..start]);
        rest = match rest[start..].find("-->") {
            Some(end) => &rest[start + end + "-->".len()..],
            None => "",
        };
    }
    kept.push_str(rest);
    kept
}

/// The value of the attribute `name` of `tag`, the text of an HTML tag.
fn attribute<'a>(tag: &'a str, name: &str) -> Option<&'a str> {
    let opening = format!(" {name}=\"");
    let value = &tag[tag.find(&opening)? + opening.len()..];
    Some(&value[..value.find('"')?])
}

/// Where trunk puts, relative to the page, the file of each `copy-file`
/// link of `html`: the file's name, in the directory its
/// `data-target-path` names if it names one.
fn copied_beside_the_page(html: &str) -> Vec<String> {
    let html = uncommented_html(html);
    html.split("<link")
        .skip(1)
        .map(|tag| &tag[..tag.find('>').unwrap_or(tag.len())])
        .filter(|tag| tag.contains(" data-trunk") && attribute(tag, "rel") == Some("copy-file"))
        .map(|tag| {
            let href = attribute(tag, "href").expect("a copy-file link names its file");
            let name = href.rsplit('/').next().unwrap_or(href);
            match attribute(tag, "data-target-path") {
                Some(directory) => format!("{}/{name}", directory.trim_end_matches('/')),
                None => name.to_string(),
            }
        })
        .collect()
}

/// `line`, of YAML or of a shell, without its comment: what follows a `#`
/// at its start or after a blank.
fn uncommented_line(line: &str) -> &str {
    let mut previous = ' ';
    for (at, character) in line.char_indices() {
        if character == '#' && previous.is_whitespace() {
            return &line[..at];
        }
        previous = character;
    }
    line
}

/// The paths below `dist/` that `workflow` names outside its comments.
fn named_in_dist(workflow: &str) -> Vec<String> {
    let mut named = Vec::new();
    for line in workflow.lines().map(uncommented_line) {
        for (at, opening) in line.match_indices("dist/") {
            let path: String = line[at + opening.len()..]
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || "._-/".contains(*c))
                .collect();
            if !path.is_empty() {
                named.push(path);
            }
        }
    }
    named
}

/// Every file of the table is published beside the page: trunk copies it
/// there (`index.html`), in the build the workflow publishes as in the one
/// `trunk serve` serves, and the workflow checks it there before it
/// publishes it.
#[test]
fn every_file_of_the_site_is_published_beside_the_page() {
    let copied = copied_beside_the_page(&source(PAGE));
    let workflow = source(WORKFLOW);
    let checked = named_in_dist(&workflow);
    assert!(
        workflow.contains("trunk build") && workflow.contains("path: dist"),
        "{WORKFLOW} publishes what trunk builds"
    );
    for file in &FILES {
        assert!(
            copied.iter().any(|path| path == file.path),
            "{PAGE} does not have trunk copy {} beside the page: it copies {copied:?}",
            file.path
        );
        assert!(
            checked.iter().any(|path| path == file.path),
            "{WORKFLOW} does not check {} in dist/: it names {checked:?}",
            file.path
        );
    }
}

/// Nothing is published beside the page that the table does not have: the
/// app would not know of it.
#[test]
fn nothing_else_is_published_beside_the_page() {
    for path in copied_beside_the_page(&source(PAGE)) {
        assert!(
            FILES.iter().any(|file| file.path == path),
            "{PAGE} has trunk copy {path} beside the page, which the table does not have"
        );
    }
}

/// The sweep reads the page and the workflow as trunk and the runner do: a
/// link in a comment copies nothing, a link without a target path copies
/// into the page's own directory, a link of another kind copies nothing,
/// and a path in a comment of the workflow is not checked.
#[test]
fn the_sweep_reads_what_trunk_and_the_runner_read() {
    let html = r#"<head>
    <!-- <link data-trunk rel="copy-file" href="old.bin" /> -->
    <link data-trunk rel="rust" data-bin="app" />
    <link data-trunk rel="copy-file" href="assets/rom/bios.bin"
          data-target-path="rom/" />
    <link data-trunk rel="copy-file" href="notes.txt"/>
    <link rel="icon" href="data:," />
  </head>"#;
    assert_eq!(copied_beside_the_page(html), ["rom/bios.bin", "notes.txt"]);

    let workflow = "      # trunk copies dist/old.bin\n      run: cmp a.bin dist/rom/bios.bin # dist/x.bin\n      path: dist\n";
    assert_eq!(named_in_dist(workflow), ["rom/bios.bin"]);
}

/// Each file of the table has a path of its own, below the page, and a
/// title of its own; a kind has the files of the table that are of it.
#[test]
fn each_file_has_a_path_below_the_page_and_a_title_of_its_own() {
    for (index, file) in FILES.iter().enumerate() {
        assert!(
            !file.path.starts_with('/') && !file.path.split('/').any(|part| part == ".."),
            "{} is relative to the page, and below it",
            file.path
        );
        assert!(!file.title.is_empty(), "{}", file.path);
        for earlier in &FILES[..index] {
            assert_ne!(earlier.path, file.path, "two files share a path");
            assert_ne!(earlier.title, file.title, "two files share a title");
        }
    }
    let roms: Vec<&str> = of_kind(Kind::Rom).map(|file| file.path).collect();
    assert_eq!(roms, ["rom/bios.bin"]);
}
