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

/// Whether the site's copy of a file of `kind` is the repository's, which
/// trunk copies beside the page, or one the workflow builds and copies
/// there itself.
fn from_the_repository(kind: Kind) -> bool {
    match kind {
        Kind::Rom => true,
        Kind::Floppy | Kind::HardDisk => false,
    }
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

/// The `copy-file` links of `html`, each as the file it names, relative to
/// the page's directory, and where trunk puts it, relative to the page: the
/// file's name, in the directory its `data-target-path` names if it names
/// one.
fn copy_links(html: &str) -> Vec<(String, String)> {
    let html = uncommented_html(html);
    html.split("<link")
        .skip(1)
        .map(|tag| &tag[..tag.find('>').unwrap_or(tag.len())])
        .filter(|tag| tag.contains(" data-trunk") && attribute(tag, "rel") == Some("copy-file"))
        .map(|tag| {
            let href = attribute(tag, "href").expect("a copy-file link names its file");
            let name = href.rsplit('/').next().unwrap_or(href);
            let beside = match attribute(tag, "data-target-path") {
                Some(directory) => format!("{}/{name}", directory.trim_end_matches('/')),
                None => name.to_string(),
            };
            (href.to_string(), beside)
        })
        .collect()
}

/// Where trunk puts, relative to the page, the file of each `copy-file`
/// link of `html`.
fn copied_beside_the_page(html: &str) -> Vec<String> {
    copy_links(html)
        .into_iter()
        .map(|(_, beside)| beside)
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

/// The paths below `dist/` that `workflow` copies a file to, outside its
/// comments: what a `cp` names last, if it is below `dist/`.
fn copied_into_dist(workflow: &str) -> Vec<String> {
    workflow
        .lines()
        .map(uncommented_line)
        .filter_map(|line| {
            let words: Vec<&str> = line.split_whitespace().collect();
            let command = words.iter().position(|word| *word == "cp")?;
            let target = words[command + 1..].last()?.trim_matches(['"', '\'']);
            let path = target.strip_prefix("dist/")?;
            (!path.is_empty()).then(|| path.to_string())
        })
        .collect()
}

/// Every file of the table is published beside the page. A file of the
/// repository is copied there by trunk (`index.html`), in the build the
/// workflow publishes as in the one `trunk serve` serves, and the workflow
/// checks it there before it publishes it. A file the workflow builds is
/// copied there by the workflow, and the page does not name it.
#[test]
fn every_file_of_the_site_is_published_beside_the_page() {
    let copied = copied_beside_the_page(&source(PAGE));
    let workflow = source(WORKFLOW);
    let checked = named_in_dist(&workflow);
    let built = copied_into_dist(&workflow);
    assert!(
        workflow.contains("trunk build") && workflow.contains("path: dist"),
        "{WORKFLOW} publishes what trunk builds"
    );
    for file in &FILES {
        if from_the_repository(file.kind) {
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
        } else {
            assert!(
                !copied.iter().any(|path| path == file.path),
                "{PAGE} has trunk copy {}, which the repository does not hold",
                file.path
            );
            assert!(
                built.iter().any(|path| path == file.path),
                "{WORKFLOW} does not copy {} into dist/: it copies {built:?}",
                file.path
            );
        }
    }
}

/// Nothing is published beside the page that the table does not have: the
/// app would not know of it.
#[test]
fn nothing_else_is_published_beside_the_page() {
    for path in copied_beside_the_page(&source(PAGE)) {
        assert!(
            FILES
                .iter()
                .any(|file| file.path == path && from_the_repository(file.kind)),
            "{PAGE} has trunk copy {path} beside the page, which the table does not have \
             as a file of the repository"
        );
    }
    for path in copied_into_dist(&source(WORKFLOW)) {
        assert!(
            FILES
                .iter()
                .any(|file| file.path == path && !from_the_repository(file.kind)),
            "{WORKFLOW} copies {path} beside the page, which the table does not have as a \
             file the workflow builds"
        );
    }
}

/// The page names only files the repository holds: `trunk serve` builds it
/// of the repository alone, without what the workflow builds.
#[test]
fn the_page_is_built_of_the_repository_alone() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    for (href, _) in copy_links(&source(PAGE)) {
        assert!(
            root.join(&href).is_file(),
            "{PAGE} has trunk copy {href}, which the repository does not hold"
        );
    }
}

/// The sweep reads the page and the workflow as trunk and the runner do: a
/// link in a comment copies nothing, a link without a target path copies
/// into the page's own directory, a link of another kind copies nothing, a
/// path in a comment of the workflow is not checked, and a file is copied
/// below `dist/` by a `cp` that names it there last.
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
    assert_eq!(
        copy_links(html)[0],
        (
            "assets/rom/bios.bin".to_string(),
            "rom/bios.bin".to_string()
        )
    );

    let workflow = "      # trunk copies dist/old.bin\n      run: cmp a.bin dist/rom/bios.bin # dist/x.bin\n      path: dist\n";
    assert_eq!(named_in_dist(workflow), ["rom/bios.bin"]);

    let workflow = "        run: |\n          mkdir -p dist/disks\n          cp \"$OUT/a.img\" dist/disks/a.img\n          # cp b.img dist/disks/b.img\n          cp dist/c.img elsewhere/c.img\n          cp -r out 'dist/d'\n";
    assert_eq!(copied_into_dist(workflow), ["disks/a.img", "d"]);
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
    let paths = |kind| of_kind(kind).map(|file| file.path).collect::<Vec<_>>();
    assert_eq!(paths(Kind::Rom), ["rom/bios.bin"]);
    assert_eq!(paths(Kind::Floppy), ["disks/msdos400.img"]);
    assert_eq!(paths(Kind::HardDisk), ["disks/doom8088.img"]);
}
