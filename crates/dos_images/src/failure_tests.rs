//! What a failure prints, and keeps.

use super::*;

/// An annotation is one line: what would end it, or end its title, is
/// written as the runner's escapes for it.
#[test]
fn an_annotation_is_one_line_with_its_title_escaped() {
    assert_eq!(
        annotation("Stage 3: the build", "NMAKE failed in CMD\\FIND"),
        "::error title=Stage 3%3A the build::NMAKE failed in CMD\\FIND"
    );
    assert_eq!(
        annotation("a, b", "100% done\r\nnext: 1, 2"),
        "::error title=a%2C b::100%25 done%0D%0Anext: 1, 2"
    );
}

/// A failure of the build, or of what it made, keeps the build disk in the
/// output directory; a failure of another stage keeps nothing.
#[test]
fn a_failed_build_keeps_its_disk() {
    let out = std::env::temp_dir().join(format!("dos_images-failure-{}", std::process::id()));
    std::fs::create_dir_all(&out).unwrap();
    let kept = out.join(FAILED_BUILD_DISK);

    let disk = vec![0xF6; 4096];
    let failure = Failure::Collect {
        error: stages::collect::Error::Missing(vec!["IO.SYS".to_string()]),
        disk: disk.clone(),
    };
    assert_eq!(failure.to_string(), "the build did not make IO.SYS");
    failure.print(&out);
    assert_eq!(std::fs::read(&kept).unwrap(), disk);

    std::fs::remove_file(&kept).unwrap();
    let failure = Failure::Output(out.join("bin"), io::Error::other("full"));
    failure.print(&out);
    assert!(!kept.exists());
    std::fs::remove_dir_all(&out).unwrap();
}
