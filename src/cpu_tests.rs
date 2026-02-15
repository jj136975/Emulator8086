/// CPU validation tests against real 8086 hardware captures.
///
/// Test data lives in `8086/v1/*.json.gz` — 2,000 tests per opcode captured
/// from a physical Intel P80C86A-2 CPU.
///
/// Run all CPU tests:
///     cargo test cpu_tests -- --ignored
///
/// Run a single opcode:
///     cargo test cpu_tests::test_opcode_00 -- --ignored
///
/// Run a range:
///     cargo test cpu_tests::test_opcode_0 -- --ignored   (matches 00–0f)

use std::collections::HashMap;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use flate2::read::GzDecoder;
use serde::Deserialize;

use crate::vm::instructions::process;
use crate::vm::runtime::Runtime;

// ── JSON deserialization types ──────────────────────────────────────────

#[derive(Deserialize)]
struct TestCase {
    name: String,
    bytes: Vec<u8>,
    initial: InitialState,
    #[serde(rename = "final")]
    final_state: FinalState,
}

#[derive(Deserialize)]
struct InitialState {
    regs: InitialRegs,
    ram: Vec<(u32, u8)>,
    #[serde(default)]
    #[allow(dead_code)]
    queue: Vec<u8>,
}

#[derive(Deserialize)]
struct InitialRegs {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    cs: u16,
    ss: u16,
    ds: u16,
    es: u16,
    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
    ip: u16,
    flags: u16,
}

#[derive(Deserialize)]
struct FinalState {
    regs: HashMap<String, u16>,
    ram: Vec<(u32, u8)>,
    #[serde(default)]
    #[allow(dead_code)]
    queue: Vec<u8>,
}

// ── Metadata types ──────────────────────────────────────────────────────

#[derive(Deserialize)]
struct Metadata {
    opcodes: HashMap<String, OpcodeInfo>,
}

#[derive(Deserialize)]
struct OpcodeInfo {
    #[serde(rename = "flags-mask")]
    flags_mask: Option<u16>,
    reg: Option<HashMap<String, OpcodeInfo>>,
}

// ── Globals ─────────────────────────────────────────────────────────────

static METADATA: OnceLock<Metadata> = OnceLock::new();

fn metadata() -> &'static Metadata {
    METADATA.get_or_init(|| {
        let data = std::fs::read_to_string("8086/v1/metadata.json")
            .expect("Failed to read 8086/v1/metadata.json");
        serde_json::from_str(&data).expect("Failed to parse metadata.json")
    })
}

// ── Helpers ─────────────────────────────────────────────────────────────

fn load_tests(path: &Path) -> Vec<TestCase> {
    let file = std::fs::File::open(path)
        .unwrap_or_else(|e| panic!("Failed to open {}: {}", path.display(), e));
    let mut decoder = GzDecoder::new(std::io::BufReader::new(file));
    let mut json = String::new();
    decoder
        .read_to_string(&mut json)
        .unwrap_or_else(|e| panic!("Failed to decompress {}: {}", path.display(), e));
    serde_json::from_str(&json)
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", path.display(), e))
}

fn is_prefix_byte(byte: u8) -> bool {
    matches!(byte, 0x26 | 0x2E | 0x36 | 0x3E | 0xF0 | 0xF2 | 0xF3)
}

/// Look up the flags mask for an opcode (e.g. "04" or "f6.6").
/// Returns 0xFFFF when all flags are defined.
fn get_flags_mask(opcode_str: &str) -> u16 {
    let meta = metadata();
    let parts: Vec<&str> = opcode_str.split('.').collect();
    let base_lower = parts[0].to_lowercase();
    let base_upper = parts[0].to_uppercase();

    // Metadata keys may be uppercase (e.g. "0A") — try both
    if let Some(info) = meta.opcodes.get(&base_lower).or_else(|| meta.opcodes.get(&base_upper)) {
        if let Some(reg_str) = parts.get(1) {
            if let Some(ref reg_map) = info.reg {
                if let Some(reg_info) = reg_map.get(*reg_str) {
                    return reg_info.flags_mask.unwrap_or(0xFFFF);
                }
            }
        }
        return info.flags_mask.unwrap_or(0xFFFF);
    }
    0xFFFF
}

/// Find all test files matching an opcode hex string.
/// Returns `(path, opcode_label)` pairs sorted by label.
fn find_test_files(opcode_hex: &str) -> Vec<(PathBuf, String)> {
    let dir = Path::new("8086/v1");
    let target = opcode_hex.to_lowercase();

    let mut files = Vec::new();
    let Ok(entries) = std::fs::read_dir(dir) else {
        return files;
    };

    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().to_string();
        if !name.ends_with(".json.gz") {
            continue;
        }
        let stem = name.trim_end_matches(".json.gz");
        let stem_lower = stem.to_lowercase();

        // Match base file (e.g. "fe") or sub-opcode file (e.g. "fe.0")
        if stem_lower == target || stem_lower.starts_with(&format!("{}.", target)) {
            files.push((entry.path(), stem.to_string()));
        }
    }
    files.sort_by(|a, b| a.1.cmp(&b.1));
    files
}

fn setup_vm(initial: &InitialState) -> Runtime {
    let mut vm = Runtime::new_test();

    // General purpose registers
    vm.registers.ax.set(initial.regs.ax);
    vm.registers.bx.set(initial.regs.bx);
    vm.registers.cx.set(initial.regs.cx);
    vm.registers.dx.set(initial.regs.dx);
    vm.registers.sp.set(initial.regs.sp);
    vm.registers.bp.set(initial.regs.bp);
    vm.registers.si.set(initial.regs.si);
    vm.registers.di.set(initial.regs.di);

    // Segment registers
    vm.registers.cs.reg_mut().set(initial.regs.cs);
    vm.registers.ds.reg_mut().set(initial.regs.ds);
    vm.registers.es.reg_mut().set(initial.regs.es);
    vm.registers.ss.reg_mut().set(initial.regs.ss);

    // IP and flags
    vm.registers.pc.set(initial.regs.ip);
    vm.flags = initial.regs.flags;

    // RAM (physical addresses)
    for &(addr, value) in &initial.ram {
        vm.memory.write_byte(addr as usize, value);
    }

    vm
}

fn initial_reg(regs: &InitialRegs, name: &str) -> u16 {
    match name {
        "ax" => regs.ax,
        "bx" => regs.bx,
        "cx" => regs.cx,
        "dx" => regs.dx,
        "sp" => regs.sp,
        "bp" => regs.bp,
        "si" => regs.si,
        "di" => regs.di,
        "cs" => regs.cs,
        "ds" => regs.ds,
        "es" => regs.es,
        "ss" => regs.ss,
        "ip" => regs.ip,
        "flags" => regs.flags,
        _ => unreachable!(),
    }
}

fn actual_reg(vm: &Runtime, name: &str) -> u16 {
    match name {
        "ax" => vm.registers.ax.word(),
        "bx" => vm.registers.bx.word(),
        "cx" => vm.registers.cx.word(),
        "dx" => vm.registers.dx.word(),
        "sp" => vm.registers.sp.word(),
        "bp" => vm.registers.bp.word(),
        "si" => vm.registers.si.word(),
        "di" => vm.registers.di.word(),
        "cs" => vm.registers.cs.reg().word(),
        "ds" => vm.registers.ds.reg().word(),
        "es" => vm.registers.es.reg().word(),
        "ss" => vm.registers.ss.reg().word(),
        "ip" => vm.registers.pc.word(),
        "flags" => vm.flags,
        _ => unreachable!(),
    }
}

// ── Core test runner ────────────────────────────────────────────────────

fn run_single_test(test: &TestCase, flags_mask: u16) -> Result<(), String> {
    let mut vm = setup_vm(&test.initial);

    // Execute: one process() call per prefix byte, plus one for the instruction
    let num_prefixes = test.bytes.iter().take_while(|&&b| is_prefix_byte(b)).count();
    for _ in 0..=num_prefixes {
        process(&mut vm);
    }

    let mut mismatches = Vec::new();

    // Check registers (excluding flags — handled separately)
    const REG_NAMES: [&str; 13] = [
        "ax", "bx", "cx", "dx", "sp", "bp", "si", "di", "cs", "ds", "es", "ss", "ip",
    ];

    for &name in &REG_NAMES {
        let expected = test
            .final_state
            .regs
            .get(name)
            .copied()
            .unwrap_or_else(|| initial_reg(&test.initial.regs, name));
        let actual = actual_reg(&vm, name);
        if actual != expected {
            mismatches.push(format!(
                "{}: expected={:#06X} actual={:#06X}",
                name, expected, actual
            ));
        }
    }

    // Check flags with mask
    let expected_flags = test
        .final_state
        .regs
        .get("flags")
        .copied()
        .unwrap_or(test.initial.regs.flags);
    if (vm.flags & flags_mask) != (expected_flags & flags_mask) {
        mismatches.push(format!(
            "flags: expected={:#06X} actual={:#06X} (mask={:#06X}, raw_exp={:#06X}, raw_act={:#06X})",
            expected_flags & flags_mask,
            vm.flags & flags_mask,
            flags_mask,
            expected_flags,
            vm.flags
        ));
    }

    // Check RAM — when an exception dispatches (SP decreased by 6, meaning 3 words
    // were pushed: FLAGS, CS, IP), mask the stacked FLAGS bytes with flags_mask so
    // that "undefined" flag bits don't cause spurious failures.
    let init_sp = test.initial.regs.sp;
    let final_sp = actual_reg(&vm, "sp");
    let stacked_flags_addrs: Option<(u32, u32)> =
        if flags_mask != 0xFFFF && init_sp.wrapping_sub(final_sp) == 6 {
            let ss = test.initial.regs.ss as u32;
            let lo = (ss << 4).wrapping_add(init_sp.wrapping_sub(2) as u32) & 0xFFFFF;
            let hi = (ss << 4).wrapping_add(init_sp.wrapping_sub(1) as u32) & 0xFFFFF;
            Some((lo, hi))
        } else {
            None
        };

    for &(addr, expected_val) in &test.final_state.ram {
        let actual_val = vm.memory.read_byte(addr as usize);
        let (ev, av) = if let Some((lo, hi)) = stacked_flags_addrs {
            if addr == lo {
                let m = (flags_mask & 0xFF) as u8;
                (expected_val & m, actual_val & m)
            } else if addr == hi {
                let m = ((flags_mask >> 8) & 0xFF) as u8;
                (expected_val & m, actual_val & m)
            } else {
                (expected_val, actual_val)
            }
        } else {
            (expected_val, actual_val)
        };
        if av != ev {
            mismatches.push(format!(
                "RAM[{:#07X}]: expected={:#04X} actual={:#04X}",
                addr, expected_val, actual_val
            ));
        }
    }

    if mismatches.is_empty() {
        Ok(())
    } else {
        Err(mismatches.join("; "))
    }
}

/// Opcodes where the emulator intentionally implements 80186+ behavior
/// instead of 8086 behavior (for DOS compatibility). Tests for these are skipped.
const SKIP_186_OPCODES: &[&str] = &[
    "60", "61", "62", "63", "64", "65", "66", "67",
    "68", "69", "6a", "6b", "6c", "6d", "6e", "6f",
    "c0", "c1", "c8", "c9",
];

/// Run all test files for an opcode (base + sub-opcodes).
fn run_opcode(opcode_hex: &str) {
    if SKIP_186_OPCODES.contains(&opcode_hex) {
        eprintln!("  skip opcode {} — intentional 80186+ implementation", opcode_hex);
        return;
    }
    let files = find_test_files(opcode_hex);
    if files.is_empty() {
        eprintln!("  skip opcode {} — no test files found", opcode_hex);
        return;
    }

    let mut total_passed = 0usize;
    let mut total_failed = 0usize;
    let mut errors: Vec<String> = Vec::new();

    for (path, label) in &files {
        let flags_mask = get_flags_mask(label);
        let tests = load_tests(path);

        for (i, test) in tests.iter().enumerate() {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_single_test(test, flags_mask)
            })) {
                Ok(Ok(())) => total_passed += 1,
                Ok(Err(msg)) => {
                    total_failed += 1;
                    if errors.len() < 10 {
                        errors.push(format!("[{}] #{} '{}': {}", label, i, test.name, msg));
                    }
                }
                Err(panic_info) => {
                    total_failed += 1;
                    let msg = panic_info
                        .downcast_ref::<String>()
                        .map(|s| s.as_str())
                        .or_else(|| panic_info.downcast_ref::<&str>().copied())
                        .unwrap_or("unknown panic");
                    if errors.len() < 10 {
                        errors.push(format!(
                            "[{}] #{} '{}': PANIC: {}",
                            label, i, test.name, msg
                        ));
                    }
                }
            }
        }
    }

    let total = total_passed + total_failed;
    eprintln!(
        "  opcode {}: {}/{} passed ({} file(s))",
        opcode_hex,
        total_passed,
        total,
        files.len()
    );

    if total_failed > 0 {
        let mut msg = format!(
            "Opcode {}: {}/{} tests failed\nFirst failures:\n",
            opcode_hex, total_failed, total
        );
        for e in &errors {
            msg.push_str(&format!("  {}\n", e));
        }
        if total_failed > 10 {
            msg.push_str(&format!("  ... and {} more failures\n", total_failed - 10));
        }
        panic!("{}", msg);
    }
}

// ── Generated per-opcode tests ──────────────────────────────────────────

macro_rules! cpu_tests {
    ($(($name:ident, $opcode:expr)),* $(,)?) => {
        $(
            #[test]
            #[ignore]
            fn $name() {
                run_opcode($opcode);
            }
        )*
    };
}

cpu_tests! {
    // 0x00 – 0x0F
    (test_opcode_00, "00"), (test_opcode_01, "01"), (test_opcode_02, "02"), (test_opcode_03, "03"),
    (test_opcode_04, "04"), (test_opcode_05, "05"), (test_opcode_06, "06"), (test_opcode_07, "07"),
    (test_opcode_08, "08"), (test_opcode_09, "09"), (test_opcode_0a, "0a"), (test_opcode_0b, "0b"),
    (test_opcode_0c, "0c"), (test_opcode_0d, "0d"), (test_opcode_0e, "0e"), (test_opcode_0f, "0f"),
    // 0x10 – 0x1F
    (test_opcode_10, "10"), (test_opcode_11, "11"), (test_opcode_12, "12"), (test_opcode_13, "13"),
    (test_opcode_14, "14"), (test_opcode_15, "15"), (test_opcode_16, "16"), (test_opcode_17, "17"),
    (test_opcode_18, "18"), (test_opcode_19, "19"), (test_opcode_1a, "1a"), (test_opcode_1b, "1b"),
    (test_opcode_1c, "1c"), (test_opcode_1d, "1d"), (test_opcode_1e, "1e"), (test_opcode_1f, "1f"),
    // 0x20 – 0x2F
    (test_opcode_20, "20"), (test_opcode_21, "21"), (test_opcode_22, "22"), (test_opcode_23, "23"),
    (test_opcode_24, "24"), (test_opcode_25, "25"), (test_opcode_26, "26"), (test_opcode_27, "27"),
    (test_opcode_28, "28"), (test_opcode_29, "29"), (test_opcode_2a, "2a"), (test_opcode_2b, "2b"),
    (test_opcode_2c, "2c"), (test_opcode_2d, "2d"), (test_opcode_2e, "2e"), (test_opcode_2f, "2f"),
    // 0x30 – 0x3F
    (test_opcode_30, "30"), (test_opcode_31, "31"), (test_opcode_32, "32"), (test_opcode_33, "33"),
    (test_opcode_34, "34"), (test_opcode_35, "35"), (test_opcode_36, "36"), (test_opcode_37, "37"),
    (test_opcode_38, "38"), (test_opcode_39, "39"), (test_opcode_3a, "3a"), (test_opcode_3b, "3b"),
    (test_opcode_3c, "3c"), (test_opcode_3d, "3d"), (test_opcode_3e, "3e"), (test_opcode_3f, "3f"),
    // 0x40 – 0x4F
    (test_opcode_40, "40"), (test_opcode_41, "41"), (test_opcode_42, "42"), (test_opcode_43, "43"),
    (test_opcode_44, "44"), (test_opcode_45, "45"), (test_opcode_46, "46"), (test_opcode_47, "47"),
    (test_opcode_48, "48"), (test_opcode_49, "49"), (test_opcode_4a, "4a"), (test_opcode_4b, "4b"),
    (test_opcode_4c, "4c"), (test_opcode_4d, "4d"), (test_opcode_4e, "4e"), (test_opcode_4f, "4f"),
    // 0x50 – 0x5F
    (test_opcode_50, "50"), (test_opcode_51, "51"), (test_opcode_52, "52"), (test_opcode_53, "53"),
    (test_opcode_54, "54"), (test_opcode_55, "55"), (test_opcode_56, "56"), (test_opcode_57, "57"),
    (test_opcode_58, "58"), (test_opcode_59, "59"), (test_opcode_5a, "5a"), (test_opcode_5b, "5b"),
    (test_opcode_5c, "5c"), (test_opcode_5d, "5d"), (test_opcode_5e, "5e"), (test_opcode_5f, "5f"),
    // 0x60 – 0x6F
    (test_opcode_60, "60"), (test_opcode_61, "61"), (test_opcode_62, "62"), (test_opcode_63, "63"),
    (test_opcode_64, "64"), (test_opcode_65, "65"), (test_opcode_66, "66"), (test_opcode_67, "67"),
    (test_opcode_68, "68"), (test_opcode_69, "69"), (test_opcode_6a, "6a"), (test_opcode_6b, "6b"),
    (test_opcode_6c, "6c"), (test_opcode_6d, "6d"), (test_opcode_6e, "6e"), (test_opcode_6f, "6f"),
    // 0x70 – 0x7F
    (test_opcode_70, "70"), (test_opcode_71, "71"), (test_opcode_72, "72"), (test_opcode_73, "73"),
    (test_opcode_74, "74"), (test_opcode_75, "75"), (test_opcode_76, "76"), (test_opcode_77, "77"),
    (test_opcode_78, "78"), (test_opcode_79, "79"), (test_opcode_7a, "7a"), (test_opcode_7b, "7b"),
    (test_opcode_7c, "7c"), (test_opcode_7d, "7d"), (test_opcode_7e, "7e"), (test_opcode_7f, "7f"),
    // 0x80 – 0x8F
    (test_opcode_80, "80"), (test_opcode_81, "81"), (test_opcode_82, "82"), (test_opcode_83, "83"),
    (test_opcode_84, "84"), (test_opcode_85, "85"), (test_opcode_86, "86"), (test_opcode_87, "87"),
    (test_opcode_88, "88"), (test_opcode_89, "89"), (test_opcode_8a, "8a"), (test_opcode_8b, "8b"),
    (test_opcode_8c, "8c"), (test_opcode_8d, "8d"), (test_opcode_8e, "8e"), (test_opcode_8f, "8f"),
    // 0x90 – 0x9F
    (test_opcode_90, "90"), (test_opcode_91, "91"), (test_opcode_92, "92"), (test_opcode_93, "93"),
    (test_opcode_94, "94"), (test_opcode_95, "95"), (test_opcode_96, "96"), (test_opcode_97, "97"),
    (test_opcode_98, "98"), (test_opcode_99, "99"), (test_opcode_9a, "9a"), (test_opcode_9b, "9b"),
    (test_opcode_9c, "9c"), (test_opcode_9d, "9d"), (test_opcode_9e, "9e"), (test_opcode_9f, "9f"),
    // 0xA0 – 0xAF
    (test_opcode_a0, "a0"), (test_opcode_a1, "a1"), (test_opcode_a2, "a2"), (test_opcode_a3, "a3"),
    (test_opcode_a4, "a4"), (test_opcode_a5, "a5"), (test_opcode_a6, "a6"), (test_opcode_a7, "a7"),
    (test_opcode_a8, "a8"), (test_opcode_a9, "a9"), (test_opcode_aa, "aa"), (test_opcode_ab, "ab"),
    (test_opcode_ac, "ac"), (test_opcode_ad, "ad"), (test_opcode_ae, "ae"), (test_opcode_af, "af"),
    // 0xB0 – 0xBF
    (test_opcode_b0, "b0"), (test_opcode_b1, "b1"), (test_opcode_b2, "b2"), (test_opcode_b3, "b3"),
    (test_opcode_b4, "b4"), (test_opcode_b5, "b5"), (test_opcode_b6, "b6"), (test_opcode_b7, "b7"),
    (test_opcode_b8, "b8"), (test_opcode_b9, "b9"), (test_opcode_ba, "ba"), (test_opcode_bb, "bb"),
    (test_opcode_bc, "bc"), (test_opcode_bd, "bd"), (test_opcode_be, "be"), (test_opcode_bf, "bf"),
    // 0xC0 – 0xCF
    (test_opcode_c0, "c0"), (test_opcode_c1, "c1"), (test_opcode_c2, "c2"), (test_opcode_c3, "c3"),
    (test_opcode_c4, "c4"), (test_opcode_c5, "c5"), (test_opcode_c6, "c6"), (test_opcode_c7, "c7"),
    (test_opcode_c8, "c8"), (test_opcode_c9, "c9"), (test_opcode_ca, "ca"), (test_opcode_cb, "cb"),
    (test_opcode_cc, "cc"), (test_opcode_cd, "cd"), (test_opcode_ce, "ce"), (test_opcode_cf, "cf"),
    // 0xD0 – 0xDF
    (test_opcode_d0, "d0"), (test_opcode_d1, "d1"), (test_opcode_d2, "d2"), (test_opcode_d3, "d3"),
    (test_opcode_d4, "d4"), (test_opcode_d5, "d5"), (test_opcode_d6, "d6"), (test_opcode_d7, "d7"),
    (test_opcode_d8, "d8"), (test_opcode_d9, "d9"), (test_opcode_da, "da"), (test_opcode_db, "db"),
    (test_opcode_dc, "dc"), (test_opcode_dd, "dd"), (test_opcode_de, "de"), (test_opcode_df, "df"),
    // 0xE0 – 0xEF
    (test_opcode_e0, "e0"), (test_opcode_e1, "e1"), (test_opcode_e2, "e2"), (test_opcode_e3, "e3"),
    (test_opcode_e4, "e4"), (test_opcode_e5, "e5"), (test_opcode_e6, "e6"), (test_opcode_e7, "e7"),
    (test_opcode_e8, "e8"), (test_opcode_e9, "e9"), (test_opcode_ea, "ea"), (test_opcode_eb, "eb"),
    (test_opcode_ec, "ec"), (test_opcode_ed, "ed"), (test_opcode_ee, "ee"), (test_opcode_ef, "ef"),
    // 0xF0 – 0xFF
    (test_opcode_f0, "f0"), (test_opcode_f1, "f1"), (test_opcode_f2, "f2"), (test_opcode_f3, "f3"),
    (test_opcode_f4, "f4"), (test_opcode_f5, "f5"), (test_opcode_f6, "f6"), (test_opcode_f7, "f7"),
    (test_opcode_f8, "f8"), (test_opcode_f9, "f9"), (test_opcode_fa, "fa"), (test_opcode_fb, "fb"),
    (test_opcode_fc, "fc"), (test_opcode_fd, "fd"), (test_opcode_fe, "fe"), (test_opcode_ff, "ff"),
}
