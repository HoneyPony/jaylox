#[cfg(feature = "run")]
use libtcc::{Guard, Context, OutputType};
use std::ffi::CString;
use std::ffi::CStr;
use std::mem::transmute;

#[cfg(feature = "run")]
fn run_libtcc_or_err(compiled_c_code: &CString) -> Result<(), &str> {
    let mut g = Guard::new()?;

    let mut ctx = Context::new(&mut g)
        .map_err(|_| "libtcc: failed to create Context")?;

    // Note: we MUST call this for libtcc to work. Otherwise it complains
    // with all sorts of include errors.
    //
    // However, it appears that setting this means we don't need to fuss
    // with include/library paths at all.
    ctx.set_output_type(OutputType::Memory);

    // Add the -w option to suppress warnings.
    ctx.set_options(c"-w");

    ctx.compile_string(&compiled_c_code)
        .map_err(|_| "libtcc: failed to compile generated C code.")?;

    let mut relocated = ctx.relocate()
        .map_err(|_| "libtcc: failed to relocate.")?;

    let addr = unsafe {
        relocated
            .get_symbol(CStr::from_bytes_with_nul_unchecked(b"main\0"))
    }.ok_or("libtcc: failed to locate 'main' symbol in compiled C code.")?;

    let main: fn() -> i32 = unsafe { transmute(addr) };

    let exit_code = main();

    std::process::exit(exit_code);
}

#[cfg(feature = "run")]
pub fn run_libtcc(compiled_c_code: &CString) {
    if let Err(msg) = run_libtcc_or_err(compiled_c_code) {
        eprintln!("failed to run Lox script: {}\n", msg);
        std::process::exit(70);
    }
}

#[cfg(not(feature = "run"))]
pub fn run_libtcc(compiled_c_code: &CString) {
    eprintln!("libtcc support is not enabled. cannot run.");
    std::process::exit(70);
}