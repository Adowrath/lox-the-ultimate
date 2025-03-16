fn main() {
    println!("cargo::rustc-check-cfg=cfg(nightly, nightly_toolchain)");
    if rustversion::cfg!(nightly) {
        println!("cargo::rustc-cfg=nightly_toolchain");
        if cfg!(feature = "nightly") {
            println!("cargo::rustc-cfg=nightly");
        }
    }
}
