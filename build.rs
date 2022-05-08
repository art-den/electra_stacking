fn main() -> std::io::Result<()> {
    #[cfg(all(target_os = "windows", not(debug_assertions)))] {
        embed_resource::compile("win_resources/resource.rc");
    }
    Ok(())
}
