fn main() -> std::io::Result<()> {
    if cfg!(target_os = "windows") && !cfg!(debug_assertions) {
        embed_resource::compile("win_resources/resource.rc");
    }
    Ok(())
}
