pub struct TimeLogger {
    start_time: std::time::Instant,
}

impl TimeLogger {
    pub fn start() -> TimeLogger {
        TimeLogger { start_time: std::time::Instant::now() }
    }

    pub fn log(self, text: &str) {
        let time = self.start_time.elapsed().as_secs_f64();
        log::info!("BENCH {} time = {:.6} s", text, time);
    }
}
