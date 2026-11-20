use std::{io::stdout, io::Write, sync::*};

/// Sentinel for `prev_percent` meaning "nothing emitted yet" (real percents are 0..=100).
const PREV_PERCENT_NONE: usize = 101;

pub trait Progress {
    /// Starts a new stage; also resets progress and deduplication state.
    fn stage(&mut self, text: &str);
    /// Total for the current stage; `total == 0` is allowed and reports 0%.
    fn set_total(&mut self, total: usize);
    fn progress(&mut self, step: bool, text: &str);
    fn percent(&mut self, value: usize, total: usize, text: &str);
}

pub type IsCancelledFun = Arc::<dyn Fn() -> bool + Send + Sync + 'static>;

pub type ProgressTs = Arc<Mutex<dyn Progress + Send>>;

pub struct ProgressConsole {
    pos: usize,
    total: usize,
    prev_percent: usize,
    prev_text: String,
}

impl ProgressConsole {
    pub fn new() -> ProgressConsole {
        ProgressConsole {
            pos: 0,
            total: 1,
            prev_percent: PREV_PERCENT_NONE,
            prev_text: String::new()
        }
    }

    fn reset_state(&mut self) {
        self.pos = 0;
        self.prev_percent = PREV_PERCENT_NONE;
        self.prev_text.clear();
    }

    pub fn new_ts() -> ProgressTs {
        Arc::new(Mutex::new(
            ProgressConsole::new()
        ))
    }

    fn show_progress(&mut self, text: &str) {
        const MAX_WIDTH: usize = 42;
        let width = usize::checked_div(MAX_WIDTH * self.pos, self.total).map_or(0, |w| usize::min(w, MAX_WIDTH));
        let percent = usize::checked_div(100 * self.pos, self.total).map_or(0, |p| usize::min(p, 100));
        if percent == self.prev_percent && text == self.prev_text {
            return;
        }
        if self.prev_percent > percent { println!();}
        print!("{:3}% [", percent);
        for _ in 0..width { print!("#"); }
        for _ in width..MAX_WIDTH { print!("-"); }
        print!("] {}                   \r", text);
        stdout().flush().unwrap();
        if text != self.prev_text { log::info!("{}", text); }
        self.prev_text = text.to_string();
        self.prev_percent = percent;
    }
}

impl Progress for ProgressConsole {
    fn stage(&mut self, text: &str) {
        if self.pos != 0 {
            println!();
        }
        self.reset_state();
        println!("{}", text);
    }

    fn set_total(&mut self, total: usize) {
        self.total = total;
        self.reset_state();
    }

    fn progress(&mut self, step: bool, text: &str) {
        if step { self.pos += 1; }
        self.show_progress(text);
    }

    fn percent(&mut self, value: usize, total: usize, text: &str) {
        self.pos = value;
        self.total = total;
        self.show_progress(text);
    }
}

type StageFn = Box<dyn Fn(&str) + Send + 'static>;
type ProgressFn = Box<dyn Fn(usize, &str) + Send + 'static>;

pub struct ProgressCallBack {
    pos: usize,
    total: usize,
    prev_percent: usize,
    prev_text: String,
    stage_cb: StageFn,
    progress_cb: ProgressFn,
}

impl ProgressCallBack {
    pub fn new_ts<SF, PF>(stage_fun: SF, progress_fun: PF) -> ProgressTs
    where
        SF: Fn(&str) + Send + 'static,
        PF: Fn(usize, &str) + Send + 'static,
    {
        Arc::new(Mutex::new(ProgressCallBack {
            pos: 0,
            total: 1,
            prev_percent: PREV_PERCENT_NONE,
            prev_text: String::new(),
            stage_cb: Box::new(stage_fun),
            progress_cb: Box::new(progress_fun),
        }))
    }

    fn reset_state(&mut self) {
        self.pos = 0;
        self.prev_percent = PREV_PERCENT_NONE;
        self.prev_text.clear();
    }

    fn show_progress(&mut self, text: &str) {
        let percent = usize::checked_div(100 * self.pos, self.total).map_or(0, |p| usize::min(p, 100));
        if percent == self.prev_percent && text == self.prev_text {
            return;
        }
        self.prev_percent = percent;
        self.prev_text = text.to_string();
        (*self.progress_cb)(percent, text);
    }
}

impl Progress for ProgressCallBack {
    fn stage(&mut self, text: &str) {
        self.reset_state();
        (*self.stage_cb)(text);
    }

    fn set_total(&mut self, total: usize) {
        self.total = total;
        self.reset_state();
    }

    fn progress(&mut self, step: bool, text: &str) {
        if step { self.pos += 1; }
        self.show_progress(text);
    }

    fn percent(&mut self, value: usize, total: usize, text: &str) {
        self.pos = value;
        self.total = total;
        self.show_progress(text);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn console_stage_resets_state() {
        let mut p = ProgressConsole::new();
        p.set_total(10);
        p.progress(true, "a");
        p.stage("next stage");
        assert_eq!(p.pos, 0);
        assert_eq!(p.prev_percent, PREV_PERCENT_NONE);
        assert!(p.prev_text.is_empty());
    }

    #[test]
    fn callback_stage_resets_dedup_state() {
        let emitted: Arc<Mutex<Vec<(usize, String)>>> = Arc::new(Mutex::new(Vec::new()));
        let progress = ProgressCallBack::new_ts(
            |_| {},
            {
                let emitted = Arc::clone(&emitted);
                move |percent, text: &str| {
                    emitted.lock().unwrap().push((percent, text.to_string()));
                }
            },
        );
        let mut p = progress.lock().unwrap();
        p.set_total(10);
        p.progress(true, "a");
        p.stage("next stage");
        p.set_total(10);
        p.progress(true, "a");
        drop(p);
        // Without the reset in stage()/set_total() the identical second emission would be deduped.
        assert_eq!(emitted.lock().unwrap().len(), 2);
    }
}
