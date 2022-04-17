use std::{io::stdout, io::Write, sync::*};

pub trait Progress {
    fn stage(&mut self, text: &str);
    fn set_total(&mut self, total: usize);
    fn progress(&mut self, step: bool, text: &str);
    fn percent(&mut self, value: usize, total: usize, text: &str);
}

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
            prev_percent: 101,
            prev_text: String::new()
        }
    }

    pub fn new_ts() -> ProgressTs {
        Arc::new(Mutex::new(
            ProgressConsole::new()
        ))
    }

    fn show_progress(&mut self, text: &str) {
        const MAX_WIDTH: usize = 42;
        let width = (MAX_WIDTH * self.pos / self.total).min(MAX_WIDTH);
        let percent = (100 * self.pos / self.total).min(100);
        if percent == self.prev_percent && text == &self.prev_text {
            return;
        }
        if self.prev_percent > percent { println!("");}
        print!("{:3}% [", percent);
        for _ in 0..width { print!("#"); }
        for _ in width..MAX_WIDTH { print!("-"); }
        print!("] {}                   \r", text);
        stdout().flush().unwrap();
        if text != &self.prev_text { log::info!("{}", text); }
        self.prev_text = text.to_string();
        self.prev_percent = percent;
    }
}

impl Progress for ProgressConsole {
    fn stage(&mut self, text: &str) {
        if self.pos != 0 {
            println!("");
            self.pos = 0;
        }
        println!("{}", text);
    }

    fn set_total(&mut self, total: usize) {
        self.total = total;
        self.pos = 0;
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

pub struct ProgressCallBack {
    pos: usize,
    total: usize,
    prev_percent: usize,
    prev_text: String,
    stage_cb: Box<dyn Fn(&str) + Send + 'static>,
    progress_cb: Box<dyn Fn(usize, &str) + Send + 'static>,
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
            prev_percent: 101,
            prev_text: String::new(),
            stage_cb: Box::new(stage_fun),
            progress_cb: Box::new(progress_fun),
        }))
    }

    fn show_progress(&mut self, text: &str) {
        let percent = (100 * self.pos / self.total).min(100);
        if percent == self.prev_percent && text == &self.prev_text {
            return;
        }
        self.prev_percent = percent;
        self.prev_text = text.to_string();
        (*self.progress_cb)(percent, text);
    }
}

impl Progress for ProgressCallBack {
    fn stage(&mut self, text: &str) {
        (*self.stage_cb)(text);
    }

    fn set_total(&mut self, total: usize) {
        self.total = total;
        self.pos = 0;
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
