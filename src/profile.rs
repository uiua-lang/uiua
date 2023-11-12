#[macro_export]
#[doc(hidden)]
macro_rules! profile_function {
    () => {
        #[cfg(feature = "profile")]
        let _scope = $crate::profile::enabled::Scope::new(
            $crate::profile::enabled::current_function_name!(),
        );
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! profile_scope {
    ($label:expr) => {
        #[cfg(feature = "profile")]
        let _scope = $crate::profile::enabled::Scope::new($label);
    };
}

pub fn run_profile() {
    #[cfg(feature = "profile")]
    enabled::run_profile();
}

#[cfg(feature = "profile")]
pub(crate) mod enabled {
    use std::{
        collections::HashMap,
        fs,
        io::{stdout, Write},
        sync::OnceLock,
        time::Instant,
    };

    use crossbeam_channel::{Receiver, Sender};

    const BENCHMARKS: &[(&str, &str)] = &[
        ("PRIMES", "▽¬∊∶♭⊞×...+2⇡1000"),
        (
            "STRIPES",
            "\
[⊃⊃⊞+⊞↥⊞-].⇡300
⍉ ÷2 +1.2 ○ ÷10",
        ),
        (
            "AUTOMATA",
            "\
Rule ← /+⊞=∶ ⍘⋯⇌◫3⇌ ⇌∸⇌∸∶ ⊚⋯
=⌊÷2∶⇡.500         # Init
⇌[⍥(Rule30.)⌊÷2⧻.] # Run",
        ),
        (
            "MANDELBROT",
            "\
Z ← ⊟/- ⁿ2 ∶×2 /×.⇌        # Step
⇌⍘⍉⊞⊟.×4 ÷∶-÷2,⇡. 300      # Init
<2 √/+ ⁿ2;∶⍥(+Z⊙.)20 ↯∶0△. # Run",
        ),
        (
            "CHORD",
            "\
[0 4 7 10]       # Notes
×220 ⁿ∶2÷12      # Freqs
○×τ ⊞× ÷∶⇡.&asr. # Generate
÷⧻∶ ≡/+          # Mix",
        ),
        (
            "LOGO",
            "\
xy ← ⍘⍉⊞⊟. ÷÷2∶ -÷2,⇡.200
Rgb ← [∶⍘⊟×.xy ↯△⊢xy0.5]
u ← ↥<0.2∶>0.7.+×2 ×.∶⍘⊟xy
c ← <∶√/+ⁿ2 xy
⍉⊂∶-¬u c1 +0.1 ∺↧c0.95Rgb",
        ),
        (
            "LIFE",
            "\
Life ← ↥⊙↧∩=3,2-,/+/+≑≃≊↻-1⇡3_3.
⁅×0.6∵⋅⚂↯⊟.30 0       # Init
⇌;⍥(⊃∘⊂Life)100⊃∘(↯1) # Run
≡(▽↯⧻,∶⍉▽↯⧻,,∶5)      # Upscale",
        ),
        (
            "SPIRAL",
            "\
⇌×τ÷⊃∘⇡20           # Frame times
⍉.↯∶×20-1×2÷∶⇡..300 # x and y
∺(-⊃∠(-π◿τ+⍜∩⍘√+))  # Generate
<0.2⌵               # Threshold",
        ),
    ];

    const RUNS: usize = 20;

    pub fn run_profile() {
        if cfg!(debug_assertions) {
            eprintln!("Profiling must be done in release mode");
            return;
        }

        const WARMUP_RUNS: usize = 3;
        for i in 0..WARMUP_RUNS {
            print!("\rProfiling... warmup {}/{}", i + 1, WARMUP_RUNS);
            stdout().flush().unwrap();
            for (_, bench) in BENCHMARKS {
                Uiua::with_native_sys().load_str(bench).unwrap();
            }
        }

        init_profiler();

        for i in 0..RUNS {
            profile_scope!("run");
            print!("\rProfiling... run {}/{}   ", i + 1, RUNS);
            stdout().flush().unwrap();
            for (name, bench) in BENCHMARKS {
                profile_scope!(name);
                Uiua::with_native_sys().load_str(bench).unwrap();
            }
        }

        println!("\rProfiling complete         ");
        end_profiler();
    }

    #[inline(always)]
    pub fn type_name_of<T>(_: T) -> &'static str {
        std::any::type_name::<T>()
    }
    #[inline]
    pub fn clean_function_name(name: &str) -> &str {
        if let Some(colon) = name.rfind("::") {
            if let Some(colon) = name[..colon].rfind("::") {
                // "foo::bar::baz::function_name" -> "baz::function_name"
                &name[colon + 2..]
            } else {
                // "foo::function_name" -> "foo::function_name"
                name
            }
        } else {
            name
        }
    }

    /// Returns the name of the calling function without a long module path prefix.
    #[macro_export]
    macro_rules! current_function_name {
        () => {{
            fn f() {}
            let name = $crate::profile::enabled::type_name_of(f);
            // Remove "::f" from the name:
            let name = &name.get(..name.len() - 3).unwrap();
            $crate::profile::enabled::clean_function_name(name)
        }};
    }
    pub(crate) use current_function_name;
    use indexmap::IndexMap;
    use serde::{Deserialize, Serialize};

    use crate::Uiua;
    pub struct Scope {
        name: &'static str,
        start: Instant,
    }

    impl Scope {
        pub fn new(name: &'static str) -> Self {
            Scope {
                name,
                start: Instant::now(),
            }
        }
    }

    struct FinishedScope {
        name: &'static str,
        dur: f64,
    }

    impl Drop for Scope {
        fn drop(&mut self) {
            let end = Instant::now();
            let finished = FinishedScope {
                name: self.name,
                dur: (end - self.start).as_secs_f64(),
            };
            if let Some(send) = SEND.get() {
                send.send(finished).unwrap();
            }
        }
    }

    static SEND: OnceLock<Sender<FinishedScope>> = OnceLock::new();
    static RECV: OnceLock<Receiver<FinishedScope>> = OnceLock::new();

    fn init_profiler() {
        let (send, recv) = crossbeam_channel::unbounded();
        SEND.set(send).unwrap();
        RECV.set(recv).unwrap();
    }

    fn end_profiler() {
        let mut times = HashMap::new();
        for scope in RECV.get().unwrap().try_iter() {
            times
                .entry(scope.name)
                .or_insert(Vec::new())
                .push(scope.dur);
        }
        if times.is_empty() {
            return;
        }
        let max_total_dur = times
            .values()
            .map(|v| v.iter().sum::<f64>())
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap();

        #[derive(Serialize, Deserialize)]
        struct Profile {
            avg_total_dur: f64,
            entries: IndexMap<String, Entry>,
        }

        #[derive(Serialize, Deserialize)]
        struct Entry {
            share: f64,
            median_dur: f64,
            mean_dur: f64,
            max_dur: f64,
            min_dur: f64,
            total_dur: f64,
            count: usize,
        }

        let mut entries = IndexMap::new();
        for (name, mut times) in times {
            times.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
            let total_dur = times.iter().sum::<f64>();
            let count = times.len();
            entries.insert(
                name.to_string(),
                Entry {
                    share: total_dur / max_total_dur,
                    total_dur,
                    max_dur: times[count - 1],
                    min_dur: times[0],
                    count,
                    mean_dur: total_dur / count as f64,
                    median_dur: times[count / 2],
                },
            );
        }
        entries.sort_by(|_, a, _, b| a.share.partial_cmp(&b.share).unwrap().reverse());

        let profile = Profile {
            avg_total_dur: max_total_dur / RUNS as f64,
            entries,
        };

        if let Some(previous) = fs::read("profile.yaml")
            .ok()
            .and_then(|bytes| serde_yaml::from_slice::<Profile>(&bytes).ok())
        {
            // Average total duration
            let avg_total_dur_change =
                percent_change(previous.avg_total_dur, profile.avg_total_dur) * 100.0;
            println!("{:<24} | {:+.0}%", "Total duration", avg_total_dur_change);

            // Scopes
            println!("{}", "-".repeat(56));
            println!(
                "{:<24} | {:>7} {:>5} | {:>5} {:>5} |",
                "scope", "median", "%Δ", "share", "%Δ"
            );
            println!("{}", "-".repeat(56));
            for (name, new_entry) in profile.entries.iter().skip(1) {
                let median_dur = format!("{:.4}", new_entry.median_dur);
                let share = format!("{:.1}%", new_entry.share * 100.0);
                let mut media_dur_change = String::new();
                let mut share_change = String::new();
                if let Some(previous_entry) = previous.entries.get(name) {
                    media_dur_change = format!(
                        "{:+.0}%",
                        percent_change(previous_entry.median_dur, new_entry.median_dur) * 100.0
                    );
                    share_change = format!(
                        "{:+.0}%",
                        percent_change(previous_entry.share, new_entry.share) * 100.0
                    );
                    if ["+0%", "-0%"].contains(&media_dur_change.as_str()) {
                        media_dur_change.clear();
                    }
                    if ["+0%", "-0%"].contains(&share_change.as_str()) {
                        share_change.clear();
                    }
                }
                println!(
                    "{:<24} | {:>7} {:>5} | {:>5} {:>5} |",
                    name, median_dur, media_dur_change, share, share_change
                );
            }
        }

        fs::write("profile.yaml", serde_yaml::to_string(&profile).unwrap()).unwrap();
    }

    fn percent_change(a: f64, b: f64) -> f64 {
        if b > a {
            b / a - 1.0
        } else {
            1.0 - a / b
        }
    }
}
