const UIUA: &str = r#"‡=¯1 ≡/-◫2 <'A' ⊂' '."Um, I um...arrays""#;
const FORMAT: &str = "# Click Run to format!\nfirst repeat (\\+ rev)0_1 10";
const D3: &str = "↯~⇡/×.2_3_4";
const LOGO: &str = "\
x ← ⊞⚇.÷÷2 ~-÷2,⇡. 256
rgb ← ⊞: (×.⊢⇌)_(×.⊢)_0.5 x
u ← ∵(↥<0.2 ~>0.7.+×2 ×.~/·) x
c ← √∵(/+ ⁿ2) x
⍉⊂~-¬u <1 c +0.1 ≡(↧<0.95 c) rgb";
const AVG: &str = "Avg ← ÷⇀~/+.\nAvg 0_2_1_5";
const QUADRATIC: &str = "{÷×2 a -b ⚇¯.√-××4 a c ⁿ2 b}1 2 0";
const STRIPES: &str = "\
r ← ⌵sin ÷10 ⊞+.⇡300 
g ← ⌵sin ÷21 ⊞↥.⇡300 
b ← ⌵sin ÷32 ⊞-.⇡300 
⍉[r g b]";
const PALINDROME: &str = r#"⊟∵(≅⇌.).⊢⊕=' '."uiua racecar wow cool!""#;
const RULE_30: &str = "\
Thirty ← ≡(↥≅0_1_1 ~=1 /+.) ◫3 ⊂~0 ⊂0
size ← 600
start ← =÷2 size ⇡+1 size
↯⊂~1 ⇌△.⇌[⍥(Thirty.) start ÷2 size]";
const PRIMES: &str = "‡∵(=2 /+ =⌊.÷⇡.).+1 ⇡60";
const TRANS_STACK: &str = "⍉↯4_4[...1 .2 .3 ...4 .5 .6]";
const MANDELBROT: &str = "\
Z ← +⚇/- ⁿ2 ~×2 /×.⇌
⍉⊞⚇.÷25 -÷2 ~⇡. 100
↯⊂~1 △.∵(<1 √/+ ⁿ2;~⍥(Z ~,) 0_0 10)";
const CHART: &str = "∵(⊙~·_:_÷_≡_⍋ ⁅÷23)⊞×.-10 ⇡20";
const ERRORS: &str = "# Change this ↓ to a 0\n!\"Oh no bad!\" 1\nprintln \"All is well\"";

pub const EXAMPLES: &[&str] = &[
    UIUA,
    FORMAT,
    D3,
    LOGO,
    AVG,
    QUADRATIC,
    STRIPES,
    PALINDROME,
    RULE_30,
    PRIMES,
    TRANS_STACK,
    MANDELBROT,
    CHART,
    ERRORS,
];

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua::Uiua;
    for example in EXAMPLES {
        Uiua::with_stdio()
            .load_str(example)
            .unwrap_or_else(|e| panic!("Example failed:\n{example}\n{e}"));
    }
}
