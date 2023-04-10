const UIUA: &str = r#"‡=¯1 ≡/-◫2 <'A' ⊂' '."Um, I um...arrays""#;
const FORMAT: &str = "# Click Run to format!\nfold(join~/+drop`2.;)0_1range10";
const D3: &str = "↯~⇡/×.2_3_4";
const LOGO: &str = "\
x ← ↶⍉⊞⊟. ÷÷2~ -÷2,⇡.200
rgb ← [×.⊢⇌x ×.⊢x ∵0.5⊢x]
u ← ↥<0.2~>0.7.+×2 ×.~/·x
c ← √/+ⁿ2 x
⍉⊂~-¬u <1 c +0.1 ≡(↧<0.95 c)rgb";
const AVG: &str = "Avg ← ÷≢~/+.\nAvg 0_2_1_5";
const AUDIO: &str = "sin ×2×π ×110 √ ÷~ ⇡.44100";
const QUADRATIC: &str = "{÷×2 a -b ⊟¯.√-××4 a c ⁿ2 b}1 2 0";
const STRIPES: &str = "\
r ← ⊞+.⇡300
g ← ⊞↥.⇡300
b ← ⊞-.⇡300
⍉ ÷2 +1.2 sin ÷10[r g b]";
const PALINDROME: &str = r#"≡(≅⇌.).⊘≠' '."uiua racecar wow cool!""#;
const RULE_30: &str = "\
Thirty ← ≡(↥≅0_1_1 ~=1 /+.) ◫3 ⊂~0 ⊂0
size ← 600
start ← =÷2 size ⇡+1 size
⇌[⍥(Thirty.) start ÷2 size]";
const PRIMES: &str = "‡∵(=2 /+ =⌊.÷⇡.).+1 ⇡60";
const TRANS_STACK: &str = "⍉↯4_4[...1 .2 .3 ...4 .5 .6]";
const MANDELBROT: &str = "\
Z ← ⊟/- ⁿ2 ~×2 /×.⇌
⇌↶⍉⊞⊟.×4 ÷~-÷2,⇡. 300
<2 √/+ ⁿ2;~⍥(+Z ~,) ~20 ∵0.";
const CHART: &str = "⊞(⊡~·_._∴_↥_⍋ ⌊×5rand;;).⇡15";
const ERRORS: &str = "# Change this ↓ to a 0\n!\"Oh no bad!\" 1\nprint \"All is well\"";

pub const EXAMPLES: &[&str] = &[
    UIUA,
    FORMAT,
    D3,
    LOGO,
    AVG,
    AUDIO,
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
