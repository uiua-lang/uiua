const UIUA: &str = "\"Um, I um...arrays\"\n≡⊢ ⊘≥'A'.";
const FORMAT: &str = "# Click Run to format!\nfold(join~/+drop`2.;)0_1range10";
const D3: &str = "↯~⇡/×.2_3_4";
const LOGO: &str = "\
x ← ↶⍉⊞⊟. ÷÷2~ -÷2,⇡.200
rgb ← [×.⊢⇌x ×.⊢x ∵0.5⊢x]
u ← ↥<0.2~>0.7.+×2 ×.~/·x
c ← √/+ⁿ2 x
⍉⊂~-¬u <1 c +0.1 ≡(↧<0.95 c)rgb";
const AVG: &str = "Avg ← ÷≢~/+.\nAvg 0_2_1_5";
const CHORD: &str = "\
H ← ⁿ~2÷12
[×H10.~ ×H7.~ ×H4. 220]
÷≢~ ≡/+ sine ×2×π ⊞× ÷~ ⇡.44100.";
const QUADRATIC: &str = "\
Quadratic ← {÷ ×2a -b ⊟¯. √- ××4a c ⁿ2 b}
Quadratic 1 2 0";
const STRIPES: &str = "\
r ← ⊞+.⇡300
g ← ⊞↥.⇡300
b ← ⊞-.⇡300
⍉ ÷2 +1.2 sin ÷10[r g b]";
const PALINDROME: &str = r#"words ← "uiua racecar wow cool!"
≑⊂~ ⊏~[" ❌" " ✅"] ≡(≅⇌.). ⊘≠' '. words"#;
const RULE_30: &str = "\
Thirty ← ≡(↥≅0_1_1 ~=1 /+.) ◫3 ⊂~0 ⊂0
size ← 600
start ← =÷2 size ⇡+1 size
⇌[⍥(Thirty.)÷2 size start]";
const PRIMES: &str = "‡∵(=2 /+ =⌊.÷⇡.).+1 ⇡60";
const MANDELBROT: &str = "\
Z ← ⊟/- ⁿ2 ~×2 /×.⇌
⇌↶⍉⊞⊟.×4 ÷~-÷2,⇡. 300
<2 √/+ ⁿ2;~⍥(+Z ~,)20 ∵0.";
const CHART: &str = "⊞(⊡ ⌊×5rand ·_._∴_↥_⍋ ;;).⇡15";
const ERRORS: &str = "# Change this ↓ to a 0\n!\"Oh no bad!\" 1\nprint \"All is well\"";

pub const EXAMPLES: &[&str] = &[
    UIUA, FORMAT, D3, LOGO, AVG, CHORD, QUADRATIC, STRIPES, PALINDROME, RULE_30, PRIMES,
    MANDELBROT, CHART, ERRORS,
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
