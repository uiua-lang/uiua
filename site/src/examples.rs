const UIUA: &str = "\"Um, I um...arrays\"\n≡⊢ ⊘≥'A'.";
const FORMAT: &str = "# Click Run to format!\nfold(join∶/+take`2.;)rang2range10";
const D3: &str = "↯∶⇡/×.2_3_4";
const LOGO: &str = "\
x ← ↶⍉⊞⊟. ÷÷2∶ -÷2,⇡.200
rgb ← [×.⊢⇌x ×.⊢x ↯△⊢x0.5]
u ← ↥<0.2∶>0.7.+×2 ×.∶/·x
C ← <∶√/+ⁿ2 x
⍉⊂∶-¬u c1 +0.1 ⫫↧rgb c0.95";
const AVG: &str = "Avg ← ÷⧻∶/+.\nAvg 0_2_1_5";
const CHORD: &str = "\
[0 4 7 10]
×220 ⁿ∶2÷12
÷⧻∶ ≡/+ ○×τ ⊞× ÷∶⇡.44100.";
const QUADRATIC: &str = "\
Quadratic ← {÷ ×2a -b ⊟¯. √- ××4a c ⁿ2 b}
Quadratic 1 2 0";
const STRIPES: &str = "\
∵{⊞a.⇡300}+_↥_-
⍉ ÷2 +1.2 ○ ÷10";
const PALINDROME: &str = r#"$ uiua racecar wow cool!
≡⊂ ⊏∶"❌✅" ≡(≅⇌.). ⊘≠' '."#;
const RULE_30: &str = "\
Thirty ← ≡(↥≅0_1_1 ∶=1 /+.) ◫3 ⊂∶0 ⊂0
size ← 500
start ← =÷2 size ⇡+1 size
⇌[⍥(Thirty.)÷2 size start]";
const PRIMES: &str = "‡∵(=2 /+ =⌊.÷⇡.).+1 ⇡50";
const MANDELBROT: &str = "\
Z ← ⊟/- ⁿ2 ∶×2 /×.⇌
⇌↶⍉⊞⊟.×4 ÷∶-÷2,⇡. 300
<2 √/+ ⁿ2;∶⍥(+Z ∶,)20 ↯∶0△.";
const CHART: &str = "\
$ The forest calls
⊞(⊡ ⌊×⚂⧻. ·_._∴_↥_⍋ ;;).⇡15";

pub const EXAMPLES: &[&str] = &[
    UIUA, FORMAT, D3, LOGO, AVG, CHORD, QUADRATIC, STRIPES, PALINDROME, RULE_30, PRIMES,
    MANDELBROT, CHART,
];

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua::Uiua;
    for example in EXAMPLES {
        Uiua::with_native_sys()
            .load_str(example)
            .unwrap_or_else(|e| panic!("Example failed:\n{example}\n{e}"));
    }
}
