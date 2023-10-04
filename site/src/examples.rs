const UIUA: &str = "\"Um, I um...arrays\"\n⊜⊢≥@A.";
const FORMAT: &str = "# Click Run to format!\nkeepnotmem:deshtab*...+2rang50";
const D3: &str = "↯∶⇡/×.2_3_4";
pub const LOGO: &str = "\
xy ← ⍘⍉⊞⊟. ÷÷2∶ -÷2,⇡.200
rgb ← [∶⍘⊟×.xy ↯△⊢xy0.5]
u ← ↥<0.2∶>0.7.+×2 ×.∶⍘⊟xy
c ← <∶√/+ⁿ2 xy
⍉⊂∶-¬u c1 +0.1 ∺↧rgb c0.95";
const AVG: &str = "Avg ← ÷⧻∶/+.\nAvg 0_2_1_5";
const CHORD: &str = "\
[0 4 7 10]
×220 ⁿ∶2÷12
÷⧻∶ ≡/+ ○×τ ⊞× ÷∶⇡.&asr.";
const QUADRATIC: &str = "\
Quad ← ÷→+⇉(→¯×2)(⊟¯.√+×.∶××¯4→∶)
Quad 1 2 0";
const STRIPES: &str = "\
∺(|2 ⊞^∶,)+_↥_- ⇡300
⍉ ÷2 +1.2 ○ ÷10";
const PALINDROME: &str = r#"$ uiua racecar wow cool!
⍛@ ⊜(⊂⊏∶"❌✅" ≅⇌..)≠@ ."#;
const AUTOMATA: &str = "\
rule ← /+⊞=∶ ⍘⋯⇌◫3⇌ ⊂∶0⊂0∶ ▽∶⇡⧻.⋯
=⌊÷2∶⇡.500         # init
⇌[⍥(rule30.)⌊÷2⧻.] # run";
const MANDELBROT: &str = "\
Z ← ⊟/- ⁿ2 ∶×2 /×.⇌
⇌⍘⍉⊞⊟.×4 ÷∶-÷2,⇡. 300
<2 √/+ ⁿ2;∶⍥(+Z ∶,)20 ↯∶0△.";
const LIFE: &str = "\
life ← ↥→↧=3∶=2.-,/+/+⍚1_2↻-1⇡3_3.
⍛0↙10_10 ⋯×4 0_2_4_7 # init
⍉;⍥(→'⊂∶.life)10.    # run
↯∶≡'↯4∵'↯4∶×4△.      # upscale";

pub const EXAMPLES: &[&str] = &[
    UIUA, FORMAT, D3, LOGO, AVG, CHORD, QUADRATIC, STRIPES, PALINDROME, AUTOMATA, MANDELBROT, LIFE,
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
