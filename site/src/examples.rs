const AVG: &str = "\
[1 5 8 2]
/+. # Sum
⧻∶  # Length
÷   # Divide";
const D3: &str = "\
2_3_4
/×. # Product
⇡   # Range
↯∶  # Reshape";
const UIUA: &str = "\
\"Unabashedly I utilize arrays\"
≠@ . # Mask of non-spaces
⊜⊢   # All first letters";
const PRIMES: &str = "\
# Click Run to format!
+1drop1range40 # Range 2 to 40
...            # Make 3 copies
deshapetable*  # List of products
keepnotmember∶ # Keep not in list";
pub const LOGO: &str = "\
Xy ← ⍘⍉⊞⊟. ÷÷2∶ -÷2,⇡.200
Rgb ← [∶⍘⊟×.Xy ↯△⊢Xy0.5]
u ← ↥<0.2∶>0.7.+×2 ×.∶⍘⊟Xy
c ← <∶√/+ⁿ2 Xy
⍉⊂∶-¬u c1 +0.1 ∺↧c0.95Rgb";
const CHORD: &str = "\
[0 4 7 10]       # Notes
×220 ⁿ∶2÷12      # Freqs
○×τ ⊞× ÷∶⇡.&asr. # Generate
÷⧻∶ ≡/+          # Mix";
const SPIRAL: &str = "\
⇌×τ÷⊃∘⇡20           # Frame times
⍉.↯∶×20-1×2÷∶⇡..300 # x and y
∺(-⊃∠(-π◿τ+⍜∩⍘√+))  # Generate
<0.2⌵               # Threshold";
const QUADRATIC: &str = "\
Disc ← +×.⊙(××¯4)∶
Quad ← ÷⊙-⊃⊙(×2)(⊟¯.√Disc∶)∶
Quad 1 2 0";
const STRIPES: &str = "\
[⊃⊃⊞+⊞↥⊞-].⇡300
⍉ ÷2 +1.2 ○ ÷10";
const PALINDROME: &str = r#"$ uiua racecar wow cool!
⬚@ ⊜(⊂⊏∶"❌✅" ≍⇌..)≠@ ."#;
const AUTOMATA: &str = "\
Rule ← /+⊞=∶ ⍘⋯⇌◫3⇌ ⇌∸⇌∸∶ ▽∶⇡⧻.⋯
=⌊÷2∶⇡.500         # Init
⇌[⍥(Rule30.)⌊÷2⧻.] # Run";
const ROMAN: &str = r#"k ← "IVXLCDM"
n ← [1 5 10 50 100 500 1000]
f ← /+-⊃(↻1×)(×¬)≡/>◫2⊂∶0.⊏∶n⊗∶k
f "LVII"
f "MCMXCIV""#;
const MANDELBROT: &str = "\
Z ← ⊟/- ⁿ2 ∶×2 /×.⇌        # Step
⇌⍘⍉⊞⊟.×4 ÷∶-÷2,⇡. 300      # Init
<2 √/+ ⁿ2;∶⍥(+Z⊙.)20 ↯∶0△. # Run";
const LIFE: &str = "\
Life ← ↥⊙↧∩=3,2-,/+/+≑≃≊↻-1⇡3_3.
⁅×0.6∵⋅⚂↯⊟.30 0       # Init
⇌;⍥(⊃∘⊂Life)100⊃∘(↯1) # Run
≡(▽↯⧻,∶⍉▽↯⧻,,∶5)      # Upscale";

pub const EXAMPLES: &[&str] = &[
    AVG, D3, UIUA, PRIMES, LOGO, CHORD, SPIRAL, QUADRATIC, STRIPES, PALINDROME, AUTOMATA, ROMAN,
    MANDELBROT, LIFE,
];

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua::Uiua;
    for example in EXAMPLES {
        let mut env = Uiua::with_native_sys();
        if let Err(e) = env.load_str(example) {
            panic!("Example failed:\n{example}\n{e}");
        } else if let Some(diag) = env.take_diagnostics().into_iter().next() {
            panic!("Example failed:\n{example}\n{diag}");
        }
    }
}
