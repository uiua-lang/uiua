const AVG: &str = "\
[1 5 8 2]
⟜/+ # Sum
⧻   # Length
÷   # Divide";
const X_MATRIX: &str = "\
⇡5  # 0 through 4
⊞=. # Identity matrix
⊸⇌  # Reverse
↥   # Max";
pub const UIUA: &str = "\
\"Unabashedly I utilize arrays\"
⊸≠@  # Mask of non-spaces
⊜⊢   # All first letters";
pub const WEEWUH: &str = "\
\"We each eat whole ugly hams\"
⊸≠@  # Mask of non-spaces
⊜⊢   # All first letters";
const PRIMES: &str = "\
# Click Run to format!
+1drop1range40   # Range 2 to 40
deshapebytable*. # List of products
keepnotbymember  # Keep not in list";
pub const LOGO: &str = "\
U ← /=⊞<0.2_0.7 /+×⟜ⁿ1_2
I ← >⌵/ℂ: # Circle
u ← +0.1↧¤ ⊃(I0.95|⊂⊙0.5⇌°√)
A ← ×⊃U(I1) # Alpha
⍜°⍉(⊂⊃u A) -1÷÷2⟜(⇡↯2) 200";
pub const WEEWUH_LOGO: &str = "\
Wee ← /=⊞<¯0.5_¯0.1 /+⍜⊣(+⊃(×4ⁿ4|×¯3ⁿ2))
w   ← >⌵/ℂ: # Circle
u   ← +0.1↧¤ ⊃(w0.95|⊂⊙0.5⇌°√)
h   ← ×⊃Wee(w1) # Alpha
⍜°⍉(⊂⊃u h) -1÷÷2⟜(⇡↯2) 200";
const CHORD: &str = "\
[0 4 7 10]     # Notes
×220 ˜ⁿ2÷12    # Freqs
∿×τ ⊞× ÷⟜⇡&asr # Generate
÷⧻⟜/+⍉         # Mix";
const SPIRAL: &str = "\
↯⟜(×20-1×2÷⟜⇡)200 # Xs
-≡⊃∠(⍜∩°√+)⊸⍉     # Spiral field
-π◿τ⊞-×τ÷⟜⇡20     # Animate";
const QUADRATIC: &str = "\
Disc ← -⊃(××4⊙⋅∘)⋅°√
Quad ← ÷⊃(×2|-⊃⋅∘(⊟⊸¯ √ℂ0 Disc))
Quad 1 ¯3 2";
const STRIPES: &str = "\
[⊞⊃⊃+↥-].⇡300
⍉ ÷2 +1.2 ∿ ÷10";
pub const PALINDROME: &str = r#"$ uiua racecar wow cool!
≡⊂ ˜⊏"❌✅" ⬚@ ⊜⊸(≍⊸⇌)⊸≠@ "#;
pub const WEEWUH_PALINDROME: &str = r#"$ weewuh racecar wow cool!
≡⊂ ˜⊏"❌✅" ⬚@ ⊜⊸(≍⊸⇌)⊸≠@ "#;
const AUTOMATA: &str = "\
Rule ← /+⊞= ⊓(⊚⋯|°⋯⇌⧈∘3⇌ ⊂⊂0⊙0)
=⌊÷2⟜⇡ 500          # Init
⇌[◌⍥⟜⊸Rule⌊÷2⧻⤙] 30 # Run";
const ROMAN: &str = r#"K ← "IVXLCDM"
N ← [1 5 10 50 100 500 1000]
F ← /+-⊃(↻1×|×¬)⊸(⧈>⊂⊙0) ⊏⊙N ⊗⊙K
F "LVII"
F "MCMXCIV""#;
const MANDELBROT: &str = "\
×2 ⊞ℂ⤙-1/4 -1/2÷⟜⇡300   # Init
⍥(⊸(+>2⌵:)⊙⊸(+×.))50 0. # Run
÷/↥⊸♭ ⊙⋅◌               # Normalize";
const LIFE: &str = "\
Life ← ↥↧⟜(∩=2⤙3/+≡↻⊂A₂C₂¤)
⁅×0.6 gen⊙⚂ ⊟.30 # Init
⇌◌⍥(⟜⊂Life)100⟜¤ # Run
≡(▽⟜≡▽) 4        # Upscale";

pub const EXAMPLES: &[&str] = &[
    AVG, X_MATRIX, UIUA, PRIMES, LOGO, CHORD, SPIRAL, QUADRATIC, STRIPES, PALINDROME, AUTOMATA,
    ROMAN, MANDELBROT, LIFE,
];

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua_editor::backend::WebBackend;

    for example in EXAMPLES {
        match uiua::Uiua::with_backend(WebBackend::default()).run_str(example) {
            Ok(mut comp) => {
                if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                    panic!("Example failed:\n{example}\n{diag}");
                }
            }
            Err(e) => panic!("Example failed:\n{example}\n{e}"),
        }
    }
}
