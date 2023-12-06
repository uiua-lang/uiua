G ← 2e4
Dt ← 0.01
Steps ← 120

Step ← (
  ⊞≠.⇡⧻.        # Make comparison matrix
  ⊃(≡▽⊙¤)⋅∘     # Make others
  ⊃≡(≡-¤)∘:     # Get diffs
  ÷⍜(☇1|≡/+×.). # Inverse square
  ≡/+×G         # Get accs
  ⊃⟜+⋅∘ ×Dt     # Apply accs
  ⊃(+×Dt)∘      # Apply vels
)

[0_60 150_0 30_15 80_110]
⊃∘(↯:0⊂:2⧻) # Init vels

⁅×0.5⇌[⊙;⍥(Step ,:)Steps] # Run
⍜⊙(☇1)≡-¤/↧☇1.            # Normalize
⬚0≡(°⊚)                   # Plot
≡(⍜(☇2)≡(/↥/↥) ◫3_3)      # Upscale

&fwa "n-body.gif" &gife 16
&p "Wrote gif to n-body.gif"
