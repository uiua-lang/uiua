# An amazing rotating donut example created by https://github.com/bhansconnect

Ro ← 1
Rt ← 2
Ko ← 150
Kt ← 5

T ← ×⇡⌈÷,τ0.07
St ← ○T
Ct ← ○+ηT
P ← ×⇡⌈÷,τ0.02
Sp ← ○P
Cp ← ○+ηP
Ox ← +Rt×Ro Ct
Oy ← ×Ro St

CalcX ← ♭+⊙(¯××Oy○:○+η)⊞×Ox+⊙(××Sp○,○,)×Cp○+η,
CalcY ← ♭+⊙(××Oy○+η:○+η)⊞×Ox+⊙(¯××Sp○,○+η,)×Cp○,
CalcOoz ← ♭÷:1++Kt ×⊞×Ox Sp○+η:×Oy○.
CalcL ← ♭↥0↧1×0.7+¯×○+η:⊙(:+⊙(×⊞×Ct Cp○:)+⊙(¯×St ○.)×⊙(○+η,)+×St○+η,¯×○,).⊞×Ct Sp

Render ← (
  ⁅+150 ××Ko CalcOoz,CalcX,,
  ⊙(×320⁅-:120 ××Ko CalcOoz,CalcY,,)
  ↯ 240_320 ⬚0↙ ×320 240⊕(↥0/↥)⊙CalcL+
)

◌◌⍥(:+0.01:+0.03 &ims Render,,)∞ 1 1
