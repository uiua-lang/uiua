# An amazing rotating donut example created by https://github.com/bhansconnect

Ro ← 1
Rt ← 2
Ko ← 150
Kt ← 5

T ← ×⇡⌈÷,τ0.07
sT ← ○T
cT ← ○+ηT
P ← ×⇡⌈÷,τ0.02
sP ← ○P
cP ← ○+ηP
Ox ← +Rt×Ro cT
Oy ← ×Ro sT

calcX ← -∶⊞×Ox+×cP○+η;↷××sP○,○,↷××Oy○+η,○,
calcY ← +⊞×Ox-∶×cP○;↷××sP○,○+η,↷××Oy○+η,○+η,
calcOoz ← ÷∶1++Kt ×⊞×Ox sP○+η∶×Oy○.
calcL ← ↥0↧1×0.7+++×○+η↶+×sT○+η∶¯×↶○.∶↷¯×↷.⊞×cT sP○+η.↷¯×sT○.↷×⊞×cT cP ○,

Render ← (
  ↷⁅+150 ××Ko ∶↶calcOoz.∶calcX,,
  ↷×320⁅-∶120 ××Ko ∶↶calcOoz.∶calcY,,
  &ims↯ 240_320 ⍛0↙ ×320 240⊕(↥0/↥)♭+↷♭calcL
)

;;⍥(∶+0.01∶+0.03 Render,,)∞ 1 1
