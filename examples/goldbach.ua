# Calculate the primes that add up to the first n even numbers

isprime ← =2 /+ =⌊.÷⇡.
primes ← ‡∵isprime.⇡
goldbach ← ∵(
  ‡/+=~.⊞+..primes↑.
  ⊂↓⊟⊢∶⊢⇌.
)×2+2⇡
goldbach 10
