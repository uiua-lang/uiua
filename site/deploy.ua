✅ ← ⍤⊃∘≍0

&cd "site"
✅&runi {"git" "checkout" "site" "-f"}
✅&runi {"git" "reset" "--hard" "origin/site"}
✅&runi {"git" "rebase" "main"}
✅&runi {"trunk" "build" "--release"}
✅&runi {"git" "add" "--all"}
✅&runi {"git" "commit" "--amend" "--no-edit"}
✅&runi {"git" "push" "-f"}
✅&runi {"git" "checkout" "main"}
