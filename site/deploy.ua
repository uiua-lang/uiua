✅ ← ⍤⟜≍:0

¬∊ □"resume" &args
⍥(
  ✅&runi {"git" "checkout" "site" "--"}
  ✅&runi {"git" "reset" "--hard" "origin/site"}
  ✅&runi {"git" "rebase" "main"}
)
&cd "site"
✅&runi {"cargo" "test" "-p" "site" "gen_blog_html"}
✅&runi {"trunk" "build" "--release" "-d" "../docs"}
✅&runi {"git" "add" "--all"}
✅&runi {"git" "commit" "--amend" "--no-edit"}
✅&runi {"git" "push" "-f"}
✅&runi {"git" "checkout" "main"}
