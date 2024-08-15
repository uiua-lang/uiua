✅ ← ⍤⟜≍:0 &runi

¬∈ &args □"resume"
⍥(
  ✅ {"git" "checkout" "site" "--"}
  ✅ {"git" "reset" "--hard" "origin/site"}
  ✅ {"git" "rebase" "main"}
)
&cd "site"
✅ {"cargo" "test" "-p" "site" "gen_blog_html"}
✅ {"trunk" "build" "--release" "-d" "../docs"}
✅ {"git" "add" "--all"}
✅ {"git" "commit" "--amend" "--no-edit"}
✅ {"git" "push" "-f"}
✅ {"git" "checkout" "main"}
