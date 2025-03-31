✅ ← ⍤⤙≍ 0 &runi

$Branch ⍣(°□⊡+1)"main"⊸(↧⊃(⊗□"-b"|⊗□"--branch")) &args
$Resume ¬∊ &args □"resume"
⊸⍥(
  ✅ {"git" "checkout" "site" "--"}
  ✅ {"git" "reset" "--hard" "origin/site"}
  ✅ {"git" "rebase" ∘}
)
&cd "site"
✅ {"cargo" "test" "-p" "site" "gen_blog_html"}
✅ {"trunk" "build" "--release" "-d" "../docs"}
✅ {"git" "add" "--all"}
✅ {"git" "commit" "--amend" "--no-edit"}
✅ {"git" "push" "-f"}
✅ {"git" "checkout" ∘}
