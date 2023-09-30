&cd "site"
&runi {"git" "checkout" "site"}
&runi {"git" "reset" "--hard" "origin/site"}
&runi {"git" "rebase" "main"}
&runi {"trunk" "build" "--release"}
&runi {"git" "add" "--all"}
&runi {"git" "commit" "--amend" "--no-edit"}
&runi {"git" "push" "-f"}
&cd ".."
