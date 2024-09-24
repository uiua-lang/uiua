Tag ← $"<__>_</_>" ⊃⊙⊙∘∘ ⊃↙↘⊸⊗@  ⊙(⍥/$"_\n_"=3type.)
Month ← ◇↙3⊏:Months-1⋕
Date ← $"_, _ +0000" ⊃(
  ◇↙3⊏:Days⌊◿7+3÷/×[60 60 24]
| datetime
  °⋕⍜↙⇌3                     # Flip date
  ⍜(↙¯3|□/(⊂⊂)@: ≡◇(⬚@0↙¯2)) # Collapse time
  ⍜(⊡1|□◇↙3⊏:Months-1⋕)      # Month
  /$"_ _"
)

# Parse out Path, Date, Title
&fras $ site/blog/list.txt
⊜□⊸≠@\n
⍚°$"_: _ - _"

Tag "rss version=\"2.0\"" Tag "channel" ⊂ {
  Tag "title" $ Uiua Blog
  Tag "description" $ Blog of the Uiua Programming Language
  Tag "link" $ https://uiua.org/blog
  Tag "lastBuildDate" Date now} ⍚(
  ⊙(Date °datetime ⋕⊜□⊸≠@-) # Convert date
  Tag "item" {
    ⊓(Tag "link" $"https://uiua.org/blog/_"
    | Tag "pubDate"
    | Tag "title"
    )})
⊂ $ <?xml version="1.0" encoding="UTF-8" ?>
&fwa "site/blog/feed.rss"
