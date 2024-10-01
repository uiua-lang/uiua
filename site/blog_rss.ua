Tag   ← $"<__>_</_>" ⊃⊙⊙∘∘ ⊃↙↘⊸⊗@  ⊙(⍥/$"_\n_"=3type.)
Month ← ◇↙3⊏:Months-1⋕
Date ← $"_, _ +0000" ⊃(
  ◇↙3⊏:Days⌊◿7+4÷/×[60 60 24]
| datetime
  °⋕⍜↙⇌3                     # Flip date
  ⍜(↙¯3|□/(⊂⊂)@: ≡◇(⬚@0↙¯2)) # Collapse time
  ⍜(⊡1|□◇↙3⊏:Months-1⋕)      # Month
  /$"_ _"
)

# Parse out Path, Guid, Date, Title
&fras $ site/blog/list.txt
⊜□⊸≠@\n
⍚°$"_(_): _ - _"

Rss      ← $ rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom"
AtomLink ← $ atom:link href="https://uiua.org/blog/feed.rss" rel="self" type="application/rss+xml"
Tag Rss Tag "channel" ⊂ {
  Tag "title" $ Uiua Blog
  Tag "description" $ Blog of the Uiua Programming Language
  Tag "link" $ https://uiua.org/blog
  Tag AtomLink ""
  Tag "lastBuildDate" Date now
} ⍚(
  ⊙⊙(Date °datetime ⋕⊜□⊸≠@-) # Convert date
  Tag "item" {
    ⊓(Tag "link" $"https://uiua.org/blog/_"
    | Tag "guid isPermaLink=\"false\""
    | Tag "pubDate"
    | Tag "title"
    )})
⊂ $ <?xml version="1.0" encoding="UTF-8" ?>
&fwa "site/blog/feed.rss"
