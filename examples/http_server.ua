# A simple, parallel HTTP server good enough to serve Uiua's website
# 
# To do that, compile the site with trunk:
# ```
# cd site
# trunk build --release
# cd ..
# ```
# Then run:
# ```
# uiua run examples/http_server.ua
# ```

# Bind TCP listener
listener ← &tcpl "0.0.0.0:8080"
&p $ Server started

# Mime types
getext ← !⊢⇌ ⊜□ ≠@. .
⌀⊟⍉[{"js" "text/javascript"}
    {"html" "text/html"}
    {"wasm" "application/wasm"}]
extmimeExt ← 
extmimeMime ← 
extmime ← !⊏∶⊂extmimeMime □⊂"text/"∶ ⊗∶extmimeExt□.

# Convert string to bytes
bytes ← -@\0

# Make response: status mime bytes
response ← (|3 
  ∋··(⧻.)
  ⊂ bytes $ HTTP/1.1 _
          $ Content-Type: _
          $ Content-Length: _
          $ 
          $ 
)

# Handlers
notfound ← response "404 Not Found" "text/plain" bytes"Not Found"
servererror ← response "500 Internal Server Error" "text/plain"
handlepageloaderror ← (
  /↥⌕"The system cannot find the file specified".
  !servererror_(notfound;)
)
page ← ⍣(
  extmime getext ∶&frab.
  response "200 OK"
)handlepageloaderror ⊂"docs"
home ← page "/index.html"

respond ← ↰(
  # Extract path from request
  !⊡1 ⊜□≠@ . &ru "\r\n\r\n".
  &p "Request:"
  &p.

  # Route to handler
  !(|1 page)_(|1 home;) ≅"/".
  &p "Response:"
  &pp⧻.
  &p " bytes"

  # Send response
  ;;⊃(|2 0 &cl;)(|2 0 &w)
)

⍥(⍣(respond)&p &tcpa listener)∞
