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
Listener ← &tcpl "0.0.0.0:8080"
&p $ Server started

# Mime types
GetExt ← °□⊣ ⊜□ ≠@. .
°⊟⍉[
  {"js" "text/javascript"}
  {"wasm" "application/wasm"}
]
ExtMimeExt  ←
ExtMimeMime ←
ExtMime     ← °□⊏:⊂ExtMimeMime □⊂"text/": ⊗:ExtMimeExt□.

# ? Status Mime Bytes
Response ← (
  ⊙⊙(⧻.)
  $$ HTTP/1.1 _
  $$ Content-Type: _
  $$ Content-Length: _
  $$ 
  $$ 
  ⊂ utf₈
)

# Handlers
NotFound    ← Response "404 Not Found" "text/plain" utf₈"Not Found"
ServerError ← Response "500 Internal Server Error" "text/plain"
PageLoadError ← ⋅(
  /↥⌕"The system cannot find the file specified".
  ⨬(NotFound◌|ServerError)
)
Page ← ⍣(
  ExtMime GetExt ⟜&frab
  Response "200 OK"
)PageLoadError ⊂"docs"
Home ← (Page "/index.html")

Respond ← ◌pool(
  &p $"Request from _" &tcpaddr.
  # Extract path from request
  °□⊡1 ⊜□≠@ . &ru "\r\n\r\n".
  &p $"Request:\n_".

  # Route to handler
  ⨬(Page|Home) ⊸≍"/"
  &p⊸($"Response: _ bytes" ⧻)

  # Send response
  ⊃⋅&cl&w
)

⍢(⍣Respond⋅&p &tcpa Listener)1
