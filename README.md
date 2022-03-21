compact-blame-mode
---

A 'minor mode' (extension) for emacs to present 'git blame' data on
top of regular text view in a manner that uses as little additional
screen space as possible

![Example code view](https://gv.github.io/stuff/images/includes.png)

Keys
---

`[` Decrease tag length limit  
`]` Increase tag length limit  
`9` Decrease color saturation  
`0` Increase color saturation  
`=` Go to diff for current file  
`/` Got to commit  
`Enter` Turn compact-blame-mode off

Config
---

`compact-blame-bg1` Main tag background. Could be "#RRGGBB" or
"rainbow" or "rainbow2" to generate color from commit id. Default: "#E0FFE0"

`compact-blame-bg2` Background for "month" part. Default: "#FFFFC0"  

`compact-blame-format` Tag format string (default: "%Y%0%.%#"):  
  - `%Y` The full year  
  - `%0` Month number  
  - `%m` Month number in Unicode superscript characters  
  - `%x` Month number in hex  
  - `%.` Author email name  
  - `%#` Number of lines in hunk if >=10  

Bugs
---

- Only current buffer updated when tuning color/length
- No Windows support
- Is a minor mode (and will be as long as we need to use major modes
  highlighting) but remaps character keys
