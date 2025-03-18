       identification division.
       function-id. html-decode.
       environment division.
       configuration section.
       data division.
       working-storage section.
       01  ws-max-string-length           constant as 2048.
       local-storage section.
       linkage section.
       01  l-html-encoded-string          pic x any length.
       01  l-html-decoded-string          pic x(ws-max-string-length).
       procedure division
           using l-html-encoded-string
           returning l-html-decoded-string.
           move l-html-encoded-string to l-html-decoded-string
           move function substitute(l-html-decoded-string, "+", space)
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%21", "!")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%27", "'")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%40", "@")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%7E", "~")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%23", "#")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%25", "%")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%5E", "^")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%26", "&")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%28", "(")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%29", ")")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%2B", "+")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%60", "`")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3D", "=")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%5B", "[")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%5D", "]")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%5C", "\")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%7B", "{")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%7D", "}")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%7C", "|")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3B", ";")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3A", ":")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%22", '"')
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%2C", ",")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%2F", "/")
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3C", space)
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3E", space)
           to l-html-decoded-string
           move function substitute(l-html-decoded-string, "%3F", "?")
           to l-html-decoded-string
           move function
           substitute(l-html-decoded-string, "%0D%0A", "<br />")
           to l-html-decoded-string
           goback.
       end function html-decode.
