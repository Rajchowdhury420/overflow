package cli

import (
    "fmt"
)

// title card template
var title = `
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.3.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────`

// default arguments template
var base = `
 :: Mode        : %s
 :: Host        : %s
 :: Port        : %d`

// fuzz command specific arguments template
var fuzz = `
 :: Step        : %d
 :: Wait        : %dms
──────────────────────────────────────────────────
`

// pattern command specific arguments template
var pattern = `
 :: Length      : %d
──────────────────────────────────────────────────
`

// offset command specific arguments template
var offset = `
 :: Query       : "%s"
 :: Reverse     : %t
 :: Length      : %d
──────────────────────────────────────────────────
`

// chars command specific arguments template
var chars = `
 :: Offset      : %d
 :: Exclude     : "%s"
──────────────────────────────────────────────────
`

// exploit command specific arguments template
var exploit = `
 :: Offset      : %d
 :: Jump        : "%s"
 :: Reverse     : %t
 :: NOPs        : %d
 :: Shell       : %s
──────────────────────────────────────────────────
`

// prints the title card when executing the fuzz command
func FuzzTitle(host string, port int, step int, wait int) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "fuzz", host, port)

    // print the arguments passed to fuzz
    fmt.Printf(fuzz, step, wait)
}

// prints the title when executing the pattern command
func PatternTitle(host string, port int, length int) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "pattern", host, port)

    // print the arguments passed to pattern
    fmt.Printf(pattern, length)
}

// prints the title when executing the offset command
func OffsetTitle(query string, reverse bool, length int) {
    // print the title
    fmt.Print(title)

    // print the arguments passed to offset
    fmt.Printf(offset, query, reverse, length)
}

// prints the title when executing the chars command
func CharsTitle(host string, port int, offset int, exclude string) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "chars", host, port)

    // print the arguments passed to chars
    fmt.Printf(chars, offset, exclude)
}

// prints the title when executing the exploit command
func ExploitTitle(host string, port int, offset int, jump string, reverse bool,
        nops int, shell string) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "exploit", host, port)

    // print the arguments passed to exploit
    fmt.Printf(exploit, offset, jump, reverse, nops, shell)
}

