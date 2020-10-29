package cli

import (
    "fmt"
)

var title = `
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  

 Overflow v0.1.0
  by Stephen Radley           github.com/sradley
─────────────────────────────────────────────────`

var base = `
 :: Mode        : %s
 :: Host        : %s
 :: Port        : %d`

var fuzz = `
 :: Step        : %d
─────────────────────────────────────────────────
`

var pattern = `
 :: Length      : %d
─────────────────────────────────────────────────
`

var chars = `
 :: Offset      : %d
 :: Exclude     : "%s"
─────────────────────────────────────────────────
`

var exploit = `
 :: Offset      : %d
 :: Jump        : "%s"
 :: Shell       : %s
─────────────────────────────────────────────────
`

func FuzzTitle(host string, port int, step int) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "fuzz", host, port)

    // print the arguments passed to fuzz
    fmt.Printf(fuzz, step)
}

func PatternTitle(host string, port int, length int) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "pattern", host, port)

    // print the arguments passed to pattern
    fmt.Printf(pattern, length)
}

func CharsTitle(host string, port int, offset int, exclude string) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "chars", host, port)

    // print the arguments passed to chars
    fmt.Printf(chars, offset, exclude)
}

func ExploitTitle(host string, port int, offset int, jump string,
        shell string) {
    // print the title
    fmt.Print(title)

    // print the default arguments
    fmt.Printf(base, "exploit", host, port)

    // print the arguments passed to exploit
    fmt.Printf(exploit, offset, jump, shell)
}
