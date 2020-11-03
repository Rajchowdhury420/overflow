package cli

import (
    "fmt"
)

// title card template
var title = `
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────`

// default arguments template
var base = `
  . mode                : %s
  . (-a|--addr)         : %s
  . (-p|--port)         : %d`

// fuzz command specific arguments template
var fuzz = `
  . (-s|--step)         : %d
  . (-w|--wait)         : %dms
───────────────────────────────────────────────
`

// pattern command specific arguments template
var pattern = `
  . (-l|--length)       : %d
───────────────────────────────────────────────
`

// offset command specific arguments template
var offset = `
  . mode                : offset
  . (-q|--query)        : "%s"
  . (-r|--reverse)      : %t
  . (-l|--length)       : %d
───────────────────────────────────────────────
`

// chars command specific arguments template
var chars = `
  . (-o|--offset)       : %d
  . (-e|--exclude)      : "%s"
───────────────────────────────────────────────
`

// exploit command specific arguments template
var exploit = `
  . (-o|--offset)       : %d
  . (-j|--jump)         : "%s"
  . (-r|--reverse)      : %t
  . (-n|--nos)          : %d
  . (-s|--shell)        : %s
───────────────────────────────────────────────
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

