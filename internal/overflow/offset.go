package overflow

import (
    "fmt"
    "strings"
)

// length of the cyclic pattern before it starts to repeat
const maxPatternLen int = 20280

// the main functionality of the offset subroutine 
func Offset(query string, reverse bool, length int) {
    // parse the bytes in the sub-pattern
    fmt.Println(" > Parsing query.")
    if len(query) != 16 {
        fmt.Printf("\n Error! invalid query length '%s'\n", query)
        return
    }

    // decode jump address
    addr, err := parseHex(query)
    if err != nil {
        fmt.Printf("\n Error! invalid bytes in query '%s'\n", query)
        return
    }

    // reverse endianness if specified
    if reverse {
        reverseBytes(addr)
    }

    // attempt to find the pattern offset
    fmt.Println(" > Searching for query within pattern.")
    result := findSubPattern(string(addr), length)

    // if no pattern offset found, print error
    if result == -1 {
        fmt.Println("\n Error! sub-pattern not found")
        return
    }

    // otherwise, print the offset of the pattern
    fmt.Printf("\n Success! Pattern offset is: %d\n", result)
}

// find the offset of the given sub-pattern
func findSubPattern(query string, length int) int {
    if len(query) == 0 {
        return -1
    }

    // generate the whole pattern
    var pstr string
    if length > 0 {
        pstr = string(cyclicPattern(length))
    } else {
        pstr = string(cyclicPattern(maxPatternLen))
    }

    // find the last occurrence of the sub-pattern within the pattern
    return strings.LastIndex(pstr, query)
}

