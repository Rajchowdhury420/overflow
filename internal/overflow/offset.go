package overflow

import (
    "fmt"
    "strings"
    "errors"
)

// length of the cyclic pattern before it starts to repeat
const maxPatternLen int = 20280

// ...
type Offset struct {
    query   string
    reverse bool
    length  int
}

// ...
func NewOffset(query string, reverse bool, length int) Offset {
    return Offset{ query, reverse, length }
}

// ...
func (o Offset) Run() {
    // parse the bytes in the sub-pattern
    fmt.Println(" > parsing query")
    addr, err := o.DecodeQuery()
    if err != nil {
        fmt.Printf("\n error: %s\n", err)
        return
    }

    // attempt to find the pattern offset
    fmt.Println(" > searching for query within pattern")
    result := findSubPattern(string(addr), o.length)

    // if no pattern offset found, print error
    if result == -1 {
        fmt.Println("\n error: sub-pattern not found")
        return
    }

    // otherwise, print the offset of the pattern
    fmt.Printf("\n success: pattern offset is: %d\n", result)
}

func (o Offset) DecodeQuery() ([]byte, error) {
    // check the length of the sub-pattern
    if len(o.query) != 16 {
        return nil, errors.New("bad query length")
    }

    // decode sub-pattern bytes
    addr, err := parseHex(o.query)
    if err != nil {
        return nil, err
    }

    // reverse endianness if told to do so
    if o.reverse {
        reverseBytes(addr)
    }

    return addr, nil
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

