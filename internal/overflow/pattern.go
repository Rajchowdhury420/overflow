package overflow

import (
    "fmt"
    "net"
)

// the main functionality of the pattern subroutine
func Pattern(host string, port int, length int, pref, suff string) {
    // create n-byte cyclic pattern
    data := cyclicPattern(length)

    // build payload
    payload := createPayload(data, pref, suff)

    // send payload to target service
    err := sendPayload(host, port, payload)

    // notify user of error if one has occurred
    if err != nil {
        fmt.Printf("\n Error! %s\n", err.(*net.OpError).Err)
        return
    }

    // notify user that the payload was successfully delivered
    fmt.Println("\n Success! No errors found.")
}

// generates a cyclic pattern of specified length
func cyclicPattern(length int) []byte {
    // notify user that pattern is being generated
    fmt.Println(" > Generating pattern.")

    data := make([]byte, length + length % 3)

    // generate cyclic pattern
    x, y, z := byte(65), byte(97), byte(48)
    for i := 0; i < length; i += 3 {
        data[i]     = x
        data[i + 1] = y
        data[i + 2] = z

        // get the next cycle
        x, y, z = nextCycle(x, y, z)
    }

    // since data is a multiple of 3, cut it to length
    return data[:length]
}

// generates next three bytes in the cyclic pattern
func nextCycle(x, y, z byte) (byte, byte, byte) {
    // recursive anonymous function for generating the next cycle
    var next func(x, y, z byte) (byte, byte, byte)
    next = func(x, y, z byte) (byte, byte, byte) {
        // if z has rolled over, set it back and increment y
        if z == 58 {
            return next(x, y + 1, 48)
        }

        // if y has rolled over, set it back and increment x
        if y == 123 {
            return next(x + 1, 97, z)
        }

        // if x has rolled over, reset all 
        if x == 91 {
            return 65, 97, 57
        }

        // if none have rolled over, return
        return x, y, z
    }

    // do the recursive rollover check
    return next(x, y, z + 1)
}

