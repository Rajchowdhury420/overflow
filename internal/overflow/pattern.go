package overflow

import (
    "fmt"
    "net"
)

// contains pattern subcommand specific parameters
type Pattern struct {
    length int
}

// creates a new pattern objecft to store parameters
func NewPattern(length int) Pattern {
    return Pattern{ length }
}

// the main functionality of the pattern subcommand
func (p Pattern) Run(host Host, tmpl string) {
    // create n-byte cyclic pattern
    fmt.Println(" > generating pattern")
    data := cyclicPattern(p.length)

    // build payload
    fmt.Println(" > building payload")
    payload := Payload{ data, tmpl }

    // send payload to target service
    fmt.Printf(" > sending %d-byte payload\n", payload.Size())
    if err := host.SendPayload(payload); err != nil {
        // notify user of error if one has occurred
        fmt.Printf("\n error: %s\n", err.(*net.OpError).Err)
        return
    }

    // notify user that the payload was successfully delivered
    fmt.Println("\n success: No errors found")
}

// generates a cyclic pattern of specified length
func cyclicPattern(length int) []byte {
    data := make([]byte, length + (3 - length % 3))

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
            return 65, 97, 48
        }

        // if none have rolled over, return
        return x, y, z
    }

    // do the recursive rollover check
    return next(x, y, z + 1)
}

