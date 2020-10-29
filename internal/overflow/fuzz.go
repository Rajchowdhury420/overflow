package overflow

import (
    "fmt"
    "net"
)

// the main functionality of the fuzz subroutine
func Fuzz(host string, port int, step int, pref, suff string) {
    var err error
    length := step

    for err == nil {
        // create n-byte byte array
        data := overflow(length)

        // build payload
        payload := createPayload(data, pref, suff)

        // send payload to target service
        err = sendPayload(host, port, payload)

        length += step
    }

    if length - step == step {
        // no payload was sent, an error occurred
        fmt.Printf("\n Error! %s\n", err.(*net.OpError).Err)
        return
    }

    // print buffer information
    fmt.Printf("\nSuccess! Length of buffer is in range (%d, %d].\n",
        length - step - step, length - step)
}

// builds a byte array of length n, populated by 0x41 characters
func overflow(length int) []byte {
    // instantiate the empty byte array
    data := make([]byte, length)

    // populate the byte array with 0x41 characters
    for i := 0; i < length; i++ {
        data[i] = 0x41
    }

    return data
}

