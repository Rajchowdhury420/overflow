package overflow

import (
    "fmt"
    "net"
    "time"
)

// ...
type Fuzz struct {
    step int
    wait int
}

// ...
func NewFuzz(step int, wait int) Fuzz {
    return Fuzz{ step, wait }
}

// ...
func (f Fuzz) Run(host Host, tmpl string) {
    var err error
    var length int

    for length = f.step; err == nil; length += f.step {
        // create n-byte byte array
        data := generateBytes(0x41, length)

        // build payload
        fmt.Println(" > Building payload.")
        payload := Payload{ data, tmpl }

        // send payload to target service
        fmt.Printf(" > Sending %d-byte payload.\n", payload.Size())
        err = host.SendPayload(payload)

        time.Sleep(time.Duration(f.wait) * time.Millisecond)
    }

    if length - f.step == f.step {
        // no payload was sent, an error occurred
        fmt.Printf("\n Error! %s\n", err.(*net.OpError).Err)
        return
    }

    // print buffer information
    fmt.Printf("\n Success! Length of buffer is in range (%d, %d].\n",
        length - f.step - f.step, length - f.step)
}

