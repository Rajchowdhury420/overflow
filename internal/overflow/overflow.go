package overflow

import (
    "fmt"
    "net"
    "time"
)

// timeout used for connection and I/O
const timeout time.Duration = 3000000000

// creates a payload to send to the target service
func createPayload(data []byte, pref, suff string) []byte {
    // build payload
    ret := append([]byte(pref), data...)
    return append(ret, []byte(suff)...)
}

// send a payload to the target service
func sendPayload(host string, port int, payload []byte) error {
    // build uri for target service
    target := fmt.Sprintf("%s:%d", host, port)

    // connect to target service
    conn, err := net.DialTimeout("tcp", target, timeout)
    if err != nil {
        return err
    }
    defer conn.Close()

    // set I/O timeout
    if err = conn.SetDeadline(time.Now().Add(timeout)); err != nil {
        return err
    }

    // send payload to target service
    if _, err = conn.Write(payload); err != nil {
        return err
    }

    // wait for a response
    buf := make([]byte, 1024)
    if _, err := conn.Read(buf); err != nil {
        return err
    }

    // payload was sent successfully
    return nil
}

// builds a byte array of length n, populated a byte of choice
func generateBytes(b byte, length int) []byte {
    // instantiate the empty byte array
    data := make([]byte, length)

    // populate the byte array with 0x41 characters
    for i := 0; i < length; i++ {
        data[i] = b
    }

    return data
}
