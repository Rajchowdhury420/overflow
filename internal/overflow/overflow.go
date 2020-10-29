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
    // notify user that a payload is being built
    fmt.Println(" > Building payload.")

    // build payload
    ret := append([]byte(pref), data...)
    return append(ret, []byte(suff)...)
}

// send a payload to the target service
func sendPayload(host string, port int, payload []byte) error {
    // print payload information
    fmt.Printf(" > Sending %d-byte payload.\n", len(payload))

    // build uri for target service
    target := fmt.Sprintf("%s:%d", host, port)

    // connect to target service
    conn, err := net.DialTimeout("tcp", target, timeout)
    if err != nil {
        return err
    }

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

