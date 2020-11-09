package overflow

import (
    "fmt"
    "net"
    "time"
    "strings"
    "errors"
)

// timeout used for connection and I/O
const timeout time.Duration = 5000000000

// struct used to contain host information
type Host struct {
    addr string
    port int
}

// struct used to contain payload data and templates
type Payload struct {
    data []byte
    tmpl string
}

// creates a new host struct
func NewHost(addr string, port int) Host {
    return Host{ addr, port }
}

// converts the host struct to string in the format "addr:port"
func (h Host) ToString() string {
    return fmt.Sprintf("%s:%d", h.addr, h.port)
}

// checks if a host struct is incomplete (i.e. missing host or port)
func (h Host) Incomplete() bool {
    return h.addr == "" || h.port == 0
}

// sends a payload to the target host
func (h Host) SendPayload(payload Payload) error {
    // connect to target service
    conn, err := net.DialTimeout("tcp", h.ToString(), timeout)
    if err != nil {
        return err
    }
    defer conn.Close()

    // set i/o timeout
    if err = conn.SetDeadline(time.Now().Add(timeout)); err != nil {
        return err
    }

    // apply the template to the payload
    data, err := payload.ApplyTemplate()
    if err != nil {
        return err
    }

    // split payload up into individual messages and send them
    msgs := strings.Split(string(data), "<CR>")
    for i, msg := range msgs {
        // what about messages that don't end in <RT>, i.e the last message
        if msg != "" && i == len(msgs) - 1 {
            if err = h.SendData(conn, []byte(msg)); err != nil {
                return err
            }

            break
        }

        // or 'foo<ENTER>', where the last message is ''
        if msg == "" && i == len(msgs) - 1 {
            break
        }

        // normal message
        if err = h.SendData(conn, []byte(msg + "\r\n")); err != nil {
            return err
        }
    }

    return nil
}

// send bytes to the target host
func (h Host) SendData(conn net.Conn, data []byte) error {
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

// get the size of the payload in bytes
func (p Payload) Size() int {
    return len(p.data)
}

// apply payload template to payload data
func (p Payload) ApplyTemplate() ([]byte, error) {
    // check that payload exists
    if !strings.Contains(p.tmpl, "{payload}") {
        return nil, errors.New("template does not contain '{payload}'")
    }

    // apply payload to template
    ret := strings.Replace(p.tmpl, "{payload}", string(p.data), 1)
    return []byte(ret), nil
}

