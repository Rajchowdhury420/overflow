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

// ...
type Host struct {
    addr string
    port int
}

// ...
type Payload struct {
    data []byte
    tmpl string
}

// ...
func NewHost(addr string, port int) Host {
    return Host{ addr, port }
}

// ...
func (h Host) ToString() string {
    return fmt.Sprintf("%s:%d", h.addr, h.port)
}

// ...
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

    // ...
    data, err := payload.ApplyTemplate()
    if err != nil {
        return err
    }

    // ...
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

// ...
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

// ...
func (p Payload) Size() int {
    return len(p.data)
}

// ...
func (p Payload) ApplyTemplate() ([]byte, error) {
    // check that payload exists
    if !strings.Contains(p.tmpl, "{payload}") {
        return nil, errors.New("template does not contain '{payload}'")
    }

    // apply payload to template
    ret := strings.Replace(p.tmpl, "{payload}", string(p.data), 1)
    return []byte(ret), nil
}

