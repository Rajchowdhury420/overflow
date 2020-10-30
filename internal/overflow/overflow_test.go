package overflow

import (
    "testing"
)

// ...
func TestCreatePayloadEmpty(t *testing.T) {
    // ...
    payload := createPayload([]byte{}, "", "")

    // ...
    if len(payload) != 0 {
        t.Errorf("len(payload) = %d, want %d", len(payload), 0)
    }
}

// ...
func TestCreatePayloadNoAffix(t *testing.T) {
    // ...
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, "", "")

    // ...
    if string(payload) != "ABCD" {
        t.Errorf("string(payload) = %s, want %s", string(payload), "ABCD")
    }
}

// ...
func TestCreatePayloadPrefix(t *testing.T) {
    // ...
    pref := "PREFIX "
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, pref, "")

    // ...
    if string(payload) != "PREFIX ABCD" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "PREFIX ABCD")
    }
}

// ...
func TestCreatePayloadSuffix(t *testing.T) {
    // ...
    suff := " SUFFIX"
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, "", suff)

    // ...
    if string(payload) != "ABCD SUFFIX" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "ABCD SUFFIX")
    }
}

// ...
func TestCreatePayloadAffix(t *testing.T) {
    // ...
    pref := "PREFIX "
    suff := " SUFFIX"
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, pref, suff)

    // ...
    if string(payload) != "PREFIX ABCD SUFFIX" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "PREFIX ABCD SUFFIX")
    }
}

// ...
func TestGenerateBytesEmpty(t *testing.T) {
    // ...
    data := generateBytes(0x41, 0)

    // ...
    if len(data) != 0 {
        t.Errorf("len(data) = %d, want %d", len(data), 0)
    }
}

// ...
func TestGenerateBytesSmall(t *testing.T) {
    // ...
    data := generateBytes(0x41, 100)

    // ...
    if len(data) != 100 {
        t.Errorf("len(data) = %d, want %d", len(data), 100)
    }
}

// ...
func TestGenerateBytesLarge(t *testing.T) {
    // ...
    data := generateBytes(0x41, 20000)

    // ...
    if len(data) != 20000 {
        t.Errorf("len(data) = %d, want %d", len(data), 20000)
    }
}

