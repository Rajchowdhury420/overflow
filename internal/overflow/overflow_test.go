package overflow

import (
    "testing"
)

// tests creating an empty payload
func TestCreatePayloadEmpty(t *testing.T) {
    // create an empty payload
    payload := createPayload([]byte{}, "", "")

    // if payload is not empty, test failed
    if len(payload) != 0 {
        t.Errorf("len(payload) = %d, want %d", len(payload), 0)
    }
}

// tests creating a payload with no prefix or suffix
func TestCreatePayloadNoAffix(t *testing.T) {
    // create a payload with no prefix or suffix
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, "", "")

    // if payload doesn't match string, test failed
    if string(payload) != "ABCD" {
        t.Errorf("string(payload) = %s, want %s", string(payload), "ABCD")
    }
}

// tests creating a payload with a prefix, but no suffix
func TestCreatePayloadPrefix(t *testing.T) {
    // create the payload with a prefix
    pref := "PREFIX "
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, pref, "")

    // if payload doesn't match string, test failed
    if string(payload) != "PREFIX ABCD" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "PREFIX ABCD")
    }
}

// tests creating a payload with a suffix, but no prefix
func TestCreatePayloadSuffix(t *testing.T) {
    // create the payload with a suffix
    suff := " SUFFIX"
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, "", suff)

    // if payload doesn't match string, test failed
    if string(payload) != "ABCD SUFFIX" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "ABCD SUFFIX")
    }
}

// tests creating a payload with both a prefix and a suffix
func TestCreatePayloadAffix(t *testing.T) {
    // create the payload with both a prefix and a suffix
    pref := "PREFIX "
    suff := " SUFFIX"
    data := []byte{0x41, 0x42, 0x43, 0x44}
    payload := createPayload(data, pref, suff)

    // if payload doesn't match string, test failed
    if string(payload) != "PREFIX ABCD SUFFIX" {
        t.Errorf("string(payload) = %s, want %s", string(payload),
            "PREFIX ABCD SUFFIX")
    }
}

// tests generating an empty sequence of bytes
func TestGenerateBytesEmpty(t *testing.T) {
    // generate empty sequence of bytes
    data := generateBytes(0x41, 0)

    // if data not empty, test failed
    if len(data) != 0 {
        t.Errorf("len(data) = %d, want %d", len(data), 0)
    }
}

// tests generating a small sequence of bytes
func TestGenerateBytesSmall(t *testing.T) {
    // generate small sequence of bytes
    data := generateBytes(0x41, 100)

    // if lengths don't match, test failed
    if len(data) != 100 {
        t.Errorf("len(data) = %d, want %d", len(data), 100)
    }
}

// test generating a large sequence of bytes
func TestGenerateBytesLarge(t *testing.T) {
    // generate large sequence of bytes
    data := generateBytes(0x41, 20000)

    // if lengths don't match, test failed
    if len(data) != 20000 {
        t.Errorf("len(data) = %d, want %d", len(data), 20000)
    }
}

