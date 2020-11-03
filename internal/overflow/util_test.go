package overflow

import (
    "testing"
)

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

// test parsing empty hex string
func TestParseHexEmpty(t *testing.T) {
    // convert empty string to bytes
    data, err := parseHex("")

    // if error occurred, test failed
    if err != nil {
        t.Errorf("error: %s", err)
    }

    // if byte string not empty, test failed
    if len(data) != 0 {
        t.Errorf("len(data) = %d, want %d", len(data), 0)
    }
}

// test parsing valid hex string
func TestParseHexValid(t *testing.T) {
    // convert valid hex string to bytes
    data, err := parseHex("\\x41\\x42\\x43\\x44")

    // if error occurred, test failed
    if err != nil {
        t.Errorf("error: %s", err)
    }

    // if byte string doesn't match string, test failed
    if string(data) != "ABCD" {
        t.Errorf("string(data) = %s, want %s", string(data), "ABCD")
    }
}

// test parsing invalid hex string
func TestParseHexInvalid(t *testing.T) {
    // attempt to convert invalid hex string to bytes
    _, err := parseHex("\\x41\\x42\x41\\x43")

    // if error didn't occur, test failed
    if err == nil {
        t.Errorf("fail: err = %s", err)
    }
}

// test reversing empty byte array
func TestReverseBytesEmpty(t *testing.T) {
    // reverse empty byte array
    data := []byte{}
    reverseBytes(data)

    // if data not empty, test failed
    if len(data) != 0 {
        t.Errorf("len(data) = %d, want %d", len(data), 0)
    }
}

// test reversing small byte array
func TestReverseBytesSmall(t *testing.T) {
    // reverse small byte array
    data := []byte{0x41, 0x42, 0x43, 0x44}
    reverseBytes(data)

    if string(data) != "DCBA" {
        t.Errorf("string(data) = %s, want %s", string(data), "DCBA")
    }
}

// test reversing large byte array
func TestReverseBytesLarge(t *testing.T) {
    // reverse large byte array
    data := []byte{
        0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a,
        0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54,
        0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
    }
    reverseBytes(data)

    if string(data) != "ZYXWVUTSRQPONMLKJIHGFEDCBA" {
        t.Errorf("string(data) = %s, want %s", string(data),
            "ZYXWVUTSRQPONMLKJIHGFEDCBA")
    }
}

