package overflow

import (
    "strings"
    "encoding/hex"
)

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

