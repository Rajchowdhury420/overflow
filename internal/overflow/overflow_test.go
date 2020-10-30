package overflow

import (
    "testing"
)

// ---------------------------- overflow.go tests -----------------------------

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

// ------------------------------ chars.go tests ------------------------------

// tests characters generation with no exclusions
func TestGenerateCharactersNoExclude(t *testing.T) {
    // generate characters
    data := generateCharacters([]byte{})

    // if not all characters in array, test failed
    if len(data) != 256 {
        t. Errorf("len(characters([]byte{}) = %d, want %d", len(data), 256)
    }
}

// tests characters generation with some exclusions
func TestGenerateCharactersSmallExclude(t *testing.T) {
    // generate characters with some exclusions
    exclude := []byte{1, 2, 3, 4, 5}
    data := generateCharacters(exclude)

    // if lengths don't match, test failed
    if len(data) != 256 - len(exclude) {
        t.Errorf("len(characters(exclude) = %d, want %d", len(data),
            256 - len(exclude))
    }
}

// tests characters generation with a lot of exclusions
func TestGenerateCharactersLargeExclude(t *testing.T) {
    // generate characters with a lot of exclusions
    exclude := []byte{
         1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
        32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
    }
    data := generateCharacters(exclude)

    // if lengths don't match, test failed
    if len(data) != 256 - len(exclude) {
        t.Errorf("len(characters(exclude) = %d, want %d", len(data),
            256 - len(exclude))
    }
}

// ----------------------------- pattern.go tests ----------------------------- 

// tests generating an empty cyclic pattern of bytes
func TestCyclicPatternEmpty(t *testing.T) {
    // generate empty cyclic pattern of bytes
    want := ""
    data := cyclicPattern(0)

    // if strings don't match, test failed
    if string(data) != want {
        t.Errorf("string(data) = %s, want %s", string(data), want)
    }
}

// tests generating a small cyclic pattern of bytes
func TestCyclicPatternSmall(t *testing.T) {
    // generate small cyclic pattern of bytes
    want := "Aa0Aa1Aa2A"
    data := cyclicPattern(10)

    // if strings don't match, test failed
    if string(data) != want {
        t.Errorf("string(data) = %s, want %s", string(data), want)
    }
}

// tests generating a large cyclic pattern of bytes
func TestCyclicPatternLarge(t *testing.T) {
    // generate large cyclic pattern of bytes
    want := "Aa0Aa1Aa2Aa3Aa4Aa5Aa6Aa7Aa8Aa9Ab0Ab1Ab2Ab3Ab4Ab5Ab6Ab7Ab8Ab9Ac0" +
            "Ac1Ac2Ac3Ac4Ac5Ac6Ac7Ac8Ac9Ad0Ad1Ad2Ad3Ad4Ad5Ad6Ad7Ad8Ad9Ae0Ae1" +
            "Ae2Ae3Ae4Ae5Ae6Ae7Ae8Ae9Af0Af1Af2Af3Af4Af5Af6Af7Af8Af9Ag0Ag1Ag2" +
            "Ag3Ag4Ag5Ag6Ag7Ag8Ag9Ah0Ah1Ah2Ah3Ah4Ah5Ah6Ah7Ah8Ah9Ai0Ai1Ai2Ai3" +
            "Ai4Ai5Ai6Ai7Ai8Ai9Aj0Aj1Aj2Aj3Aj4Aj5Aj6Aj7Aj8Aj9Ak0Ak1Ak2Ak3Ak4" +
            "Ak5Ak6Ak7Ak8Ak9Al0Al1Al2Al3Al4Al5Al6Al7Al8Al9Am0Am1Am2Am3Am4Am5" +
            "Am6Am7Am8Am9An0An1An2An3An4An5An6An7An8An9Ao0Ao1Ao2Ao3Ao4Ao5Ao6" +
            "Ao7Ao8Ao9Ap0Ap1Ap2Ap3Ap4Ap5Ap6Ap7Ap8Ap9Aq0Aq1Aq2Aq3Aq4Aq5Aq6Aq7" +
            "Aq8Aq9Ar0Ar1Ar2Ar3Ar4Ar5Ar6Ar7Ar8Ar9As0As1As2As3As4As5As6As7As8" +
            "As9At0At1At2At3At4At5At6At7At8At9Au0Au1Au2Au3Au4Au5Au6Au7Au8Au9" +
            "Av0Av1Av2Av3Av4Av5Av6Av7Av8Av9Aw0Aw1Aw2Aw3Aw4Aw5Aw6Aw7Aw8Aw9Ax0" +
            "Ax1Ax2Ax3Ax4Ax5Ax6Ax7Ax8Ax9Ay0Ay1Ay2Ay3Ay4Ay5Ay6Ay7Ay8Ay9Az0Az1" +
            "Az2Az3Az4Az5Az6Az7Az8Az9Ba0Ba1Ba2Ba3Ba4Ba5Ba6Ba7Ba8Ba9Bb0Bb1Bb2" +
            "Bb3Bb4Bb5Bb6Bb7Bb8Bb9Bc0Bc1Bc2Bc3Bc4Bc5Bc6Bc7Bc8Bc9Bd0Bd1Bd2Bd3" +
            "Bd4Bd5Bd6Bd7Bd8Bd9Be0Be1Be2Be3Be4Be5Be6Be7Be8Be9Bf0Bf1Bf2Bf3Bf4" +
            "Bf5Bf6Bf7Bf8Bf9Bg0Bg1Bg2Bg3Bg4Bg5Bg6Bg7Bg8Bg9Bh0Bh1Bh2B"
    data := cyclicPattern(1000)

    // if strings don't match, test failed
    if string(data) != want {
        t.Errorf("string(data) = %s, want %s", string(data), want)
    }
}

// tests generating the next cycle, with no rollover
func TestNextCycleSimple(t *testing.T) {
    // generate next cycle
    x, y, z := nextCycle(65, 97, 51)

    // if results don't match, test failed
    if x != 65 || y != 97 || z != 52 {
        t.Errorf("x, y, z = %d, %d, %d, want %d, %d, %d", x, y, z, 65, 97, 52)
    }
}

// tests generating the next cycle, with a rollover on z
func TestNextCycleRollZ(t *testing.T) {
    // generate next cycle
    x, y, z := nextCycle(65, 97, 57)

    // if results don't match, test failed
    if x != 65 || y != 98 || z != 48 {
        t.Errorf("x, y, z = %d, %d, %d, want %d, %d, %d", x, y, z, 65, 98, 48)
    }
}

// tests generating the next cycle, with rollovers on z and y
func TestNextCycleRollY(t *testing.T) {
    // generate next cycle
    x, y, z := nextCycle(65, 122, 57)

    // if results don't match, test failed
    if x != 66 || y != 97 || z != 48 {
        t.Errorf("x, y, z = %d, %d, %d, want %d, %d, %d", x, y, z, 66, 97, 48)
    }
}

// tests generating the next cycle, with rollovers on all values
func TestNextCycleRollX(t *testing.T) {
    // generate next cycle
    x, y, z := nextCycle(90, 122, 57)

    // if results don't match, test failed
    if x != 65 || y != 97 || z != 48 {
        t.Errorf("x, y, z = %d, %d, %d, want %d, %d, %d", x, y, z, 65, 97, 48)
    }
}

