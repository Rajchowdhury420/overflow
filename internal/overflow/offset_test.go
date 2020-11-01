package overflow

import (
    "testing"
)

// tests attempting to find an empty sub-pattern with no length specified
func TestFindSubPatternEmpty(t *testing.T) {
    // attempt to find an empty sub-pattern
    data := findSubPattern("", 0)

    if data != -1 {
        t.Errorf("data = %d, want %d", data, -1)
    }
}

// tests attempting to find an empty sub-pattern with a length specified
func TestFindSubPatternEmptyWithLength(t *testing.T) {
    // attempt to find an empty sub-pattern with length
    data := findSubPattern("", 3000)

    if data != -1 {
        t.Errorf("data = %d, want %d", data, -1)
    }
}

// tests finding a valid sub-pattern with no length specified
func TestFindSubPatternSmallValid(t *testing.T) {
    // attempt to find a valid sub-pattern
    data := findSubPattern("7Co8", 0)

    if data != 2003 {
        t.Errorf("data = %d, want %d", data, 2003)
    }
}

// tests finding a valid sub-pattern with a length specified
func TestFindSubPatternSmallWithLengthValid(t *testing.T) {
    // attempt to find a valid sub-pattern with length
    data := findSubPattern("7Co8", 3000)

    if data != 2003 {
        t.Errorf("data = %d, want %d", data, 2003)
    }
}

// tests finding an invalid sub-pattern with no length specified
func TestFindSubPatternInvalid(t *testing.T) {
    // attempt to find an invalid sub-pattern
    data := findSubPattern("abcd", 0)

    if data != -1 {
        t.Errorf("data = %d, want %d", data, -1)
    }
}

// tests finding an invalid sub-pattern with a length specified
func TestFindSubPatternWithLengthInvalid(t *testing.T) {
    // attempt to find an invalid sub-pattern with length
    data := findSubPattern("abcd", 3000)

    if data != -1 {
        t.Errorf("data = %d, want %d", data, -1)
    }
}

