package overflow

import (
    "testing"
)

// ...
func TestCharactersNoExclude(t *testing.T) {
    // ...
    data := characters([]byte{})

    // ...
    if len(data) != 256 {
        t. Errorf("len(characters([]byte{}) = %d, want %d", len(data), 256)
    }
}

// ...
func TestCharactersSmallExclude(t *testing.T) {
    // ...
    exclude := []byte{1, 2, 3, 4, 5}
    data := characters(exclude)

    if len(data) != 256 - len(exclude) {
        t.Errorf("len(characters(exclude) = %d, want %d", len(data),
            256 - len(exclude))
    }
}

// ...
func TestCharactersLargeExclude(t *testing.T) {
    // ...
    exclude := []byte{
         1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
        32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
    }
    data := characters(exclude)

    if len(data) != 256 - len(exclude) {
        t.Errorf("len(characters(exclude) = %d, want %d", len(data),
            256 - len(exclude))
    }
}
