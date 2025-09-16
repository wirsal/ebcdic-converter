package utils

import (
	"encoding/hex"
	"testing"
)

// Test Hex2string_comp3 simple conversion
func TestHex2stringComp3(t *testing.T) {
	in := "ABC"
	got := Hex2string_comp3(in)
	want := hex.EncodeToString([]byte(in)) // "414243"
	if got != want {
		t.Fatalf("Hex2string_comp3(%q) = %q; want %q", in, got, want)
	}
}

// Test Hex2string for all-zero bytes -> should return spaces of same length
func TestHex2string_ZeroBytes(t *testing.T) {
	in := string([]byte{0x00, 0x00, 0x00})
	got := Hex2string(in)
	want := "   " // 3 spasi
	if got != want {
		t.Fatalf("Hex2string(zero-bytes) = %q; want %q", got, want)
	}
}

// Test cleanString replacements directly
func TestCleanString(t *testing.T) {
	in := "A§B\r\nC\rD\nE|F\x00G"
	got := cleanString(in)
	// Expected replacements:
	// "§" -> "@"
	// CRLF/CR/LF -> " "
	// "|" -> " "
	// "\x00" -> " "
	want := "A@B C D E F G"
	if got != want {
		t.Fatalf("cleanString(%q) = %q; want %q", in, got, want)
	}
}

// Tests for ParseComp3SignedMode covering modes and sign-nibbles
func TestParseComp3SignedMode(t *testing.T) {
	type tc struct {
		name string
		in   string
		mode string
		want string
	}

	cases := []tc{
		{
			name: "mode d with sign 'd' (last hex char 'd') -> negative",
			in:   "123\r", // '\r' == 0x0d -> hex ends with 'd'
			mode: "d",
		},
		{
			name: "mode d with non-d sign -> prefixed space",
			in:   "123X", // 'X' -> not 'd'
			mode: "d",
		},
		{
			name: "mode c with sign 'c' -> plus",
			in:   "12\x0c", // 0x0c -> hex ends with 'c'
			mode: "c",
		},
		{
			name: "mode c with non-c sign -> minus",
			in:   "12Z",
			mode: "c",
		},
		{
			name: "empty input -> empty output",
			in:   "",
			mode: "d",
			want: "",
		},
		{
			name: "default mode -> return number (no last-char sign handling)",
			in:   "AB",
			mode: "",
		},
	}

	for i, c := range cases {
		// compute expected if not explicitly provided
		if c.want == "" && c.in != "" {
			val := hex.EncodeToString([]byte(c.in))
			if val == "" {
				c.want = ""
			} else {
				last := val[len(val)-1:]
				number := val[:len(val)-1]
				switch c.mode {
				case "d", "D":
					if last == "d" || last == "D" {
						c.want = "-" + number
					} else {
						c.want = " " + number
					}
				case "c", "C":
					if last == "c" || last == "C" {
						c.want = "+" + number
					} else {
						c.want = "-" + number
					}
				default:
					c.want = val
				}
			}
		}

		got := ParseComp3SignedMode(c.in, c.mode)
		if got != c.want {
			t.Errorf("case %d (%s): ParseComp3SignedMode(%q, %q) = %q; want %q",
				i, c.name, c.in, c.mode, got, c.want)
		}
	}
}

// SafeDecode: normal and panic cases
func TestSafeDecode(t *testing.T) {
	// normal
	res := SafeDecode("normal", func() string {
		return "OK"
	})
	if res != "OK" {
		t.Fatalf("SafeDecode normal = %q; want %q", res, "OK")
	}

	// panic case: should be recovered and return ""
	res2 := SafeDecode("panic-case", func() string {
		panic("boom")
	})
	if res2 != "" {
		t.Fatalf("SafeDecode panic-case = %q; want empty string", res2)
	}
}
