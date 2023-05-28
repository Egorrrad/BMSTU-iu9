package main

import (
	"bytes"
	"fmt"
)

func encode(utf32 []rune) []byte {
	var buf bytes.Buffer
	for _, r := range utf32 {
		if r <= 0x7F {
			buf.WriteByte(byte(r))
		} else if r <= 0x7FF {
			buf.WriteByte(byte(r>>6) | 0xC0)
			buf.WriteByte(byte(r)&0x3F | 0x80)
		} else if r <= 0xFFFF {
			buf.WriteByte(byte(r>>12) | 0xE0)
			buf.WriteByte(byte(r>>6)&0x3F | 0x80)
			buf.WriteByte(byte(r)&0x3F | 0x80)
		} else if r <= 0x10FFFF {
			buf.WriteByte(byte(r>>18) | 0xF0)
			buf.WriteByte(byte(r>>12)&0x3F | 0x80)
			buf.WriteByte(byte(r>>6)&0x3F | 0x80)
			buf.WriteByte(byte(r)&0x3F | 0x80)
		}
	}
	return buf.Bytes()
}

func decode(utf8 []byte) []rune {
	var runes []rune
	for i := 0; i < len(utf8); {
		if utf8[i]&0x80 == 0 {
			runes = append(runes, rune(utf8[i]))
			i += 1
		} else if utf8[i]&0xE0 == 0xC0 {
			if i+1 >= len(utf8) {
				return nil
			}
			r := rune(utf8[i]&0x1F)<<6 | rune(utf8[i+1]&0x3F)
			runes = append(runes, r)
			i += 2
		} else if utf8[i]&0xF0 == 0xE0 {
			if i+2 >= len(utf8) {
				return nil
			}
			r := rune(utf8[i]&0x0F)<<12 | rune(utf8[i+1]&0x3F)<<6 | rune(utf8[i+2]&0x3F)
			runes = append(runes, r)
			i += 3
		} else if utf8[i]&0xF8 == 0xF0 {
			if i+3 >= len(utf8) {
				return nil
			}
			r := rune(utf8[i]&0x07)<<18 | rune(utf8[i+1]&0x3F)<<12 | rune(utf8[i+2]&0x3F)<<6 | rune(utf8[i+3]&0x3F)
			runes = append(runes, r)
			i += 4
		} else {
			return nil
		}
	}
	return runes
}

func main000000() {
	input := ([]byte)("😃")
	//expected := ([]rune)("😃")
	fmt.Printf("% x\n", input)
	runes := decode(input)
	fmt.Println(string(runes))
}

//+++++++
