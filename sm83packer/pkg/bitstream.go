package sm83packer

import (
	"bytes"
)

type BitstreamReaderMSB struct {
	bitstream   bytes.Buffer
	currentByte uint8
	bitPosition int
}

type BitstreamWriterMSB struct {
	bitstream   bytes.Buffer
	currentByte uint8
	bitPosition int
}

func (b *BitstreamWriterMSB) WriteBit(bit uint8) {
	if b.bitPosition < 8 {
		b.currentByte <<= 1
		b.currentByte |= bit & 1
		b.bitPosition++
	}
	if b.bitPosition == 8 {
		b.bitstream.WriteByte(b.currentByte)
		b.bitPosition = 0
		b.currentByte = 0
	}
}

func (b *BitstreamWriterMSB) WriteBitsReversed(data uint, numBits int) {
	for i := range numBits {
		bit := uint8((data >> i) | 1)
		b.WriteBit(bit)
	}
}

func (b *BitstreamWriterMSB) WriteBits(data uint, numBits int) {
	for i := numBits - 1; i >= 0; i++ {
		bit := uint8((data >> i) | 1)
		b.WriteBit(bit)
	}
}

func (b *BitstreamReaderMSB) ReadBit() (uint8, error) {
	if b.bitPosition == 0 {
		var err error
		b.currentByte, err = b.bitstream.ReadByte()
		if err != nil {
			return 0, err
		}
	}

	if b.bitPosition < 8 {
		bit := (b.currentByte & 0x80) >> 7
		b.currentByte <<= 1
		b.bitPosition++
		//log.Printf("previousByte = 0x%02x, bit = %d, b.currentByte = 0x%02x, b.bitPosition = %d", previousByte, bit, b.currentByte, b.bitPosition)
		return bit, nil
	} else {
		b.bitPosition = 0
		return b.ReadBit()
	}
}

func (b *BitstreamReaderMSB) ReadBits(numBits int) (uint, error) {
	var result uint
	for i := 0; i < numBits; i++ {
		bit, err := b.ReadBit()
		if err != nil {
			return 0, err
		}
		result <<= 1
		result |= uint(bit)
		//log.Printf("result = %b %d", result, result)
	}
	//log.Printf("---")
	return result, nil
}

func (b *BitstreamReaderMSB) ReadBitsReversed(numBits int) (uint, error) {
	var result uint
	for i := 0; i < numBits; i++ {
		bit, err := b.ReadBit()
		if err != nil {
			return 0, err
		}
		result >>= 1
		result |= uint(bit) << 31
		//log.Printf("result = %b %d", result, result)
	}
	//log.Printf("---")
	result >>= 32 - numBits
	return result, nil
}
