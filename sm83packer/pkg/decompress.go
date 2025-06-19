package sm83packer

import "errors"

var ErrEndOfChunk = errors.New("end of chunk")
var ErrZeroOffset = errors.New("invalid zero offset in LZSS-masked chunk")
var ErrInvalidOffset = errors.New("invalid offset in LZSS-masked chunk")

func Decompress(chunkReader *SM83ChunkReader) ([]byte, error) {
	var err error
	var decompressedData []byte
	var nextData []byte
	var LZSSMode bool
	for err == nil {
		if LZSSMode {
			nextData, err = decompressLZSSMasked(decompressedData, chunkReader)
		} else {
			nextData, err = decompressLiterals(chunkReader)
		}
		decompressedData = append(decompressedData, nextData...)
	}
	if errors.Is(err, ErrEndOfChunk) {
		return decompressedData, nil
	}
	return decompressedData, err
}

func decompressLiterals(chunkReader *SM83ChunkReader) ([]byte, error) {
	len, err := chunkReader.bytestream.ReadByte()
	if err != nil {
		return nil, err
	}
	if len == 0 {
		return nil, ErrEndOfChunk
	}
	var data []byte
	for range len {
		b, err := chunkReader.bytestream.ReadByte()
		if err != nil {
			return data, err
		}
		data = append(data, b)
	}
	return data, nil
}

func decompressLZSSMasked(decompressedData []byte, chunkReader *SM83ChunkReader) ([]byte, error) {
	var offset uint8
	length, err := chunkReader.bytestream.ReadByte()
	if err != nil {
		return nil, err
	}

	if length == 0x80 {
		return nil, ErrEndOfChunk
	}
	if length < 0x80 {
		offset = (length & 0x0F) + 1
		length = (length >> 4) - 1
	} else {
		length &= 0x7F
		offset, err = chunkReader.bytestream.ReadByte()
		if err != nil {
			return nil, err
		}
		if offset == 0 {
			return nil, ErrZeroOffset
		}
	}

	startOffset := len(decompressedData) - int(offset)
	if startOffset < 0 {
		return nil, ErrInvalidOffset
	}
	dataToCopy := decompressedData[startOffset : startOffset+int(length)]
	var data []byte
	for _, b := range dataToCopy {
		isMasked, err := chunkReader.bitstream.ReadBit()
		if err != nil {
			return data, err
		}
		if isMasked > 0 {
			b, err = maskOpcode(b, chunkReader)
			if err != nil {
				return data, err
			}
		}
		data = append(data, b)
	}
	return data, nil
}

func maskOpcode(b byte, chunkReader *SM83ChunkReader) (byte, error) {
	var mask byte
	var shiftAmt int
	switch b & 0xC0 {
	case 0x00, 0xC0:
		mask = 0xC7
		shiftAmt = 3
	case 0x40, 0x80:
		upper, err := chunkReader.bitstream.ReadBit()
		if err != nil {
			return b, err
		}
		if upper > 0 {
			mask = 0xC7
			shiftAmt = 3
		} else {
			mask = 0xF8
			shiftAmt = 0
		}
	}
	fill, err := chunkReader.bitstream.ReadBitsReversed(3)
	if err != nil {
		return b, err
	}
	b &= mask
	b |= byte(fill) << uint(shiftAmt)
	return b, nil
}
