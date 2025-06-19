package sm83packer

const (
	minMatchLength = 3
)

type SlidingWindow struct {
	data   []byte
	maxLen int
}

func (s *SlidingWindow) Append(b byte) {
	if len(s.data) >= s.maxLen {
		s.data = s.data[len(s.data)-(s.maxLen-1) : len(s.data)]
	}
	s.data = append(s.data, b)
}

func (s *SlidingWindow) AppendBytes(data []byte) {
	for _, b := range data {
		s.Append(b)
	}
}

func (s *SlidingWindow) FindMatchMasked(data []byte, maxMatchLen int) (dataStart, offset, length int, ok bool) {
	if len(data) < minMatchLength || len(s.data) < minMatchLength {
		return 0, 0, 0, false
	}
	oldData := make([]byte, len(s.data))
	copy(oldData, s.data)

	var curLength, curOffset, curDataStart int
	var inMatch bool
	for curDataStart = 0; len(data) >= minMatchLength; curDataStart++ {
		curLength = 0
		curOffset = 0
		for pos := 0; pos < len(s.data) && pos < len(data); pos++ {
			if compareMasked(data[pos], s.data[pos]) {
				if !inMatch {
					inMatch = true
					curOffset = len(s.data) - pos
				}
				curLength++
			} else if inMatch {
				inMatch = false
				if curLength > minMatchLength && (curLength > length || (curLength == length && curOffset < offset)) {
					length = curLength
					offset = curOffset
					dataStart = curDataStart
				}
			}
		}
		s.Append(data[0])
		data = data[1:]
	}
	s.data = oldData
	if length >= minMatchLength {
		ok = true
	}
	return
}

func compareMasked(dst, src byte) bool {
	if dst == src {
		return true
	}
	switch dst & 0xC0 {
	case 0x00, 0xC0:
		if dst&0xC7 == src&0xC7 {
			return true
		}
	case 0x40, 0x80:
		if dst&0xC7 == src&0xC7 || dst&0xF8 == src&0xF8 {
			return true
		}
	}
	return false
}

func sliceAt(data []byte, offset int, length int) []byte {
	endOffset := offset + length
	if offset > len(data) {
		return nil
	}
	if endOffset > len(data) {
		endOffset = len(data)
	}
	return data[offset:endOffset]
}

func sliceAtBounded(data []byte, offset int, length int) []byte {
	endOffset := offset + length
	if offset > len(data) || endOffset > len(data) {
		return nil
	}
	return data[offset:endOffset]
}
