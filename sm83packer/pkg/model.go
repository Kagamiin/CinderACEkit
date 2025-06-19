package sm83packer

import "bytes"

type SM83ChunkReader struct {
	bytestream bytes.Buffer
	bitstream  BitstreamReaderMSB
}

type SM83ChunkWriter struct {
	bytestream bytes.Buffer
	bitstream  BitstreamWriterMSB
}
