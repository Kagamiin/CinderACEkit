package sm83packer

import "bytes"

func Compress(inputData bytes.Buffer) (SM83ChunkReader, error) {
	var writer SM83ChunkWriter
	var LZSSMode bool
	var slidingBuffer []byte
	var nextChunkData []byte
	var b byte
	var err error
	b, err = inputData.ReadByte()
	for err == nil {
		nextChunkData = append(nextChunkData, b)

		b, err = inputData.ReadByte()
	}
}
