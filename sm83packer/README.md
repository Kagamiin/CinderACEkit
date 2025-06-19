# SM83Packer

**SM83Packer** is a compression codec for SM83, Z80 and 8080 code designed by Kagamiin~.
It was originally written for optimizing compression ratio on machine code for the Game Boy's SM83 instruction set, but can also work for similar instruction sets such as the ones found on the Zilog Z80 and Intel 8080.

## How it works

SM83Packer is based on a modified version of LZSS coding.
SM83Packer operates in two different modes and switches back and forth between them:

- **Literal:**
  - read a byte `n` from the input
  - if `n` == **0**, the decompression is over
  - otherwise, copy `n` bytes from the input to the output
  - switch to the LZSS-masked mode
- **LZSS-masked:**
  - read a byte `l` from the input
  - if `l` == **0x80**, the decompression is over
  - if `l` == **0xFF**, switch back to the literal mode
  - if `l` > **0x80**:
    - this is a long backreference
    - subtract **0x80** from `l` and read a byte `o` from the input
    - **NOTE:** `o` cannot be equal to **0xFF** or else garbage data will be copied
  - otherwise:
    - this is a short backreference
    - let `o` := `l` & **0x0F**, `l` := (`l` >> **4**) + **1**
  - subtract (`o` + **1**) from the output pointer to get the backreference pointer
  - copy `l` bytes from the backreference to the output, using the bitstream in order to decide whether a byte should be copied verbatim or "masked" (modified) before copying.
    - for each new bit read from the bitstream:
      - if the bit is **0**, copy one byte verbatim
      - if the bit is **1**, copy one byte modifying it according to the LZSS-masked modification scheme
  - switch to the literal mode

## LZSS-masked coding

The LZSS-masked codec used in SM83Packer is a modified version of the original LZSS coder.

In normal LZSS, there is a bitstream, usually in blocks of 8 bits, where each bit determines whether to read a byte from the input or to copy a chunk of data from a backreference.

LZSS-masked coding works a bit differently. The bitstream in LZSS-masked decides instead whether to read a byte from the input verbatim, or to read a byte from the input "masked", where certain bits of it are modified using further data read from the bitstream and depending on which kind of opcode it is.

### How the masking works

LZSS-masked coding in SM83Packer is based on the organization of the SM83's instruction set.

By looking at an opcode chart (e.g. <https://gbdev.io/gb-opcodes/optables>), one can notice that SM83 opcodes are divided into four contiguous zones, each 64 entries long. Those can be lumped up into two zones that are further subdivided into two subzones:

- **Outer zone** - comprises the ranges **0x00**-**0x3F** and **0xC0**-**0xFF**, containing various sorts of opcodes. Each of those ranges is its own subzone, but they have similar organization - opcodes with similar functionality but different arguments repeat every 16 entries, and opcodes 8 entries apart can also have similar functionality with different arguments.

- **Inner zone** - comprises the range **0x40**-**0xBF**. Is divided into two subranges:
  - **0x40**-**0x7F** - mostly contains register-register loads; bits 0-2 select the source and bits 3-5 select the destination.
  - **0x80**-**0xBF** - mostly contains arithmetic-logic operations between the a register and another register; bits 0-2 select the source and bits 3-5 select the operation to be performed.

Based on these principles, opcodes to be "masked" are classified within one of the two zones and modified as such:

- **Outer zone:** bits 3-5 are replaced with 3 bits read from the LZSS bitstream
  - This modifies the opcode offset within the outer zone in increments of 8 entries, allowing for variations of the same opcode to be encoded in a smaller number of bits.
- **Inner zone:** 1 bit is read to determine which portion of the instruction to replace:
  - If the read bit is **0**, bits 0-2 are replaced with 3 further bits read from the LZSS bitstream, which changes the opcode's source as explained above.
  - If the read bit is **1**, bits 3-5 are replaced with 3 further bits read from the LZSS bitstream, which changes the destination for load opcodes or the operation for arithmetic-logic opcodes.
