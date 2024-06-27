# CinderACEkit

An arbitrary code execution framework for Pokémon Yellow.

## Functional requirements - bare minimum (version 0)

- Support for storing up to 32 payloads
  - Uses unused SRAM space to store its code and the payloads
  - Manages SRAM as a very crude filesystem
- Loads into memory only the payloads you want to execute
  - The payload to execute is selected via an API call
- Simple binary format
  - This executable format has type 0, this is indicated by a `$00` at the beginning of the executable header
    - The type field is currently ignored
  - The header starts with a 16-bit load/run address which can be anywhere in memory
  - The header ends with a 16-bit length
  - After the header comes the code for the binary
- API functions to manipulate payloads
  - Get number of payloads
  - Store payload (in the simple binary format specified above)
  - Load payload by number
  - Update payload by number
  - Delete payload by number
- Crude front-end for selecting one of the payloads by number and executing it
  - Uses those API functions

## Functional requirements - version 1

- Support for storing up to an arbitrary number of payloads
  - Dynamic allocation of directory entries
- Low-level API calls
  - Open handle for writing
    - (only one at a time may be opened globally)
    - (also blocks file deletion)
  - Close handle
  - Open handle for reading
  - 
- Support for loading libraries into the system
  - Function blocks
  - Syscalls
- Kernel code gets copied to fixed RAM bank when a payload is executed
- Segmented executable format
  - This executable format has type $d3, this is indicated by a `$d3` at the beginning of the executable header
  - The header starts with the name of the program, which is encoded using the Pokémon text encoding/charset and is terminated by byte `$50` (ASCII '@').
    - The name of the program, excluding the terminator, must not contain any characters outside the range `$7F`-`$FF` and must not be longer than 18 bytes.
  - Following that name is a 16-bit run address
    - If the run address is `$0000`, the payload is considered a library and cannot be executed directly
  - Following that, each segment is indicated with a 16-bit load address and a 16-bit length
    - The load address must be between `$A000` and `$DFFF` or between `$FF80` and `$FFFE`, and the length of the segment must not exceed these sections, or the executable is deemed invalid
      - If the load address is between `$A000` and `$BFFF`, the program will be loaded into SRAM bank 0.
      - For the second rule, an exception is made for Echo RAM - a segment must start in normal RAM, but it may continue into the `$E000`-`$FDFF` region
    - There can be up to 64 segments; a header with more than 64 segments is deemed invalid
    - A load address of `$0000` terminates the list of segments
  - After the header comes the code for each segment, concatenated in order
- Built-in scheduler for VBlank-driven tasks
  - Hardly a scheduler at all, at this point - all it would do is run each of the tasks in a certain order.
- Built-in program to convert loose binaries into segmented executables (for convenience)

## Functional requirements - version 2

- Support for GBC extended WRAM
  - Uses the 6 extra 4KiB RAM banks available in the Game Boy Color to allow for more available memory for resident tasks to run
- Adds a new executable format
  - This executable format has type $db, this is indicated by a `$db` at the beginning of the executable header
  - The header starts with the name of the program, which is encoded using the Pokémon text encoding/charset and is terminated by byte `$50` (ASCII '@').
    - The name of the program, excluding the terminator, must not contain any characters outside the range `$7F`-`$FF` and must not be longer than 18 bytes.
  - Following that is a 16-bit run address
    - If the run address is `$0000`, the payload is considered a library and cannot be executed directly
  - Following that is a byte defining the number of segments, which may range from 1 to 64
    - An executable with 0 segments or more than 64 segments is invalid
  - Following that byte is a bitmap defining the segment type for each segment, LSB-first
    - A 0 bit means the segment is a normal-mode segment
    - A 1 bit means the segment is an extended-mode segment
    - The bitmap is padded to the nearest byte
  - Following that is the list of 16-bit load address and 16-bit length for each segment
    - Normal-mode segments follow the same rules as in the type 0 executables
    - Extended-mode segments, however, can only be loaded in the `$D000` to `$DFFF` range
  - After the header comes the code for each segment, concatenated in order
- Stack switching for jumping between banks

## Functional requirements - version 3


## Non-functional requirements

- Streamlined installation on real hardware: very desirable
- Automatic startup upon game continue: very desirable
- 
