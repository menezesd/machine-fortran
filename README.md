# machine-fortran

A Z-machine interpreter written in Fortran 90.

The Z-machine is the virtual machine designed by Infocom in 1979 for their
text adventure games, including Zork, Hitchhiker's Guide to the Galaxy, and
many others. This interpreter implements the
[Z-Machine Standards Document v1.1](https://www.inform-fiction.org/zmachine/standards/z1point1/index.html)
and runs story files from versions 1 through 8.

Tested with Mini-Zork I (v3) and Balances (v5).

## Building

Requires `gfortran`. The default Makefile target uses `gfortran-mp-15`
(MacPorts); adjust the `FC` variable in the Makefile if your compiler has a
different name.

```
make
```

For a debug build with bounds checking and backtraces:

```
make debug
```

## Usage

```
./zmachine <story-file>
```

Story files (`.z3`, `.z5`, `.z8`, etc.) are available from the
[IF Archive](https://ifarchive.org/indexes/if-archive/games/zcode/).

## Architecture

The interpreter is split into eight modules:

| Module | Purpose |
|---|---|
| `memory_mod` | Raw memory array, byte/word I/O, story file loading |
| `header_mod` | Header field parsing for all Z-machine versions |
| `stack_mod` | Evaluation stack, call frames, local/global variables |
| `text_mod` | Z-character decoding/encoding, ZSCII, abbreviations |
| `object_mod` | Object tree, attributes, property tables |
| `decode_mod` | Instruction decoder (long, short, variable, extended forms) |
| `quetzal_mod` | Save/restore in standard Quetzal IFF format |
| `execute_mod` | Opcode execution engine |

`zmachine.f90` is the main program that ties everything together.

## Features

- All 2OP, 1OP, 0OP, VAR, and EXT opcodes
- Z-character text decoding with abbreviations and all three alphabets
- Object tree manipulation (attributes, properties, insert, remove)
- Dictionary lookup and input tokenisation
- Save and restore in standard Quetzal format (CMem XOR compression)
- Save undo / restore undo
- Output stream 3 (write to memory table)
- Random number generation with seedable RNG

## Not yet implemented

- Status line display (v3)
- Screen splitting and upper window management
- Colors and text styles
- Sound effects
- Timed input

## References

- [Z-Machine Standards Document v1.1](https://www.inform-fiction.org/zmachine/standards/z1point1/index.html)
- [Quetzal: Z-machine Common Save-File Format](https://www.ifarchive.org/if-archive/infocom/interpreters/specification/savefile_14.txt)
- [The IF Archive](https://ifarchive.org/)
