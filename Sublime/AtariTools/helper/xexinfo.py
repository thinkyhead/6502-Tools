#!/usr/bin/env python3
"""
xexinfo.py

Scan an Atari XEX/BIN file, list each chunk (start → end, payload size),
and report any trailing garbage that cannot form a valid chunk.

Usage:
    xexinfo.py [--strict] [--verbose] <file>
"""

import sys, argparse, struct
from pathlib import Path

MARKER = b'\xFF\xFF'

def parse_chunks(data: bytes, strict: bool = False):
    """
    Generator that yields (chunk_index, start, end, size).

    If *strict* is True, a trailing garbage block that is
    not a valid chunk raises a ValueError.  Otherwise the
    generator simply stops at the first incomplete header.
    """
    offset = 0
    chunk_index = 1
    total_len = len(data)

    while offset < total_len:
        # Skip optional marker if present
        if data[offset:offset + 2] == MARKER:
            offset += 2

        # Not enough bytes left for a header
        if offset + 4 > total_len:
            if strict:
                raise ValueError(f"Truncated header at offset ${offset:04X}")
            # Not enough data for another header → finish
            break

        # Little‑endian 16‑bit start/end
        start, end = struct.unpack_from("<HH", data, offset)
        offset += 4

        # Sanity checks
        if start > end:
            raise ValueError(f"Chunk {chunk_index}: start ${start:04X} > end ${end:04X}")

        datasize = end - start + 1

        # Ensure the payload fits in the remaining data
        if offset + datasize > total_len:
            if strict:
                raise ValueError(
                    f"Chunk {chunk_index}: payload size ${datasize:04X} "
                    f"exceeds file size at offset ${offset:04X}"
                )
            # In lenient mode we simply stop here
            break

        payload = data[offset : offset + datasize]
        yield chunk_index, start, end, datasize, payload

        # Skip payload
        offset += datasize
        chunk_index += 1

    # Anything left after the last complete chunk is *garbage*
    if offset < total_len:
        yield None, None, None, None, None, data[offset:]

def format_address(addr: int) -> str:
    """Return the address in the form $XXXX."""
    return f"${addr:04X}"

def main():
    parser = argparse.ArgumentParser(description=("List the address span of each chunk in an Atari XEX/BIN file."))
    parser.add_argument("file", type=Path, help="Path to the XEX or BIN file to inspect.")
    parser.add_argument("-v", "--verbose", action="store_true", help="Print the data of each chunk.")
    parser.add_argument("-s", "--strict", action="store_true", help="Report trailing data as an error.")
    args = parser.parse_args()

    if not args.file.is_file():
        print(f"Error: {args.file} is not a readable file.", file=sys.stderr)
        sys.exit(1)

    with args.file.open("rb") as f:
        data = f.read()

    print(f"Scanning {args.file} ({len(data)} bytes) …\n")

    for result in parse_chunks(data, strict=args.strict):
        # Garbage block
        if len(result) == 6 and result[0] is None:
            _, _, _, _, _, garbage = result
            print(f"*** Trailing garbage: {len(garbage)} bytes ***")
            if args.verbose:
                hex_bytes = " ".join(f"{b:02X}" for b in garbage)
                print(f"  {hex_bytes}")
            break  # no more chunks after garbage

        idx, start, end, size, payload = result

        line = f"Chunk {idx:02d}: {format_address(start)} → {format_address(end)}  ({size:5d} bytes)"

        # Detect RUNAD / INITAD chunks
        if start == 0x2E0 and end == 0x2E1 and size == 2:
            runad = payload[0] | (payload[1] << 8)
            line += f"  RUNAD={format_address(runad)}"
        elif start == 0x2E2 and end == 0x2E3 and size == 2:
            initad = payload[0] | (payload[1] << 8)
            line += f"  INITAD={format_address(initad)}"

        print(line)

        if args.verbose:
            hex_bytes = " ".join(f"{b:02X}" for b in payload)
            print(f"  Payload: {hex_bytes}\n")

    print("\nDone.")


if __name__ == "__main__":
    main()
