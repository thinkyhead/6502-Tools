#!/usr/bin/env python3
"""
Unicode Character Range Generator

This script generates a text file containing all Unicode characters
between two specified code points (inclusive).
"""

import sys
import os

def parse_hex_input(hex_str):
    """
    Parse and validate hexadecimal input string.

    Args:
        hex_str: String representing a hexadecimal number

    Returns:
        int: The integer value of the hex string

    Raises:
        ValueError: If the input is invalid
    """
    try:
        # Remove '0x' or '0X' prefix if present
        cleaned = hex_str.strip()
        if cleaned.lower().startswith('0x'):
            cleaned = cleaned[2:]

        code_point = int(cleaned, 16)

        # Validate Unicode range (U+0000 to U+10FFFF)
        if not (0 <= code_point <= 0x10FFFF):
            raise ValueError(f"Code point {hex(code_point)} is outside valid Unicode range")

        return code_point

    except ValueError as e:
        raise ValueError(f"Invalid hexadecimal input '{hex_str}': {e}")

def generate_unicode_file(start_code, end_code, output_filename):
    """
    Generate a file containing all Unicode characters between two code points.

    Args:
        start_code: Starting Unicode code point (integer)
        end_code: Ending Unicode code point (integer)
        output_filename: Name of the output file

    Returns:
        bool: True if successful, False otherwise
    """
    # Ensure valid range
    if start_code > end_code:
        raise ValueError(f"Start code point ({hex(start_code)}) cannot be greater than end code point ({hex(end_code)})")

    try:
        with open(output_filename, 'w', encoding='utf-8') as f:
            for i in range(start_code, end_code + 1):
                # Skip invalid surrogate pairs (U+D800 to U+DFFF)
                if 0xD800 <= i <= 0xDFFF:
                    continue
                char = chr(i)
                f.write(char)

        print(f"Successfully created '{output_filename}' with {end_code - start_code + 1} characters")
        return True

    except IOError as e:
        raise IOError(f"Failed to create file '{output_filename}': {e}")

def main():
    """Main function to handle command-line arguments."""
    if len(sys.argv) != 4:
        print("Usage: python unicode_range.py <start_hex> <end_hex> <output_file>")
        print("\nExamples:")
        print("  python unicode_range.py 0x41 0x5A output.txt     # A-Z")
        print("  python unicode_range.py 0x61 0xFF output.txt     # a-z and more")
        print("  python unicode_range.py 32 127 output.txt        # ASCII space to DEL")
        sys.exit(1)

    try:
        start_hex = sys.argv[1]
        end_hex = sys.argv[2]
        output_file = sys.argv[3]

        start_code_point = parse_hex_input(start_hex)
        end_code_point = parse_hex_input(end_hex)

        generate_unicode_file(start_code_point, end_code_point, output_file)

    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
