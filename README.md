# 6502-Tools
Tools and enhancements to make working with 6502 Assembly Language on macOS more enjoyable, especially for retro-coding the Atari Computer and Atari 2600 VCS.

![Atari800.tmTheme](_img/atari-code-theme.png)

## For Sublime Text 3
Open the `Sublime` folder and copy the two plugin folders to your `Packages` folder (`~/Library/Application Support/Sublime Text 3/Packages`). Then, copy all the `.sublime-settings` files to the `User` subfolder. For the BASIC theme you'll need the font linked below.

Two plugins are currently provided:
- [*ATASCII Insert*](Sublime/AtasciiInsert) adds commands to insert ATASCII text and to invert characters.
- [*AtariTools*](Sublime/AtariTools) provides two perfect themes, support for AtariBASIC and Batari BASIC, and a variety of useful capabilities to enhance your classic Atari 8-bit coding experience.

## Atari BASIC
See my [Atari BASIC Source Book](//github.com/thinkyhead/AtariBASIC) repository for a nicely-curated canonical version of the source code for Atari BASIC. And keep an eye on this space because I'm working on implementing an AtariBASIC tokenizer (and runner?) in Python and Javascript. The cleaned up source code (and the _Atari BASIC Source Book_ itself) will be very helpful in completing this challenging project.

## For Atom (obsolete)
### `.atom-build.js`
This file, set up for a 2600 cartridge build, is used by Atom's standard [Build package](https://atom.io/packages/build) to specify the project build command and error pattern. Use the `AtomBuild` folder as a template for other 2600 code builds. For some cases `make` might be preferable.
### Fast build with `dasm`.
  - Set `asm_name` in `.atom-build.js`. Then it will build with `F9` (and build-on-save, if set). Passes all errors back to the **build** module.
  - Uses `/usr/local` install of '`dasm`' and '`vcs`' to build the project. Change if needed.
  - Public domain 2600 combat game source code included for testing.

## Other things you'll need…
  - [Sublime Text 3](https://www.sublimetext.com/3) - A light and fast extensible editor with a Python core. Can use TextMate bundles.
  - [atari800](https://atari800.github.io/) - A good, working Atari emulator that fails much of the acid test but runs pretty well. (Use F1 for settings!) Also available via package managers like [MacPorts](https://www.macports.org/ports.php?by=name&substr=atari800) and [Homebrew](https://formulae.brew.sh/formula/atari800#default).
  - [Atari800MacX](https://github.com/atarimacosx/Atari800MacX) - This nice Mac UI wrapper for Atari800. There's a Universal version 6.0.1 but sometimes 5.3.1 seems to work better.
  - [Atari Classic Fonts](http://members.bitstream.net/marksim/atarimac/fonts.html) - Regular, Chunky, Smooth, and ExtraSmooth - especially for use with `Atari800.tmTheme`. Use the newest all-in-one font for full ATASCII support. This is the preferred font for editing AtariBASIC.
  - [Eightbit-Atari-Fonts](https://github.com/TheRobotFactory/EightBit-Atari-Fonts) - A collection of custom Atari fonts for use with `Atari800.tmTheme`.
  - [dasm](http://dasm-dillon.sourceforge.net) - A 6502 cross-assembler most commonly used to build Atari 2600 code. Also included with bAtariBASIC.
  - [cc65](https://github.com/cc65/cc65) - A C cross-compiler for many 6502 platforms. A rich set of libraries is included.
  - [ca65](https://github.com/cc65/cc65) - A 6502 macro-assembler, also including headers for many 6502 platforms.
  - [Atari tools](https://github.com/jhallen/atari-tools) - Includes the `atr` command-line tool to manage ATR disk images and files.

## Future Plans
Python is a great language for quick implementations and it's natively supported in Sublime Text.
- Sublime Importer to convert Atari BASIC tokenized files.
- Tokenizer for AtariBASIC so no emulator is needed to convert LST to BAS.
- Interpreter to run and debug AtariBASIC in other environments.
- Better support for Batari Basic in the macOS ecosystem.
- Tools: Character, Map, Player/Missile, and Bitmap editing tools.
