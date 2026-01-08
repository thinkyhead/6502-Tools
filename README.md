# 6502-Tools
Tools and enhancements to make working with 6502 Assembly Language on macOS more enjoyable, especially for retro-coding the Atari Computer and Atari 2600 VCS.

## For Sublime Text 3

Copy the contents of the `Sublime` folder to your `Packages/User` folder (e.g., `~/Library/Application Support/Sublime Text 3/Packages/User`). Edit `AtariTools.sh` to configure the paths of your installed emulators and build tools. See below for the other tools you'll need.

### Atari Look-and-Feel

#### `Atari800.tmTheme`
The Atari800 theme provides the classic Atari blue-colored background and bluish-white text, plus extra color-coding used by the syntaxes for AtariBASIC and bAtariBASIC.

In order to edit Atari BASIC (.LST) files you'll need to install the free Atari Classic fonts (see below). Tgese provide full ATASCII support by mapping the special characters to the Unicode user-defined area. Special ATASCII characters are automatically translated when passing code to and from the emulator.

![Atari800.tmTheme](_img/atari-code-theme.png)

### AtariTools Settings
`AtariTools.sh` is the common location where you'll set up the paths to your tools and other settings. Use this file to set the path to `atari800` (default is MacPorts `/opt/local/bin`), the path to your `HardDrive1` folder, preferred Atari 800 emulator, emulation settings, etc. For the Atari 800, you can mount up to 2 floppy disk images. (At least one floppy is required to get a DOS loaded.)

For bAtariBASIC, the `$bB` environment variable should already exist, but it can be set or overridden here if needed.

### 6502 Support

#### `6502.sublime-settings`
Settings suitable for 6502 code. Applies to `.asm` and `.s` files.

#### `6502.sublime-syntax`
This syntax parser for *Atari* and *Apple* 6502 code needs more work to support modern formats.
- For best results with `ca65` code use the **ca65 Syntax** included with ["65816" Package](//github.com/ksherlock/65816.tmbundle).
- Includes `syntax_test_6502.asm` to test the markup with Sublime [PackageDev](https://packagecontrol.io/packages/PackageDev) (Cmd-B).

#### `LD65Config.sublime-syntax`
Syntax coloring for `.lnk` files (used with `ld65 -C`).

### Atari VCS Build

#### `Atari VCS.sublime-build`
Uses '`dasm`' with the '`vcs`' libraries to build the active 6502 source file for the Atari 2600 and run it in the configured emulator. This is the preferred build method during game development. It doesn't (currently) kill previous instances of `atari800`, so be sure to quit them to ensure they don't pile up!

### bAtariBasic Support
To help with the development of bAtariBasic, I've created a proper Github fork of [bAtariBASIC](https://github.com/thinkyhead/bAtariBasic) where I'm cleaning up code, fixing bugs, and integrating [64K support by RevEng](http://atariage.com/forums/topic/214909-bb-with-native-64k-cart-support-11dreveng/) (v39 2017-01-08). I highly recommend working with this fork as it goes forward.

#### `bAtariBASIC.sublime-settings`
Settings for bAtariBASIC (.BAS) files, including the use of the Atari800 theme (no special font) to gain the extra syntax highlighting.

#### `bAtariBASIC.sublime-syntax`
Context-aware syntax coloring of bAtariBASIC 1.1 syntax with built-in error-checking. (work-in-progress)

### AtariBASIC Support
AtariTools includes AtariBASIC syntax parsing/coloring and a build command to run AtariBASIC in your preferred Atari 800 emulator.

#### `AtariBASIC.sublime-settings`
This file provides the hook for Sublime to use the `Atari800` theme for `.LST` files, defines rulers at multiples of 38/40 characters, and a max length ruler at 254 characters. If the **Atari Classic (Regular)** font is installed, it will be used. (This font is needed for proper ATASCII support in Sublime.)

#### `AtariBASIC.sublime-syntax`
This syntax provides context-aware syntax coloring of AtariBASIC with built-in error-checking. It makes the code much more readable with standard code fonts, but of course it looks best with `Atari800.tmTheme`. A work in progress, it needs better expression handling.

#### `AtariBASIC.sublime-build`
This adds `Tools` > `Build System` > `AtariBASIC` to the menu so you can use **Build** `Command-B` to load and run the active LST file in your favorite Atari 800 emulator.

The **Build** command will run the current BASIC file in **atari800** or **Atari800MacX**, depending on what you last selected under **Build With…**. Make sure your **atari800** and **Atari800MacX** default settings have BASIC enabled, a local folder exposed as hard drive H:, and up to two floppies mounted for development purposes.

**Build** saves a copy of the current file with ATASCII encoding as **SUBLIME.LST** to your configured Atari hard drive (H:) folder. The emulator then loads and runs **H:SUBLIME.LST** directly for fastest file loading. At this stage you can toggle Turbo in the emulator to speed things up.

## Helper Scripts

### `AtariTools.sh`
**_Edit this file!_** The `AtariTools.sh` file contains the configuration values and file paths that will be used by the helper scripts. Provide paths to your emulators and your "H1:" hard drive folder.

### `bAtariBASIC-build.sh`
Used by the **Build** command to convert bAtariBASIC code to Assembly and compile it with `dasm`.

### `AtariVCS-build.sh`
Used by the **Build** command to do a standard build for the Atari 2600 using `dasm`.

### `AtariBASIC-run.sh`
Used by the **Build** command to run AtariBASIC code in the emulator you have configured in the `AtariTools.sh` script.

### `Atari800-build.sh`
Build and run Atari Assembly Language code with `ca65` / `cl65`.

### `atari2subl.sh`
This wrapper for `atascii` is used to pipe "P:" device output from `atari800` or `Atari800MacX` to Sublime Text (`subl`).
- Configure the `atari800` "Print Command" to use this script.
- Configure the `Atari800MacX` "Print cmd" to use this script.

### `atascii`
Native binary of `atascii.cpp` to convert ATASCII <-> Unicode with ATASCII extensions.

### `atascii.py`
Python version of `atascii` converted from C++ with assistance from Mistral-small-3.2-24b.

### `atascii.cpp`
The C++ source code of `atascii` which converts ATASCII <-> Unicode. Requires `unistd.h`, `stdio.h`, `stdlib.h`, `libgen.h`.

### `tokenize.cpp`
WIP - Tokenizer to compile AtariBASIC (`.LST`) text into `.BAS` format.

### `basic.py`
WIP - Loader, tokenizer, and interpreter for AtariBASIC written in Python. It can load and list BAS files in tokenized form but it currently has no tokenizer or interpreter. See `basic.py -h` for command-line options.

### `xexinfo.py`
Script to read a structured Atari XEX, COM, or BIN file and display its contents. See `xexinfo.py -h` for command-line options.

## Atari BASIC Source Book

In celebration of the 50th birthday of the 6502 I cleaned up and repaired the [Atari BASIC Source Code](//raw.githubusercontent.com/thinkyhead/6502-Tools/refs/heads/master/AtariBasic-Source-Book/BASIC/ataribas.asm) based on the original "Atari BASIC Source Book" by Bill Wilkinson, Kathleen O'Brien, and Paul Laughton. I started with a very good cleaned up dump [Atari BASIC Rev.A/B/C](//forums.atariage.com/applications/core/interface/file/attachment.php?id=84877) from AtariAge that builds with `ca65` and made the following improvements:

- **BNF Macros**: BNF elements are now neatly defined inline with `.byte`. Much easier to modify and extend.
- Added `sourcebook.inc` header with aliases for older / original symbol names for better alignment with the Source Book.
- Applied original comments throughout the code — including almost all original typos!
- Applied uppercase to all the Rev.1 mnemonics, leaving Rev.2/3 lowercase and un-documented.

This language can be improved by a mile. To modify the language you should add new `BASIC_REVISION` conditionals, then modify the link size and headers as-needed to get it to build and run. The `Makefile` is set up to build the historical cartridge ROMs, but it shouldn't be too hard to make runnable XEX files.

Watch this space as I'm working on implementing an AtariBASIC tokenizer and runner in Python and Javascript. This source code is going to be a very helpful guide in that project.

## For Atom (obsolete)

### `.atom-build.js`
This file, set up for a 2600 cartridge build, is used by Atom's standard [Build package](https://atom.io/packages/build) to specify the project build command and error pattern. Use the `AtomBuild` folder as a template for other 2600 code builds. For some cases `make` might be preferable.
- Fast build with `dasm`.
  - Set `asm_name` in `.atom-build.js`. Then it will build with `F9` (and build-on-save, if set). Passes all errors back to the **build** module.
  - Uses `/usr/local` install of '`dasm`' and '`vcs`' to build the project. Change if needed.
  - Public domain 2600 combat game source code included for testing.

## Other things you'll need…
  - [Sublime Text 3](https://www.sublimetext.com/3) - A light and fast extensible editor with a Python core. Can use TextMate bundles.
  - [atari800](https://atari800.github.io/) - A good, working Atari emulator that fails much of the acid test but runs pretty well. (Use F1 for settings!) Also available via package managers like [MacPorts](https://www.macports.org/ports.php?by=name&substr=atari800) and [Homebrew](https://formulae.brew.sh/formula/atari800#default).
  - [Atari800MacX](https://github.com/atarimacosx/Atari800MacX) - This nice Mac UI wrapper for Atari800. There's a Universal version 6.0.1 but sometimes 5.3.1 seems to work better.
  - [Atari Classic Fonts](http://members.bitstream.net/marksim/atarimac/fonts.html) - Regular, Chunky, Smooth, and ExtraSmooth - especially for use with `Atari800.tmTheme`. Use the newest all-in-one font for full ATASCII support.
  - [Eightbit-Atari-Fonts](https://github.com/TheRobotFactory/EightBit-Atari-Fonts) - A collection of custom Atari fonts for use with `Atari800.tmTheme`. "EightBit Atari" is now the default for AtariBASIC syntax.
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
