# AtariTools for Sublime Text

This plugin adds enhancements to improve the coding experience for Atari 8-bit computers and the Atari 2600 VCS.
- Atari Computer color scheme, both light and dark
- 6502 Assembly and ld65 syntax highlighting
- AtariBASIC Syntax and Build System
- bAtariBASIC Syntax and Build System
- Atari VCS Build System

## Installation

In Sublime, from the menu bar select *Sublime Text* > *Settings* > *Browse Packages* to open the `Packages` folder. Then simply copy this (`AtariTools`) folder into the `Packages` folder.

### Atari Classic Fonts
For the best experience you'll also need these fonts:
  - [Atari Classic Fonts](http://members.bitstream.net/marksim/atarimac/fonts.html) - Regular, Chunky, Smooth, and ExtraSmooth - especially for use with `Atari800.sublime-color-scheme`. Use the newest all-in-one font for full ATASCII support. This is the preferred font for editing AtariBASIC.
  - [Eightbit-Atari-Fonts](https://github.com/TheRobotFactory/EightBit-Atari-Fonts) - A collection of custom Atari fonts with added kerning for use with `Atari800.sublime-color-scheme`.

### AtariTools Settings

You'll need to edit `AtariTools-settings.sh` to use the included Build Systems. This is where you'll set the path to `atari800` (default is MacPorts `/opt/local/bin`), the path to your `HardDrive1` folder, preferred Atari 800 emulator, emulation settings, etc. For the Atari 800, you can mount up to 2 floppy disk images. (At least one floppy is required to get a DOS loaded.)

For bAtariBASIC, the `$bB` environment variable should already exist, but it can be set or overridden in `AtariTools-settings.sh` if needed.

### Atari Look-and-Feel

Install the [Atari800 Color Schemes](//github.com/thinkyhead/Atari800) for a perfect classic Atari blue-colored background and bluish-white text, plus extra color-coding used by the syntaxes for AtariBASIC and bAtariBASIC. Both light and dark themes are provided.

In order to edit the included Atari BASIC (`.LST`) Unicode files you'll need to install the free Atari Classic fonts (see below). These provide full ATASCII support by mapping the special characters to the Unicode user-defined area. Special ATASCII characters are automatically translated when passing code to and from the emulator.

![Atari800.sublime-color-scheme](../../_img/atari-code-theme.png)

### AtariBASIC Support
AtariTools includes AtariBASIC syntax parsing/coloring and a build command to run AtariBASIC in your preferred Atari 800 emulator.

#### `AtariBASIC.sublime-settings`
This file provides the hook for Sublime to use the `Atari800` theme for `.LST` files, defines rulers at multiples of 38/40 characters, and a max length ruler at 254 characters. If the **Atari Classic (Regular)** font is installed, it will be used. (This font is needed for proper ATASCII support in Sublime.)

#### `AtariBASIC.sublime-syntax`
This syntax provides context-aware syntax coloring of AtariBASIC with built-in error-checking. It makes the code much more readable with standard code fonts, but of course it looks best with `Atari800.sublime-color-scheme`. A work in progress, it needs better expression handling. Syntax test included.

#### `AtariBASIC.sublime-build`
This adds `Tools` > `Build System` > `AtariBASIC` to the menu so you can use **Build** `Command-B` to load and run the active LST file in your favorite Atari 800 emulator.

The **Build** command will run the current BASIC file in **atari800** or **Atari800MacX**, depending on what you last selected under **Build With…**. Make sure your **atari800** and **Atari800MacX** default settings have BASIC enabled, a local folder exposed as hard drive H:, and up to two floppies mounted for development purposes.

**Build** saves a copy of the current file with ATASCII encoding as **SUBLIME.LST** to your configured Atari hard drive (H:) folder. The emulator then loads and runs **H:SUBLIME.LST** directly for fastest file loading. At this stage you can toggle Turbo in the emulator to speed things up.

### bAtariBasic Support
To help with the development of bAtariBasic, I've created a proper Github fork of [bAtariBASIC](https://github.com/thinkyhead/bAtariBasic) where I'm cleaning up code, fixing bugs, and integrating [64K support by RevEng](http://atariage.com/forums/topic/214909-bb-with-native-64k-cart-support-11dreveng/) (v39 2017-01-08). I highly recommend working with this fork as it goes forward.

#### `bAtariBASIC.sublime-settings`
Settings for bAtariBASIC (.BAS) files, including the use of the Atari800 theme (no special font) to gain the extra syntax highlighting.

#### `bAtariBASIC.sublime-syntax`
Context-aware syntax coloring of bAtariBASIC 1.1 syntax with built-in error-checking. (work-in-progress) Syntax test included.

### Atari VCS Build

#### `Atari VCS.sublime-build`
Uses '`dasm`' with the '`vcs`' libraries to build the active 6502 source file for the Atari 2600 and run it in the configured emulator. This is the preferred build method during game development. It doesn't (currently) kill previous instances of `stella`, so be sure to quit them to ensure they don't pile up!

### 6502 Assembler Support

#### `6502.sublime-settings`
Settings suitable for 6502 code. Applies to `.asm` and `.s` files.

#### `6502.sublime-syntax`
This syntax parser for *Atari* and *Apple* 6502 code needs more work to support modern formats. I'll continue to enhance this whenever I do 6502 coding.
- For best results with `ca65` code use the **ca65 Syntax** included with ["65816" Package](//github.com/ksherlock/65816.tmbundle).
- Includes `syntax_test_6502.asm` to test the markup with Sublime [PackageDev](https://packagecontrol.io/packages/PackageDev) (Cmd-B).

#### `LD65Config.sublime-syntax`
Syntax coloring for `.lnk` files (used with `ld65 -C`). Syntax test included.

## Helper Scripts

### `AtariTools-settings.sh`
**_Edit this file!_** The `AtariTools-settings.sh` file contains the configuration values and file paths that will be used by the helper scripts. Provide paths to your emulators and your "H1:" hard drive folder.

### `bAtariBASIC-build.sh`
Used by the **Build** command to convert bAtariBASIC code to Assembly and compile it with `dasm`.

### `AtariVCS-build.sh`
Used by the **Build** command to do a standard build for the Atari 2600 using `dasm`.

### `AtariBASIC-run.sh`
Used by the **Build** command to run AtariBASIC code in the emulator you have configured in the `AtariTools-settings.sh` script.

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

### `xexinfo.py`
Script to read a structured Atari XEX, COM, or BIN file and display its contents. See `xexinfo.py -h` for command-line options.
