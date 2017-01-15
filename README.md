# 6502-Tools
Tools and enhancements to make working with 6502 Assembly Language on macOS more enjoyable, especially for retro-coding on the Atari Computer.

## For Atom

### `.atom-build.js`
This file, set up for a 2600 cartridge build, is used by Atom's standard [Build package](https://atom.io/packages/build) to specify the project build command and error pattern. Use the `AtomBuild` folder as a template for other 2600 code builds. For some cases `make` might be preferable.
- Fast build with `dasm`.
  - Set `asm_name` in `.atom-build.js`. Then it will build with `F9` (and build-on-save, if set). Passes all errors back to the **build** module.
  - Uses `/usr/local` install of '`dasm`' and '`vcs`' to build the project. Change if needed.
  - Public domain 2600 combat game source code included for testing.

## For Sublime Text 3

Copy the contents of the `Sublime` folder to your `Packages/User` folder (e.g., `~/Library/Application Support/Sublime Text 3/Packages/User`). You'll need to edit the `.sublime-build` files to set the paths to your installed tools.

### 6502 Support

#### `6502.sublime-settings`
Settings suitable for 6502 code. Applies to `.asm` and `.s` files.

#### `6502.sublime-syntax`
- Pretty smart syntax colorer for *Atari* and *Apple* 6502 code.
  - Parses well-formed `dasm`, `ca65`, and *Atari 8-bit* syntax.
  - Mostly good at auto-finding the end of valid commands, so it can parse code with implicit comments (e.g., *Apple DOS*).
  - Includes `syntax_test_6502.asm` to test the markup with Sublime [PackageDev](https://packagecontrol.io/packages/PackageDev) (Cmd-B).

#### `dasm.sublime-build`
- Uses `/usr/local` install of '`dasm`' and '`vcs`' to build the active 6502 source file. Change if needed. Passes compile (not link) errors to Sublime (`F4`).

### Atari Look-and-Feel

#### `Atari800.tmTheme`
The Atari800 theme provides the classic Atari blue-colored background and bluish-white text, plus extra color-coding used by the syntaxes for AtariBASIC and bAatariBASIC. Free Atari classic fonts (see below) may be installed to get full ATASCII support via the Unicode user-defined area (work-in-progress).

![Atari800.tmTheme](_img/atari-code-theme.png)

### AtariBASIC Support

#### `AtariBASIC.sublime-settings`
This file provides the hook for Sublime to use the Atari800 theme for .LST files, to show rulers at multiples of 38/40 characters, and to draw a final ruler at 254 characters. If the **Atari Classic (Regular)** font is installed, it will be used. (This font will be required for full ATASCII support in future.)

#### `AtariBASIC.sublime-syntax`
Context-aware syntax coloring of AtariBASIC with built-in error-checking. Makes the code more readable with standard code fonts. Best with `Atari800.tmTheme`.

This makes it easy to write AtariBASIC in Sublime 3. For direct input you can just paste code onto the emulator window. For a more robust input, use the `ENTER "K:"` command in the **Atari800MacX** emulator. Then you can paste BASIC code with lines up to 254 characters long. (Exit the `ENTER "K:"` mode by pasting or typing the `END` command.)

#### `AtariBASIC.sublime-build`
Adds `Tools` > `Build System` > `AtariBASIC` (`Command-B`) to run the current code in **atari800** (via `AtariBasic-run.sh`). You will need to edit `AtariBasic-run.sh` to set the path to `atari800` (default is MacPorts `/opt/local/bin`) and the path to your `HardDrive1` folder. You can also mount up to 2 floppy disk images (at least one is required to get a DOS loaded).

##### Run in Atari800MacX
It's possible to run code in **atari800MacX** instead (using the included `AtariBasic-pipe.sh` script). Edit `AtariBasic.sublime-build` and change `AtariBasic-run.sh` to `AtariBasic-pipe.sh`. Edit the `AtariBasic-pipe.sh` script to set some of the same environment parameters as described above. Floppies will be mounted according to the default settings of Atari800MacX.

#### `AtariBASIC.sublime-settings`
AtariBASIC files with LST extension will automatically use `Atari800.tmTheme`, display rulers at 38, 40, 76, 80, 114, 120, and 254, and use the "AtariClassic-Regular" font, if enabled. Line wrap is set at the customary 38 characters. If the font fails to appear, edit this file and change the font name to "Atari Classic". Editing the font name seems to be needed to trigger recognition.

### bAtari Basic Support
The bAtari Basic project was languishing for many years so I created a proper Github fork of [bAtariBASIC](/thinkyhead/bAtariBASIC) to clean up, fix bugs, and work on integrating [64K support by RevEng](http://atariage.com/forums/topic/214909-bb-with-native-64k-cart-support-11dreveng/) (v39 2017-01-08). But since I created that fork, an official [bAtari-Basic](https://github.com/batari-Basic/batari-Basic) GitHub repository has been added, and I've already contributed a few patches.

#### `bAtariBASIC.sublime-settings`
Settings for bAtariBASIC (.BAS) files, including the use of the Atari800 theme (no special font) to gain the extra syntax highlighting.

#### `bAtariBASIC.sublime-syntax`
Context-aware syntax coloring of bAtariBASIC 1.1 syntax with built-in error-checking. (Still a work-in-progress.)

## Other things you'll need…
  - [Sublime Text 3](https://www.sublimetext.com/3) - A light and fast extensible editor with a Python core. Can use TextMate bundles.
  - [Atom](http://atom.io) - A powerful modular editor. May run sluggishly on older systems.
  - [atari800](https://sourceforge.net/projects/atari800/files/atari800/) - A good, working Atari emulator that fails much of the acid test but runs pretty well. (Use F1 for settings!) Also available via package managers like MacPorts and Homebrew.
  - [Atari800MacX](https://github.com/atarimacosx/Atari800MacX/releases) - The latest version seems reliable, but if you experience any crashing try **disabling sound** as a workaround.
  - [Atari Classic Fonts](http://members.bitstream.net/marksim/atarimac/fonts.html) - Regular, Chunky, Smooth, and ExtraSmooth - especially for use with `Atari800.tmTheme`. The newest all-in-one font is preferred, because it has full ATASCII support.
  - [dasm](http://dasm-dillon.sourceforge.net) - A 6502 cross-assembler most commonly used to build Atari 2600 code. Also included with bAtariBASIC.
  - [cc65](https://github.com/cc65/cc65) - A C cross-compiler for many 6502 platforms. A rich set of libraries is included.
  - [ca65](https://github.com/cc65/cc65) - A 6502 macro-assembler, also including headers for many 6502 platforms.
  - [Atari tools](https://github.com/jhallen/atari-tools) - Includes the `atr` command-line tool to manage ATR disk images and files.

## Upcoming Plans
- Better support for Batari Basic in the macOS ecosystem. Syntax coloring, Sublime/Atom build, playfield/sprite tools (in Javascript).
- Updated build of Atari800MacX with latest Atari800 code. It will need new Quartz-based UI code to work with SDL 1.2.15 and the latest macOS.
