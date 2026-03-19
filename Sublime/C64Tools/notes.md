## Capabilities
 - Edit and save C64 BASIC (.l64) listing files. L64 is the Unicode representation of Commodore BASIC.
 - Load PRG files with petcat with Unicode conversion
 	- Older docs at https://vice-emu.sourceforge.io/vice_16.html#SEC400 indicate semicolon comments are ignored
 	- Include an option to choose unshifted or shifted conversion
 	- Look at loading PRG files within a disk file (.d64) or a C64 Archiver file (.car)
 - Syntax check and tokenize L64 to PRG
 	- See if https://www.ajordison.co.uk/ can help
 	- Apparently petcat can do this once the Unicode has been properly filtered
 		- petcat -wsimon -o outputfile.prg -- inputfile.txt
 		- petcat -wsimon -o outputfile.prg -- $(petunia.py -k inputfile.l64)
 		- petcat -wsimon -o outputfile.prg -- $(~/Projects/Retro/6502-Tools/Sublime/C64Tools/helper/petunia.py -k ~/Projects/Retro/6502-Tools/Crossover/CommodoreBASIC/Hello World.l64)
 		- ~/Projects/Retro/6502-Tools/Sublime/C64Tools/helper/petunia.py

## Translating between PETSCII and Unicode
 - The C64 shows chars either shifted or unshifted. In mixed case, lowercase is 65-90, uppercase goes where lowercase is in ASCII.
 - So there is no mix of upper and lowercase.
 - When sending to C64 the distinction between character sets is thrown away.
 - In VICE use CTRL-N to go to "shifted" mode. No idea how to get back to "unshifted" mode.
 - petcat can output characters in {$hh} hex format as the first stage of PRG -> ASCII -> L64 (Unicode) conversion.

## Plugin Architecture
 - Currently uses a helper/script.sh for Build. The first half is just for parsing shell parameters!
 - So, convert to a Python window command that can access settings for required paths.
 - However, this means no Makefile companion to convert L64 to PRG.
 - The command can absorb the functionality of the `petunia.py` convenience methods that interface with VICE.
 - Getting output from VICE is still unsolved. Neither Print nor Save is working.

## petcat

	petcat (VICE 3.10) -- Basic list/crunch utility.

Usage:  petcat  [-c | -nc]  [-h | -nh]  [-text | -<version> | -w<version>]
		[-skip <bytes>] [-l <hex>]  [--] [file list]
		[-k[<version>]]

   -help -?	Output this help screen here
   -version	print petcat version
   -v		verbose output
   -c		controls (interpret also control codes) <default if textmode>
   -nc		no controls (suppress control codes in printout)
   		<default if non-textmode>
   -ic		interpret control codes case-insensitive
   -qc		convert all non alphanumeric characters inside quotes into controlcodes
   -d		output raw codes in decimal
   -h		write header <default if output is stdout>
   -nh		no header <default if output is a file>
   -skip <n>	Skip <n> bytes in the beginning of input file. Ignored on P00.
   -text	Force text mode
   -C<chksum>	add listing checksums of the specified type.
   -<version>	use keywords for <version> instead of the v7.0 ones
   -w<version>	tokenize using keywords on specified Basic version.
   -k<version>	list all keywords for the specified Basic version
   -k		list all Basic versions available.
   -l		Specify load address for program (in hex, no leading chars!).
   -o <name>	Specify the output file name
   -f		Force overwritten the output file
   		The default depends on the BASIC version.

	Versions:
	10	Basic v10.0 (C65/C64DX)
	1p	Basic v1.0 (PET)
	2	Basic v2.0 (C64/VIC20/PET)
	3	Basic v3.5 (C16)
	40	Basic v4.0 (PET/CBM2)
	4e	Basic v2.0 with Basic v4.0 extension (C64)
	4v	Basic v2.0 with Basic v4.0 extension (VIC20)
	5	Basic v2.0 with Basic v5.0 extension (VIC20)
	65	Basic v65.0 (Mega65)
	70	Basic v7.0 (C128)
	71	Basic v7.0 with Basic v7.1 extension (C128)
	a	Basic v2.0 with @Basic (C64)
	bk	Basic v2.0 with Kipper Basic (C64)
	blarg	Basic v2.0 with Blarg (C64)
	bob	Basic v2.0 with Basic on Bails (C64)
	bsx	Basic v2.0 with Basex (C64)
	bwarsaw	Basic v2.0 with Warsaw Basic (C64)
	bws	Basic v2.0 with WS Basic (C64)
	bwsf	Basic v2.0 with WS Basic final (C64)
	drago	Basic v2.0 with Drago Basic v2.2 (C64)
	easy	Basic v2.0 with Easy Basic (VIC20)
	eve	Basic v2.0 with Eve Basic (C64)
	exp20	Basic v2.0 with Expanded Basic (VIC20)
	exp64	Basic v2.0 with Expanded Basic (C64)
	f	Basic v2.0 with Final Cartridge III (C64)
	game	Basic v2.0 with Game Basic (C64)
	graph	Basic v2.0 with Graphics Basic (C64)
	handy	Basic v2.0 with Handy Basic v1.0 (VIC20)
	lightning	Basic v2.0 with Basic Lightning (C64)
	magic	Basic v2.0 with Magic Basic (C64)
	mighty	Basic v2.0 with Mighty Basic (VIC20)
	pegasus	Basic v2.0 with Pegasus Basic v4.0 (C64)
	reu	Basic v2.0 with REU-Basic (C64)
	simon	Basic v2.0 with Simons' Basic (C64)
	speech	Basic v2.0 with Speech Basic v2.7 (C64)
	superbas	Basic v2.0 with Super Basic (C64)
	superexp	Basic v2.0 with Super Expander (VIC20)
	supergra	Basic v2.0 with Supergrafik 64 (C64)
	sxc	Basic v2.0 with Super Expander Chip (C64)
	tt64	Basic v2.0 with The Tool 64 (C64)
	turtle	Basic v2.0 with Turtle Basic v1.0 (VIC20)
	ultra	Basic v2.0 with Ultrabasic-64 (C64)
	xbasic	Basic v2.0 with Xbasic (C64)


	Checksummers:
	64er-v1	64'er Checksummer v1 + v2
	64er-v3	64'er Checksummer v3
	dossier	Commodore Dossier Checksummer
	f64	F64Summer

	Usage examples:
	petcat -2 -o outputfile.txt -- inputfile.prg
		De-tokenize, convert inputfile.prg to a text file
		in outputfile.txt, using BASIC V2 only
	petcat -wsimon -o outputfile.prg -- inputfile.txt
		Tokenize, convert inputfile.txt to a PRG file
		in outputfile.prg, using Simons' BASIC
	petcat -text -o outputfile.txt -- inputfile.seq
		Convert inputfile.seq to a Ascii text file
		in outputfile.txt.
	petcat -text -w2 -o outputfile.seq -- inputfile.txt
		Convert inputfile.txt to a Petscii text SEQ file
		in outputfile.seq.

## x64 Emulator
```
-logfile <Name>
	Specify log file name
-loglimit <Type>
	Set log level: (0: None, 255: All)
-silent
	Disable all log output, except errors.
-verbose
	Enable verbose log output.
-debug
	Enable verbose debug log output.
-logtofile
	Log to the log file.
+logtofile
	Do not log to the log file.
-logtostdout
	Log to stdout.
+logtostdout
	Do not log to stdout.
-logtomonitor
	Log to the monitor.
+logtomonitor
	Do not log to the monitor.
-logcolorize
	Colorize the log output.
+logcolorize
	Do not colorize the log output.
-help
	Show a list of the available options and exit normally
-?
	Show a list of the available options and exit normally
-h
	Show a list of the available options and exit normally
-version
	Show the program name and version
-features
	Show a list of the available compile-time options and their configuration.
-default
	Restore default settings
-config <filename>
	Specify config file
-addconfig <filename>
	Specify extra config file for loading additional resources.
-dumpconfig <filename>
	Dump all resources to specified config file
-chdir Directory
	Change current working directory.
-limitcycles <value>
	Specify number of cycles to run before quitting with an error.
-console
	Console mode (for music playback)
-seed <value>
	Set random seed (for debugging)
-core
	Allow production of core dumps
+core
	Do not produce core dumps
-autostart <Name>
	Attach and autostart tape/disk image <name>
-autoload <Name>
	Attach and autoload tape/disk image <name>
-1 <Name>
	Attach <name> as a tape image
-8 <Name>
	Attach <name> as a disk image in unit #8
-8d1 <Name>
	Attach <name> as a disk image in unit #8 drive #1
-9 <Name>
	Attach <name> as a disk image in unit #9
-9d1 <Name>
	Attach <name> as a disk image in unit #9 drive #1
-10 <Name>
	Attach <name> as a disk image in unit #10
-10d1 <Name>
	Attach <name> as a disk image in unit #10 drive #1
-11 <Name>
	Attach <name> as a disk image in unit #11
-11d1 <Name>
	Attach <name> as a disk image in unit #11 drive #1
-directory <Path>
	Define search path to locate system files
-confirmonexit
	Confirm quitting VICE
+confirmonexit
	Do not confirm quitting VICE
-pauseonsettings
	Pause emulation when activating settings dialog
+pauseonsettings
	Do not pause emulation when activating settings dialog
-saveres
	Save settings on exit
+saveres
	Do not save settings on exit
-minimized
	Start VICE minimized
+minimized
	Do not start VICE minimized
-maximized
	Start VICE maximized
+maximized
	Do not start VICE maximized
-nativemonitor
	Use native monitor on OS terminal
+nativemonitor
	Use VICE Gtk3 monitor terminal
-fullscreen-decorations
	Enable fullscreen decorations
+fullscreen-decorations
	Disable fullscreen decorations
-monitorfont font-description
	Set monitor font for the Gtk3 monitor
-monitorbg font-background
	Set monitor font background color
-monitorfg font-foreground
	Set monitor font foreround color
-autostart-on-doubleclick
	Autostart files on doubleclick
+autostart-on-doubleclick
	Open files on doubleclick
-settings-node settings-node
	Open settings dialog at <settings-node>
-monitorxpos X
	Set monitor window X position
-monitorypos Y
	Set monitor window Y position
-monitorwidth width
	Set monitor window width
-monitorheight height
	Set monitor window height
-windowxpos X
	Set window X position
-windowypos Y
	Set window Y position
-windowwidth width
	Set window width
-windowheight height
	Set window height
-windowxpos1 X
	Set secondary window X position
-windowypos1 Y
	Set secondary window Y position
-windowwidth1 width
	Set secondary window width
-windowheight1 height
	Set secondary window height
-romsetfile <File>
	load the given romset file
-romsetarchive <File>
	load the given romset archive
-romsetarchiveselect <Item number>
	select the given item from the current romset archive
-moncommands <Name>
	Execute monitor commands from file
-monlogname <Name>
	Set name of monitor log file
-monlog
	Enable logging monitor output to a file
+monlog
	Disable logging monitor output to a file
-initbreak <value>
	Set an initial breakpoint for the monitor: <address>, ready, or reset
-keepmonopen
	Keep the monitor open
+keepmonopen
	Do not keep the monitor open
-refreshonbreak
	Refresh display after monitor command
+refreshonbreak
	Do not refresh display after monitor command
-monscrollbacklines <value>
	Set number of lines to keep in the monitor scrollback buffer
-monchislines <value>
	Set number of lines to keep in the cpu history
-jamaction <Type>
	Set action on CPU JAM: (0: Ask, 1: continue, 2: Monitor, 3: Reset, 4: Power cycle, 5: Quit Emulator)
-exitscreenshot <Name>
	Set name of screenshot to save when emulator exits.
-speed <percent or negative fps>
	Limit emulation speed to specified value
-warp
	Initially enable warp mode
+warp
	Do not initially enable warp mode (default)
-sound
	Enable sound playback
+sound
	Disable sound playback
-soundrate <value>
	Set sound sample rate to <value> Hz
-soundbufsize <value>
	Set sound buffer size to <value> msec
-soundfragsize <value>
	Set sound fragment size (0: very small, 1: small, 2: medium, 3: large, 4: very large)
-soundoutput <output mode>
	Sound output mode: (0: system decides mono/stereo, 1: always mono, 2: always stereo)
-soundvolume <Volume>
	Specify the sound volume (0..100)
-soundwarpmode <mode>
	Specify how to handle sound emulation in warp mode: (0: do not emulate the sound chips, 1: keep emulating the sound chips)
-sounddev <Name>
	Specify sound driver. (coreaudio/dummy/dump)
-soundarg <args>
	Specify initialization parameters for sound driver
-soundrecdev <Name>
	Specify recording sound driver. (fs/wav/voc/iff/aiff/flac/ogg/soundmovie)
-soundrecarg <args>
	Specify initialization parameters for recording sound driver
-keymap <number>
	Specify index of keymap file (0=symbolic, 1=positional, 2=symbolic (user), 3=positional (user))
-keyboardmapping <number>
	Specify host keyboard layout
-keyboardtype <number>
	Specify emulated keyboard type
-symkeymap <Name>
	Specify name of symbolic keymap file
-poskeymap <Name>
	Specify name of positional keymap file
-kbdstatusbar
	Enable keyboard-status bar
+kbdstatusbar
	Disable keyboard-status bar
-trapdevice1
	Enable general mechanisms for fast disk/tape emulation
+trapdevice1
	Disable general mechanisms for fast disk/tape emulation
-trapdevice2
	Enable general mechanisms for fast disk/tape emulation
+trapdevice2
	Disable general mechanisms for fast disk/tape emulation
-trapdevice4
	Enable general mechanisms for fast disk/tape emulation
+trapdevice4
	Disable general mechanisms for fast disk/tape emulation
-trapdevice5
	Enable general mechanisms for fast disk/tape emulation
+trapdevice5
	Disable general mechanisms for fast disk/tape emulation
-trapdevice6
	Enable general mechanisms for fast disk/tape emulation
+trapdevice6
	Disable general mechanisms for fast disk/tape emulation
-trapdevice7
	Enable general mechanisms for fast disk/tape emulation
+trapdevice7
	Disable general mechanisms for fast disk/tape emulation
-trapdevice8
	Enable general mechanisms for fast disk/tape emulation
+trapdevice8
	Disable general mechanisms for fast disk/tape emulation
-trapdevice9
	Enable general mechanisms for fast disk/tape emulation
+trapdevice9
	Disable general mechanisms for fast disk/tape emulation
-trapdevice10
	Enable general mechanisms for fast disk/tape emulation
+trapdevice10
	Disable general mechanisms for fast disk/tape emulation
-trapdevice11
	Enable general mechanisms for fast disk/tape emulation
+trapdevice11
	Disable general mechanisms for fast disk/tape emulation
-aneloglevel <Type>
	Set ANE log level: (0: None, 1: Unstable, 2: All)
-lxaloglevel <Type>
	Set LXA log level: (0: None, 1: Unstable, 2: All)
-pal
	Use PAL sync factor
-ntsc
	Use NTSC sync factor
-ntscold
	Use old NTSC sync factor
-paln
	Use PAL-N sync factor
-power50
	Use 50Hz Power-grid frequency
-power60
	Use 60Hz Power-grid frequency
-kernal <Name>
	Specify name of Kernal ROM image
-basic <Name>
	Specify name of BASIC ROM image
-chargen <Name>
	Specify name of character generator ROM image
-kernalrev <Revision>
	Patch the Kernal ROM to the specified <revision> (0/jap: japanese 1: rev. 1, 2: rev. 2, 3: rev. 3, 39/gs: C64 GS, 67/sx: sx64, 100/4064: 4064)
-acia1
	Enable the ACIA RS232 interface emulation
+acia1
	Disable the ACIA RS232 interface emulation
-ciamodel <Model>
	Set both CIA models (0 = old 6526, 1 = new 8521)
-cia1model <Model>
	Set CIA 1 model (0 = old 6526, 1 = new 8521)
-cia2model <Model>
	Set CIA 2 model (0 = old 6526, 1 = new 8521)
-model <Model>
	Set C64 model (c64/c64c/c64old, ntsc/newntsc/oldntsc, drean, jap, c64gs, pet64, ultimax)
-burstmod <value>
	Burst modification (0 = None, 1 = CIA1, 2 = CIA2)
-iecreset <value>
	Computer reset goes to IEC bus (0 = No, 1 = Yes)
-memoryexphack <device>
	Set the 'memory expansion hack' device (0: None, 1: C64 256K, 2: +60K, 3: +256K)
-plus60kimage <Name>
	Specify name of PLUS60K image
-plus60kbase <Base address>
	Base address of the PLUS60K expansion. (0xD040/0xD100)
-plus256kimage <Name>
	Specify name of PLUS256K image
-256kimage <Name>
	Specify name of 256K image
-256kbase <Base address>
	Base address of the 256K expansion. (0xDE00/0xDE80/0xDF00/0xDF80)
-VICIIvcache
	Enable the video cache
+VICIIvcache
	Disable the video cache
-VICIIdsize
	Enable double size
+VICIIdsize
	Disable double size
-VICIIdscan
	Enable double scan
+VICIIdscan
	Disable double scan
-VICIIaudioleak
	Enable audio leak emulation
+VICIIaudioleak
	Disable audio leak emulation
-VICIIfilter <Mode>
	Select rendering filter: (0: none, 1: CRT emulation, 2: scale2x)
-VICIIintpal
	Use an internal calculated palette
-VICIIextpal
	Use an external palette (file)
-VICIIpalette <Name>
	Specify name of file of external palette
-VICIIfull
	Enable fullscreen
+VICIIfull
	Disable fullscreen
-VICIIshowstatusbar
	Show status bar
+VICIIshowstatusbar
	Hide status bar
-VICIIaspectmode <mode>
	Set aspect ratio mode (0 = off, 1 = custom, 2 = true)
-VICIIaspect <aspect ratio>
	Set custom aspect ratio (0.5 - 2.0)
-VICIIglfilter <mode>
	Set OpenGL filtering mode (0 = nearest, 1 = linear, 2 = bicubic)
-VICIIflipx
	Enable X flip
+VICIIflipx
	Disable X flip
-VICIIflipy
	Enable Y flip
+VICIIflipy
	Disable Y flip
-VICIIrotate
	Rotate 90 degrees clockwise
+VICIIrotate
	Do not rotate
-VICIIvsync
	Enable vsync to prevent screen tearing
+VICIIvsync
	Disable vsync to allow screen tearing
-VICIIsaturation <0-2000>
	Set saturation of internal calculated palette
-VICIIcontrast <0-2000>
	Set contrast of internal calculated palette
-VICIIbrightness <0-2000>
	Set brightness of internal calculated palette
-VICIIgamma <0-4000>
	Set gamma of internal calculated palette
-VICIItint <0-2000>
	Set tint of internal calculated palette
-VICIIcrtblur <0-1000>
	Amount of horizontal blur for the CRT emulation.
-VICIIcrtscanlineshade <0-1000>
	Amount of scan line shading for the CRT emulation
-VICIIoddlinesphase <0-2000>
	Set phase for color carrier in odd lines
-VICIIoddlinesoffset <0-2000>
	Set phase offset for color carrier in odd lines
-VICIIcrtdelaylinetype <type>
	Set type of delay line used in the CRT (0: normal, 1: U only (1084 style)).
-VICIIborders <Mode>
	Set border display mode (0: normal, 1: full, 2: debug, 3: none)
-VICIIchecksb
	Enable sprite-background collision registers
+VICIIchecksb
	Disable sprite-background collision registers
-VICIIcheckss
	Enable sprite-sprite collision registers
+VICIIcheckss
	Disable sprite-sprite collision registers
-VICIIvspbug
	Enable VSP bug emulation
+VICIIvspbug
	Disable VSP bug emulation
-VICIImodel <Model>
	Set VIC-II model (6569/6569r1/8565/6567/8562/6567r56a/6572)
-sidenginemodel <engine and model>
	Specify SID engine and model (256: ReSID 6581, 257: ReSID 8580, 258: ReSID 8580 + digiboost)
-sidengine <engine>
	Specify SID engine (1: ReSID)
-sidmodel <model>
	Specify SID model (0: 6581, 1: 8580, 2: 8580 + digiboost)
-residsamp <method>
	reSID sampling method (0: fast, 1: interpolating, 2: resampling, 3: fast resampling)
-residpass <percent>
	reSID resampling passband in percentage of total bandwidth (0 - 90)
-residgain <percent>
	reSID gain in percent (90 - 100)
-residfilterbias <number>
	reSID filter bias setting, which can be used to adjust DAC bias in millivolts.
-resid8580pass <percent>
	reSID 8580 resampling passband in percentage of total bandwidth (0 - 90)
-resid8580gain <percent>
	reSID 8580 gain in percent (90 - 100)
-resid8580filterbias <number>
	reSID 8580 filter bias setting, which can be used to adjust DAC bias in millivolts.
-residrawoutput
	Enable writing raw reSID output to resid.raw, 16bit little endian data (WARNING: 1MiB per second).
+residrawoutput
	Disable writing raw reSID output to resid.raw.
-sidextra <amount>
	amount of extra SID chips. (0..3)
-sid2address <Base address>
	Specify base address for 2nd SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid3address <Base address>
	Specify base address for 3rd SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid4address <Base address>
	Specify base address for 4th SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid5address <Base address>
	Specify base address for 5th SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid6address <Base address>
	Specify base address for 6th SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid7address <Base address>
	Specify base address for 7th SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sid8address <Base address>
	Specify base address for 8th SID. (0xD420/0xD440/0xD460/0xD480/0xD4A0/0xD4C0/0xD4E0/0xD500/0xD520/0xD540/0xD560/0xD580/0xD5A0/0xD5C0/0xD5E0/0xD600/0xD620/0xD640/0xD660/0xD680/0xD6A0/0xD6C0/0xD6E0/0xD700/0xD720/0xD740/0xD760/0xD780/0xD7A0/0xD7C0/0xD7E0/0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-sidfilters
	Emulate SID filters
+sidfilters
	Do not emulate SID filters
-rsdev1 <Name>
	Specify name of first RS232 device
-rsdev2 <Name>
	Specify name of second RS232 device
-rsdev3 <Name>
	Specify name of third RS232 device
-rsdev4 <Name>
	Specify name of fourth RS232 device
-rsdev1ip232
	Enable IP232 protocol on first RS232 device
+rsdev1ip232
	Disable IP232 protocol on first RS232 device
-rsdev2ip232
	Enable IP232 protocol on second RS232 device
+rsdev2ip232
	Disable IP232 protocol on second RS232 device
-rsdev3ip232
	Enable IP232 protocol on third RS232 device
+rsdev3ip232
	Disable IP232 protocol on third RS232 device
-rsdev4ip232
	Enable IP232 protocol on fourth RS232 device
+rsdev4ip232
	Disable IP232 protocol on fourth RS232 device
-rsdev1baud <baudrate>
	Specify baudrate of first RS232 device
-rsdev2baud <baudrate>
	Specify baudrate of second RS232 device
-rsdev3baud <baudrate>
	Specify baudrate of third RS232 device
-rsdev4baud <baudrate>
	Specify baudrate of fourth RS232 device
-busdevice4
	Enable IEC/IEEE-488 device emulation for device #4
+busdevice4
	Disable IEC/IEEE-488 device emulation for device #4
-busdevice5
	Enable IEC/IEEE-488 device emulation for device #5
+busdevice5
	Disable IEC/IEEE-488 device emulation for device #5
-busdevice6
	Enable IEC/IEEE-488 device emulation for device #6
+busdevice6
	Disable IEC/IEEE-488 device emulation for device #6
-busdevice7
	Enable IEC/IEEE-488 device emulation for device #7
+busdevice7
	Disable IEC/IEEE-488 device emulation for device #7
-busdevice8
	Enable IEC/IEEE-488 device emulation for device #8
+busdevice8
	Disable IEC/IEEE-488 device emulation for device #8
-busdevice9
	Enable IEC/IEEE-488 device emulation for device #9
+busdevice9
	Disable IEC/IEEE-488 device emulation for device #9
-busdevice10
	Enable IEC/IEEE-488 device emulation for device #10
+busdevice10
	Disable IEC/IEEE-488 device emulation for device #10
-busdevice11
	Enable IEC/IEEE-488 device emulation for device #11
+busdevice11
	Disable IEC/IEEE-488 device emulation for device #11
-prusertxtdev <0-2>
	Specify printer text output device for userport printer
-prtxtdev1 <Name>
	Specify name of printer text device or dump file
-prtxtdev2 <Name>
	Specify name of printer text device or dump file
-prtxtdev3 <Name>
	Specify name of printer text device or dump file
-pr4txtdev <0-2>
	Specify printer text output device for printer #4
-pr5txtdev <0-2>
	Specify printer text output device for printer #5
-pr6txtdev <0-2>
	Specify printer text output device for printer #6
-pr4output <Name>
	Specify name of output device for device #4. (graphics, text)
-pr5output <Name>
	Specify name of output device for device #5. (graphics, text)
-pr6output <Name>
	Specify name of output device for device #6. (graphics, text)
-pr4drv <Name>
	Specify name of printer driver for device #4. (ascii/raw/mps801/mps802/mps803/220/4032/8023/nl10)
-pr5drv <Name>
	Specify name of printer driver for device #5. (ascii/raw/mps801/mps802/mps803/220/4032/8023/nl10)
-pr6drv <Name>
	Specify name of printer driver for device #6. (1520/raw)
-devicebackend4 <Type>
	Set device type for device #4 (0: None, 1: Filesystem, 2: Real)
-devicebackend5 <Type>
	Set device type for device #5 (0: None, 1: Filesystem, 2: Real)
-devicebackend6 <Type>
	Set device type for device #6 (0: None, 1: Filesystem, 2: Real)
-devicebackend7 <Type>
	Set device type for device #7 (0: None, 2: Real)
-controlport1device Device
	Set Control port 1 device (0: None, 1: Joystick, 2: Paddles, 3: Mouse (1351), 4: Mouse (NEOS), 5: Mouse (Amiga), 6: Trackball (Atari CX-22), 7: Mouse (Atari ST), 8: Mouse (SmartMouse), 9: Mouse (Micromys), 10: KoalaPad, 11: Light Pen (up trigger), 12: Light Pen (left trigger), 13: Light Pen (Datel), 14: Light Gun (Magnum Light Phaser), 15: Light Gun (Stack Light Rifle), 16: Light Pen (Inkwell), 18: Sampler (2bit), 19: Sampler (4bit), 20: RTC (BBRTC), 21: Dongle (Paperclip64), 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 24: Keypad (Atari CX85), 25: Keypad (RushWare), 26: Keypad (Atari CX21), 27: Dongle (Script 64), 28: Dongle (VizaWrite 64), 29: Dongle (WaaSoft), 30: SNES Pad Adapter (Trapthem), 31: SNES Pad Adapter (Ninja), 32: Joystick Adapter (Spaceballs), 33: Joystick Adapter (Inception), 34: Joystick Adapter (MultiJoy Joysticks), 35: Joystick Adapter (MultiJoy Logic), 36: Protopad, 37: Joyport I/O simulation, 38: Microflyte Joystick, 39: Dongle (Paperclip64E), 40: Dongle (Paperclip64SC), 41: Dongle (Paperclip2), 42: PowerPad)
-controlport2device Device
	Set Control port 2 device (0: None, 1: Joystick, 2: Paddles, 3: Mouse (1351), 4: Mouse (NEOS), 5: Mouse (Amiga), 6: Trackball (Atari CX-22), 7: Mouse (Atari ST), 8: Mouse (SmartMouse), 9: Mouse (Micromys), 10: KoalaPad, 18: Sampler (2bit), 19: Sampler (4bit), 20: RTC (BBRTC), 21: Dongle (Paperclip64), 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 24: Keypad (Atari CX85), 25: Keypad (RushWare), 26: Keypad (Atari CX21), 27: Dongle (Script 64), 28: Dongle (VizaWrite 64), 29: Dongle (WaaSoft), 30: SNES Pad Adapter (Trapthem), 31: SNES Pad Adapter (Ninja), 32: Joystick Adapter (Spaceballs), 33: Joystick Adapter (Inception), 34: Joystick Adapter (MultiJoy Joysticks), 35: Joystick Adapter (MultiJoy Logic), 36: Protopad, 37: Joyport I/O simulation, 38: Microflyte Joystick, 39: Dongle (Paperclip64E), 40: Dongle (Paperclip64SC), 41: Dongle (Paperclip2), 42: PowerPad)
-controlport3device Device
	Set Joystick adapter port 1 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport4device Device
	Set Joystick adapter port 2 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport5device Device
	Set Joystick adapter port 3 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport6device Device
	Set Joystick adapter port 4 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport7device Device
	Set Joystick adapter port 5 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport8device Device
	Set Joystick adapter port 6 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport9device Device
	Set Joystick adapter port 7 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-controlport10device Device
	Set Joystick adapter port 8 device (0: None, 1: Joystick, 22: Keypad (Coplin), 23: Keypad (Cardco Cardkey 1), 25: Keypad (RushWare))
-bbrtcsave
	Enable saving of the BBRTC data when changed.
+bbrtcsave
	Disable saving of the BBRTC data when changed.
-joyopposite
	Enable opposite joystick directions
+joyopposite
	Disable opposite joystick directions
-keyset
	Enable keyset
+keyset
	Disable keyset
-joydev1 <0-9>
	Set device for native joystick port 1 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-joystick1autofire
	Enable autofire for joystick/joypad in native joystick port 1
+joystick1autofire
	Disable autofire for joystick/joypad in native joystick port 1
-joystick1autofiremode <0-1>
	Set autofire mode for joystick/joypad in native joystick port 1 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-joystick1autofirespeed <1-255>
	Set autofire speed for joystick/joypad in native joystick port 1 (amount of fire button presses per second)
-joydev2 <0-9>
	Set device for native joystick port 2 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-joystick2autofire
	Enable autofire for joystick/joypad in native joystick port 2
+joystick2autofire
	Disable autofire for joystick/joypad in native joystick port 2
-joystick2autofiremode <0-1>
	Set autofire mode for joystick/joypad in native joystick port 2 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-joystick2autofirespeed <1-255>
	Set autofire speed for joystick/joypad in native joystick port 2 (amount of fire button presses per second)
-extrajoydev1 <0-9>
	Set device for joystick adapter port 1 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick1autofire
	Enable autofire for joystick/joypad in joystick adapter port 1
+extrajoystick1autofire
	Disable autofire for joystick/joypad in joystick adapter port 1
-extrajoystick1autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 1 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick1autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 1 (amount of fire button presses per second)
-extrajoydev2 <0-9>
	Set device for joystick adapter port 2 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick2autofire
	Enable autofire for joystick/joypad in joystick adapter port 2
+extrajoystick2autofire
	Disable autofire for joystick/joypad in joystick adapter port 2
-extrajoystick2autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 2 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick2autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 2 (amount of fire button presses per second)
-extrajoydev3 <0-9>
	Set device for joystick adapter port 3 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick3autofire
	Enable autofire for joystick/joypad in joystick adapter port 3
+extrajoystick3autofire
	Disable autofire for joystick/joypad in joystick adapter port 3
-extrajoystick3autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 3 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick3autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 3 (amount of fire button presses per second)
-extrajoydev4 <0-9>
	Set device for joystick adapter port 4 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick4autofire
	Enable autofire for joystick/joypad in joystick adapter port 4
+extrajoystick4autofire
	Disable autofire for joystick/joypad in joystick adapter port 4
-extrajoystick4autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 4 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick4autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 4 (amount of fire button presses per second)
-extrajoydev5 <0-9>
	Set device for joystick adapter port 5 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick5autofire
	Enable autofire for joystick/joypad in joystick adapter port 5
+extrajoystick5autofire
	Disable autofire for joystick/joypad in joystick adapter port 5
-extrajoystick5autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 5 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick5autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 5 (amount of fire button presses per second)
-extrajoydev6 <0-9>
	Set device for joystick adapter port 6 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick6autofire
	Enable autofire for joystick/joypad in joystick adapter port 6
+extrajoystick6autofire
	Disable autofire for joystick/joypad in joystick adapter port 6
-extrajoystick6autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 6 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick6autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 6 (amount of fire button presses per second)
-extrajoydev7 <0-9>
	Set device for joystick adapter port 7 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick7autofire
	Enable autofire for joystick/joypad in joystick adapter port 7
+extrajoystick7autofire
	Disable autofire for joystick/joypad in joystick adapter port 7
-extrajoystick7autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 7 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick7autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 7 (amount of fire button presses per second)
-extrajoydev8 <0-9>
	Set device for joystick adapter port 8 (0: None, 1: Numpad, 2: Keyset 1, 3: Keyset 2, 4: Analog joystick 0, 5: Analog joystick 1, 6: Analog joystick 2, 7: Analog joystick 3, 8: Analog joystick 4, 9: Analog joystick 5)
-extrajoystick8autofire
	Enable autofire for joystick/joypad in joystick adapter port 8
+extrajoystick8autofire
	Disable autofire for joystick/joypad in joystick adapter port 8
-extrajoystick8autofiremode <0-1>
	Set autofire mode for joystick/joypad in joystick adapter port 8 (0: Autofire when fire button is pressed, 1: Permanently autofire (pressing fire overrides autofire)
-extrajoystick8autofirespeed <1-255>
	Set autofire speed for joystick/joypad in joystick adapter port 8 (amount of fire button presses per second)
-userportdevice <device>
	Set userport device (0: None, 1: Userport printer, 2: Userport RS232/Modem, 3: CGA userport joy adapter, 4: PET userport joy adapter, 6: OEM userport joy adapter, 7: HIT userport joy adapter, 8: KingSoft userport joy adapter, 9: StarByte userport joy adapter, 11: WheelOfJoy userport joy adapter, 12: Userport DAC, 13: Userport DigiMAX, 14: Userport 4bit sampler, 15: Userport 8bit stereo sampler, 16: Userport RTC (RTC58321A), 17: Userport RTC (DS1307), 18: Userport Petscii SNES pad, 19: Userport SuperPad64, 21: Userport parallel drive cable, 22: Userport I/O Simulation, 23: Userport WiC64, 24: Joystick Adapter (Spaceballs), 25: Userport Stupid Pet Tricks joystick adapter)
-pruserdrv <Name>
	Specify name of printer driver for the userport printer. (ascii/nl10/raw)
-pruseroutput <Name>
	Specify name of output device for the userport printer. (graphics, text)
-rsuserup9600
	Enable UP9600 interface emulation.
+rsuserup9600
	Disable UP9600 interface emulation.
-rsuserbaud <baud>
	Set the baud rate of the RS232 userport emulation.
-rsuserdev <0-3>
	Specify VICE RS232 device for userport
-rsuserrtsinv
	Invert RS232 userport emulation RTS line
+rsuserrtsinv
	Do not invert RS232 userport emulation RTS line
-rsuserctsinv
	Invert RS232 userport emulation CTS line
+rsuserctsinv
	Do not invert RS232 userport emulation CTS line
-rsuserdsrinv
	Invert RS232 userport emulation DSR line
+rsuserdsrinv
	Do not invert RS232 userport emulation DSR line
-rsuserdcdinv
	Invert RS232 userport emulation DCD line
+rsuserdcdinv
	Do not invert RS232 userport emulation DCD line
-rsuserdtrinv
	Invert RS232 userport emulation DTR line
+rsuserdtrinv
	Do not invert RS232 userport emulation DTR line
-rsuserriinv
	Invert RS232 userport emulation RI line
+rsuserriinv
	Do not invert RS232 userport emulation RI line
-userportrtc58321asave
	Enable saving of the Userport RTC (58321a) data when changed.
+userportrtc58321asave
	Disable saving of the Userport RTC (58321a) data when changed.
-userportrtcds1307save
	Enable saving of the Userport RTC (DS1307) data when changed.
+userportrtcds1307save
	Disable saving of the Userport RTC (DS1307) data when changed.
-wic64server <URL>
	Specify default server URL
-wic64timezone <0..31>
	Specify default timezone index, e.g. 2: European Central Time
-wic64ipaddress <IP>
	Specify WiC64 IP
-wic64macaddress <MAC>
	Specify WiC64 MAC
-wic64token <token>
	Specify WiC64 security token
-wic64dhcp
	Enable WiC64 DHCP
+wic64DHCP
	Disable WiC64 DHCP
-wic64trace
	Enable WiC64 tracing
+wic64trace
	Disable WiC64 tracing
-wic64tracelevel <0..4>
	Set WiC64 tracing level (0: off, 1: cmd-level, >2: debug-level), implicitly turns on/off WiC64 tracing
-wic64reset
	Reset WiC64 to factory defaults
-wic64hexdumplines <value>
	Limit WiC64 hexdump lines (0: unlimited)
-wic64remotetimeout <value>
	Set WIC64 remote timeout (1 - 255)
-samplename <Name>
	Specify name of sample file
-samplerdev Device
	Specify sampler device. (0: media file input)
-samplergain <Sampler gain>
	Specify sampler gain in percent (1%-200%)
-flipname <Name>
	Specify name of the flip list file image
-devicebackend8 <Type>
	Set device type for device #8 (0: None, 1: Filesystem, 2: OpenCBM)
-devicebackend9 <Type>
	Set device type for device #9 (0: None, 1: Filesystem, 2: OpenCBM)
-devicebackend10 <Type>
	Set device type for device #10 (0: None, 1: Filesystem, 2: OpenCBM)
-devicebackend11 <Type>
	Set device type for device #11 (0: None, 1: Filesystem, 2: OpenCBM)
-attach8ro
	Attach disk image for drive #8:0 read only
-attach8rw
	Attach disk image for drive #8:0 read write (if possible)
-attach9ro
	Attach disk image for drive #9:0 read only
-attach9rw
	Attach disk image for drive #9:0 read write (if possible)
-attach10ro
	Attach disk image for drive #10:0 read only
-attach10rw
	Attach disk image for drive #10:0 read write (if possible)
-attach11ro
	Attach disk image for drive #11:0 read only
-attach11rw
	Attach disk image for drive #11:0 read write (if possible)
-attach8d1ro
	Attach disk image for drive #8:1 read only
-attach8d1rw
	Attach disk image for drive #8:1 read write (if possible)
-attach9d1ro
	Attach disk image for drive #9:1 read only
-attach9d1rw
	Attach disk image for drive #9:1 read write (if possible)
-attach10d1ro
	Attach disk image for drive #10:1 read only
-attach10d1rw
	Attach disk image for drive #10:1 read write (if possible)
-attach11d1ro
	Attach disk image for drive #11:1 read only
-attach11d1rw
	Attach disk image for drive #11:1 read write (if possible)
-fs8 <Name>
	Use <name> as directory for file system device #8
-fs9 <Name>
	Use <name> as directory for file system device #9
-fs10 <Name>
	Use <name> as directory for file system device #10
-fs11 <Name>
	Use <name> as directory for file system device #11
-fs8convertp00
	Enable on-read support for P00 files on drive 8
+fs8convertp00
	Disable on-read support for P00 files on drive 8
-fs9convertp00
	Enable on-read support for P00 files on drive 9
+fs9convertp00
	Disable on-read support for P00 files on drive 9
-fs10convertp00
	Enable on-read support for P00 files on drive 10
+fs10convertp00
	Disable on-read support for P00 files on drive 10
-fs11convertp00
	Enable on-read support for P00 files on drive 11
+fs11convertp00
	Disable on-read support for P00 files on drive 11
-fs8savep00
	Enable saving of P00 files on drive 8
+fs8savep00
	Disable saving of P00 files on drive 8
-fs9savep00
	Enable saving of P00 files on drive 9
+fs9savep00
	Disable saving of P00 files on drive 9
-fs10savep00
	Enable saving of P00 files on drive 10
+fs10savep00
	Disable saving of P00 files on drive 10
-fs11savep00
	Enable saving of P00 files on drive 11
+fs11savep00
	Disable saving of P00 files on drive 11
-fs8hidecbm
	Hide CBM files (only show P00 files) for drive 8
+fs8hidecbm
	Do not hide CBM files (show all files) for drive 8
-fs9hidecbm
	Hide CBM files (only show P00 files) for drive 9
+fs9hidecbm
	Do not hide CBM files (show all files) for drive 9
-fs10hidecbm
	Hide CBM files (only show P00 files) for drive 10
+fs10hidecbm
	Do not hide CBM files (show all files) for drive 10
-fs11hidecbm
	Hide CBM files (only show P00 files) for drive 11
+fs11hidecbm
	Do not hide CBM files (show all files) for drive 11
-fslongnames
	Allow filenames longer than 16 characters
+fslongnames
	Do not allow filenames longer than 16 characters
-fsoverwrite
	Overwrite files without using SAVE"@0:name"
+fsoverwrite
	Refuse to overwrite files without using SAVE"@0:name"
-playback
	Playback recorded events
-eventsnapshotdir <Name>
	Set event snapshot directory
-eventstartsnapshot <Name>
	Set event start snapshot
-eventendsnapshot <Name>
	Set event end snapshot
-eventstartmode <Mode>
	Set event start mode (0: file save, 1: file load, 2: reset, 3: playback)
-eventimageinc
	Enable including disk images
+eventimageinc
	Disable including disk images
-keybuf <string>
	Put the specified string into the keyboard buffer.
-keybuf-delay <value>
	Set additional keyboard buffer delay (0: use default)
-basicload
	On autostart from disk, load to BASIC start (without ',1')
+basicload
	On autostart from disk, load with ',1'
-tapebasicload
	On autostart from tape, load to BASIC start (without ',1')
+tapebasicload
	On autostart from tape, load with ',1'
-autostartwithcolon
	On autostart, use the 'RUN' command with a colon, i.e., 'RUN:'
+autostartwithcolon
	On autostart, do not use the 'RUN' command with a colon; i.e., 'RUN'
-autostart-handle-tde
	Handle True Drive Emulation on autostart
+autostart-handle-tde
	Do not handle True Drive Emulation on autostart
-autostart-warp
	Enable warp mode during autostart
+autostart-warp
	Disable warp mode during autostart
-autostartprgmode <Mode>
	Set autostart mode for PRG files (0: VirtualFS, 1: Inject, 2: Disk image)
-autostartprgdiskimage <Name>
	Set disk image for autostart of PRG files
-autostart-delay <seconds>
	Set initial autostart delay (0: use default)
-autostart-delay-random
	Enable random initial autostart delay.
+autostart-delay-random
	Disable random initial autostart delay.
-autostarttapoffset <value>
	Set initial offset in .tap file
-autostart-drop-mode <Mode>
	Set autostart drop mode (0/attach: attach only, 1/load: attach and load, 2/run: attach, load and run)
-netplayserver <hostname>
	Set the netplay server hostname
-netplaybind <hostname>
	Set the netplay binding address
-netplayport <port>
	Set the netplay port (0..65535)
-netplayctrl <key,joy1,joy2,dev,rsrc>
	Set the netplay control elements (keyboard, joystick1, joystick2, devices and resources), each item takes a value (0: None, 1: Server, 2: Client, 3: Both)
-mouse
	Enable mouse grab
+mouse
	Disable mouse grab
-paddles1inputmouse
	Use host mouse as input for paddles in port 1.
-paddles1inputjoyaxis
	Use host joystick axis as input for paddles in port 1.
-paddles2inputmouse
	Use host mouse as input for paddles in port 2.
-paddles2inputjoyaxis
	Use host joystick axis as input for paddles in port 2.
-smartmousertcsave
	Enable saving of smart mouse RTC data when changed.
+smartmousertcsave
	Disable saving of smart mouse RTC data when changed.
-drive8rtcsave
	Enable saving of FD2000/4000 RTC data when changed.
+drive8rtcsave
	Disable saving of FD2000/4000 RTC data when changed.
-drive8type <Type>
	Set drive type (0: no drive, 1540: CBM 1540, 1541: CBM 1541, 1542: CBM 1541-II, 1570: CBM 1570, 1571: CBM 1571, 1581: CBM 1581, 2000: CMD FD-2000, 4000: CMD FD-4000, 4844: CMD HD, 2031: CBM 2031, 2040: CBM 2040, 3040: CBM 3040, 4040: CBM 4040, 1001: CBM 1001, 8050: CBM 8050, 8250: CBM 8250, 9000: CBM D9090/D9060)
-drive8extend <method>
	Set drive 40 track extension policy (0: never, 1: ask, 2: on access)
-drive8idle <method>
	Set drive idling method (0: no traps, 1: skip cycles, 2: trap idle)
-drive8rpm <RPM>
	Set drive rpm (30000 = 300rpm)
-drive8wobblefrequency <Frequency>
	Set drive wobble frequency
-drive8wobbleamplitude <Amplitude>
	Set drive wobble amplitude
-drive8truedrive
	Enable hardware-level emulation of disk drive
+drive8truedrive
	Disable hardware-level emulation of disk drive
-drive9rtcsave
	Enable saving of FD2000/4000 RTC data when changed.
+drive9rtcsave
	Disable saving of FD2000/4000 RTC data when changed.
-drive9type <Type>
	Set drive type (0: no drive, 1540: CBM 1540, 1541: CBM 1541, 1542: CBM 1541-II, 1570: CBM 1570, 1571: CBM 1571, 1581: CBM 1581, 2000: CMD FD-2000, 4000: CMD FD-4000, 4844: CMD HD, 2031: CBM 2031, 2040: CBM 2040, 3040: CBM 3040, 4040: CBM 4040, 1001: CBM 1001, 8050: CBM 8050, 8250: CBM 8250, 9000: CBM D9090/D9060)
-drive9extend <method>
	Set drive 40 track extension policy (0: never, 1: ask, 2: on access)
-drive9idle <method>
	Set drive idling method (0: no traps, 1: skip cycles, 2: trap idle)
-drive9rpm <RPM>
	Set drive rpm (30000 = 300rpm)
-drive9wobblefrequency <Frequency>
	Set drive wobble frequency
-drive9wobbleamplitude <Amplitude>
	Set drive wobble amplitude
-drive9truedrive
	Enable hardware-level emulation of disk drive
+drive9truedrive
	Disable hardware-level emulation of disk drive
-drive10rtcsave
	Enable saving of FD2000/4000 RTC data when changed.
+drive10rtcsave
	Disable saving of FD2000/4000 RTC data when changed.
-drive10type <Type>
	Set drive type (0: no drive, 1540: CBM 1540, 1541: CBM 1541, 1542: CBM 1541-II, 1570: CBM 1570, 1571: CBM 1571, 1581: CBM 1581, 2000: CMD FD-2000, 4000: CMD FD-4000, 4844: CMD HD, 2031: CBM 2031, 2040: CBM 2040, 3040: CBM 3040, 4040: CBM 4040, 1001: CBM 1001, 8050: CBM 8050, 8250: CBM 8250, 9000: CBM D9090/D9060)
-drive10extend <method>
	Set drive 40 track extension policy (0: never, 1: ask, 2: on access)
-drive10idle <method>
	Set drive idling method (0: no traps, 1: skip cycles, 2: trap idle)
-drive10rpm <RPM>
	Set drive rpm (30000 = 300rpm)
-drive10wobblefrequency <Frequency>
	Set drive wobble frequency
-drive10wobbleamplitude <Amplitude>
	Set drive wobble amplitude
-drive10truedrive
	Enable hardware-level emulation of disk drive
+drive10truedrive
	Disable hardware-level emulation of disk drive
-drive11rtcsave
	Enable saving of FD2000/4000 RTC data when changed.
+drive11rtcsave
	Disable saving of FD2000/4000 RTC data when changed.
-drive11type <Type>
	Set drive type (0: no drive, 1540: CBM 1540, 1541: CBM 1541, 1542: CBM 1541-II, 1570: CBM 1570, 1571: CBM 1571, 1581: CBM 1581, 2000: CMD FD-2000, 4000: CMD FD-4000, 4844: CMD HD, 2031: CBM 2031, 2040: CBM 2040, 3040: CBM 3040, 4040: CBM 4040, 1001: CBM 1001, 8050: CBM 8050, 8250: CBM 8250, 9000: CBM D9090/D9060)
-drive11extend <method>
	Set drive 40 track extension policy (0: never, 1: ask, 2: on access)
-drive11idle <method>
	Set drive idling method (0: no traps, 1: skip cycles, 2: trap idle)
-drive11rpm <RPM>
	Set drive rpm (30000 = 300rpm)
-drive11wobblefrequency <Frequency>
	Set drive wobble frequency
-drive11wobbleamplitude <Amplitude>
	Set drive wobble amplitude
-drive11truedrive
	Enable hardware-level emulation of disk drive
+drive11truedrive
	Disable hardware-level emulation of disk drive
-drivesound
	Enable sound emulation of disk drives
+drivesound
	Disable sound emulation of disk drives
-drivesoundvolume <Volume>
	Set volume for disk drive sound emulation (0-4000)
-drive8ram2000
	Enable 8KiB RAM expansion at $2000-$3FFF
+drive8ram2000
	Disable 8KiB RAM expansion at $2000-$3FFF
-drive8ram4000
	Enable 8KiB RAM expansion at $4000-$5FFF
+drive8ram4000
	Disable 8KiB RAM expansion at $4000-$5FFF
-drive8ram6000
	Enable 8KiB RAM expansion at $6000-$7FFF
+drive8ram6000
	Disable 8KiB RAM expansion at $6000-$7FFF
-drive8ram8000
	Enable 8KiB RAM expansion at $8000-$9FFF
+drive8ram8000
	Disable 8KiB RAM expansion at $8000-$9FFF
-drive8rama000
	Enable 8KiB RAM expansion at $A000-$BFFF
+drive8rama000
	Disable 8KiB RAM expansion at $A000-$BFFF
-drive8fixedsize <Size>
	Fixed Disk Size
-drive9ram2000
	Enable 8KiB RAM expansion at $2000-$3FFF
+drive9ram2000
	Disable 8KiB RAM expansion at $2000-$3FFF
-drive9ram4000
	Enable 8KiB RAM expansion at $4000-$5FFF
+drive9ram4000
	Disable 8KiB RAM expansion at $4000-$5FFF
-drive9ram6000
	Enable 8KiB RAM expansion at $6000-$7FFF
+drive9ram6000
	Disable 8KiB RAM expansion at $6000-$7FFF
-drive9ram8000
	Enable 8KiB RAM expansion at $8000-$9FFF
+drive9ram8000
	Disable 8KiB RAM expansion at $8000-$9FFF
-drive9rama000
	Enable 8KiB RAM expansion at $A000-$BFFF
+drive9rama000
	Disable 8KiB RAM expansion at $A000-$BFFF
-drive9fixedsize <Size>
	Fixed Disk Size
-drive10ram2000
	Enable 8KiB RAM expansion at $2000-$3FFF
+drive10ram2000
	Disable 8KiB RAM expansion at $2000-$3FFF
-drive10ram4000
	Enable 8KiB RAM expansion at $4000-$5FFF
+drive10ram4000
	Disable 8KiB RAM expansion at $4000-$5FFF
-drive10ram6000
	Enable 8KiB RAM expansion at $6000-$7FFF
+drive10ram6000
	Disable 8KiB RAM expansion at $6000-$7FFF
-drive10ram8000
	Enable 8KiB RAM expansion at $8000-$9FFF
+drive10ram8000
	Disable 8KiB RAM expansion at $8000-$9FFF
-drive10rama000
	Enable 8KiB RAM expansion at $A000-$BFFF
+drive10rama000
	Disable 8KiB RAM expansion at $A000-$BFFF
-drive10fixedsize <Size>
	Fixed Disk Size
-drive11ram2000
	Enable 8KiB RAM expansion at $2000-$3FFF
+drive11ram2000
	Disable 8KiB RAM expansion at $2000-$3FFF
-drive11ram4000
	Enable 8KiB RAM expansion at $4000-$5FFF
+drive11ram4000
	Disable 8KiB RAM expansion at $4000-$5FFF
-drive11ram6000
	Enable 8KiB RAM expansion at $6000-$7FFF
+drive11ram6000
	Disable 8KiB RAM expansion at $6000-$7FFF
-drive11ram8000
	Enable 8KiB RAM expansion at $8000-$9FFF
+drive11ram8000
	Disable 8KiB RAM expansion at $8000-$9FFF
-drive11rama000
	Enable 8KiB RAM expansion at $A000-$BFFF
+drive11rama000
	Disable 8KiB RAM expansion at $A000-$BFFF
-drive11fixedsize <Size>
	Fixed Disk Size
-dos1540 <Name>
	Specify name of 1540 DOS ROM image
-dos1541 <Name>
	Specify name of 1541 DOS ROM image
-dos1541II <Name>
	Specify name of 1541-II DOS ROM image
-dos1570 <Name>
	Specify name of 1570 DOS ROM image
-dos1571 <Name>
	Specify name of 1571 DOS ROM image
-dos1581 <Name>
	Specify name of 1581 DOS ROM image
-dos2000 <Name>
	Specify name of 2000 DOS ROM image
-dos4000 <Name>
	Specify name of 4000 DOS ROM image
-dosCMDHD <Name>
	Specify name of CMD HD Boot ROM image
-parallel8 <Type>
	Set parallel cable type (0: none, 1: standard, 2: Dolphin DOS 3, 3: Formel 64)
-drive8profdos
	Enable Professional DOS
+drive8profdos
	Disable Professional DOS
-drive8supercard
	Enable Supercard+
+drive8supercard
	Disable Supercard+
-drive8stardos
	Enable StarDOS
+drive8stardos
	Disable StarDOS
-drive8dd3
	Enable DolphinDOS3
+drive8dd3
	Disable DolphinDOS3
-parallel9 <Type>
	Set parallel cable type (0: none, 1: standard, 2: Dolphin DOS 3, 3: Formel 64)
-drive9profdos
	Enable Professional DOS
+drive9profdos
	Disable Professional DOS
-drive9supercard
	Enable Supercard+
+drive9supercard
	Disable Supercard+
-drive9stardos
	Enable StarDOS
+drive9stardos
	Disable StarDOS
-drive9dd3
	Enable DolphinDOS3
+drive9dd3
	Disable DolphinDOS3
-parallel10 <Type>
	Set parallel cable type (0: none, 1: standard, 2: Dolphin DOS 3, 3: Formel 64)
-drive10profdos
	Enable Professional DOS
+drive10profdos
	Disable Professional DOS
-drive10supercard
	Enable Supercard+
+drive10supercard
	Disable Supercard+
-drive10stardos
	Enable StarDOS
+drive10stardos
	Disable StarDOS
-drive10dd3
	Enable DolphinDOS3
+drive10dd3
	Disable DolphinDOS3
-parallel11 <Type>
	Set parallel cable type (0: none, 1: standard, 2: Dolphin DOS 3, 3: Formel 64)
-drive11profdos
	Enable Professional DOS
+drive11profdos
	Disable Professional DOS
-drive11supercard
	Enable Supercard+
+drive11supercard
	Disable Supercard+
-drive11stardos
	Enable StarDOS
+drive11stardos
	Disable StarDOS
-drive11dd3
	Enable DolphinDOS3
+drive11dd3
	Disable DolphinDOS3
-profdos1571 <Name>
	Specify name of Professional DOS 1571 ROM image
-supercard <Name>
	Specify name of Supercard+ ROM image
-stardos <Name>
	Specify name of StarDOS ROM image
-dos2031 <Name>
	Specify name of 2031 DOS ROM image
-dos2040 <Name>
	Specify name of 2040 DOS ROM image
-dos3040 <Name>
	Specify name of 3040 DOS ROM image
-dos4040 <Name>
	Specify name of 4040 DOS ROM image
-dos1001 <Name>
	Specify name of 1001/8050/8250 DOS ROM image
-dos9000 <Name>
	Specify name of D9090/D9060 DOS ROM image
-tapeport1device Device
	Set Tapeport 0 device (0: None, 1: Datasette, 2: Tape RTC (PCF8583), 4: Sense dongle, 6: tapecart)
-cpclockf83save
	Enable saving of the CP Clock F83 (PCF8583 RTC) data when changed.
+cpclockf83save
	Disable saving of the CP Clock F83 (PCF8583 RTC) data when changed.
-tcrt <Name>
	Attach TCRT tapecart image
-tapecartupdatetcrt
	Enable updating tapecart .tcrt image
+tapecartupdatetcrt
	Disable updating tapecart .tcrt image
-tapecartoptimizetcrt
	Enable tapecart .tcrt image optimization on write
+tapecartoptimizetcrt
	Disable tapecart .tcrt image optimization on write
-tapecartloglevel <Loglevel>
	Set tapecart log verbosity
-dsresetwithcpu
	Enable automatic Datasette-Reset
+dsresetwithcpu
	Disable automatic Datasette-Reset
-dszerogapdelay <value>
	Set delay in cycles for a zero in a v0 tap file
-dsspeedtuning <value>
	Set constant deviation from correct motor speed
-dstapewobblefreq <value>
	Set tape wobble frequency
-dstapewobbleamp <value>
	Set tape wobble amplitude
-dstapeerror <value>
	Set amount of azimuth error (misalignment)
-datasettesound
	Enable Datasette sound
+datasettesound
	Disable Datasette sound
-dssoundvolume <value>
	Set volume of Datasette sound
-gluelogictype <Type>
	Set glue logic type (0 = discrete, 1 = 252535-01)
-iocollision <method>
	Select the way the I/O collisions should be handled, (0: error message and detach all involved carts, 1: error message and detach last attached involved carts, 2: warning in log and 'AND' the valid return values
-mmc64
	Enable the MMC64 expansion
+mmc64
	Disable the MMC64 expansion
-mmc64bios <Name>
	Specify name of MMC64 BIOS image
-mmc64image <Name>
	Specify name of MMC64 image
-mmc64readonly
	Set the MMC64 card to read-only
-mmc64readwrite
	Set the MMC64 card to read/write
-mmc64bioswrite
	Save the MMC64 bios when changed
-mmc64biosreadonly
	Do not save the MMC64 bios when changed
-mmc64flash
	Set the MMC64 Flash Jumper
+mmc64flash
	Remove the MMC64 Flash Jumper
-mmc64rev <Revision>
	Specify MMC64 revision (0: Rev A, 1: Rev B)
-mmc64sdtype <Type>
	Specify MMC64 SD type (0: auto, 1: MMC, 2: SD, 3: SDHC)
-mmc64clockportdevice <device>
	Clockport device. (0: None, 2: RRNet, 3: RRNet MK3)
-magicvoiceimage <Name>
	Specify name of Magic Voice ROM image
-magicvoice
	Enable the Magic Voice cartridge
+magicvoice
	Disable the Magic Voice cartridge
-ieee488
	Enable the IEEE488 interface emulation
+ieee488
	Disable the IEEE488 interface emulation
-ieee488image <Name>
	specify IEEE488 interface image name
-ramlink
	Enable the RAMLink Unit
+ramlink
	Disable the RAMLink Unit
-ramlinkbios <Name>
	Specify name of RAMLink BIOS image
-ramlinkmode <Mode>
	RAMPort Mode (1=Normal, 0=Direct)
-ramlinkrtcsave
	Enable saving of the RTC data when changed.
+ramlinkrtcsave
	Disable saving of the RTC data when changed.
-ramlinksize <value>
	RAMLink RAMCARD size in MiB (0=Disabled)
-ramlinkimage <Name>
	Specify name of RAMLink image
-ramlinkimagerw
	Allow writing to RAMLink image
+ramlinkimagerw
	Do not write to RAMLink image
-dqbb
	Enable Double Quick Brown Box
+dqbb
	Disable Double Quick Brown Box
-dqbbsize <Size>
	Set Double Quick Brown Box RAM size (16/32/64/128/256kiB)
-dqbbmode <Mode>
	Set Double Quick Brown Box mode switch (0: C128, 1:C64)
-dqbbimage <Name>
	Specify Double Quick Brown Box filename
-dqbbimagerw
	Allow writing to DQBB image
+dqbbimagerw
	Do not write to DQBB image
-expert
	Enable the Expert Cartridge
+expert
	Disable the Expert Cartridge
-expertimagename <Name>
	Set Expert Cartridge image name
-expertimagerw
	Allow writing to Expert Cartridge image
+expertimagerw
	Do not write to Expert Cartridge image
-expertmode <Mode>
	Set Expert Cartridge mode (0: Off, 1: Prg, 2: On)
-isepic
	Enable the ISEPIC cartridge
+isepic
	Disable the ISEPIC cartridge
-isepicimagename <Name>
	Set ISEPIC image name
-isepicimagerw
	Allow writing to ISEPIC image
+isepicimagerw
	Do not write to ISEPIC image
-isepicswitch
	Enable the ISEPIC switch
+isepicswitch
	Disable the ISEPIC switch
-ramcart
	Enable the RamCart expansion
+ramcart
	Disable the RamCart expansion
-ramcartsize <size in KiB>
	Size of the RAMCART expansion. (64/128)
-ramcartimage <Name>
	Specify name of RAMCART image
-ramcartimagerw
	Allow writing to RAMCart image
+ramcartimagerw
	Do not write to RAMCart image
-ramcartro
	Set the RAMCart switch to read-only
-ramcartrw
	Set the RAMCart switch to read/write
-miditype <0-4>
	MIDI interface type (0: Sequential, 1: Passport, 2: DATEL, 3: Namesoft, 4: Maplin)
-midi
	Enable MIDI emulation
+midi
	Disable MIDI emulation
-midiname <Name>
	Name of MIDI Client
-midiinname <Name>
	Name of MIDI-In Port
-midioutname <Name>
	Name of MIDI-Out Port
-acia1base <Base address>
	Set the base address of the ACIA cartridge. (0xDE00, 0xDF00)
-acia1irq <interrupt>
	Set the ACIA interrupt (0: None, 1: NMI, 2: IRQ)
-acia1mode <Mode>
	Set the ACIA mode (0: Normal, 1: Swiftlink, 2: Turbo232)
-myaciadev <0-3>
	Specify RS232 device this ACIA should work on
-digimax
	Enable the DigiMAX cartridge
+digimax
	Disable the DigiMAX cartridge
-digimaxbase <Base address>
	Base address of the DigiMAX cartridge. (0xDE00/0xDE20/0xDE40/0xDE60/0xDE80/0xDEA0/0xDEC0/0xDEE0/0xDF00/0xDF20/0xDF40/0xDF60/0xDF80/0xDFA0/0xDFC0/0xDFE0)
-ds12c887rtc
	Enable the DS12C887 RTC cartridge
+ds12c887rtc
	Disable the DS12C887 RTC cartridge
-ds12c887rtchalted
	Set the RTC oscillator to 'halted'
-ds12c887rtcrunning
	Set the RTC oscillator to 'running'
-ds12c887rtcsave
	Enable saving of the DS12C887 RTC data when changed.
+ds12c887rtcsave
	Disable saving of the DS12C887 RTC data when changed.
-ds12c887rtcbase <Base address>
	Base address of the DS12C887 RTC cartridge. (0xD500/0xD600/0xD700/0xDE00/0xDF00)
-georam
	Enable the GEO-RAM expansion unit
+georam
	Disable the GEO-RAM expansion unit
-georamsize <size in KiB>
	Size of the GEORAM expansion unit
-georamimage <Name>
	Specify name of GEORAM image
-georamimagerw
	Allow writing to GEORAM image
+georamimagerw
	Do not write to GEORAM image
-reu
	Enable the RAM Expansion Unit
+reu
	Disable the RAM Expansion Unit
-reusize <size in KiB>
	Size of the RAM expansion unit. (128/256/512/1024/2048/4096/8192/16384)
-reuimage <Name>
	Specify name of REU image
-reuimagerw
	Allow writing to REU image
+reuimagerw
	Do not write to REU image
-sfxse
	Enable the SFX Sound Expander cartridge
+sfxse
	Disable the SFX Sound Expander cartridge
-sfxsetype <Type>
	Set YM chip type (3526 / 3812)
-sfxss
	Enable the SFX Sound Sampler cartridge
+sfxss
	Disable the SFX Sound Sampler cartridge
-ethernetiodriver <Name>
	Set the low-level driver for Ethernet emulation (tuntap, pcap).
-ethernetioif <Name>
	Set the system ethernet interface
-ethernetcart
	Enable the Ethernet Cartridge (TFE/RR-Net/64NIC/FB-NET)
+ethernetcart
	Disable the Ethernet Cartridge (TFE/RR-Net/64NIC/FB-NET)
-tfe
	Enable the Ethernet Cartridge in TFE ("The Final Ethernet") compatible mode and set default I/O address
-rrnet
	Enable the Ethernet Cartridge in RR-Net compatible mode and set default I/O address
-ethernetcartmode <Mode>
	Mode of Ethernet Cartridge (0: TFE, 1: RR-Net)
-ethernetcartbase <Base address>
	Base address of the Ethernet Cartridge. (0xDE00/0xDE10/0xDE20/0xDE30/0xDE40/0xDE50/0xDE60/0xDE70/0xDE80/0xDE90/0xDEA0/0xDEB0/0xDEC0/0xDED0/0xDEE0/0xDEF0/0xDF00/0xDF10/0xDF20/0xDF30/0xDF40/0xDF50/0xDF60/0xDF70/0xDF80/0xDF90/0xDFA0/0xDFB0/0xDFC0/0xDFD0/0xDFE0/0xDFF0)
-comal80rev <Revision>
	Set Comal 80 Revision (0: Grey, 1: Commodore/Black)
-easyflashjumper
	Enable EasyFlash jumper
+easyflashjumper
	Disable EasyFlash jumper
-easyflashcrtwrite
	Enable writing to EasyFlash .crt image
+easyflashcrtwrite
	Disable writing to EasyFlash .crt image
-easyflashcrtoptimize
	Enable EasyFlash .crt image optimize on write
+easyflashcrtoptimize
	Disable writing to EasyFlash .crt image
-gmod2eepromimage <filename>
	Specify GMod2 EEPROM image filename
-gmod2eepromrw
	Enable writes to GMod2 EEPROM image
+gmod2eepromrw
	Disable writes to GMod2 EEPROM image
-gmod2flashwrite
	Enable saving of the GMod2 ROM at exit
+gmod2flashwrite
	Disable saving of the GMod2 ROM at exit
-gmod3flashwrite
	Enable saving of the GMod3 ROM at exit
+gmod3flashwrite
	Disable saving of the GMod3 ROM at exit
-sbdigimax
	Enable the Short Bus DigiMAX expansion
+sbdigimax
	Disable the Short Bus DigiMAX expansion
-sbdigimaxbase <Base address>
	Base address of the Short Bus DigiMAX expansion. (0xDE40/0xDE48)
-sbetfe
	Enable the Short Bus ETFE expansion
+sbetfe
	Disable the Short Bus ETFE expansion
-sbetfebase <Base address>
	Base address of the Short Bus ETFE expansion. (56832: $de00, 56848: $de10, 57088: $df00)
-IDE64image1 <Name>
	Specify name of IDE64 image file
-IDE64image2 <Name>
	Specify name of IDE64 image file
-IDE64image3 <Name>
	Specify name of IDE64 image file
-IDE64image4 <Name>
	Specify name of IDE64 image file
-IDE64cyl1 <value>
	Set number of cylinders for the IDE64 emulation. (1..65535)
-IDE64cyl2 <value>
	Set number of cylinders for the IDE64 emulation. (1..65535)
-IDE64cyl3 <value>
	Set number of cylinders for the IDE64 emulation. (1..65535)
-IDE64cyl4 <value>
	Set number of cylinders for the IDE64 emulation. (1..65535)
-IDE64hds1 <value>
	Set number of heads for the IDE64 emulation. (1..16)
-IDE64hds2 <value>
	Set number of heads for the IDE64 emulation. (1..16)
-IDE64hds3 <value>
	Set number of heads for the IDE64 emulation. (1..16)
-IDE64hds4 <value>
	Set number of heads for the IDE64 emulation. (1..16)
-IDE64sec1 <value>
	Set number of sectors for the IDE64 emulation. (1..63)
-IDE64sec2 <value>
	Set number of sectors for the IDE64 emulation. (1..63)
-IDE64sec3 <value>
	Set number of sectors for the IDE64 emulation. (1..63)
-IDE64sec4 <value>
	Set number of sectors for the IDE64 emulation. (1..63)
-IDE64autosize1
	Autodetect image size
+IDE64autosize1
	Do not autodetect geometry of formatted images
-IDE64autosize2
	Autodetect image size
+IDE64autosize2
	Do not autodetect geometry of formatted images
-IDE64autosize3
	Autodetect image size
+IDE64autosize3
	Do not autodetect geometry of formatted images
-IDE64autosize4
	Autodetect image size
+IDE64autosize4
	Do not autodetect geometry of formatted images
-IDE64version <value>
	IDE64 cartridge version
-IDE64USB
	Enable IDE64 USB server
+IDE64USB
	Disable IDE64 USB server
-IDE64USBAddress <Name>
	IDE64 USB server address
-IDE64rtcsave
	Enable saving of IDE64 RTC data when changed.
+IDE64rtcsave
	Disable saving of IDE64 RTC data when changed.
-ide64clockportdevice <device>
	Clockport device. (0: None, 2: RRNet, 3: RRNet MK3)
-ieeeflash64
	Enable IEEE Flash! 64
+ieeeflash64
	Disable IEEE Flash! 64
-ieeeflash64dev8
	Route device 8 to IEC bus
+ieeeflash64dev8
	Route device 8 to IEEE bus
-ieeeflash64dev910
	Route devices 9 and 10 to IEC bus
+ieeeflash64dev910
	Route devices 9 and 10 to IEEE bus
-ieeeflash64dev4
	Route device 4 to IEC bus
+ieeeflash64dev4
	Route device 4 to IEEE bus
-ieeeflash64image <Name>
	specify ieeeflash64 interface kernal image name
-ltkimage0 <Name>
	Specify name of LTK image file 0
-ltkimage1 <Name>
	Specify name of LTK image file 1
-ltkimage2 <Name>
	Specify name of LTK image file 2
-ltkimage3 <Name>
	Specify name of LTK image file 3
-ltkimage4 <Name>
	Specify name of LTK image file 4
-ltkimage5 <Name>
	Specify name of LTK image file 5
-ltkimage6 <Name>
	Specify name of LTK image file 6
-ltkserial <Name>
	Specify LTK serial number (XXXXXXXX) to override the ROM
-ltkport <value>
	Set LTK port number (0..15)
-ltkio <value>
	Set LTK IO page (0=$DExx, 1=$DFxx=default)
-megabytercrtwrite
	Enable writing to PTV Megabyter .crt image
+megabytercrtwrite
	Disable writing to PTV Megabyter .crt image
-megabytercrtoptimize
	Enable PTV Megabyter .crt image optimize on write
+megabytercrtoptimize
	Disable optimizing PTV Megabyter .crt image on write
-mmcrrescue
	Enable MMC Replay rescue mode
+mmcrrescue
	Disable MMC Replay rescue mode
-mmcrimagerw
	Allow writing to MMC Replay image
+mmcrimagerw
	Do not write to MMC Replay image
-mmcrcardimage <filename>
	Specify MMC Replay card image filename
-mmcrcardrw
	Enable writes to MMC Replay card image
+mmcrcardrw
	Disable writes to MMC Replay card image
-mmcreepromimage <filename>
	Specify MMC Replay EEPROM image filename
-mmcreepromrw
	Enable writes to MMC Replay EEPROM image
+mmcreepromrw
	Disable writes to MMC Replay EEPROM image
-mmcrsdtype <Type>
	Specify MMC Replay SD type (0: auto, 1: MMC, 2: SD, 3: SDHC)
-mmcrclockportdevice <device>
	Clockport device. (0: None, 2: RRNet, 3: RRNet MK3)
-rrbioswrite
	Enable saving of the RR ROM at exit
+rrbioswrite
	Disable saving of the RR ROM at exit
-rrbankjumper
	Set RR Bank Jumper
+rrbankjumper
	Unset RR Bank Jumper
-rrflashjumper
	Set RR Flash Jumper
+rrflashjumper
	Unset RR Bank Jumper
-rrrev <Revision>
	Set RR Revision (0: Retro Replay, 1: Nordic Replay)
-rrclockportdevice <device>
	Clockport device. (0: None, 2: RRNet, 3: RRNet MK3)
-rexramfloppyimage <Name>
	Specify REX RAM-Floppy filename
-rexramfloppyimagerw
	Allow writing to REX RAM-Floppy image
+rexramfloppyimagerw
	Do not write to REX RAM-Floppy image
-rgcdrev <Revision>
	Set RGCD Revision (0: RGCD 64K, 1: Hucky)
-rrnetmk3bioswrite
	Save the RRNETMK3 bios when changed
+rrnetmk3bioswrite
	Do not save the RRNETMK3 bios when changed
-rrnetmk3flash
	Set the RRNETMK3 Flash Jumper
+rrnetmk3flash
	Remove the RRNETMK3 Flash Jumper
-ssramexpansion
	Enable SS 32KiB RAM expansion
+ssramexpansion
	Disable SS 32KiB RAM expansion
-debugcart
	Enable Debug cartridge
+debugcart
	Disable Debug cartridge
-cpmcart
	Enable the CP/M cartridge
+cpmcart
	Disable the CP/M cartridge
-cart8 <Name>
	Attach raw 8KiB cartridge image
-cart16 <Name>
	Attach raw 16KiB cartridge image
-cartultimax <Name>
	Attach generic 16KiB Ultimax cartridge image
-cartcrt <Name>
	Attach CRT cartridge image
-cartap <Name>
	Attach raw 32KiB Atomic Power cartridge image
-cartar2 <Name>
	Attach raw 16KiB Action Replay MK2 cartridge image
-cartar3 <Name>
	Attach raw 16KiB Action Replay MK3 cartridge image
-cartar4 <Name>
	Attach raw 32KiB Action Replay MK4 cartridge image
-cartar5 <Name>
	Attach raw 32KiB Action Replay cartridge image
-cartbb3 <Name>
	Attach raw 8KiB Blackbox V3 cartridge image
-cartbb4 <Name>
	Attach raw 16KiB Blackbox V4 cartridge image
-cartbb8 <Name>
	Attach raw 32/64KiB Blackbox V8 cartridge image
-cartbb9 <Name>
	Attach raw 32KiB Blackbox V9 cartridge image
-cartbdt <Name>
	Attach raw 16KiB BMP Data Turbo 2000 cartridge image
-cartbis <Name>
	Attach raw 2/4/8KiB BIS-Plus cartridge image
-cartcap <Name>
	Attach raw 8KiB Capture cartridge image
-cartcomal <Name>
	Attach raw 64KiB Comal 80 cartridge image
-cartdep256 <Name>
	Attach raw Dela EP256 cartridge image
-cartdep64 <Name>
	Attach raw Dela EP64 cartridge image
-cartdep7x8 <Name>
	Attach raw Dela EP7x8 cartridge image
-cartdin <Name>
	Attach raw 128KiB Dinamic cartridge image
-cartdsm <Name>
	Attach raw 8KiB Diashow-Maker cartridge image
-cartdqbb <Name>
	Attach raw 16KiB Double Quick Brown Box cartridge image
-cartdrean <Name>
	Attach raw 32KiB Drean cartridge image
-carteasy <Name>
	Attach raw EasyFlash cartridge image
-carteasycalc <Name>
	Attach raw 24KiB Easy Calc Result cartridge image
-cartepyx <Name>
	Attach raw 8KiB Epyx FastLoad cartridge image
-cartexos <Name>
	Attach raw 8KiB EXOS cartridge image
-cartexpert <Name>
	Attach raw 8KiB Expert Cartridge image
-cartf64 <Name>
	Attach raw 32KiB Formel 64 image
-cartfc1 <Name>
	Attach raw 16KiB Final Cartridge image
-cartfc3 <Name>
	Attach raw 64KiB Final Cartridge III image
-cartfcplus <Name>
	Attach raw 32KiB Final Cartridge Plus image
-cartff <Name>
	Attach raw 8KiB Freeze Frame image
-cartff2 <Name>
	Attach raw 16KiB Freeze Frame MK2/MK3 image
-cartfm <Name>
	Attach raw 32KiB Freeze Machine image
-cartfp <Name>
	Attach raw 128KiB Fun Play/Power Play cartridge image
-cartgmod2 <Name>
	Attach raw GMod2 cartridge image
-cartgmod3 <Name>
	Attach raw GMod2 cartridge image
-cartgk <Name>
	Attach raw 8KiB Game Killer cartridge image
-cartgeoram <Name>
	Attach raw GEO-RAM cartridge image
-cartgs <Name>
	Attach raw 512KiB Game System cartridge image
-carthyper <Name>
	Attach raw 64KiB Hyper-BASIC cartridge image
-cartide64 <Name>
	Attach raw 64KiB IDE64 cartridge image
-cartieee <Name>
	Attach IEEE-488 Interface cartridge image
-cartieeeflash64 <Name>
	Attach raw 8KiB IEEE Flash! 64 cartridge image
-cartisepic <Name>
	Attach raw 2KiB ISEPIC cartridge image
-cartkcs <Name>
	Attach raw 16KiB KCS Power cartridge image
-cartks <Name>
	Attach raw 24KiB Kingsoft cartridge image
-cartltk <Name>
	Attach raw 8kB Lt. Kernal boot image
-cartmach5 <Name>
	Attach raw 8KiB MACH 5 cartridge image
-cartmd <Name>
	Attach raw 32/64/128KiB Magic Desk cartridge image
-cartmd16 <Name>
	Attach raw up to 2048KiB Magic Desk 16K cartridge image
-cartmb <Name>
	Attach raw 1024KiB PTV Megabyter cartridge image
-cartmf <Name>
	Attach raw Magic Formel cartridge image
-cartmax <Name>
	Attach raw MAX Basic cartridge image
-cartmikro <Name>
	Attach raw 8KiB Mikro Assembler cartridge image
-cartmmc64 <Name>
	Attach raw 8KiB MMC64 cartridge image
-cartmmcr <Name>
	Attach raw 512KiB MMC Replay cartridge image
-cartmv <Name>
	Attach raw 16KiB Magic Voice cartridge image
-cartmm <Name>
	Attach raw 1MiB MultiMAX cartridge image
-cartocean <Name>
	Attach raw Ocean cartridge image
-cartpf <Name>
	Attach raw 64KiB Pagefox cartridge image
-cartp64 <Name>
	Attach raw 256KiB Prophet 64 cartridge image
-cartpartner64 <Name>
	Attach raw 16KiB Partner 64 cartridge image
-cartpd <Name>
	Attach raw 16KiB Profi-DOS cartridge image
-cartramcart <Name>
	Attach raw RamCart cartridge image
-cartramlink <Name>
	Attach raw 64KiB RAMLink ROM image
-cartreu <Name>
	Attach raw REU cartridge image
-cartrep256 <Name>
	Attach raw REX EP256 cartridge image
-cartrrf <Name>
	Attach raw REX RAM-Floppy cartridge image
-cartrgcd <Name>
	Attach raw 64KiB RGCD cartridge image
-cartuc1 <Name>
	Attach raw 32/64/128KiB UC-1 cartridge image
-cartuc15 <Name>
	Attach raw 128/256/512KiB UC-1.5 cartridge image
-cartuc2 <Name>
	Attach raw 128/256/512KiB UC-2 cartridge image
-cartrrnet <Name>
	Attach raw 8KiB RR-Net MK3 cartridge image
-cartross <Name>
	Attach raw 16/32KiB ROSS cartridge image
-cartrr <Name>
	Attach raw 64KiB Retro Replay cartridge image
-cartru <Name>
	Attach raw 8KiB REX Utility cartridge image
-cartsdbox <Name>
	Attach raw 128KiB SD-BOX cartridge image
-carts64 <Name>
	Attach raw 4KiB Snapshot 64 cartridge image
-cartsb <Name>
	Attach raw Structured Basic cartridge image
-cartse5 <Name>
	Attach raw 16KiB Super Explode V5 cartridge image
-cartsg <Name>
	Attach raw 64KiB Super Games cartridge image
-cartsilver <Name>
	Attach raw Silverrock 128 cartridge image
-cartsimon <Name>
	Attach raw 16KiB Simons Basic cartridge image
-cartss4 <Name>
	Attach raw 32KiB Super Snapshot V4 cartridge image
-cartss5 <Name>
	Attach raw 64KiB or 128KiB Super Snapshot V5 cartridge image
-cartstar <Name>
	Attach raw 16KiB Stardos cartridge image
-cartturtle <Name>
	Attach raw 16KiB HES Turtle Graphics II cartridge image
-cartwl <Name>
	Attach raw 16KiB Westermann Learning cartridge image
-cartws <Name>
	Attach raw 16KiB Warp Speed cartridge image
-cartzaxxon <Name>
	Attach raw 16KiB Zaxxon cartridge image
-cartzipp <Name>
	Attach raw 8KiB ZIPP-CODE 48 cartridge image
-cartreset
	Reset machine if a cartridge is attached or detached
+cartreset
	Do not reset machine if a cartridge is attached or detached
+cart
	Disable default cartridge
-raminitvalueoffset <offset>
	The first pattern is shifted by this many bytes
-raminitstartvalue <value>
	Set the value for the very first RAM address after powerup
-raminitvalueinvert <num of bytes>
	Length of memory block initialized with the same value
-raminitpatterninvert <num of bytes>
	Length of memory block initialized with the same pattern
-raminitpatterninvertvalue <value>
	Value to invert with in second pattern
-raminitstartrandom <num of bytes>
	Number of random bytes in random pattern
-raminitrepeatrandom <num of bytes>
	Repeat random pattern after this many bytes
-raminitrandomchance <value>
	Random chance for any bit to flip (0-0xfff)
-remotemonitor
	Enable remote monitor
+remotemonitor
	Disable remote monitor
-remotemonitoraddress <Name>
	The local address the remote monitor should bind to
-binarymonitor
	Enable binary monitor
+binarymonitor
	Disable binary monitor
-binarymonitoraddress <Name>
	The local address the binary monitor should bind to
-ffmpegaudiobitrate <value>
	Set bitrate for audio stream in media file
-ffmpegvideobitrate <value>
	Set bitrate for video stream in media file
-ocpoversize <method>
	Select the way the oversized input should be handled, (0: scale down, 1: crop left top, 2: crop center top,  3: crop right top, 4: crop left center, 5: crop center, 6: crop right center, 7: crop left bottom, 8: crop center bottom, 9:  crop right bottom)
-ocpundersize <method>
	Select the way the undersized input should be handled, (0: scale up, 1: borderize)
-ocpmc <method>
	Select the way the multicolor to hires should be handled, (0: b&w, 1: 2 colors, 2: 4 colors, 3: gray scale,  4: best cell colors)
-koalaoversize <method>
	Select the way the oversized input should be handled, (0: scale down, 1: crop left top, 2: crop center top,  3: crop right top, 4: crop left center, 5: crop center, 6: crop right center, 7: crop left bottom, 8: crop center bottom, 9:  crop right bottom)
-koalaundersize <method>
	Select the way the undersized input should be handled, (0: scale up, 1: borderize)
-minipaintoversize <method>
	Select the way the oversized input should be handled, (0: scale down, 1: crop left top, 2: crop center top,  3: crop right top, 4: crop left center, 5: crop center, 6: crop right center, 7: crop left bottom, 8: crop center bottom, 9:  crop right bottom)
-minipaintundersize <method>
	Select the way the undersized input should be handled, (0: scale up, 1: borderize)
-quicksaveformat <Format>
	Specify format of quicksave screenshots (png, gif ,bmp, iff, pcx, ppm, 4bt, artstudio, koala, minipaint)
```
