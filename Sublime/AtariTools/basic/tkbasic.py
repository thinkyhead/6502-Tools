#!/usr/bin/env python3
"""
A tiny “compiler viewer” built with Tkinter.

* Left pane – editable text area (fixed‑width font).
  You can load a file into it with the “Open… ” menu or the button below.
* Right pane – read‑only text area that shows the *binary* result of a
  `process()` function, formatted as a 16‑column hex dump.
* Bottom – a “Process” button that calls the compiler routine.

The `process()` function is intentionally left as a stub – replace it with
your actual compiler/assembler routine that returns a `bytes` object.
"""

import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext, font
import os
import random   # <-- used only in the demo implementation

# --------------------------------------------------------------------------- #
# Utility: format a bytes buffer as a 16‑column hex dump
# --------------------------------------------------------------------------- #
def format_hex_dump(data: bytes, cols: int = 16) -> str:
    """
    Return a string containing a hex dump of *data*.

    Each line starts with the 4‑digit offset in hex, then a space, then
    the hex bytes grouped in *cols* bytes per line, separated by spaces.
    Example line:
        0000  04 B1 D9 73 1F 00 00 00 00 00 00 00 00 00 00 00
    """
    lines = []
    for offset in range(0, len(data), cols):
        chunk = data[offset:offset+cols]
        hex_bytes = " ".join(f"{b:02X}" for b in chunk)
        lines.append(f"{offset:04X}  {hex_bytes}")
    return "\n".join(lines)


# --------------------------------------------------------------------------- #
# Demo "process" routine – replace this with your compiler logic!
# --------------------------------------------------------------------------- #
def process(source_text: str) -> bytes:
    """
    Dummy compiler routine.
    For demonstration, it simply returns a deterministic sequence
    of bytes derived from the source text.
    Replace this with your real code that turns *source_text* into binary.
    """
    # For real life, put your compiler/assembler here.
    # The function must return a `bytes` object (binary buffer).
    # This demo just generates random data for illustration.
    seed = sum(ord(c) for c in source_text)  # simple deterministic seed
    random.seed(seed)
    return bytes(random.getrandbits(8) for _ in range(256))   # 256‑byte buffer


# --------------------------------------------------------------------------- #
# Main application window
# --------------------------------------------------------------------------- #
class CompilerGUI(tk.Tk):
    def __init__(self):
        super().__init__()

        self.title("Tiny Compiler Viewer")
        self.geometry("1000x600")
        self.minsize(600, 400)

        # Fixed‑width font for both panes
        self.fixed_font = font.Font(family="Atari Classic", size=12)

        self.create_widgets()
        self.bind_shortcuts()

        # Bring window to front
        self.after(0, lambda: (self.lift(), self.focus_force()))

    # --------------------------------------------------------------------- #
    # UI Construction
    # --------------------------------------------------------------------- #
    def create_widgets(self):
        # ---- Menu -------------------------------------------------------
        menubar = tk.Menu(self)
        filemenu = tk.Menu(menubar, tearoff=0)
        filemenu.add_command(label="Open…", accelerator="Ctrl+O", command=self.load_file)
        filemenu.add_separator()
        filemenu.add_command(label="Exit", accelerator="Ctrl+Q", command=self.quit)
        menubar.add_cascade(label="File", menu=filemenu)
        self.config(menu=menubar)

        # ---- Main frame with two text panes ----------------------------
        main_frame = tk.Frame(self)
        main_frame.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)

        # Left Text (editable)
        left_label = tk.Label(main_frame, text="Source Code")
        left_label.grid(row=0, column=0, sticky="w")
        self.source_text = scrolledtext.ScrolledText(
            main_frame, wrap=tk.NONE, font=self.fixed_font, undo=True
        )
        self.source_text.grid(row=1, column=0, sticky="nsew", padx=(0, 2))
        # ---- Right Text (read‑only)
        right_label = tk.Label(main_frame, text="Compiled Output")
        right_label.grid(row=0, column=1, sticky="w")
        self.output_text = scrolledtext.ScrolledText(
            main_frame, wrap=tk.NONE, font=self.fixed_font, state=tk.DISABLED
        )
        self.output_text.grid(row=1, column=1, sticky="nsew", padx=(2, 0))

        # Configure grid weights so panes expand proportionally
        main_frame.columnconfigure(0, weight=1)
        main_frame.columnconfigure(1, weight=1)
        main_frame.rowconfigure(1, weight=1)

        # ---- Bottom toolbar --------------------------------------------
        toolbar = tk.Frame(self)
        toolbar.pack(fill=tk.X, padx=5, pady=5)

        self.process_btn = tk.Button(toolbar, text="Tokenize", command=self.on_process)
        self.process_btn.pack(side=tk.RIGHT)

        # Load a small demo file automatically (optional)
        # self.load_demo_file()

    # --------------------------------------------------------------------- #
    # File I/O
    # --------------------------------------------------------------------- #
    def load_file(self):
        """Open a file dialog, read the selected file, and load it into the source pane."""
        file_path = filedialog.askopenfilename(
            title="Open Source File",
            filetypes=[("BASIC Files", "*.lst *.txt *.bas *.ataribas *.ataribasic"), ("All Files", "*.*")],
        )
        if not file_path:
            return

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()
        except Exception as e:
            messagebox.showerror("Error", f"Could not read file:\n{e}")
            return

        self.source_text.delete("1.0", tk.END)
        self.source_text.insert(tk.END, content)

    # --------------------------------------------------------------------- #
    # Process button callback
    # --------------------------------------------------------------------- #
    def on_process(self):
        """Retrieve source text, run the compiler, format the output, and display it."""
        source = self.source_text.get("1.0", tk.END).rstrip("\n")
        if not source.strip():
            messagebox.showwarning("No source", "The source text is empty.")
            return

        try:
            binary = process(source)   # <- YOUR compiler goes here
        except Exception as exc:
            messagebox.showerror("Compilation error", f"An error occurred:\n{exc}")
            return

        hex_dump = format_hex_dump(binary)

        self.output_text.config(state=tk.NORMAL)
        self.output_text.delete("1.0", tk.END)
        self.output_text.insert(tk.END, hex_dump)
        self.output_text.config(state=tk.DISABLED)

    # --------------------------------------------------------------------- #
    # Keyboard shortcuts
    # --------------------------------------------------------------------- #
    def bind_shortcuts(self):
        self.bind_all("<Command-o>", lambda e: self.load_file())
        self.bind_all("<Command-q>", lambda e: self.quit())
        self.bind_all("<Command-Return>", lambda e: self.on_process())


# --------------------------------------------------------------------------- #
# Application entry point
# --------------------------------------------------------------------------- #
if __name__ == "__main__":
    app = CompilerGUI()
    app.mainloop()
