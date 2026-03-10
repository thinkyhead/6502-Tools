import sublime
import sublime_plugin

# Show only for C64 BASIC code
class _PetsciiCommand(sublime_plugin.TextCommand):
    def is_visible(self):
        return sublime.score_selector(self.view.syntax().scope, "source.c64basic") > 0

# Show a palette and insert the clicked choice
class _PetsciiInsertCommand(_PetsciiCommand):
    def run(self, edit, pop, fn):
        ps = sublime.load_settings("PETSCII.sublime-settings")
        content = self.get_characters_html(fn)
        flags = sublime.HTML + sublime.HIDE_ON_CHARACTER_EVENT
        if not ps.get("hide_on_select") and not pop:
            flags += sublime.HIDE_ON_MOUSE_MOVE_AWAY
        self.view.show_popup(content, flags, location=-1, max_width=600, max_height=640, on_navigate=self.on_choice_symbol)

    def get_characters_html(self, fn):
        resources = sublime.find_resources(fn)
        content = sublime.load_resource(resources[0])
        return content

    def on_choice_symbol(self, symbol):
        ps = sublime.load_settings("PETSCII.sublime-settings")
        self.view.run_command("insert", {"characters": symbol})
        if ps.get("hide_on_select"): self.view.hide_popup()

# Commands just show different palettes
class PetsciiInsertSpecialCommand(_PetsciiInsertCommand):
    def run(self, edit, pop=False): super().run(edit, pop, 'petscii-special.html')

class PetsciiInsertUnshiftedCommand(_PetsciiInsertCommand):
    def run(self, edit, pop=False): super().run(edit, pop, 'petscii-unshifted.html')

class PetsciiInsertShiftedCommand(_PetsciiInsertCommand):
    def run(self, edit, pop=False): super().run(edit, pop, 'petscii-shifted.html')

class PetsciiInsertInvCommand(_PetsciiInsertCommand):
    def run(self, edit, pop=False): super().run(edit, pop, 'petscii-inverted.html')

class PetsciiInsertDrawCommand(_PetsciiInsertCommand):
    def run(self, edit, pop=False): super().run(edit, pop, 'petscii-drawing.html')

# Popup variants just have no auto-hide flag
class PetsciiInsertSpecialPopCommand(PetsciiInsertSpecialCommand):
    def run(self, edit): super().run(edit, True)

class PetsciiInsertUnshiftedPopCommand(PetsciiInsertUnshiftedCommand):
    def run(self, edit): super().run(edit, True)

class PetsciiInsertShiftedPopCommand(PetsciiInsertShiftedCommand):
    def run(self, edit): super().run(edit, True)

class PetsciiInsertInvPopCommand(PetsciiInsertInvCommand):
    def run(self, edit): super().run(edit, True)

class PetsciiInsertDrawPopCommand(PetsciiInsertDrawCommand):
    def run(self, edit): super().run(edit, True)

# Replace selected characters with their inverted counterparts.
# This will remap common chars to ASCII when possible.
class PetsciiInvertTextCommand(_PetsciiCommand):
    def run(self, edit):
        tr1 = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz"
        tr2 = ""
        invert_map = {ord(a): ord(b) for a, b in zip(tr1, tr2)}
        invert_map.update({ord(b): ord(a) for a, b in zip(tr1, tr2)})

        sels = self.view.sel()
        for sel in sels:
            if sel.empty(): continue
            text = self.view.substr(sel)
            newtext = text.translate(invert_map)
            if newtext != text:
                self.view.replace(edit, sel, newtext)

# Replace selected characters with uppercase or lowercase.
# This preserves their mapping.
class _PetsciiCaseCommand(_PetsciiCommand):
    def run(self, edit, lcase):
        lstr = "abcdefghijklmnopqrstuvwxyz"
        ustr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        case_map = { ord(a): ord(b) for a, b in (zip(ustr, lstr) if lcase else zip(lstr, ustr)) }

        sels = self.view.sel()
        for sel in sels:
            if sel.empty(): continue
            text = self.view.substr(sel)
            newtext = text.translate(case_map)
            if newtext != text:
                self.view.replace(edit, sel, newtext)

class PetsciiLowerCommand(_PetsciiCaseCommand):
    def run(self, edit): super().run(edit, True)

class PetsciiUpperCommand(_PetsciiCaseCommand):
    def run(self, edit): super().run(edit, False)
