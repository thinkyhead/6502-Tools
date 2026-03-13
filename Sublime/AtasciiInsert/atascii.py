import sublime
import sublime_plugin

# Show a palette and insert the clicked choice
class _AtasciiCommand(sublime_plugin.TextCommand):
    def is_visible(self):
        return sublime.score_selector(self.view.syntax().scope, "source.ataribasic") > 0

    def run(self, edit, fn):
        ps = sublime.load_settings("AtasciiInsert.sublime-settings")
        content = self.get_characters_html(fn)
        flags = sublime.HTML + sublime.HIDE_ON_CHARACTER_EVENT
        if not ps.get("hide_on_select"): flags += sublime.HIDE_ON_MOUSE_MOVE_AWAY
        self.view.show_popup(content, flags, location=-1, max_width=600, max_height=640, on_navigate=self.on_choice_symbol)

    def get_characters_html(self, fn):
        resources = sublime.find_resources(fn)
        content = sublime.load_resource(resources[0])
        return content

    def on_choice_symbol(self, symbol):
        ps = sublime.load_settings("AtasciiInsert.sublime-settings")
        self.view.run_command("insert", {"characters": symbol})
        if ps.get("hide_on_select"): self.view.hide_popup()

class AtasciiInsertSpecialCommand(_AtasciiCommand):
    def run(self, edit): super().run(edit, 'atascii-special.html')

class AtasciiInsertInvCommand(_AtasciiCommand):
    def run(self, edit): super().run(edit, 'atascii-inverted.html')

class AtasciiInsertDrawCommand(_AtasciiCommand):
    def run(self, edit): super().run(edit, 'atascii-drawing.html')

# Replace selected characters with their inverted counterparts
class AtasciiInvertTextCommand(sublime_plugin.TextCommand):
    def is_visible(self):
        return sublime.score_selector(self.view.syntax().scope, "source.ataribasic") > 0

    def run(self, edit):
        tr1 = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz"
        tr2 = ""
        invert_map = {ord(a): ord(b) for a, b in zip(tr1, tr2)}
        invert_map.update({ord(b): ord(a) for a, b in zip(tr1, tr2)})

        sels = self.view.sel()
        for sel in sels:
            if sel.empty(): continue
            text = self.view.substr(sel)
            newtext = text.translate(invert_map)
            if newtext != text:
                self.view.replace(edit, sel, newtext)
