var ASM_NAME = 'clock003';
module.exports = {
  name: "Build Clock!",
  keymap: 'ctrl-b',
  cmd: 'dasm',
  args: [
    ASM_NAME+'.s',
    '-f3',
    '-o'+ASM_NAME+'.bin',
    '-I/usr/local/include/vcs',
    '-E2'
  ],
  sh: true,
  functionMatch: function(terminal_output) {
    // Match output, pass errors to Atom
    var matches = [];
    terminal_output.split(/\n/).forEach(
      function(line, line_number, terminal_output) {
        var m = /(.+\..+) \(([0-9]+)\): (error: .*)/.exec(line);
        if (m) this.push({file:m[1], line:m[2], message:m[3]});
        m = /Unresolved Symbols?|Warning: Unable to open '.+'/.exec(line);
        if (m) this.push({file:ASM_NAME+'.s', line:1, message:m[0]});
      }.bind(matches)
    );
    return matches;
  }
};
