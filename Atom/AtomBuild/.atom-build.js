var asm_name = 'dicombat';
module.exports = {
  cmd: 'dasm',
  args: [
    asm_name+'.asm',
    '-f3',
    '-o'+asm_name+'.bin',
    '-I/usr/local/include/vcs',
    '-E2'
  ],
  sh: false,
  functionMatch: function(terminal_output) {
    // Match output, pass errors to Atom
    var matches = [];
    terminal_output.split(/\n/).forEach(
      function(line, line_number, terminal_output) {
        var m = /(.+\..+):([0-9]+): (error: .*)/.exec(line);
        if (m) this.push({file:m[1], line:m[2], message:m[3]});
        m = /Unresolved Symbols?|Warning: Unable to open '.+'/.exec(line);
        if (m) this.push({file:asm_name+'.asm', line:1, message:m[0]});
      }.bind(matches)
    );
    return matches;
  }
};
