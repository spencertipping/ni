let fs = require('fs');

let ibufs = [];
let inls  = [];
let stdin_eof = false;

// NOTE: this code is all written async-style even though it should use sync
// methods. We have to do this because readSync() is bugged in a number of
// nodeJS releases. See https://github.com/jxmot/nodejs-readSync-bug.

const read_another_buf = (f) =>
  fs.read(3, Buffer.allocUnsafe(65536), 0, 65536, null, (err, n, b) => {
    if (err || n <= 0) return f(false);

    b = b.slice(0, n);
    ibufs.push(b);
    inls.push(b.indexOf(10));
    return f(true);
  });

// Read a line with buffered ops.
const rl = (f, next) => {
  while (true)
  {
    if (stdin_eof) return f(null);

    let i = 0;
    while (i < inls.length && inls[i] === -1) ++i;
    if (i >= inls.length)
      return read_another_buf(b => {
        if (!b)
        {
          stdin_eof = true;
          f(Buffer.concat(ibufs));
          ibufs = inls = null;
          return next();
        }
        else
          return next();
      });

    if (i === 0)
    {
      // Optimized common case: don't build a prefix
      let r = ibufs[0].slice(0, inls[0] + 1);
      ibufs[0] = ibufs[0].slice(inls[0] + 1);
      inls[0] = ibufs[0].indexOf(10);
      f(r);
    }
    else
    {
      let b = ibufs[i].slice(0, inls[i] + 1);
      ibufs[i] = ibufs[i].slice(inls[i] + 1);
      inls[i] = ibufs[i].indexOf(10);
      let bs = ibufs.splice(0, i);
      inls.splice(0, i);
      bs.push(b);
      f(Buffer.concat(bs));
    }
  }
};
