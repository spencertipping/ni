let fs = require('fs');
let process = require('process');

process.on('uncaughtException', err => {
  console.log(err, 'uncaught exception');
  process.exit(1);
});

let ibufs = [];
let inls  = [];
let L_    = null;
let F_    = null;
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
    if (stdin_eof)
    {
      F_ = null;
      return f(L_ = null);
    }

    let i = 0;
    while (i < inls.length && inls[i] === -1) ++i;
    if (i >= inls.length)
      return read_another_buf(b => {
        if (!b)
        {
          stdin_eof = true;
          F_ = null;
          f(L_ = Buffer.concat(ibufs));
          ibufs = inls = null;
          return next();
        }
        else
          return next();
      });

    if (i === 0)
    {
      // Optimized common case: don't build a prefix
      let r = ibufs[0].slice(0, inls[0]);
      ibufs[0] = ibufs[0].slice(inls[0] + 1);
      inls[0] = ibufs[0].indexOf(10);
      F_ = null;
      f(L_ = r);
    }
    else
    {
      let b = ibufs[i].slice(0, inls[i]);
      ibufs[i] = ibufs[i].slice(inls[i] + 1);
      inls[i] = ibufs[i].indexOf(10);
      let bs = ibufs.splice(0, i);
      inls.splice(0, i);
      bs.push(b);
      F_ = null;
      f(L_ = Buffer.concat(bs));
    }
  }
};

function r()
{
  while (true)
  {
    try
    {
      fs.writeSync(1, Array.prototype.slice.call(arguments).join("\t") + "\n");
      return null;
    }
    catch (e)
    {
      if (e.message.indexOf('EAGAIN') < 0)
        throw e;
    }
  }
};

function F(i)
{
  if (F_ == null)
  {
    F_ = [];
    let t = -1;
    do
    {
      let n = L_.indexOf(9, t + 1);
      F_.push(L_.slice(t + 1, n === -1 ? L_.length : n));
      t = n;
    } while (t !== -1);
  }

  if (arguments.length === 1) return F_[i];
  if (arguments.length === 0) return F_;
  let r = [];
  for (let i = 0; i < arguments.length; ++i)
    r.push(F_[arguments[i]]);
  return r;
}

function FM() { return F().length - 1 }

function a() { return F(0) }
function b() { return F(1) }
function c() { return F(2) }
function d() { return F(3) }
function e() { return F(4) }
function f() { return F(5) }
function g() { return F(6) }
function h() { return F(7) }
function i() { return F(8) }
function j() { return F(9) }
function k() { return F(10) }
function l() { return F(11) }
