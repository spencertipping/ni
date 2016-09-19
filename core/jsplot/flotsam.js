/**
 * Flotsam encoder/decoder. For example:
 *
 * var xs = [1.0, 2.0, Math.PI];
 * var s  = flotsam.encode(xs);
 * var ys = flotsam.decode(s);
 */
flotsam = (function () {

var flotsam = {};

/** Set to true if the environment supports typed arrays, false otherwise. */
flotsam.typed_array_support = typeof Float64Array !== 'undefined';

/**
 * Encodes an array of numbers and returns a string. Throws an error if any
 * number is infinite or NaN. xs may be a normal or typed array.
 */
flotsam.encode = function (xs) {
  // According to jsperf.com, consing strings one element at a time is faster
  // than building an array and joining.
  var result = '';
  for (var i = 0, l = xs.length; i < l; ++i)
    result += flotsam.encode_single(xs[i]);
  return result;
};

/**
 * Decodes a string into an array of numbers. Uses a Float64Array if supported,
 * otherwise uses a regular Javascript array.
 */
flotsam.decode = function (s) {
  var result = flotsam.typed_array_support
             ? new Float64Array(s.length / 10)
             : new Array(s.length / 10);
  for (var i = 0, l = result.length; i < l; ++i)
    result[i] = flotsam.decode_single(s, i);
  return result;
};

// Floating point constants
///////////////////////////

var float_powers = flotsam.typed_array_support
                 ? new Float64Array(2048)
                 : new Array(2048);
float_powers[1023] = 1.0;
for (var i = 1024, base = 1.0; i <  2047; ++i) float_powers[i] = base *= 2.0;
for (var i = 1022, base = 1.0; i >= 1;    --i) float_powers[i] = base *= 0.5;

// Same exponent, different interpretation.
float_powers[0]    = float_powers[1];
float_powers[2047] = float_powers[2046];

var mantissa_norm = float_powers[1023 + 52];

var base94_decode = flotsam.typed_array_support
                  ? new Float64Array(128 * 8)
                  : new Array(128 * 8);
var base = 1;
for (var i = 0; i < 8; ++i) {
  for (var j = 0; j < 94; ++j)
    base94_decode[i << 7 | j + 32] = base * j / mantissa_norm;
  base *= 94;
}

var digits = new Array(94);
for (var i = 0; i < 94; ++i) digits[i] = String.fromCharCode(i + 32);

var digit_pairs = new Array(94 * 94);
for (var i = 0; i < 94; ++i)
  for (var j = 0; j < 94; ++j)
    digit_pairs[j * 94 + i] = digits[i] + digits[j];

/**
 * Encodes a single number as a 10-character string. We could in theory use
 * typed array aliasing to expose the bits in the float, but doing this exposes
 * the library to byte ordering issues. Instead, we just use a certain subset
 * of floating-point operations that are likely to be lossless.
 */
flotsam.encode_single = function (x) {
  // Enabling the following decreases performance by about 10%, but is useful
  // if you're unsure of your inputs.
  /*
    if (!isFinite(x) || isNaN(x))
      throw new Error(
        'Flotsam encode_single cannot be used with +/- infinity or NaN');
  */

  if (x === 0) return '          ';

  var sign = x < 0;
  if (sign) x = -x;

  // Binary-split for the observed exponent. We converge on the exact value in
  // 11 iterations.
  var l = 0, u = 2048, m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;      // 1
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;      // 6
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;
  if (x < float_powers[m = l + u >> 1]) u = m; else l = m;      // 11

  var exponent = l;

  // At this point we have an exponent such that float_powers[exponent] is no
  // larger than the quantity, with equality iff all mantissa bits are zero. So
  // exponent is the logical position of the implied high bit, or zero for
  // subnormal numbers.
  //
  // In order to get to the bits, we need to first shift the number to set the
  // exponent to 1023 (the encoding of 0). Then we subtract off the implied
  // mantissa norm, at which point we have just the remaining bits that we can
  // access by casting to an integer or using FP modular arithmetic.
  var mantissa = exponent !== 0
               ? (x * float_powers[2046 - exponent] - 1.0) * mantissa_norm
               :  x * float_powers[2045]                   * mantissa_norm;

  var result   = digits[sign << 5 | exponent >> 6 & 0x1f]
               + digits[            exponent      & 0x3f];

  var d78 =  mantissa                        * (1.0 / 689869781056) | 0;
  var d56 = (mantissa -= d78 * 689869781056) * (1.0 / 78074896)     | 0;
  var d34 = (mantissa -= d56 * 78074896)     * (1.0 / 8836)         | 0;
  var d12 = (mantissa -= d34 * 8836)                                | 0;
  return result + (digit_pairs[d12] + digit_pairs[d34]
                +  digit_pairs[d56] + digit_pairs[d78]);
};

/**
 * Decodes a single number from the position specified within the given string.
 * The position is the logical element position, not the character offset.
 */
flotsam.decode_single = function (s, n) {
  var i             = n * 10;
  var sign_exponent = s.charCodeAt(i)     - 32 << 6
                    | s.charCodeAt(i + 1) - 32;
  var exponent      = sign_exponent & 0x7ff;

  var result = base94_decode[0 << 7 | s.charCodeAt(i + 2)]
             + base94_decode[1 << 7 | s.charCodeAt(i + 3)]
             + base94_decode[2 << 7 | s.charCodeAt(i + 4)]
             + base94_decode[3 << 7 | s.charCodeAt(i + 5)]
             + base94_decode[4 << 7 | s.charCodeAt(i + 6)]
             + base94_decode[5 << 7 | s.charCodeAt(i + 7)]
             + base94_decode[6 << 7 | s.charCodeAt(i + 8)]
             + base94_decode[7 << 7 | s.charCodeAt(i + 9)]
             + (exponent ? 1 : 0);

  if (sign_exponent & 0x800) result = -result;
  result *= float_powers[exponent];
  return result;
};

return flotsam;

})();
