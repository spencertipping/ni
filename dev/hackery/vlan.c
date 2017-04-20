#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

/* https://github.com/floodyberry/poly1305-donna/blob/master/poly1305-donna.h */
#include <stddef.h>

typedef struct poly1305_context {
	size_t aligner;
	unsigned char opaque[136];
} poly1305_context;

void poly1305_init(poly1305_context *ctx, const unsigned char key[32]);
void poly1305_update(poly1305_context *ctx, const unsigned char *m, size_t bytes);
void poly1305_finish(poly1305_context *ctx, unsigned char mac[16]);
void poly1305_auth(unsigned char mac[16], const unsigned char *m, size_t bytes, const unsigned char key[32]);

int poly1305_verify(const unsigned char mac1[16], const unsigned char mac2[16]);
int poly1305_power_on_self_test(void);

/* auto detect between 32bit / 64bit */
#define HAS_SIZEOF_INT128_64BIT (defined(__SIZEOF_INT128__) && defined(__LP64__))
#define HAS_MSVC_64BIT (defined(_MSC_VER) && defined(_M_X64))
#define HAS_GCC_4_4_64BIT (defined(__GNUC__) && defined(__LP64__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 4))))

#if (HAS_SIZEOF_INT128_64BIT || HAS_MSVC_64BIT || HAS_GCC_4_4_64BIT)
/* https://github.com/floodyberry/poly1305-donna/blob/master/poly1305-donna-64.h */
#if defined(_MSC_VER)
	#include <intrin.h>

	typedef struct uint128_t {
		unsigned long long lo;
		unsigned long long hi;
	} uint128_t;

	#define MUL(out, x, y) out.lo = _umul128((x), (y), &out.hi)
	#define ADD(out, in) { unsigned long long t = out.lo; out.lo += in.lo; out.hi += (out.lo < t) + in.hi; }
	#define ADDLO(out, in) { unsigned long long t = out.lo; out.lo += in; out.hi += (out.lo < t); }
	#define SHR(in, shift) (__shiftright128(in.lo, in.hi, (shift)))
	#define LO(in) (in.lo)

	#define POLY1305_NOINLINE __declspec(noinline)
#elif defined(__GNUC__)
	#if defined(__SIZEOF_INT128__)
		typedef unsigned __int128 uint128_t;
	#else
		typedef unsigned uint128_t __attribute__((mode(TI)));
	#endif

	#define MUL(out, x, y) out = ((uint128_t)x * y)
	#define ADD(out, in) out += in
	#define ADDLO(out, in) out += in
	#define SHR(in, shift) (unsigned long long)(in >> (shift))
	#define LO(in) (unsigned long long)(in)

	#define POLY1305_NOINLINE __attribute__((noinline))
#endif

#define poly1305_block_size 16

/* 17 + sizeof(size_t) + 8*sizeof(unsigned long long) */
typedef struct poly1305_state_internal_t {
	unsigned long long r[3];
	unsigned long long h[3];
	unsigned long long pad[2];
	size_t leftover;
	unsigned char buffer[poly1305_block_size];
	unsigned char final;
} poly1305_state_internal_t;

/* interpret eight 8 bit unsigned integers as a 64 bit unsigned integer in little endian */
static unsigned long long
U8TO64(const unsigned char *p) {
	return
		(((unsigned long long)(p[0] & 0xff)      ) |
		 ((unsigned long long)(p[1] & 0xff) <<  8) |
		 ((unsigned long long)(p[2] & 0xff) << 16) |
		 ((unsigned long long)(p[3] & 0xff) << 24) |
		 ((unsigned long long)(p[4] & 0xff) << 32) |
		 ((unsigned long long)(p[5] & 0xff) << 40) |
		 ((unsigned long long)(p[6] & 0xff) << 48) |
		 ((unsigned long long)(p[7] & 0xff) << 56));
}

/* store a 64 bit unsigned integer as eight 8 bit unsigned integers in little endian */
static void
U64TO8(unsigned char *p, unsigned long long v) {
	p[0] = (v      ) & 0xff;
	p[1] = (v >>  8) & 0xff;
	p[2] = (v >> 16) & 0xff;
	p[3] = (v >> 24) & 0xff;
	p[4] = (v >> 32) & 0xff;
	p[5] = (v >> 40) & 0xff;
	p[6] = (v >> 48) & 0xff;
	p[7] = (v >> 56) & 0xff;
}

void
poly1305_init(poly1305_context *ctx, const unsigned char key[32]) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;
	unsigned long long t0,t1;

	/* r &= 0xffffffc0ffffffc0ffffffc0fffffff */
	t0 = U8TO64(&key[0]);
	t1 = U8TO64(&key[8]);

	st->r[0] = ( t0                    ) & 0xffc0fffffff;
	st->r[1] = ((t0 >> 44) | (t1 << 20)) & 0xfffffc0ffff;
	st->r[2] = ((t1 >> 24)             ) & 0x00ffffffc0f;

	/* h = 0 */
	st->h[0] = 0;
	st->h[1] = 0;
	st->h[2] = 0;

	/* save pad for later */
	st->pad[0] = U8TO64(&key[16]);
	st->pad[1] = U8TO64(&key[24]);

	st->leftover = 0;
	st->final = 0;
}

static void
poly1305_blocks(poly1305_state_internal_t *st, const unsigned char *m, size_t bytes) {
	const unsigned long long hibit = (st->final) ? 0 : ((unsigned long long)1 << 40); /* 1 << 128 */
	unsigned long long r0,r1,r2;
	unsigned long long s1,s2;
	unsigned long long h0,h1,h2;
	unsigned long long c;
	uint128_t d0,d1,d2,d;

	r0 = st->r[0];
	r1 = st->r[1];
	r2 = st->r[2];

	h0 = st->h[0];
	h1 = st->h[1];
	h2 = st->h[2];

	s1 = r1 * (5 << 2);
	s2 = r2 * (5 << 2);

	while (bytes >= poly1305_block_size) {
		unsigned long long t0,t1;

		/* h += m[i] */
		t0 = U8TO64(&m[0]);
		t1 = U8TO64(&m[8]);

		h0 += (( t0                    ) & 0xfffffffffff);
		h1 += (((t0 >> 44) | (t1 << 20)) & 0xfffffffffff);
		h2 += (((t1 >> 24)             ) & 0x3ffffffffff) | hibit;

		/* h *= r */
		MUL(d0, h0, r0); MUL(d, h1, s2); ADD(d0, d); MUL(d, h2, s1); ADD(d0, d);
		MUL(d1, h0, r1); MUL(d, h1, r0); ADD(d1, d); MUL(d, h2, s2); ADD(d1, d);
		MUL(d2, h0, r2); MUL(d, h1, r1); ADD(d2, d); MUL(d, h2, r0); ADD(d2, d);

		/* (partial) h %= p */
		              c = SHR(d0, 44); h0 = LO(d0) & 0xfffffffffff;
		ADDLO(d1, c); c = SHR(d1, 44); h1 = LO(d1) & 0xfffffffffff;
		ADDLO(d2, c); c = SHR(d2, 42); h2 = LO(d2) & 0x3ffffffffff;
		h0  += c * 5; c = (h0 >> 44);  h0 =    h0  & 0xfffffffffff;
		h1  += c;

		m += poly1305_block_size;
		bytes -= poly1305_block_size;
	}

	st->h[0] = h0;
	st->h[1] = h1;
	st->h[2] = h2;
}


POLY1305_NOINLINE void
poly1305_finish(poly1305_context *ctx, unsigned char mac[16]) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;
	unsigned long long h0,h1,h2,c;
	unsigned long long g0,g1,g2;
	unsigned long long t0,t1;

	/* process the remaining block */
	if (st->leftover) {
		size_t i = st->leftover;
		st->buffer[i] = 1;
		for (i = i + 1; i < poly1305_block_size; i++)
			st->buffer[i] = 0;
		st->final = 1;
		poly1305_blocks(st, st->buffer, poly1305_block_size);
	}

	/* fully carry h */
	h0 = st->h[0];
	h1 = st->h[1];
	h2 = st->h[2];

	             c = (h1 >> 44); h1 &= 0xfffffffffff;
	h2 += c;     c = (h2 >> 42); h2 &= 0x3ffffffffff;
	h0 += c * 5; c = (h0 >> 44); h0 &= 0xfffffffffff;
	h1 += c;     c = (h1 >> 44); h1 &= 0xfffffffffff;
	h2 += c;     c = (h2 >> 42); h2 &= 0x3ffffffffff;
	h0 += c * 5; c = (h0 >> 44); h0 &= 0xfffffffffff;
	h1 += c;

	/* compute h + -p */
	g0 = h0 + 5; c = (g0 >> 44); g0 &= 0xfffffffffff;
	g1 = h1 + c; c = (g1 >> 44); g1 &= 0xfffffffffff;
	g2 = h2 + c - ((unsigned long long)1 << 42);

	/* select h if h < p, or h + -p if h >= p */
	c = (g2 >> ((sizeof(unsigned long long) * 8) - 1)) - 1;
	g0 &= c;
	g1 &= c;
	g2 &= c;
	c = ~c;
	h0 = (h0 & c) | g0;
	h1 = (h1 & c) | g1;
	h2 = (h2 & c) | g2;

	/* h = (h + pad) */
	t0 = st->pad[0];
	t1 = st->pad[1];

	h0 += (( t0                    ) & 0xfffffffffff)    ; c = (h0 >> 44); h0 &= 0xfffffffffff;
	h1 += (((t0 >> 44) | (t1 << 20)) & 0xfffffffffff) + c; c = (h1 >> 44); h1 &= 0xfffffffffff;
	h2 += (((t1 >> 24)             ) & 0x3ffffffffff) + c;                 h2 &= 0x3ffffffffff;

	/* mac = h % (2^128) */
	h0 = ((h0      ) | (h1 << 44));
	h1 = ((h1 >> 20) | (h2 << 24));

	U64TO8(&mac[0], h0);
	U64TO8(&mac[8], h1);

	/* zero out the state */
	st->h[0] = 0;
	st->h[1] = 0;
	st->h[2] = 0;
	st->r[0] = 0;
	st->r[1] = 0;
	st->r[2] = 0;
	st->pad[0] = 0;
	st->pad[1] = 0;
}
#else
/* https://github.com/floodyberry/poly1305-donna/blob/master/poly1305-donna-32.h */
#if defined(_MSC_VER)
	#define POLY1305_NOINLINE __declspec(noinline)
#elif defined(__GNUC__)
	#define POLY1305_NOINLINE __attribute__((noinline))
#else
	#define POLY1305_NOINLINE
#endif

#define poly1305_block_size 16

/* 17 + sizeof(size_t) + 14*sizeof(unsigned long) */
typedef struct poly1305_state_internal_t {
	unsigned long r[5];
	unsigned long h[5];
	unsigned long pad[4];
	size_t leftover;
	unsigned char buffer[poly1305_block_size];
	unsigned char final;
} poly1305_state_internal_t;

/* interpret four 8 bit unsigned integers as a 32 bit unsigned integer in little endian */
static unsigned long
U8TO32(const unsigned char *p) {
	return
		(((unsigned long)(p[0] & 0xff)      ) |
	     ((unsigned long)(p[1] & 0xff) <<  8) |
         ((unsigned long)(p[2] & 0xff) << 16) |
         ((unsigned long)(p[3] & 0xff) << 24));
}

/* store a 32 bit unsigned integer as four 8 bit unsigned integers in little endian */
static void
U32TO8(unsigned char *p, unsigned long v) {
	p[0] = (v      ) & 0xff;
	p[1] = (v >>  8) & 0xff;
	p[2] = (v >> 16) & 0xff;
	p[3] = (v >> 24) & 0xff;
}

void
poly1305_init(poly1305_context *ctx, const unsigned char key[32]) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;

	/* r &= 0xffffffc0ffffffc0ffffffc0fffffff */
	st->r[0] = (U8TO32(&key[ 0])     ) & 0x3ffffff;
	st->r[1] = (U8TO32(&key[ 3]) >> 2) & 0x3ffff03;
	st->r[2] = (U8TO32(&key[ 6]) >> 4) & 0x3ffc0ff;
	st->r[3] = (U8TO32(&key[ 9]) >> 6) & 0x3f03fff;
	st->r[4] = (U8TO32(&key[12]) >> 8) & 0x00fffff;

	/* h = 0 */
	st->h[0] = 0;
	st->h[1] = 0;
	st->h[2] = 0;
	st->h[3] = 0;
	st->h[4] = 0;

	/* save pad for later */
	st->pad[0] = U8TO32(&key[16]);
	st->pad[1] = U8TO32(&key[20]);
	st->pad[2] = U8TO32(&key[24]);
	st->pad[3] = U8TO32(&key[28]);

	st->leftover = 0;
	st->final = 0;
}

static void
poly1305_blocks(poly1305_state_internal_t *st, const unsigned char *m, size_t bytes) {
	const unsigned long hibit = (st->final) ? 0 : (1UL << 24); /* 1 << 128 */
	unsigned long r0,r1,r2,r3,r4;
	unsigned long s1,s2,s3,s4;
	unsigned long h0,h1,h2,h3,h4;
	unsigned long long d0,d1,d2,d3,d4;
	unsigned long c;

	r0 = st->r[0];
	r1 = st->r[1];
	r2 = st->r[2];
	r3 = st->r[3];
	r4 = st->r[4];

	s1 = r1 * 5;
	s2 = r2 * 5;
	s3 = r3 * 5;
	s4 = r4 * 5;

	h0 = st->h[0];
	h1 = st->h[1];
	h2 = st->h[2];
	h3 = st->h[3];
	h4 = st->h[4];

	while (bytes >= poly1305_block_size) {
		/* h += m[i] */
		h0 += (U8TO32(m+ 0)     ) & 0x3ffffff;
		h1 += (U8TO32(m+ 3) >> 2) & 0x3ffffff;
		h2 += (U8TO32(m+ 6) >> 4) & 0x3ffffff;
		h3 += (U8TO32(m+ 9) >> 6) & 0x3ffffff;
		h4 += (U8TO32(m+12) >> 8) | hibit;

		/* h *= r */
		d0 = ((unsigned long long)h0 * r0) + ((unsigned long long)h1 * s4) + ((unsigned long long)h2 * s3) + ((unsigned long long)h3 * s2) + ((unsigned long long)h4 * s1);
		d1 = ((unsigned long long)h0 * r1) + ((unsigned long long)h1 * r0) + ((unsigned long long)h2 * s4) + ((unsigned long long)h3 * s3) + ((unsigned long long)h4 * s2);
		d2 = ((unsigned long long)h0 * r2) + ((unsigned long long)h1 * r1) + ((unsigned long long)h2 * r0) + ((unsigned long long)h3 * s4) + ((unsigned long long)h4 * s3);
		d3 = ((unsigned long long)h0 * r3) + ((unsigned long long)h1 * r2) + ((unsigned long long)h2 * r1) + ((unsigned long long)h3 * r0) + ((unsigned long long)h4 * s4);
		d4 = ((unsigned long long)h0 * r4) + ((unsigned long long)h1 * r3) + ((unsigned long long)h2 * r2) + ((unsigned long long)h3 * r1) + ((unsigned long long)h4 * r0);

		/* (partial) h %= p */
		              c = (unsigned long)(d0 >> 26); h0 = (unsigned long)d0 & 0x3ffffff;
		d1 += c;      c = (unsigned long)(d1 >> 26); h1 = (unsigned long)d1 & 0x3ffffff;
		d2 += c;      c = (unsigned long)(d2 >> 26); h2 = (unsigned long)d2 & 0x3ffffff;
		d3 += c;      c = (unsigned long)(d3 >> 26); h3 = (unsigned long)d3 & 0x3ffffff;
		d4 += c;      c = (unsigned long)(d4 >> 26); h4 = (unsigned long)d4 & 0x3ffffff;
		h0 += c * 5;  c =                (h0 >> 26); h0 =                h0 & 0x3ffffff;
		h1 += c;

		m += poly1305_block_size;
		bytes -= poly1305_block_size;
	}

	st->h[0] = h0;
	st->h[1] = h1;
	st->h[2] = h2;
	st->h[3] = h3;
	st->h[4] = h4;
}

POLY1305_NOINLINE void
poly1305_finish(poly1305_context *ctx, unsigned char mac[16]) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;
	unsigned long h0,h1,h2,h3,h4,c;
	unsigned long g0,g1,g2,g3,g4;
	unsigned long long f;
	unsigned long mask;

	/* process the remaining block */
	if (st->leftover) {
		size_t i = st->leftover;
		st->buffer[i++] = 1;
		for (; i < poly1305_block_size; i++)
			st->buffer[i] = 0;
		st->final = 1;
		poly1305_blocks(st, st->buffer, poly1305_block_size);
	}

	/* fully carry h */
	h0 = st->h[0];
	h1 = st->h[1];
	h2 = st->h[2];
	h3 = st->h[3];
	h4 = st->h[4];

	             c = h1 >> 26; h1 = h1 & 0x3ffffff;
	h2 +=     c; c = h2 >> 26; h2 = h2 & 0x3ffffff;
	h3 +=     c; c = h3 >> 26; h3 = h3 & 0x3ffffff;
	h4 +=     c; c = h4 >> 26; h4 = h4 & 0x3ffffff;
	h0 += c * 5; c = h0 >> 26; h0 = h0 & 0x3ffffff;
	h1 +=     c;

	/* compute h + -p */
	g0 = h0 + 5; c = g0 >> 26; g0 &= 0x3ffffff;
	g1 = h1 + c; c = g1 >> 26; g1 &= 0x3ffffff;
	g2 = h2 + c; c = g2 >> 26; g2 &= 0x3ffffff;
	g3 = h3 + c; c = g3 >> 26; g3 &= 0x3ffffff;
	g4 = h4 + c - (1UL << 26);

	/* select h if h < p, or h + -p if h >= p */
	mask = (g4 >> ((sizeof(unsigned long) * 8) - 1)) - 1;
	g0 &= mask;
	g1 &= mask;
	g2 &= mask;
	g3 &= mask;
	g4 &= mask;
	mask = ~mask;
	h0 = (h0 & mask) | g0;
	h1 = (h1 & mask) | g1;
	h2 = (h2 & mask) | g2;
	h3 = (h3 & mask) | g3;
	h4 = (h4 & mask) | g4;

	/* h = h % (2^128) */
	h0 = ((h0      ) | (h1 << 26)) & 0xffffffff;
	h1 = ((h1 >>  6) | (h2 << 20)) & 0xffffffff;
	h2 = ((h2 >> 12) | (h3 << 14)) & 0xffffffff;
	h3 = ((h3 >> 18) | (h4 <<  8)) & 0xffffffff;

	/* mac = (h + pad) % (2^128) */
	f = (unsigned long long)h0 + st->pad[0]            ; h0 = (unsigned long)f;
	f = (unsigned long long)h1 + st->pad[1] + (f >> 32); h1 = (unsigned long)f;
	f = (unsigned long long)h2 + st->pad[2] + (f >> 32); h2 = (unsigned long)f;
	f = (unsigned long long)h3 + st->pad[3] + (f >> 32); h3 = (unsigned long)f;

	U32TO8(mac +  0, h0);
	U32TO8(mac +  4, h1);
	U32TO8(mac +  8, h2);
	U32TO8(mac + 12, h3);

	/* zero out the state */
	st->h[0] = 0;
	st->h[1] = 0;
	st->h[2] = 0;
	st->h[3] = 0;
	st->h[4] = 0;
	st->r[0] = 0;
	st->r[1] = 0;
	st->r[2] = 0;
	st->r[3] = 0;
	st->r[4] = 0;
	st->pad[0] = 0;
	st->pad[1] = 0;
	st->pad[2] = 0;
	st->pad[3] = 0;
}
#endif


void
poly1305_update(poly1305_context *ctx, const unsigned char *m, size_t bytes) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;
	size_t i;

	/* handle leftover */
	if (st->leftover) {
		size_t want = (poly1305_block_size - st->leftover);
		if (want > bytes)
			want = bytes;
		for (i = 0; i < want; i++)
			st->buffer[st->leftover + i] = m[i];
		bytes -= want;
		m += want;
		st->leftover += want;
		if (st->leftover < poly1305_block_size)
			return;
		poly1305_blocks(st, st->buffer, poly1305_block_size);
		st->leftover = 0;
	}

	/* process full blocks */
	if (bytes >= poly1305_block_size) {
		size_t want = (bytes & ~(poly1305_block_size - 1));
		poly1305_blocks(st, m, want);
		m += want;
		bytes -= want;
	}

	/* store leftover */
	if (bytes) {
		for (i = 0; i < bytes; i++)
			st->buffer[st->leftover + i] = m[i];
		st->leftover += bytes;
	}
}

void
poly1305_auth(unsigned char mac[16], const unsigned char *m, size_t bytes, const unsigned char key[32]) {
	poly1305_context ctx;
	poly1305_init(&ctx, key);
	poly1305_update(&ctx, m, bytes);
	poly1305_finish(&ctx, mac);
}

int
poly1305_verify(const unsigned char mac1[16], const unsigned char mac2[16]) {
	size_t i;
	unsigned int dif = 0;
	for (i = 0; i < 16; i++)
		dif |= (mac1[i] ^ mac2[i]);
	dif = (dif - 1) >> ((sizeof(unsigned int) * 8) - 1);
	return (dif & 1);
}


/* test a few basic operations */
int
poly1305_power_on_self_test(void) {
	/* example from nacl */
	static const unsigned char nacl_key[32] = {
		0xee,0xa6,0xa7,0x25,0x1c,0x1e,0x72,0x91,
		0x6d,0x11,0xc2,0xcb,0x21,0x4d,0x3c,0x25,
		0x25,0x39,0x12,0x1d,0x8e,0x23,0x4e,0x65,
		0x2d,0x65,0x1f,0xa4,0xc8,0xcf,0xf8,0x80,
	};

	static const unsigned char nacl_msg[131] = {
		0x8e,0x99,0x3b,0x9f,0x48,0x68,0x12,0x73,
		0xc2,0x96,0x50,0xba,0x32,0xfc,0x76,0xce,
		0x48,0x33,0x2e,0xa7,0x16,0x4d,0x96,0xa4,
		0x47,0x6f,0xb8,0xc5,0x31,0xa1,0x18,0x6a,
		0xc0,0xdf,0xc1,0x7c,0x98,0xdc,0xe8,0x7b,
		0x4d,0xa7,0xf0,0x11,0xec,0x48,0xc9,0x72,
		0x71,0xd2,0xc2,0x0f,0x9b,0x92,0x8f,0xe2,
		0x27,0x0d,0x6f,0xb8,0x63,0xd5,0x17,0x38,
		0xb4,0x8e,0xee,0xe3,0x14,0xa7,0xcc,0x8a,
		0xb9,0x32,0x16,0x45,0x48,0xe5,0x26,0xae,
		0x90,0x22,0x43,0x68,0x51,0x7a,0xcf,0xea,
		0xbd,0x6b,0xb3,0x73,0x2b,0xc0,0xe9,0xda,
		0x99,0x83,0x2b,0x61,0xca,0x01,0xb6,0xde,
		0x56,0x24,0x4a,0x9e,0x88,0xd5,0xf9,0xb3,
		0x79,0x73,0xf6,0x22,0xa4,0x3d,0x14,0xa6,
		0x59,0x9b,0x1f,0x65,0x4c,0xb4,0x5a,0x74,
		0xe3,0x55,0xa5
	};

	static const unsigned char nacl_mac[16] = {
		0xf3,0xff,0xc7,0x70,0x3f,0x94,0x00,0xe5,
		0x2a,0x7d,0xfb,0x4b,0x3d,0x33,0x05,0xd9
	};

	/* generates a final value of (2^130 - 2) == 3 */
	static const unsigned char wrap_key[32] = {
		0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	};

	static const unsigned char wrap_msg[16] = {
		0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
		0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
	};

	static const unsigned char wrap_mac[16] = {
		0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
		0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	};

	/*
		mac of the macs of messages of length 0 to 256, where the key and messages
		have all their values set to the length
	*/
	static const unsigned char total_key[32] = {
		0x01,0x02,0x03,0x04,0x05,0x06,0x07,
		0xff,0xfe,0xfd,0xfc,0xfb,0xfa,0xf9,
		0xff,0xff,0xff,0xff,0xff,0xff,0xff,
		0xff,0xff,0xff,0xff,0xff,0xff,0xff
	};

	static const unsigned char total_mac[16] = {
		0x64,0xaf,0xe2,0xe8,0xd6,0xad,0x7b,0xbd,
		0xd2,0x87,0xf9,0x7c,0x44,0x62,0x3d,0x39
	};

	poly1305_context ctx;
	poly1305_context total_ctx;
	unsigned char all_key[32];
	unsigned char all_msg[256];
	unsigned char mac[16];
	size_t i, j;
	int result = 1;

	for (i = 0; i < sizeof(mac); i++)
		mac[i] = 0;
	poly1305_auth(mac, nacl_msg, sizeof(nacl_msg), nacl_key);
	result &= poly1305_verify(nacl_mac, mac);

	for (i = 0; i < sizeof(mac); i++)
		mac[i] = 0;
	poly1305_init(&ctx, nacl_key);
	poly1305_update(&ctx, nacl_msg +   0, 32);
	poly1305_update(&ctx, nacl_msg +  32, 64);
	poly1305_update(&ctx, nacl_msg +  96, 16);
	poly1305_update(&ctx, nacl_msg + 112,  8);
	poly1305_update(&ctx, nacl_msg + 120,  4);
	poly1305_update(&ctx, nacl_msg + 124,  2);
	poly1305_update(&ctx, nacl_msg + 126,  1);
	poly1305_update(&ctx, nacl_msg + 127,  1);
	poly1305_update(&ctx, nacl_msg + 128,  1);
	poly1305_update(&ctx, nacl_msg + 129,  1);
	poly1305_update(&ctx, nacl_msg + 130,  1);
	poly1305_finish(&ctx, mac);
	result &= poly1305_verify(nacl_mac, mac);

	for (i = 0; i < sizeof(mac); i++)
		mac[i] = 0;
	poly1305_auth(mac, wrap_msg, sizeof(wrap_msg), wrap_key);
	result &= poly1305_verify(wrap_mac, mac);

	poly1305_init(&total_ctx, total_key);
	for (i = 0; i < 256; i++) {
		/* set key and message to 'i,i,i..' */
		for (j = 0; j < sizeof(all_key); j++)
			all_key[j] = i;
		for (j = 0; j < i; j++)
			all_msg[j] = i;
		poly1305_auth(mac, all_msg, i, all_key);
		poly1305_update(&total_ctx, mac, 16);
	}
	poly1305_finish(&total_ctx, mac);
	result &= poly1305_verify(total_mac, mac);

	return result;
}

/* http://chacha20.insanecoding.org/ */
/*
Copyright (C) 2014 insane coder (http://insanecoding.blogspot.com/, http://chacha20.insanecoding.org/)

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#ifndef CHACHA20_SIMPLE_H
#define CHACHA20_SIMPLE_H
#include <stdint.h>

#define ROTL32(v, n) ((v) << (n)) | ((v) >> (32 - (n)))

#define LE(p) (((uint32_t)((p)[0])) | ((uint32_t)((p)[1]) << 8) | ((uint32_t)((p)[2]) << 16) | ((uint32_t)((p)[3]) << 24))
#define FROMLE(b, i) (b)[0] = i & 0xFF; (b)[1] = (i >> 8) & 0xFF; (b)[2] = (i >> 16) & 0xFF; (b)[3] = (i >> 24) & 0xFF;

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct
{
  uint32_t schedule[16];
  uint32_t keystream[16];
  size_t available;
} chacha20_ctx;

//Call this to initilize a chacha20_ctx, must be called before all other functions
void chacha20_setup(chacha20_ctx *ctx, const uint8_t *key, size_t length, uint8_t nonce[8]);

//Call this if you need to process a particular block number
void chacha20_counter_set(chacha20_ctx *ctx, uint64_t counter);

//Raw keystream for the current block, convert output to uint8_t[] for individual bytes. Counter is incremented upon use
void chacha20_block(chacha20_ctx *ctx, uint32_t output[16]);

//Encrypt an arbitrary amount of plaintext, call continuously as needed
void chacha20_encrypt(chacha20_ctx *ctx, const uint8_t *in, uint8_t *out, size_t length);

//Decrypt an arbitrary amount of ciphertext. Actually, for chacha20, decryption is the same function as encryption
void chacha20_decrypt(chacha20_ctx *ctx, const uint8_t *in, uint8_t *out, size_t length);

#ifdef __cplusplus
}
#endif

#endif

/*
Copyright (C) 2014 insane coder (http://insanecoding.blogspot.com/, http://chacha20.insanecoding.org/)

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

This implementation is intended to be simple, many optimizations can be performed.
*/

#include <string.h>

void chacha20_setup(chacha20_ctx *ctx, const uint8_t *key, size_t length, uint8_t nonce[8])
{
  const char *constants = (length == 32) ? "expand 32-byte k" : "expand 16-byte k";

  ctx->schedule[0] = LE(constants + 0);
  ctx->schedule[1] = LE(constants + 4);
  ctx->schedule[2] = LE(constants + 8);
  ctx->schedule[3] = LE(constants + 12);
  ctx->schedule[4] = LE(key + 0);
  ctx->schedule[5] = LE(key + 4);
  ctx->schedule[6] = LE(key + 8);
  ctx->schedule[7] = LE(key + 12);
  ctx->schedule[8] = LE(key + 16 % length);
  ctx->schedule[9] = LE(key + 20 % length);
  ctx->schedule[10] = LE(key + 24 % length);
  ctx->schedule[11] = LE(key + 28 % length);
  //Surprise! This is really a block cipher in CTR mode
  ctx->schedule[12] = 0; //Counter
  ctx->schedule[13] = 0; //Counter
  ctx->schedule[14] = LE(nonce+0);
  ctx->schedule[15] = LE(nonce+4);

  ctx->available = 0;
}

void chacha20_counter_set(chacha20_ctx *ctx, uint64_t counter)
{
  ctx->schedule[12] = counter & UINT32_C(0xFFFFFFFF);
  ctx->schedule[13] = counter >> 32;
  ctx->available = 0;
}

#define QUARTERROUND(x, a, b, c, d) \
    x[a] += x[b]; x[d] = ROTL32(x[d] ^ x[a], 16); \
    x[c] += x[d]; x[b] = ROTL32(x[b] ^ x[c], 12); \
    x[a] += x[b]; x[d] = ROTL32(x[d] ^ x[a], 8); \
    x[c] += x[d]; x[b] = ROTL32(x[b] ^ x[c], 7);

void chacha20_block(chacha20_ctx *ctx, uint32_t output[16])
{
  uint32_t *const nonce = ctx->schedule+12; //12 is where the 128 bit counter is
  int i = 10;

  memcpy(output, ctx->schedule, sizeof(ctx->schedule));

  while (i--)
  {
    QUARTERROUND(output, 0, 4, 8, 12)
    QUARTERROUND(output, 1, 5, 9, 13)
    QUARTERROUND(output, 2, 6, 10, 14)
    QUARTERROUND(output, 3, 7, 11, 15)
    QUARTERROUND(output, 0, 5, 10, 15)
    QUARTERROUND(output, 1, 6, 11, 12)
    QUARTERROUND(output, 2, 7, 8, 13)
    QUARTERROUND(output, 3, 4, 9, 14)
  }
  for (i = 0; i < 16; ++i)
  {
    uint32_t result = output[i] + ctx->schedule[i];
    FROMLE((uint8_t *)(output+i), result);
  }

  /*
  Official specs calls for performing a 64 bit increment here, and limit usage to 2^64 blocks.
  However, recommendations for CTR mode in various papers recommend including the nonce component for a 128 bit increment.
  This implementation will remain compatible with the official up to 2^64 blocks, and past that point, the official is not intended to be used.
  This implementation with this change also allows this algorithm to become compatible for a Fortuna-like construct.
  */
  if (!++nonce[0] && !++nonce[1] && !++nonce[2]) { ++nonce[3]; }
}

static inline void chacha20_xor(uint8_t *keystream, const uint8_t **in, uint8_t **out, size_t length)
{
  uint8_t *end_keystream = keystream + length;
  do { *(*out)++ = *(*in)++ ^ *keystream++; } while (keystream < end_keystream);
}

void chacha20_encrypt(chacha20_ctx *ctx, const uint8_t *in, uint8_t *out, size_t length)
{
  if (length)
  {
    uint8_t *const k = (uint8_t *)ctx->keystream;

    //First, use any buffered keystream from previous calls
    if (ctx->available)
    {
      size_t amount = MIN(length, ctx->available);
      chacha20_xor(k + (sizeof(ctx->keystream)-ctx->available), &in, &out, amount);
      ctx->available -= amount;
      length -= amount;
    }

    //Then, handle new blocks
    while (length)
    {
      size_t amount = MIN(length, sizeof(ctx->keystream));
      chacha20_block(ctx, ctx->keystream);
      chacha20_xor(k, &in, &out, amount);
      length -= amount;
      ctx->available = sizeof(ctx->keystream) - amount;
    }
  }
}

void chacha20_decrypt(chacha20_ctx *ctx, const uint8_t *in, uint8_t *out, size_t length)
{
  chacha20_encrypt(ctx, in, out, length);
}

int main(int argc, char **argv)
{
  if (poly1305_power_on_self_test())
    fprintf(stderr, "poly1305 POST pass\n");
  else
  {
    fprintf(stderr, "poly1305 failed POST\n");
    exit(1);
  }

  if (argc == 2 && !strcmp(argv[1], "--test"))
  {
    char buf[8192];
    char enc[8192];
    char key[32];
    char nonce[8];
    chacha20_ctx cc;

    memset(key, 0, sizeof(key));
    memset(nonce, 0, sizeof(nonce));

    ssize_t n;
    while ((n = fread(buf, sizeof(char), sizeof(buf), stdin)) > 0)
    {
      chacha20_setup(&cc, key, sizeof(key), nonce);
      printf("P[%ld: %s]\n", n, buf);
      chacha20_encrypt(&cc, buf, enc, n);
      printf("E[%ld: ", n);
      for (int i = 0; i < n; ++i) printf("%02X", enc[i] & 0xff);
      printf("]\n");

      chacha20_setup(&cc, key, sizeof(key), nonce);
      chacha20_decrypt(&cc, enc, buf, n);
      printf("D[%ld: %s]\n", n, buf);
    }
  }

  /* Incoming FDs 3 and 4 represent the UDP socket and UNIX dgram socket,
   * respectively. */
}
