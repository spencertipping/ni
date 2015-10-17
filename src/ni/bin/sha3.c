/* Calculates the SHA3-256 of stdin and argv. This is used throughout ni to
 * cache intermediate results.
 *
 * Code from https://github.com/gvanas/KeccakCodePackage/blob/master/Standalone/CompactFIPS202/Keccak-more-compact.c;
 * modified here to read from a file descriptor and to alias pointers for a
 * speedup of about 4x */

#include <unistd.h>
#include <sys/types.h>

#define FOR(i,n) for(i=0; i<n; ++i)
typedef unsigned char u8;
typedef unsigned long long int u64;
typedef unsigned int ui;

void Keccak(ui r, ui c, const u8 *in, u64 inLen, u8 sfx, u8 *out, u64 outLen);

int LFSR86540(u8 *R) { (*R)=((*R)<<1)^(((*R)&0x80)?0x71:0); return ((*R)&2)>>1; }
#define ROL(a,o) ((((u64)a)<<o)^(((u64)a)>>(64-o)))
#define rL(x,y) (*(u64*)((u8*)s+8*(x+5*y)))
#define wL(x,y,l) (*(u64*)((u8*)s+8*(x+5*y))=(l))
#define XL(x,y,l) (*(u64*)((u8*)s+8*(x+5*y))^=(l))
void KeccakF1600(void *s)
{
    ui r,x,y,i,j,Y; u8 R=0x01; u64 C[5],D;
    for(i=0; i<24; i++) {
        /*θ*/ FOR(x,5) C[x]=rL(x,0)^rL(x,1)^rL(x,2)^rL(x,3)^rL(x,4); FOR(x,5) { D=C[(x+4)%5]^ROL(C[(x+1)%5],1); FOR(y,5) XL(x,y,D); }
        /*ρπ*/ x=1; y=r=0; D=rL(x,y); FOR(j,24) { r+=j+1; Y=(2*x+3*y)%5; x=y; y=Y; C[0]=rL(x,y); wL(x,y,ROL(D,r%64)); D=C[0]; }
        /*χ*/ FOR(y,5) { FOR(x,5) C[x]=rL(x,y); FOR(x,5) wL(x,y,C[x]^((~C[(x+1)%5])&C[(x+2)%5])); }
        /*ι*/ FOR(j,7) if (LFSR86540(&R)) XL(0,0,(u64)1<<((1<<j)-1));
    }
}

#define IN_BLOCK 4

int main()
{
    /*initialize*/ u8 eof = 0, ia[1088 * IN_BLOCK], *in = ia, oa[32], *out = oa; u64 r, inLen = 0; u8 s[200]; ui R=1088/8; ui i,b=0; u64 outLen=32; FOR(i,200) s[i]=0;
    /*absorb*/ while (!eof) {r = 0;
                             while ((inLen += r) < 1088 * IN_BLOCK && !eof) eof |= !(r = read(0, (in = ia) + inLen, sizeof(ia) - inLen));
                             while(inLen>0) { b=(inLen<R)?inLen:R; FOR(i,b) s[i]^=in[i]; in+=b; inLen-=b; if (b==R) { KeccakF1600(s); b=0; } } }
    /*pad*/ s[b]^=6; if((6&0x80)&&(b==(R-1))) KeccakF1600(s); s[R-1]^=0x80; KeccakF1600(s);
    /*squeeze*/ while(outLen>0) { b=(outLen<R)?outLen:R; FOR(i,b) out[i]=s[i]; out+=b; outLen-=b; if(outLen>0) KeccakF1600(s); }

    u8 oh[65], *hex="0123456789abcdef"; FOR(i, 32) { oh[i<<1]=hex[oa[i]>>4]; oh[i<<1|1]=hex[oa[i]&15]; }
    oh[64]='\n'; write(1, oh, 65); return 0;
}
