#include <cstdio>
#include <cstdint>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <array>

template<bool S, int bsize>
struct IType;

template<> struct IType<1, 8> { using type =  int8_t; };
template<> struct IType<0, 8> { using type = uint8_t; };
template<> struct IType<1, 16> { using type =  int16_t; };
template<> struct IType<0, 16> { using type = uint16_t; };
template<> struct IType<1, 32> { using type =  int32_t; };
template<> struct IType<0, 32> { using type = uint32_t; };


template<bool S, int IBIT, int FBIT>
class FBit
{
public:

    static constexpr int bitsize = S + IBIT + FBIT;
    static constexpr int size = bitsize / 8;

    static_assert (bitsize == 8 || bitsize == 16 || bitsize == 32);

    using T = uint64_t;
    static constexpr T mask = ((T) 1 << bitsize) - 1;
    static constexpr T signmask = (T) 1 << (bitsize - 1);
    static constexpr T maxV = mask >> S;
    static constexpr FBit half = FBit::gen ((T) 1 << (FBIT - 1));

    using I = typename IType<S, bitsize>::type;
    using U = typename IType<0, bitsize>::type;

    T v;

    static constexpr T extend (T t)
    {
        return S && (t & signmask)
            ? t | -signmask
            : t & mask;
    }

    void operator = (float f)
    {
        *this = FBit (f);
    }

    constexpr FBit () : v(0) {}

    FBit (double f)
    {
        bool fsign = 0;
        if (S)
        {
            fsign = f < 0.0;
            f = fabs (f);
        }
        else
            f = fmax (f, 0.0);

        v = ldexp (f, FBIT) + 0.0;

        if (fsign == 0)
            v = std::min (v, maxV);
        else
            v = - std::min (v, 1 + maxV);
    }

    static constexpr FBit gen (T v)
    {
        //printf ("{v=%d}",(int)v);
        FBit x;
        x.v = extend (v);
        return x;
    }

    I asInt () const
    {
        return (I) v;
    }

    explicit operator double () const
    {
        return ldexp (asInt(), -FBIT);
    }

    explicit operator float () const
    {
        return ldexpf (asInt(), -FBIT);
    }

    bool sign () const
    {
        return S ? v & signmask : 0;
    }

    bool operator == (const FBit &y) const { return extend(v) == extend(y.v); }
    bool operator != (const FBit &y) const { return extend(v) != extend(y.v); }
    bool operator <= (const FBit &y) const { return extend(v) <= extend(y.v); }
    bool operator >= (const FBit &y) const { return extend(v) >= extend(y.v); }
    bool operator <  (const FBit &y) const { return extend(v) <  extend(y.v); }
    bool operator >  (const FBit &y) const { return extend(v) >  extend(y.v); }

    FBit operator - () const
    {
        return gen (- extend (v));
    }

    FBit operator + (const FBit &y) const
    {
        return gen (extend (v + y.v));
    }

    void operator += (const FBit &y)
    {
        *this = *this + y;
    }

    void operator *= (const FBit &y)
    {
        *this = *this * y;
    }

    FBit operator - (const FBit &y) const
    {
        return gen (extend (v - y.v));
    }

    FBit operator >> (int i) const
    {
        T t = extend (v);
        return gen (extend (t >> i));
    }

    FBit operator << (int i) const
    {
        T t = extend (v);
        return gen (extend (t << i));
    }

    FBit operator * (const FBit &y) const
    {
        T a = v;
        T b = y.v;
        //printf ("[a=%x, b=%x]", (unsigned) a, (unsigned) b);
        bool signab = 0;
        if (S)
        {
            bool signa = sign();
            bool signb = y.sign();
            if (signa) a = - extend (a);
            if (signb) b = - extend (b);
            signab = signa ^ signb;
        }

        T ab = a * b;

        T round_mask = (T) 1 << (FBIT - 1);
        if (FBIT > 8)
            ab += round_mask;
        ab = (ab >> FBIT) & mask;

        if (S && signab)
            ab = - ab;

        return gen (ab);
    }

    FBit sqrt () const
    {
        double d = std::ldexp ((double) asInt(), FBIT);
        double q = std::sqrt (d);
        return FBit::gen ((I) q);
    }

    FBit absdiff (const FBit &y) const
    {
        return *this > y ? *this - y : y - *this;
    }

    void print (bool raw = 0) const
    {
        //int fnibbles = (FBIT + 3) / 4;
        //T fmask = ((T) 1 << FBIT) - 1;
        //T t = v << (((unsigned) -FBIT) % 4);
        char name[5], *p = name;
        if (!S)        *p++ = 'U';
        if (FBIT == 8) *p++ = 'H';
        *p++ = IBIT ? 'K' : 'R';
        *p++ = '\0';

        char s_hex[20];
        sprintf (s_hex, "0x%0*x", (int) (2 * sizeof (I)), (unsigned) v);
        if (raw)
            printf ("%s", s_hex);
        else
            printf (" %s(%s), // % .7f",
                    name, s_hex, (double) *this);
    }
};

using uaccum = FBit<0, 16, 16>;
using accum = FBit<1, 16, 15>;
using ufract = FBit<0, 0, 16>;
using uhfract = FBit<0, 0, 8>;

uaccum  ukbits  (int i) { return uaccum::gen (i);  }
ufract  urbits  (int i) { return ufract::gen (i);  }
uhfract uhrbits (int i) { return uhfract::gen (i); }

unsigned bitsuk  (uaccum x)  { return x.asInt(); }
unsigned bitsur  (ufract x)  { return x.asInt(); }
unsigned bitsuhr (uhfract x) { return x.asInt(); }

template<typename T>
T fxbits (typename T::I i) { return T::gen (i); }

template<typename T>
typename T::I bitsfx (T x) { return x.asInt(); }

template<typename T>
static inline float fxtof (T x) { return (float) x; }
