#include <vector>
#include <cassert>

struct Radix
{
    struct Digit
    {
        using Event = void (*)(int, int);
        Event event = nullptr;
        int id, val = 0, lo, hi, step;
        Digit (Event e, int id, int lo, int hi, int step)
            : event(e), id(id), lo(lo), hi(hi), step(step)
        {
            assert (id >= 0);
            assert (hi >= lo);
            assert (step > 0);
            assert (lo % step == 0);
            assert (hi % step == 0);
            reset ();
        }

        void notify (Event ev = nullptr) const
        {
            if (ev)
                ev (id, val);
            else if (event)
                event (id, val);
        }

        void reset ()
        {
            val = lo;
            notify();
        }

        bool next ()
        {
            val = val + step > hi ? lo : val + step;
            notify ();
            return val == lo;
        }
    };

    std::vector<Digit> ds;
    Digit::Event event = nullptr;

    Radix (Digit::Event e = nullptr) : event (e) {}

    void addDigit (int lo, int hi, int step = 1)
    {
        Digit d = Digit (event, ds.size(), lo, hi, step);
        ds.push_back (d);
    }

    void addDigit (Digit::Event evt, int lo, int hi, int step = 1)
    {
        Digit d = Digit (evt, ds.size(), lo, hi, step);
        ds.push_back (d);
    }

    int size () const
    {
        return ds.size ();
    }

    void reset ()
    {
        for (Digit &d : ds)
            d.reset ();
    }

    bool next ()
    {
        for (Digit &d : ds)
        {
            bool carry = d.next();
            if (!carry)
                return false;
        }
        return true;
    }

    void notify (Digit::Event evt = nullptr) const
    {
        for (const Digit &d : ds)
            d.notify (evt);
    }

    void print () const
    {
        for (const Digit &d : ds)
            printf (" %d ", d.val);
    }
};
