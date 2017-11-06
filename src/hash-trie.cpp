// Copyright (C) 2016  Stefan Vargyas
// 
// This file is part of Hash-Trie.
// 
// Hash-Trie is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// Hash-Trie is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Hash-Trie.  If not, see <http://www.gnu.org/licenses/>.

#ifndef __GNUC__
#error we need a GCC compiler
#elif __cplusplus < 201103L
#error we need a GCC compiler which supports C++11
#endif

#ifndef __GNUC_MINOR__
#error __GNUC_MINOR__ is not defined
#endif

#ifndef __GNUC_PATCHLEVEL__
#error __GNUC_PATCHLEVEL__ is not defined
#endif

#define GCC_VERSION \
    (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)

// stev: default TRIE_SIZE is 32767                                             // [17, p. 158]
#ifndef CONFIG_HASH_TRIE_TRIE_SIZE
#define CONFIG_HASH_TRIE_TRIE_SIZE 32767
#endif

// stev: default TOLERANCE is 1000                                              // [24, p. 160]
#ifndef CONFIG_HASH_TRIE_TOLERANCE
#define CONFIG_HASH_TRIE_TOLERANCE 1000
#endif

// stev: default CHAR_TYPE is 0 (ie. plain char)
#ifndef CONFIG_HASH_TRIE_CHAR_TYPE
#define CONFIG_HASH_TRIE_CHAR_TYPE 0
#endif

#if CONFIG_HASH_TRIE_CHAR_TYPE == 0
#define SYS_CHAR_TYPE_NAME char
#define SYS_CHAR_TYPE_FMTS ""
#elif CONFIG_HASH_TRIE_CHAR_TYPE == 1
#define SYS_CHAR_TYPE_NAME wchar_t
#define SYS_CHAR_TYPE_FMTS "l"
#else
#error CONFIG_HASH_TRIE_CHAR_TYPE has to be \
    either 0 (for char) or 1 (for wchar_t)
#endif

// stev: employing strict types within the hash trie
// class implies bounds checking on the arrays used
#ifdef CONFIG_HASH_TRIE_STRICT_TYPES
#define CONFIG_HASH_TRIE_ARRAY_BOUNDS
#endif

#include <getopt.h>
#include <execinfo.h>

#ifdef CONFIG_HASH_TRIE_STATISTICS
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include <cstring>
#include <cstdio>
#include <cstdarg>
#include <cmath>

#include <limits>
#include <type_traits>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

#include "demangle.hpp"

#define STRINGIFY0(S) #S
#define STRINGIFY(S)  STRINGIFY0(S)

const char program[] = STRINGIFY(PROGRAM);
const char verdate[] = "v0.1 2015-09-23"; // $ date +%F

const char license[] =
"Copyright (C) 2016  Stefan Vargyas.\n"
"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
"This is free software: you are free to change and redistribute it.\n"
"There is NO WARRANTY, to the extent permitted by law.\n";

#ifdef DEBUG
enum debug_t {
    debug_probing = 1 << 0,
};
#endif

struct global_options_t
{
#ifdef CONFIG_HASH_TRIE_STATISTICS
    enum class time_type_t {
        real, user, sys
    };
#endif

    global_options_t() :
        dump_backtrace(false),
        print_chars(false),
        print_words(true),
#ifdef CONFIG_HASH_TRIE_STATISTICS
        print_stats(false),
#endif
        quiet(false)
    {}

#ifdef DEBUG
    unsigned debug = 0;
#endif
    unsigned dump_backtrace : 1;
    unsigned print_chars : 1;
    unsigned print_words : 1;
#ifdef CONFIG_HASH_TRIE_STATISTICS
    unsigned print_stats : 1;
#endif
    unsigned quiet : 1;

#ifdef CONFIG_HASH_TRIE_STATISTICS
    time_type_t time_type = time_type_t::real;
#endif
};

global_options_t globals;

#define PRINTF_FMT(F) \
    __attribute__ ((format(printf, F, F + 1)))
#define NORETURN \
    __attribute__ ((noreturn))
#define UNUSED \
    __attribute__ ((unused))

// stev: variadic preprocessor macros, __VA_ARGS__:
// ISO/IEC 14882:2011, 16.3 Macro replacement, pts.
// 4, 5 and 10; 16.3.1 Argument substitution, pt. 2;
// 16.3.5 Scope of macro definitions, pt. 9.

#define SYS_UNEXPECT_ERR(M, ...) \
    do { \
        Sys::unexpect_error(__FILE__, __LINE__, M, ## __VA_ARGS__); \
    } \
    while (0)

#ifdef DEBUG
# define SYS_ASSERT(E) \
    do { \
        if (!(E)) \
            Sys::assert_failed(__FILE__, __LINE__, #E); \
    } \
    while (0)
#else
# define SYS_ASSERT(E) \
    do {} while (0)
#endif

#define CXX_ASSERT(E) \
    static_assert(E, #E)

namespace Sys {

using char_t = SYS_CHAR_TYPE_NAME;

using ostream = std::basic_ostream<char_t>;
using istream = std::basic_istream<char_t>;

// stev: avoid mixing std::cin with std::wcin;
// std::cout with std::wcout and std::cerr with
// std::wcerr -- see ISO/IEC 9899:2011, 7.21.2
// Streams, pts. 5 and 6 and ISO/IEC 14882:2011,
// 27.4.1 Standard iostream objects/ Overview
// pt. 3

#if CONFIG_HASH_TRIE_CHAR_TYPE == 0

istream& cin  = std::cin;
ostream& cout = std::cout;
ostream& cerr = std::cerr;

#elif CONFIG_HASH_TRIE_CHAR_TYPE == 1

istream& cin  = std::wcin;
ostream& cout = std::wcout;
ostream& cerr = std::wcerr;

#endif

void die(const char* msg, ...)
    PRINTF_FMT(1)
    NORETURN;

void die(const char* msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    Sys::cerr << program << ": fatal error: " << buf << std::endl;

    if (globals.dump_backtrace) {
        const size_t n = 128;
        void* b[n];
        auto s = backtrace(b, n);
        for (void
            **p = b,
            **e = p + (s % (n + 1));
            p < e; ++ p)
            Sys::cerr << *p << '\n';
    }

    exit(127);
}

void assert_failed(const char* file, int line, const char* expr)
    NORETURN;

void assert_failed(const char* file, int line, const char* expr)
{
    char buf[256];

    snprintf(buf, sizeof buf - 1, "%s:%d: %s", file, line, expr);
    buf[255] = 0;

    die("assertion failed: %s", buf);
}

void unexpect_error(const char* file, int line, const char* msg, ...)
    PRINTF_FMT(3)
    NORETURN;

void unexpect_error(const char* file, int line, const char* msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    die("unexpected error:%s:%d: %s", file, line, buf);
}

template<typename T>
void error(const char* msg, ...)
    PRINTF_FMT(1)
    NORETURN;

template<typename T>
void error(const char* msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    throw T(buf);
}

#ifdef CONFIG_HASH_TRIE_STATISTICS

struct clocks_t
{
public:
    class type_t
    {
    public:
        CXX_ASSERT(999999U < SIZE_MAX); // stev: not '<='

        static constexpr size_t max_usecs = 999999U;

        type_t() :
            secs(0),
            usecs(0)
        {}
        type_t(size_t _secs, size_t _usecs) :
            secs(_secs),
            usecs(_usecs)
        { SYS_ASSERT(usecs <= max_usecs); }

        type_t(const type_t& a)
        { assign(a); }

        type_t& operator=(const type_t& a)
        { assign(a); return *this; }

        type_t& operator+=(const type_t& a)
        { add(a); return *this; }

        type_t& operator-=(const type_t& a)
        { sub(a); return *this; }

        type_t operator+(const type_t& a) const
        { return type_t{*this} += a; }

        type_t operator-(const type_t& a) const
        { return type_t{*this} -= a; }

        friend
        std::ostream& operator<<(
            std::ostream& ost, const type_t& obj)
        { obj.print(ost); return ost; }

    private:
        friend struct clock_t;

        void assign(const type_t& a);

        void add(const type_t& a);
        void sub(const type_t& a);

        void print(std::ostream& ost) const;

        size_t secs;
        size_t usecs;
    };

    clocks_t() :
        real(0, 0),
        user(0, 0),
        sys (0, 0)
    {}
    clocks_t(
        type_t _real,
        type_t _user,
        type_t _sys) :
        real(_real),
        user(_user),
        sys(_sys)
    {}

    clocks_t& operator+=(const clocks_t& a)
    { add(a); return *this; }

    clocks_t& operator-=(const clocks_t& a)
    { sub(a); return *this; }

    clocks_t operator+(const clocks_t& a) const
    { return clocks_t{*this} += a; }

    clocks_t operator-(const clocks_t& a) const
    { return clocks_t{*this} -= a; }

    void print(std::ostream& ost) const
    { ost << real << ',' << user << ',' << sys; }

    friend
    std::ostream& operator<<(
        std::ostream& ost, const clocks_t& obj)
    { obj.print(ost); return ost; }

    type_t real;
    type_t user;
    type_t sys;

private:
    void add(const clocks_t& a)
    { real += a.real; user += a.user; sys += a.sys; }

    void sub(const clocks_t& a)
    { real -= a.real; user -= a.user; sys -= a.sys; }
};

inline void clocks_t::type_t::assign(const type_t& a)
{
    SYS_ASSERT(
        a.usecs <= max_usecs);
    secs = a.secs;
    usecs = a.usecs;
}

inline void clocks_t::type_t::add(const type_t& a)
{
    SYS_ASSERT(
        secs <= SIZE_MAX - a.secs);
    SYS_ASSERT(
        usecs <= max_usecs &&
        a.usecs <= max_usecs);

    secs += a.secs;
    usecs += a.usecs;

    const auto m = max_usecs + 1;
    // (1): usecs, a.usecs < m
    //      => usecs + a.usecs < 2 * m
    // (2): usecs + a.usecs >= m
    //      => (usecs + a.usecs) % m = (usecs + a.usecs) - m:
    //      by 0 < b <= a < 2 * b => a % b = a - b (see a
    //      proof of this relation in 'HashTrie<>::find')
    if (usecs >= m) {
        usecs -= m;
        secs ++;
    }
}

inline void clocks_t::type_t::sub(const type_t& a)
{
    SYS_ASSERT(
        secs >= a.secs);
    SYS_ASSERT(
        usecs <= max_usecs &&
        a.usecs <= max_usecs);

    secs -= a.secs;

    if (usecs < a.usecs) {
        SYS_ASSERT(secs > 0);
        // usecs + max_usecs >= max_usecs >= a.usecs
        usecs += max_usecs;
        secs --;
    }
    usecs -= a.usecs;
}

void clocks_t::type_t::print(std::ostream& ost) const
{
    using namespace std;
    auto c = ost.fill();
    ost << secs
        << '.'
        << setw(6)
        << setfill('0')
        << right
        << usecs
        << setfill(c);
}

struct clock_t
{
    using flags_t = size_t;

    enum : flags_t {
        real_time = 1 << 0,
        user_time = 1 << 1,
        sys_time  = 1 << 2,
        useconds  = 1 << 3, 
    };

    clock_t(const clocks_t& _clocks, flags_t _flags) :
        clocks(_clocks), flags(_flags)
    {} 

    void print(std::ostream& ost) const
    {
        SYS_ASSERT(
            bool(flags & real_time) +
            bool(flags & user_time) +
            bool(flags & sys_time) < 2);

        clocks_t::type_t time;

        if (flags & real_time)
            time = clocks.real;
        else
        if (flags & user_time)
            time = clocks.user;
        else
        if (flags & sys_time)
            time = clocks.sys;
        else
            time = clocks.real;

        if (flags & useconds) {
            auto u = time.usecs;
            auto s = time.secs;

            const auto m =
                clocks_t::type_t::max_usecs + 1;
            SYS_ASSERT(s <= SIZE_MAX / m);
            s *= m;

            SYS_ASSERT(s <= SIZE_MAX - u);
            // s + u <= SIZE_MAX
            ost << u + s;
        }
        else
            ost << time;
    }

    friend
    std::ostream& operator<<(
        std::ostream& ost, const clock_t& obj)
    { obj.print(ost); return ost; }

    clocks_t clocks;
    flags_t  flags;
};

clock_t clock(const clocks_t& clocks, clock_t::flags_t flags)
{ return clock_t{clocks, flags}; }

class utime_t
{
public:
    clocks_t operator()() const;
    clocks_t operator()(const utime_t& t) const;

private:
    struct timeval_t : timeval
    {
        timeval_t();
        timeval_t(const timeval&);
        timeval_t(time_t, suseconds_t);
        void operator=(const timeval&);
        timeval_t operator-(const timeval_t&) const;
        timeval_t operator+(const timeval_t&) const;
    };
    struct tms_t
    {
        tms_t();
        timeval_t tms_rtime;
        timeval_t tms_utime;
        timeval_t tms_stime;
        timeval_t tms_cutime;
        timeval_t tms_cstime;
    };

    struct clocks_impl_t : clocks_t
    {
        clocks_impl_t(const tms_t&, const tms_t&);
        static type_t secs(timeval_t);
    };

    static constexpr auto limit_usecs = 1000000;

    tms_t  tms;
};

inline clocks_t utime_t::operator()() const
{ return clocks_impl_t(tms, tms_t()); }

inline clocks_t utime_t::operator()(const utime_t& t) const
{ return clocks_impl_t(tms, t.tms); }

inline utime_t::timeval_t::timeval_t()
{}

inline utime_t::timeval_t::timeval_t(const timeval& t)
{
    tv_sec = t.tv_sec;
    tv_usec = t.tv_usec;
}

inline utime_t::timeval_t::timeval_t(::time_t sec, suseconds_t usec)
{
    tv_sec = sec;
    tv_usec = usec;
}

inline void utime_t::timeval_t::operator=(const timeval& t)
{
    tv_sec = t.tv_sec;
    tv_usec = t.tv_usec;
}

inline utime_t::timeval_t utime_t::timeval_t::operator-(
    const timeval_t& t) const
{
    timeval_t r(
        tv_sec  - t.tv_sec,
        tv_usec - t.tv_usec);
    if (r.tv_usec < 0) {
        r.tv_usec += limit_usecs;
        r.tv_sec --;
        if (r.tv_sec < 0) {
            r.tv_sec = 0;
            r.tv_usec = 0;
        }
    }
    return r;
}

inline utime_t::timeval_t utime_t::timeval_t::operator+(
    const timeval_t& t) const
{
    timeval_t r(
        tv_sec  + t.tv_sec,
        tv_usec + t.tv_usec);
    if (r.tv_usec >= limit_usecs) {
        r.tv_usec -= limit_usecs;
        r.tv_sec ++;
    }
    return r;
}

inline utime_t::tms_t::tms_t()
{
    gettimeofday(&tms_rtime, 0);
    rusage r;
    getrusage(RUSAGE_SELF, &r);
    tms_utime = r.ru_utime;
    tms_stime = r.ru_stime;
    getrusage(RUSAGE_CHILDREN, &r);
    tms_cutime = r.ru_utime;
    tms_cstime = r.ru_stime;
}

inline utime_t::clocks_impl_t::clocks_impl_t(
    const tms_t& b, const tms_t& a) :
    clocks_t(
        secs((a.tms_rtime  - b.tms_rtime)),
        secs((a.tms_utime  - b.tms_utime) +
             (a.tms_cutime - b.tms_cutime)),
        secs((a.tms_stime  - b.tms_stime) +
             (a.tms_cstime - b.tms_cstime)))
{}

inline clocks_t::type_t utime_t::clocks_impl_t::secs(
    timeval_t t)
{
    SYS_ASSERT(t.tv_sec >= 0);
    SYS_ASSERT(t.tv_usec >= 0);
    if (t.tv_usec >= limit_usecs) {
        t.tv_usec -= limit_usecs;
        t.tv_sec ++;
    }
    return type_t{
        static_cast<size_t>(t.tv_sec),
        static_cast<size_t>(t.tv_usec)};
}

#endif // CONFIG_HASH_TRIE_STATISTICS

} // namespace Sys

namespace Ext {

template<typename T, typename U>
constexpr bool is_same()
{ return std::is_same<T, U>::value; }

template<typename T, typename U>
constexpr bool is_cv_same()
{ return is_same<
    typename std::remove_cv<T>::type,
    typename std::remove_cv<U>::type>(); }

template<typename T>
constexpr bool is_int()
{ return std::numeric_limits<T>::is_integer; }

template<typename T>
constexpr bool is_signed()
{ return std::numeric_limits<T>::is_signed; }

template<typename T>
constexpr bool is_signed_int()
{ return is_int<T>() && is_signed<T>(); }

template<typename T>
constexpr bool is_unsigned_int()
{ return is_int<T>() && !is_signed<T>(); }

template<typename T>
constexpr bool is_enum()
{ return std::is_enum<T>::value; }

template<typename T>
constexpr T min()
{ return std::numeric_limits<T>::min(); }

template<typename T>
constexpr T max()
{ return std::numeric_limits<T>::max(); }

template<typename T>
constexpr size_t digits()
{ return std::numeric_limits<T>::digits; }

template<typename T, typename U>
constexpr bool is_wider()
{ return
    is_signed<T>() == is_signed<U>() &&
    digits<T>() > digits<U>(); }

// stev: signed_type_t<>, unsigned_type_t<> and
// same_type_t<> are needed as tags in several
// tag-dispatching idiom instances below

template<typename T>
using signed_type_t =
    typename std::conditional<
        !is_signed<T>(),
        std::false_type,
        std::true_type>::type;

template<typename T>
using unsigned_type_t =
    typename std::conditional<
        is_signed<T>(),
        std::false_type,
        std::true_type>::type;

template<
    typename T, typename U>
using same_type_t =
    typename std::conditional<
        !is_cv_same<T, U>(),
        std::false_type,
        std::true_type>::type;

#if GCC_VERSION < 40900

// stev: GCC 4.8.x does not properly defines
// std::make_signed<> and std::make_unsigned<>

template<typename T>
struct make_type_t
{
    typedef
        typename std::make_signed<T>::type
        signed_t;
    typedef
        typename std::make_unsigned<T>::type
        unsigned_t;
};

template<>
struct make_type_t<wchar_t>
{
    typedef
        signed wchar_t
        signed_t;
    typedef
        unsigned wchar_t
        unsigned_t;
};

template<typename T>
using signed_t = typename make_type_t<T>::signed_t;

template<typename T>
using unsigned_t = typename make_type_t<T>::unsigned_t;

#else

template<typename T>
using signed_t = typename std::make_signed<T>::type;

template<typename T>
using unsigned_t = typename std::make_unsigned<T>::type;

#endif

// stev: partial implementation of size_cast: only for
// integer types of narrower or equal width than size_t
template<typename T>
inline typename std::enable_if<
    is_signed_int<T>(),
size_t>::type
    size_cast(T v)
{
    CXX_ASSERT(
        digits<T>() <=
        digits<size_t>() &&
        !is_signed<size_t>());
    SYS_ASSERT(v >= 0);
    return static_cast<size_t>(v);
}

template<typename T>
inline typename std::enable_if<
    is_unsigned_int<T>(),
size_t>::type
    size_cast(T v)
{
    CXX_ASSERT(
        digits<T>() <=
        digits<size_t>() &&
        !is_signed<size_t>());
    return static_cast<size_t>(v);
}

template<typename T>
inline typename std::enable_if<
    is_enum<T>(),
size_t>::type
    size_cast(T v)
{
    typedef
        typename
            std::underlying_type<T>::type
        enum_t;
    CXX_ASSERT(
        is_int<enum_t>());
    return
        size_cast(
            static_cast<enum_t>(v));
}

template<typename T, size_t N>
class array_t
{
public:
    typedef T elem_t;
    typedef T type_t[N];

    array_t(type_t& _a) : a(_a) {}

    template<
        typename K,
        typename = typename std::enable_if<
            is_int<K>() || is_enum<K>()>::type>
    elem_t operator[](K k) const
    { auto i = size_cast(k); SYS_ASSERT(i < N);
        return a[i]; }

private:
    elem_t* a;
};

template<typename T, size_t N, size_t M>
class array_t<T[M], N>
{
public:
    typedef array_t<T, M> elem_t;
    typedef T type_t[N][M];

    array_t(type_t& _a) : a(_a) {}

    template<
        typename K,
        typename = typename std::enable_if<
            is_int<K>() || is_enum<K>()>::type>
    elem_t operator[](K k) const
    { auto i = size_cast(k); SYS_ASSERT(i < N);
        return elem_t{a[i]}; }

private:
    typedef T inner_t[M];
    inner_t* a;
};

template<typename T, size_t N>
inline array_t<T, N> array(T (&v)[N])
{ return array_t<T, N>(v); }

template<typename T, size_t N>
constexpr size_t array_size(T (&)[N])
{ return N; }

template<typename T>
struct wider_t;

template<>
struct wider_t<signed char>
{ typedef int type_t; };

template<>
struct wider_t<signed short>
{ typedef int type_t; };

template<>
struct wider_t<signed int>
{ typedef signed long long type_t; };

template<>
struct wider_t<unsigned char>
{ typedef unsigned int type_t; };

template<>
struct wider_t<unsigned short>
{ typedef unsigned int type_t; };

template<>
struct wider_t<unsigned int>
{ typedef unsigned long long type_t; };

template<>
struct wider_t<char>
{
    typedef
        typename wider_t<
            typename std::conditional<
                !is_signed<char>(),
                unsigned char,
                signed char
            >::type
        >::type_t
        type_t;
};

template<typename T>
struct assert_wider_t
{
    typedef T type_t;
    typedef typename
        wider_t<type_t>::type_t
        wider_t;

    enum {
        value =
            digits<wider_t>() >
            digits<type_t>()
    };
};

CXX_ASSERT(
    assert_wider_t<char>::value &&

    assert_wider_t<signed char>::value &&
    assert_wider_t<signed short>::value &&
    assert_wider_t<signed int>::value &&

    assert_wider_t<unsigned char>::value &&
    assert_wider_t<unsigned short>::value &&
    assert_wider_t<unsigned int>::value);

enum class op_type_t {
    add, sub,
};

const char* const op_types[] = {
    "add", // op_type_t::add
    "sub"  // op_type_t::sub
}; 

template<
    op_type_t t,
    typename T,
    typename U = T,
    typename = typename std::enable_if<
        is_int<T>() &&
        is_int<U>()>::type>
class operation_type_t
{
private:
    typedef
        T operand1_t;
    typedef
        U operand2_t;
    typedef
        typename std::conditional<
            t == op_type_t::add,
            decltype(operand1_t{} + operand2_t{}),
            decltype(operand1_t{} - operand2_t{})>::type
        operator_t;
    typedef
        typename std::conditional<
            digits<operand1_t>() >=
            digits<operand2_t>(),
            operand1_t,
            operand2_t
        >::type
        operand_t;

    CXX_ASSERT(
        digits<operator_t>() >=
        digits<operand_t>());

    typedef
        typename std::conditional<
            digits<operand_t>() ==
            digits<operator_t>(),
            typename wider_t<operator_t>::type_t,
            operator_t
        >::type
        wider_t;
    typedef 
        signed_t<wider_t>
        result_t;

    CXX_ASSERT(
        digits<operand_t>() <
        digits<result_t>());

    template<typename C>
    static void print(
        std::basic_ostream<C>& ost, std::false_type)
    {
        CXX_ASSERT((
            !is_cv_same<operand1_t, operand2_t>()));
        ost << array(op_types)[t] << '\t'
            << demangle<operand1_t>() << '\t'
            << demangle<operand2_t>() << '\t'
            << demangle<operator_t>() << '\t'
            << demangle<wider_t>() << '\t'
            << demangle<type_t>() << '\n';
    }

    template<typename C>
    static void print(
        std::basic_ostream<C>& ost, std::true_type)
    {
        CXX_ASSERT((
            is_cv_same<operand1_t, operand2_t>()));
        ost << array(op_types)[t] << '\t'
            << demangle<operand1_t>() << '\t'
            << demangle<operator_t>() << '\t'
            << demangle<wider_t>() << '\t'
            << demangle<type_t>() << '\n';
    }

public:
    typedef result_t type_t;

    template<typename C>
    static void print(std::basic_ostream<C>& ost)
    { print(ost, same_type_t<operand1_t, operand2_t>{}); }
};

template<
    typename T>
using addition_type_t =
    operation_type_t<op_type_t::add, T>;

template<
    typename T>
using subtraction_type_t =
    operation_type_t<op_type_t::sub, T>;

template<
    typename T>
using addition_t =
    typename addition_type_t<T>::type_t;

template<
    typename T>
using subtraction_t =
    typename subtraction_type_t<T>::type_t;

template<
    typename R,
    typename T1,
    typename T2,
    typename = typename std::enable_if<
        is_int<R>() &&
        is_unsigned_int<T1>() &&
        is_unsigned_int<T2>()>::type>
inline constexpr R
    add(T1 a, T2 b)
{
    // stev: the static assertion below
    // ensures that 'add' avoids narrowing
    // casts on its input args -- which are
    // supposed to be of identical types.
    CXX_ASSERT((
        is_cv_same<T1, T2>()));
    CXX_ASSERT(
        digits<R>() > digits<T1>());
    CXX_ASSERT(
        min<R>() < 0 &&
        max<R>() > 0);
    CXX_ASSERT(
        min<T1>() == 0 &&
        max<R>() / 2 >= R(max<T1>()));
    // 0 <= a, b <= m
    // => 0 <= a + b <= 2 * m
    return R(a) + R(b);
}

template<
    typename R,
    typename T1,
    typename T2,
    typename = typename std::enable_if<
        is_signed_int<R>() &&
        is_unsigned_int<T1>() &&
        is_unsigned_int<T2>()>::type>
inline constexpr R
    sub(T1 a, T2 b, std::false_type)
{
    // stev: the static assertion below
    // ensures that 'sub' avoids narrowing
    // casts on its input args -- which are
    // supposed to be of identical types.
    CXX_ASSERT((
        is_cv_same<T1, T2>()));
    CXX_ASSERT(
        digits<R>() > digits<T1>());
    CXX_ASSERT(
        min<R>() < 0 &&
        max<R>() > 0);
    CXX_ASSERT(
        min<T1>() == 0 &&
        max<R>() > R(max<T1>()) &&
        min<R>() < -R(max<T1>()));
    // 0 <= a, b <= m
    // => -m <= a - b <= m
    return R(a) - R(b);
}

template<
    typename R,
    typename T1,
    typename T2,
    typename = typename std::enable_if<
        is_unsigned_int<R>() &&
        is_unsigned_int<T1>() &&
        is_unsigned_int<T2>()>::type>
inline constexpr R
    sub(T1 a, T2 b, std::true_type)
{
    // stev: the static assertion below
    // ensures that 'sub' avoids narrowing
    // casts on its input args -- which are
    // supposed to be of identical types.
    CXX_ASSERT((
        is_cv_same<T1, T2>()));
    CXX_ASSERT(
        digits<R>() >= digits<T1>());
    CXX_ASSERT(
        min<R>() == 0 &&
        max<R>() > 0);
    CXX_ASSERT(
        min<T1>() == 0 &&
        max<R>() >= R(max<T1>()));
    // 0 <= a, b <= m
    // => -m <= a - b <= m
    return
        R(a) > R(b) ? R(a) - R(b) : R(b) - R(a);
}

template<
    typename R,
    typename T1,
    typename T2,
    typename = typename std::enable_if<
        is_unsigned_int<T1>() &&
        is_unsigned_int<T2>()>::type>
inline constexpr R
    sub(T1 a, T2 b)
{ return sub<R>(a, b, unsigned_type_t<R>{}); }

template<
    typename C, typename T, typename A>
inline size_t offset(
    const std::basic_string<C, T, A>& obj,
    const C* ptr)
{ return size_cast(ptr - obj.data()); }

template<
    typename C, typename T, typename A>
std::basic_istream<C, T>& getline(
    std::basic_istream<C, T>& ist,
    std::basic_string<C, T, A>& str,
    C delim)
{
    auto sz = str.capacity();
    SYS_ASSERT(sz <= str.max_size());
    // stev: enlarge 'str' to full capacity
    str.resize(sz);

    // stev: new in C++11 standard is that
    // std::string of char-like objects is
    // required to store contiguously its
    // elements -- see ISO/IEC 14882:2011,
    // 21.4.1 pt. 5.
    C *ptr = &str[0];
    while (true) {
        ist.getline(ptr, sz, delim);
        auto n = size_cast(ist.gcount());
        // (1) eof:  partial final line
        // (2) fail: partial long line
        // (3) else: complete line ending in '\n'
        const auto eof = ist.eof();
        const auto fail = ist.fail();

        if (!eof && !fail) {
            // stev: account for '\n'
            SYS_ASSERT(n > 0);
            n --;
        }
        SYS_ASSERT(n <= sz);
        ptr += n;
        sz -= n;
        if (eof || !fail)
            break;
        // stev: !eof && fail
        ist.clear(
            ist.rdstate() &
            ~std::ios::failbit);

        if (sz > 1)
            continue;
        sz = str.size();
        if (sz == 0)
            sz = 1;
        SYS_ASSERT(
            sz <= max<size_t>() / 2);
        SYS_ASSERT(
            sz <= str.max_size() / 2);
        sz *= 2;
        auto k = offset(str, ptr);
        str.resize(sz);
        ptr = &str[0] + k;
        sz -= k;
    }
    sz = offset(str, ptr);
    SYS_ASSERT(sz <= str.size());
    str.resize(sz);

    return ist;
}

template<
    typename C, typename T, typename A>
inline std::basic_istream<C, T>& getline(
    std::basic_istream<C, T>& ist, std::basic_string<C, T, A>& str)
{ return Ext::getline(ist, str, ist.widen('\n')); }

template<typename V>
struct box_traits_t
{
    static constexpr V min = min<V>();
    static constexpr V max = max<V>();

    CXX_ASSERT(is_int<V>());
};

template<
    typename V,
    typename T = box_traits_t<V>>
class box_t final
{
public:
    typedef
        T traits_t;
    typedef
        typename
            std::enable_if<
                is_int<V>(),
                V>::type
            type_t;
    typedef
        typename
            std::conditional<
                is_signed<V>(),
                ssize_t,
                size_t>::type
            size_type_t;

    template<typename V2>
    static constexpr bool eq_sign()
    { return is_signed<V>() == is_signed<V2>(); }

    template<typename V2>
    static constexpr bool is_type()
    { return
        (is_int<V2>()
            && !is_wider<V2, type_t>())
        || is_cv_same<V2, size_type_t>(); }

    template<typename V2>
    static constexpr bool is_sign_type()
    { return eq_sign<V2>() && is_type<V2>(); }

    template<typename V2>
    static constexpr bool is_sign_int()
    { return eq_sign<V2>() && is_int<V2>(); }

    // [A] operations on scalars:
    // (1) assign     is_int
    // (2) test       is_int
    // (3) compare    is_int && eq_sign
    // (4) arithmetic is_type

    // [B] operations on boxes:
    // (1) assign     is_int
    // (2) test       is_type && eq_sign
    // (3) compare    is_type && eq_sign
    // (4) arithmetic is_type && eq_sign

    box_t(box_t&&) = default;
    box_t(const box_t&) = default;

    box_t& operator=(box_t&&) = default;
    box_t& operator=(const box_t&) = default;

    box_t() :
        v(traits_t::min)
    {}

    template<
        typename V2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t(V2 a) :
        v(traits_t::min)
    { assign(a); }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t(box_t<V2, T2>&& a) :
        v(traits_t::min)
    { assign(a.v); }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t(const box_t<V2, T2>& a) :
        v(traits_t::min)
    { assign(a.v); }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t& operator=(box_t<V2, T2>&& a)
    { assign(a.v); return *this; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t& operator=(const box_t<V2, T2>& a)
    { assign(a.v); return *this; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_int<V2>()>::type>
    box_t& operator=(V2 a)
    { assign(a); return *this; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_cv_same<V2, size_type_t>() ||
            is_cv_same<V2, bool>()>::type>
    operator V2() const
    { return cast<V2>(same_type_t<V2, bool>{}); }

    box_t& operator++()
    { inc(); return *this; }

    box_t operator++(int)
    { auto r = *this; inc(); return r; }

    box_t& operator--()
    { dec(); return *this; }

    box_t operator--(int)
    { auto r = *this; dec(); return r; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    box_t& operator+=(V2 a)
    { add(a, signed_type_t<V2>{}); return *this; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    box_t& operator-=(V2 a)
    { sub(a, signed_type_t<V2>{}); return *this; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    box_t& operator+=(box_t<V2, T2> a)
    { add(a.v, signed_type_t<V2>{}); return *this; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    box_t& operator-=(box_t<V2, T2> a)
    { sub(a.v, signed_type_t<V2>{}); return *this; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    box_t operator+(V2 a) const
    { return box_t{v} += a; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    box_t operator-(V2 a) const
    { return box_t{v} -= a; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    box_t operator+(box_t<V2, T2> a) const
    { return operator+(a.v); }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    box_t operator-(box_t<V2, T2> a) const
    { return operator-(a.v); }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    bool operator==(V2 a) const
    { return v == a; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_type<V2>()>::type>
    bool operator!=(V2 a) const
    { return v != a; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    bool operator==(box_t<V2, T2> a) const
    { return v == a.v; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_int<V2>()>::type>
    bool operator!=(box_t<V2, T2> a) const
    { return v != a.v; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_sign_int<V2>()>::type>
    bool operator<(V2 a) const
    { return v < a; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_sign_int<V2>()>::type>
    bool operator>(V2 a) const
    { return v > a; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_sign_int<V2>()>::type>
    bool operator<=(V2 a) const
    { return v <= a; }

    template<
        typename V2,
        typename = typename std::enable_if<
            is_sign_int<V2>()>::type>
    bool operator>=(V2 a) const
    { return v >= a; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    bool operator<(box_t<V2, T2> a) const
    { return v < a.v; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    bool operator>(box_t<V2, T2> a) const
    { return v > a.v; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    bool operator<=(box_t<V2, T2> a) const
    { return v <= a.v; }

    template<
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_sign_type<V2>()>::type>
    bool operator>=(box_t<V2, T2> a) const
    { return v >= a.v; }

    template<
        typename R,
        typename V2,
        typename = typename std::enable_if<
            is_signed_int<R>() &&
            is_cv_same<V, V2>()>::type>
    constexpr R add(V2 a) const
    { return Ext::add<R>(v, a); }

    template<
        typename R,
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_signed_int<R>() &&
            is_cv_same<V, V2>()>::type>
    constexpr R add(box_t<V2, T2> a) const
    { return Ext::add<R>(v, a.v); }

    template<
        typename R,
        typename V2,
        typename = typename std::enable_if<
            is_signed_int<R>() &&
            is_cv_same<V, V2>()>::type>
    constexpr R sub(V2 a) const
    { return Ext::sub<R>(v, a); }

    template<
        typename R,
        typename V2,
        typename T2,
        typename = typename std::enable_if<
            is_signed_int<R>() &&
            is_cv_same<V, V2>()>::type>
    constexpr R sub(box_t<V2, T2> a) const
    { return Ext::sub<R>(v, a.v); }

    template<typename C> friend
    std::basic_ostream<C>& operator<<(
        std::basic_ostream<C>& ost, box_t a)
    { ost << a.v; return ost; }

private:
    template<typename V2, typename T2>
    friend class box_t;

    template<typename V2>
    void assign(V2 a)
    {
        CXX_ASSERT(
            is_int<V2>());
        SYS_ASSERT(
            a >= traits_t::min &&
            a <= traits_t::max);
        v = a;
    }

    template<typename V2>
    V2 cast(std::false_type) const
    {
        CXX_ASSERT(
            is_int<V2>());
        SYS_ASSERT(
            v >= min<V2>() &&
            v <= max<V2>());
        return static_cast<V2>(v);
    }

    template<typename V2>
    V2 cast(std::true_type) const
    {
        CXX_ASSERT((
            is_cv_same<V2, bool>()));
        return v;
    }

    void inc()
    {
        SYS_ASSERT(
            v >= traits_t::min &&
            v < traits_t::max);
        v ++; 
    }

    void dec()
    {
        SYS_ASSERT(
            v > traits_t::min &&
            v <= traits_t::max);
        v --;
    }

    template<typename V2>
    void add(V2 a, std::false_type)
    {
        CXX_ASSERT(
            is_unsigned_int<V2>());
        SYS_ASSERT(
            v >= traits_t::min &&
            v <= traits_t::max &&
            a <= traits_t::max - v);
        v += a;
    }

    template<typename V2>
    void add(V2 a, std::true_type)
    {
        CXX_ASSERT(
            is_signed_int<V2>());
        if (a >= 0)
            add(static_cast<unsigned_t<V2>>(a),
                std::false_type{});
        else {
            const auto b = a == min<V2>();
            if (b) a ++;
            sub(static_cast<unsigned_t<V2>>(-a),
                std::false_type{});
            if (b) dec();
        }
    }

    template<typename V2>
    void sub(V2 a, std::false_type)
    {
        CXX_ASSERT(
            is_unsigned_int<V2>());
        SYS_ASSERT(
            v >= traits_t::min &&
            v <= traits_t::max &&
            a <= v - traits_t::min);
        v -= a;
    }

    template<typename V2>
    void sub(V2 a, std::true_type)
    {
        CXX_ASSERT(
            is_signed_int<V2>());
        if (a >= 0)
            sub(static_cast<unsigned_t<V2>>(a),
                std::false_type{});
        else {
            const auto b = a == min<V2>();
            if (b) a ++;
            add(static_cast<unsigned_t<V2>>(-a),
                std::false_type{});
            if (b) inc();
        }
    }

    type_t v;
};

} // namespace Ext

namespace HashTrie {

// Knuth: Literate Programming
// Chapter 6: Programming Pearls, Continued: Common Words (1986), pp. 151-167

// Home page: http://www-cs-faculty.stanford.edu/~uno/lp.html
// Errata:    http://www-cs-faculty.stanford.edu/~uno/lp-err.ps.gz (from 31 December 2002)

struct Error : std::runtime_error
{
    Error(const char* err) : std::runtime_error{err} {}
};

enum types_t {
    count_type,
    pointer_type,
    letter_type,
    cell_type
};

template<typename T, types_t t>
struct types_traits_t;

template<typename T>
struct types_traits_t<T, count_type>
{
    typedef unsigned short base_t;
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    typedef Ext::box_t<
        base_t, types_traits_t<T, count_type>> type_t;
#else
    typedef base_t type_t;
#endif

    // count_t: [0..max_count]                                                  // [32, p. 162]
    static constexpr size_t min = 0;
    static constexpr size_t max = T::max_count;

    CXX_ASSERT(
        min <= max &&
        min >= Ext::min<base_t>() &&
        max <= Ext::max<base_t>());
};

template<typename T>
struct types_traits_t<T, pointer_type>
{
    typedef unsigned short base_t;
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    typedef Ext::box_t<
        base_t, types_traits_t<T, pointer_type>> type_t;
#else
    typedef base_t type_t;
#endif

    // pointer_t: [0..trie_size]                                                // [17, p. 158]
    static constexpr size_t min = 0;
    static constexpr size_t max = T::trie_size;

    CXX_ASSERT(
        min <= max &&
        min >= Ext::min<base_t>() &&
        max <= Ext::max<base_t>());
};

template<typename T>
struct types_traits_t<T, letter_type>
{
    typedef unsigned char base_t;
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    typedef Ext::box_t<
        base_t, types_traits_t<T, letter_type>> type_t;
#else
    typedef base_t type_t;
#endif

    // letter_t: [min_letter..max_letter]
    static constexpr size_t min = T::min_letter;
    static constexpr size_t max = T::max_letter;

    CXX_ASSERT(
        min <= max &&
        min >= Ext::min<base_t>() &&
        max <= Ext::max<base_t>());
};

template<typename T>
struct types_traits_t<T, cell_type>
{
    typedef unsigned char base_t;
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    typedef Ext::box_t<
        base_t, types_traits_t<T, cell_type>> type_t;
#else
    typedef base_t type_t;
#endif

    // cell_t: [empty_slot..header]                                             // [18, p. 159]
    static constexpr size_t min = T::empty_slot;
    static constexpr size_t max = T::header;

    CXX_ASSERT(
        min <= max &&
        min >= Ext::min<base_t>() &&
        max <= Ext::max<base_t>());
};

template<typename T>
constexpr typename std::enable_if<
    Ext::is_int<T>(),
T>::type
    min()
{ return Ext::min<T>(); }

template<typename T>
constexpr typename std::enable_if<
    Ext::is_int<T>(),
T>::type
    max()
{ return Ext::max<T>(); }

#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)

template<typename T>
struct box_type_t
{ static constexpr bool is_box = false; };

template<typename V, typename T>
struct box_type_t<Ext::box_t<V, T>>
{
    typedef typename Ext::box_t<V, T>::type_t type_t;
    static constexpr bool is_box = true;
};

template<typename T>
constexpr bool is_box()
{ return box_type_t<T>::is_box; }

template<typename T>
constexpr typename std::enable_if<
    is_box<T>(),
typename box_type_t<T>::type_t>::type
    min()
{ return Ext::min<typename box_type_t<T>::type_t>(); }

template<typename T>
constexpr typename std::enable_if<
    is_box<T>(),
typename box_type_t<T>::type_t>::type
    max()
{ return Ext::max<typename box_type_t<T>::type_t>(); }

#endif

using Ext::is_int;
using Ext::is_signed_int;
using Ext::is_unsigned_int;

template<typename V>
struct types_type_t
{
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    static constexpr bool is_types = is_box<V>();
    typedef typename box_type_t<V>::type_t type_t;
#else
    static constexpr bool is_types = is_int<V>();
    typedef V type_t;
#endif
};

template<typename V>
static constexpr bool is_types()
{ return types_type_t<V>::is_types; }

template<
    typename V,
    typename = typename std::enable_if<
        is_types<V>()>::type>
using addition_type_t = 
    Ext::addition_type_t<typename types_type_t<V>::type_t>;

template<
    typename V,
    typename = typename std::enable_if<
        is_types<V>()>::type>
using subtraction_type_t = 
    Ext::subtraction_type_t<typename types_type_t<V>::type_t>;

template<typename V>
using addition_t =
    typename addition_type_t<V>::type_t;

template<typename V>
using subtraction_t =
    typename subtraction_type_t<V>::type_t;

template<
    typename V,
    typename = typename std::enable_if<
        is_types<V>()>::type>
inline
    addition_t<V>
    add(V a, V b)
{
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    return a.template add<addition_t<V>>(b);
#else
    return Ext::add<addition_t<V>>(a, b);
#endif
}

template<
    typename V,
    typename = typename std::enable_if<
        is_types<V>()>::type>
inline
    subtraction_t<V>
    sub(V a, V b)
{
#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
    return a.template sub<subtraction_t<V>>(b);
#else
    return Ext::sub<subtraction_t<V>>(a, b);
#endif
}

template<typename C = char>
struct char_traits_t
{
    typedef
        typename
            std::remove_cv<C>::type
        char_t;
    typedef
        Ext::unsigned_t<char_t>
        uchar_t;
    typedef
        std::char_traits<char_t>
        traits_t;
    typedef
        Ext::unsigned_t<
            typename traits_t::int_type>
        uint_t;
    typedef
        size_t code_t;
    typedef
        std::basic_string<char_t>
        string_t;

    CXX_ASSERT(
        Ext::digits<uint_t>() <
        Ext::digits<code_t>());
    CXX_ASSERT(
        !Ext::is_signed<uchar_t>());
    CXX_ASSERT(
        !Ext::is_signed<uint_t>());

    static constexpr uint_t to_int(char_t ch)
    { return traits_t::to_int_type(ch); }

    static constexpr char_t to_char(uint_t uint)
    { return traits_t::to_char_type(uint); }

    static char_t to_lower(char_t ch);

    static constexpr auto first_letter =
        to_int(to_char('a'));
    static constexpr auto last_letter =
        to_int(to_char('z'));

    static constexpr bool is_valid_char(char_t a)
    { return
        to_int(a) >= first_letter &&
        to_int(a) <= last_letter; }

    static constexpr bool is_valid_code(code_t a)
    { return
        a >= min_letter &&
        a <= max_letter; }

    static code_t to_code(char_t c)
    {
        auto c0 = c;
        c = to_lower(c);
        if (!is_valid_char(c))
            Sys::error<Error>(
                "invalid input char '\\x%zx'",
                Ext::size_cast(to_int(c0)));
        return 
            Ext::sub<code_t>(
                to_int(c), first_letter) +
            min_letter;
    }

    static char_t from_code(code_t c)
    {
        SYS_ASSERT(
            is_valid_code(c));
        return
            Ext::sub<code_t>(
                c + first_letter,
                min_letter);
    }

    static constexpr code_t min_letter = 1;
    static constexpr code_t max_letter = min_letter +
        Ext::sub<code_t>(last_letter, first_letter);

    static constexpr code_t header     = max_letter + 1;                        // [18, p. 159]
    static constexpr code_t empty_slot = 0;                                     // [18, p. 159]
};

#if CONFIG_HASH_TRIE_CHAR_TYPE == 0
template<>
char_traits_t<char>::char_t
char_traits_t<char>::to_lower(char_t ch)
{ return tolower(ch); }
#endif

#if CONFIG_HASH_TRIE_CHAR_TYPE == 1
template<>
char_traits_t<wchar_t>::char_t
char_traits_t<wchar_t>::to_lower(char_t ch)
{ return towlower(ch); }
#endif

struct size_traits_t
{
    static constexpr size_t max_count = 32767;                                  // [32, p. 162]
    static constexpr size_t trie_size = CONFIG_HASH_TRIE_TRIE_SIZE; // = 32767; // [17, p. 158]
    static constexpr size_t tolerance = CONFIG_HASH_TRIE_TOLERANCE; // = 1000;  // [24, p. 160]
};

template<
    typename C = char,
    template<typename> class T = char_traits_t,
    typename S = size_traits_t>
class HashTrie :
    private T<C>,
    private S
{
public:
    typedef S size_traits_t;
    typedef T<C> char_traits_t;
    typedef typename char_traits_t::char_t char_t;
    typedef typename char_traits_t::string_t string_t;

    HashTrie();

    bool put(const char_t*);

    bool put(const string_t& str)
    { return put(str.c_str()); }

    void print(std::basic_ostream<char_t>&) const;
    void dump(std::basic_ostream<char_t>&) const;

#ifdef CONFIG_HASH_TRIE_STATISTICS
    void print_stats(std::basic_ostream<char_t>&) const;
#endif

private:
    template<typename T2, types_t t2>
    friend struct types_traits_t;

    template<types_t t>
    using types_traits_t = types_traits_t<HashTrie, t>;

    using size_traits_t::max_count;
    using size_traits_t::trie_size;
    using size_traits_t::tolerance;
    using char_traits_t::min_letter;
    using char_traits_t::max_letter;
    using char_traits_t::empty_slot;
    using char_traits_t::header;

    template<types_t t>
    struct is_range_t
    {
        enum {
            value =
                types_traits_t<t>::min <=
                types_traits_t<t>::max &&

                types_traits_t<t>::min >=
                min<typename types_traits_t<t>::type_t>() &&

                types_traits_t<t>::max <=
                max<typename types_traits_t<t>::type_t>()
        };
    };

    static constexpr bool loosely_less(size_t a, size_t b)
    { return a <= b; }
    static constexpr bool strictly_less(size_t a, size_t b)
    { return a < b; }

    using comparator_t = bool(size_t, size_t);

    // T1 includes T2 by comparators min and max
    template<
        types_t t1, types_t t2,
        comparator_t min = HashTrie::strictly_less,
        comparator_t max = min>
    struct includes_t
    {
        enum {
            value =
                min(types_traits_t<t1>::min,
                    types_traits_t<t2>::min) &&

                max(types_traits_t<t2>::max,
                    types_traits_t<t1>::max)
        };
    };

    CXX_ASSERT(
        is_range_t<count_type>::value);
    CXX_ASSERT(
        is_range_t<pointer_type>::value);
    CXX_ASSERT(
        is_range_t<letter_type>::value);
    CXX_ASSERT(
        is_range_t<cell_type>::value);

    CXX_ASSERT((
        includes_t<
            cell_type, letter_type>::value));
    CXX_ASSERT((
        includes_t<
            pointer_type, cell_type,
            loosely_less, strictly_less>::value));

    template<types_t i, types_t e>
    struct array_of_t
    {
        typedef
            typename types_traits_t<e>::type_t
            elem_t;

        class type_t
        {
        public:
            static constexpr size_t lo =
                types_traits_t<i>::min;
            static constexpr size_t hi =
                types_traits_t<i>::max;

            template<typename K>
            typename std::enable_if<
                Ext::is_int<K>(),
            elem_t>::type
                operator[](K k) const
            {
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(k >= lo && k <= hi);
#endif
                return array[k - lo];
            }

#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
            template<typename K>
            typename std::enable_if<
                is_box<K>(),
            elem_t>::type
                operator[](K k) const
            {
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(k >= lo && k <= hi);
#endif
                return array[static_cast<size_t>(k - lo)];
            }
#endif

            template<typename K>
            typename std::enable_if<
                Ext::is_int<K>(),
            elem_t&>::type
                operator[](K k)
            {
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(k >= lo && k <= hi);
#endif
                return array[k - lo];
            }

#if defined(CONFIG_HASH_TRIE_STRICT_TYPES)
            template<typename K>
            typename std::enable_if<
                is_box<K>(),
            elem_t&>::type
                operator[](K k)
            {
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(k >= lo && k <= hi);
#endif
                return array[static_cast<size_t>(k - lo)];
            }

            typename std::enable_if<
                is_box<elem_t>(),
            void>::type
                assign(
                    size_t l, size_t h,
                    typename box_type_t<elem_t>::type_t v)
            {	
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(h >= l && l >= lo && h <= hi);
#endif
                const auto b = array + (l - lo);
                std::fill(b, b + size_of(l, h), elem_t{v});
            }
#else
            typename std::enable_if<
                Ext::is_int<elem_t>() &&
                Ext::min<elem_t>() >= Ext::min<int>() &&
                Ext::max<elem_t>() <= Ext::max<int>(),
            void>::type
                assign(size_t l, size_t h, elem_t v)
            {
#if defined(CONFIG_HASH_TRIE_ARRAY_BOUNDS)
                SYS_ASSERT(h >= l && l >= lo && h <= hi);
#endif
                memset(array + (l - lo), v, size_of(l, h));
            }
#endif

        private:
            CXX_ASSERT(lo <= hi);

            static size_t size_of(size_t l, size_t h)
            { return sizeof(elem_t) * (h - l + 1); }

            elem_t array[hi - lo + 1];
        };
    };

    typedef typename types_traits_t<count_type>::type_t   count_t;              // [32, p. 162]
    typedef typename types_traits_t<pointer_type>::type_t pointer_t;            // [17, p. 158]
    typedef typename types_traits_t<letter_type>::type_t  letter_t;
    typedef typename types_traits_t<cell_type>::type_t    cell_t;               // [18, p. 159]

    typedef
        typename array_of_t<pointer_type, count_type>::type_t
        count_array_t;                                                          // [32, p. 162]
    typedef
        typename array_of_t<pointer_type, pointer_type>::type_t
        pointer_array_t;                                                        // [18, p. 159]
    typedef
        typename array_of_t<pointer_type, cell_type>::type_t
        cell_array_t;                                                           // [18, p. 159]

    cell_array_t    ch;                                                         // [18, p. 159]
    count_array_t   count;                                                      // [32, p. 162]
    pointer_array_t link;                                                       // [18, p. 159]
    pointer_array_t sibling;                                                    // [18, p. 159]

    // x_n = (alpha * n) % mod_x                                                // [22, p. 160]
    pointer_t x;                                                                // [22, p. 160]

    // from child to parent                                                     // [17, p. 158]
    void move_to_prefix(pointer_t& p) const                                     // [18, p. 159]
    { p = link[p - ch[p]]; }

    void move_to_last_suffix(pointer_t& p) const                                // [18, p. 159]
    { while (link[p]) p = sibling[link[p]]; }

    static letter_t to_letter(char_t c)
    { return char_traits_t::to_code(c); }

    static char_t to_char(letter_t l)
    { return char_traits_t::from_code(l); }  

    static size_t to_int(letter_t l)
    { return l; }

    pointer_t find(const char_t*);                                              // [20, p. 159]

    void print_word(pointer_t, std::basic_ostream<char_t>&, bool) const;        // [35, p. 163]
    void dump_word(pointer_t, std::basic_ostream<char_t>&, bool&) const;
    void dump_empty(std::basic_ostream<char_t>&) const;

    static constexpr size_t make_alpha(size_t trie_size, size_t max_letter)
    // stev: by ISO/IEC 14882:2011, 'std::ceil' doesn't have to be constexpr,
    // but GCC in the cmath file boils 'std::ceil' down to the undocumented
    // built-ins '__builtin_ceilf' and '__builtin_ceill'
    { return std::ceil(0.61803 * (trie_size - 2 * max_letter)); }

    CXX_ASSERT((make_alpha(32767, 26) == 20219));                               // [22, p. 160]

    // alpha =~ 0.61803 * trie_size                                             // [22, p. 160]
    // correction by lp-err.ps mentioned above:
    // alpha =~ 0.61803 * (trie_size - 52)
    static constexpr size_t alpha = make_alpha(trie_size, max_letter);
    static constexpr size_t mod_x = trie_size - 2 * max_letter;                 // [24, p. 161]
    static constexpr size_t max_h = mod_x + max_letter;                         // [24, p. 161]
    static constexpr size_t max_x = mod_x - alpha;                              // [24, p. 161]

    CXX_ASSERT(alpha > 0);
    CXX_ASSERT(tolerance > 0);
    CXX_ASSERT(trie_size > 2 * max_letter);
    CXX_ASSERT(max_h > tolerance);
    CXX_ASSERT(mod_x > tolerance);
    CXX_ASSERT(mod_x > alpha);

    // mod_x > tolerance => max_h > tolerance:
    // max_h = mod_x + max_letter > tolerance + max_letter > tolerance

#ifdef CONFIG_HASH_TRIE_STATISTICS
    struct stats_t
    {
        size_t loaded_words = 0;
        size_t failed_words = 0;
        size_t duplicated_words = 0;
        size_t empty_slots = 0;

        Sys::clocks_t trie_time = {};

        bool need_empty_slots = true;
    };

    mutable stats_t stats;
#endif
};

template<
    typename C,
    template<typename> class T,
    typename S>
HashTrie<C, T, S>::HashTrie()
{
    ch.assign(header, trie_size, empty_slot);                                   // [19, p. 159]
    link.assign(1, max_letter, 0);                                              // [19, p. 159]
    count.assign(1, max_letter, 0);                                             // [19, p. 159]

    for (pointer_t i = 1; i <= max_letter; ++ i) {                              // [19, p. 159]
        ch[i] = i;
        sibling[i] = i - 1;
    }

    link[0] = 0;                                                                // [19, p. 159]
    count[0] = 0; // cf. the errata on the web page mentioned above
    ch[0] = header;                                                             // [19, p. 159]
    sibling[0] = header - 1;                                                    // [19, p. 159]

    x = 0;                                                                      // [23, p. 160]
}

template<
    typename C,
    template<typename> class T,
    typename S>
typename
    HashTrie<C, T, S>::pointer_t
    HashTrie<C, T, S>::find(const char_t* str)                                  // [20, p. 159]
{
#ifdef DEBUG
    static bool debug_consts = true;
    if (debug_consts && globals.debug) {
        debug_consts = false;
        Sys::cerr
            << "!!! constants: trie_size=" << trie_size
            << " tolerance=" << tolerance
            << " alpha=" << alpha
            << " mod_x=" << mod_x
            << " max_h=" << max_h
            << " max_x=" << max_x
            << '\n';
    }

    class debug_probing_t
    {
    public:
        debug_probing_t(
            size_t id,
            const char_t* _str,
            const letter_t& _c,
            const pointer_t& _x,
            const pointer_t& _last_h,
            const pointer_t& _h) :
            str(_str),
            c(_c),
            x(_x),
            last_h(_last_h),
            h(_h),
            h2({_h, _h})
        {
            if (debugging) {
                Sys::cerr
                    << "!!! probing:" << id
                    << ": str=\"" << str
                    << "\" c='" << to_char(c)
                    << "' x=" << x
                    << " last_h=" << last_h
                    << " h=" << h;
            }
        }

        ~debug_probing_t()
        { end(false); }

        void next()
        {
            if (debugging) {
                if (h2.second == h - 1)
                    h2.second = h;
                else {
                    print_h(h);
                    h2 = {h, h};
                }
            }
        }

        void end(bool found = true)
        {
            if (debugging)
                print_h(h);
            if (debugging && !found)
                Sys::cerr << " !!!";
            if (debugging)
                Sys::cerr << '\n';
            debugging = false;
        }

    private:
        void print_h(pointer_t h) const
        {
            if (debugging) {
                SYS_ASSERT(
                    h2.first <=
                    h2.second);
                auto n =
                    h2.second -
                    h2.first + 1;
                if (n > 1)
                    Sys::cerr
                        << '-' << h2.second;
                if (true)
                    Sys::cerr
                        << " [" << n << ']';
                if (h2.second != h) {
                    Sys::cerr
                        << ", " << h;
                }
            }
        }

        bool debugging = globals.debug & debug_probing;

        const char_t*    str;
        const letter_t&  c;
        const pointer_t& x;
        const pointer_t& last_h;
        const pointer_t& h;

        std::pair<pointer_t, pointer_t> h2;
    };

    const auto str0 = str;

#define DEBUG_PROBING_BEGIN(ID) \
    debug_probing_t debug_probing(ID, str0, c, x, last_h, h)
#define DEBUG_PROBING_NEXT() \
    debug_probing.next()
#define DEBUG_PROBING_END() \
    debug_probing.end()
#else
#define DEBUG_PROBING_BEGIN(ID)
#define DEBUG_PROBING_NEXT()
#define DEBUG_PROBING_END()
#endif

    const pointer_t tolerance2 = tolerance;
    // trial header location                                                    // [26, p. 161]
    pointer_t h;                                                                // [26, p. 161]
    // the final one to try                                                     // [26, p. 161]
    pointer_t last_h; //!!!INT int last_h;                                      // [26, p. 161]

    const auto get_set_for_computing_header_locations = [&]() {                 // [24, p. 160]
        // 24. Get set for computing header locations                           // [24, p. 160]

        // (0): floor(x) := max M(x), for x in R
        //      where M(x) := { k in Z | k <= x }
        // (1): floor(x) <= x, for x in R
        // (2): n = floor(n), for n in Z
        // (3): x <= y => floor(x) <= floor(y), for x, y in R
        // (4): n <= x < n + 1 => floor(x) = n, for n in Z and x in R
        // (5): a mod b = a - floor(a / b) * b, for a in Z and b in N*
        // (6): 0 <= a < b => a % b = a:
        //      0 <= a < b => 0 <= a / b < 1
        //      then by (4) and (5)
        // (7): 0 < b <= a < 2 * b => a % b = a - b:
        //      0 < b <= a < 2 * b => 1 <= a / b < 2
        //      then by (4) and (5)
        // (8): x < max_x => (x + alpha) % mod_x = x + alpha:
        //      x < max_x <=> x + alpha < mod_x
        //      alpha > 0, x >= 0 => x + alpha > 0
        //      then by (6)
        // (9): x >= max_x => (x + alpha) % mod_x = x - max_x:
        //      x >= max_x <=> x + alpha >= mod_x
        //      x, alpha < mod_x => x + alpha < 2 * mod_x
        //      mod_x > tolerance > 0 => mod_x > 0
        //      then by (7), because x + alpha - mod_x = x - max_x

        if (x >= max_x)                                                         // [24, p. 161]
            x -= max_x;                                                         // [24, p. 161]
        else                                                                    // [24, p. 161]
            x += alpha;                                                         // [24, p. 161]

        h = x + max_letter + 1; // now max_letter < h <= trie_size - max_letter // [24, p. 161]

        // (1): max_letter < h <= trie_size - max_letter = max_h
        // (2): (1)  => h <= max_h
        //          <=> h + tolerance - mod_x <= max_h - mod_x + tolerance
        //          <=> h + tolerance - mod_x <= max_letter + tolerance
        // (3): h + tolerance > max_h
        //          <=> h + tolerance - mod_x > max_h - mod_x = max_letter
        // (4): (1) && h + tolerance > max_h
        //           => max_letter < h + tolerance - mod_x <= max_letter + tolerance
        // (5): (1) <=> 0 <= max_h - h < max_h - max_letter = mod_x
        //          <=> 0 <= max_h - h < mod_x
        // (6): max_h + tolerance > trie_size
        //      (max_h = trie_size - max_letter)
        //      <=> tolerance > max_letter
        // (7): max_h > tolerance
        //      (max_h = trie_size - max_letter)
        //      <=> trie_size - max_letter > tolerance
        //      <=> max_letter + tolerance < trie_size
        // (8): h <= max_h - tolerance
        //      <=> h + tolerance <= max_h
        //      (max_h = trie_size - max_letter < trie_size)
        //       => h + tolerance < trie_size

        if (h > max_h - tolerance) {                                            // [24, p. 161]
            // by (4) above:
            //  (h + tolerance) - mod_x >  max_letter &&
            //  (h + tolerance) - mod_x <= max_letter + tolerance

            // mod_x > tolerance (asserted above) =>
            //  (h + tolerance) - mod_x <= max_letter + mod_x = max_h
            //  => max_letter + 1 <= last_h = h + tolerance - mod_x <= max_h

            // by (7) above the expression 'h + tolerance - mod_x'
            // is within the bounds of 'pointer_t' (we asserted
            // above that max_h > tolerance)

            // by (6) above we see that the expression 'h + tolerance'
            // is exceeding the upper bound 'trie_size' of 'pointer_t'
            // (e.g. when 'h == max_h' and 'tolerance > max_letter').
            // Therefore we have to use 'add<>' instead of plain '+'.
            last_h = add(h, tolerance2) - mod_x; //!!! = h + tolerance - mod_x; // [24, p. 161]
        }
        else {
            // h + tolerance <= max_h                                           // [24, p. 161]
            // h = x + max_letter + 1 >= max_letter + 1 =>
            // h + tolerance >= tolerance + max_letter + 1 >= max_letter + 1
            // => max_letter + 1 <= last_h = h + tolerance <= max_h

            // by (8) above we see that the expression 'h + tolerance'
            // is within the upper bound 'trie_size' of 'pointer_t'
            last_h = h + tolerance;                                             // [24, p. 161]
        }
    };                                                                          // [24, p. 161]

    const auto compute_the_next_trial_header_location = [&]() {                 // [25, p. 161]
        // 25. Compute the next trial header location h, or abort find          // [25, p. 161]

        // Note that by the relations established above:
        //   max_letter + 1 <= last_h <= max_h
        // This relation assures the the loops below which
        // are based on 'h' computed below are finite!

        if (h == last_h)                                                        // [25, p. 161]
            return false;                                                       // [25, p. 161]
        if (h == max_h)                                                         // [25, p. 161]
            h = max_letter + 1;                                                 // [25, p. 161]
        else                                                                    // [25, p. 161]
            h ++;                                                               // [25, p. 161]
        return true;                                                            // [25, p. 161]
    };                                                                          // [25, p. 161]

    // the current word position                                                // [20, p. 159]
    pointer_t p                                                                 // [20, p. 159]
        = to_letter(*str ++);                                                   // [20, p. 159]
    while (*str) {                                                              // [20, p. 159]
        // current letter code                                                  // [20, p. 159]
        letter_t c                                                              // [20, p. 159]
            = to_letter(*str ++);                                               // [20, p. 159]
        // 21. Advance p to its child number c                                  // [21, p. 160]
        if (link[p] == 0) {                                                     // [21, p. 160]
            // 27. Insert the firstborn child of p and move to it, or abort find// [27, p. 161]
            get_set_for_computing_header_locations();                           // [27, p. 161]
            DEBUG_PROBING_BEGIN(27);
            do {                                                                // [27, p. 161]
                if (!compute_the_next_trial_header_location())                  // [27, p. 161]
                    return 0;                                                   // [27, p. 161]
                DEBUG_PROBING_NEXT();
            } while (ch[h] != empty_slot || ch[h + c] != empty_slot);           // [27, p. 161]
            DEBUG_PROBING_END();
            link[p] = h;                                                        // [27, p. 161]
            link[h] = p;                                                        // [27, p. 161]
            p = h + c;                                                          // [27, p. 161]
            ch[h] = header;                                                     // [27, p. 161]
            ch[p] = c;                                                          // [27, p. 161]
            sibling[h] = p;                                                     // [27, p. 161]
            sibling[p] = h;                                                     // [27, p. 161]
            count[h] = 0; //!!!
            count[p] = 0;                                                       // [27, p. 161]
            link[p] = 0;                                                        // [27, p. 161]
        }                                                                       // [21, p. 160]
        else {                                                                  // [21, p. 160]
            // the next word position                                           // [20, p. 159]
            pointer_t q                                                         // [20, p. 159]
                = link[p] + c;                                                  // [21, p. 160]
            if (ch[q] != c) {                                                   // [21, p. 160]
                if (ch[q] != empty_slot) {                                      // [21, p. 160]
                    // 29. Move p's family to a place where child c will fit,   // [29, p. 162]
                    //     or abort find                                        // [29, p. 162]
                    // family member to be moved                                // [30, p. 162]
                    pointer_t r;                                                // [30, p. 162]
                    // amount of motion                                         // [30, p. 162]
                    //!!!INT int delta;                                         // [30, p. 162]
                    // have we found a new homestead?                           // [30, p. 162]
                    bool slot_found;                                            // [30, p. 162]
                    // 31. Find a suitable place h to move, or abort find       // [31, p. 162]
                    slot_found = false;                                         // [31, p. 162]
                    get_set_for_computing_header_locations();                   // [31, p. 162]
                    DEBUG_PROBING_BEGIN(31);
                    do {                                                        // [31, p. 162]
                        if (!compute_the_next_trial_header_location())          // [31, p. 162]
                            return 0;                                           // [31, p. 162]
                        DEBUG_PROBING_NEXT();
                        if (ch[h + c] == empty_slot) {                          // [31, p. 162]
                            r = link[p];                                        // [31, p. 162]
                            auto delta = sub(h, r); //!!!INT delta = h - r;     // [31, p. 162]
                            while (ch[r + delta] == empty_slot &&               // [31, p. 162]
                                    sibling[r] != link[p])                      // [31, p. 162]
                                r = sibling[r];                                 // [31, p. 162]
                            slot_found = ch[r + delta] == empty_slot;           // [31, p. 162]
                        }                                                       // [31, p. 162]
                    } while (!slot_found);                                      // [31, p. 162]
                    DEBUG_PROBING_END();
                    q = h + c;                                                  // [29, p. 162]
                    r = link[p];                                                // [29, p. 162]
                    auto delta = sub(h, r); //!!!INT delta = h - r;             // [29, p. 162]
                    do {                                                        // [29, p. 162]
                        sibling[r + delta] = sibling[r] + delta;                // [29, p. 162]
                        ch[r + delta] = ch[r];                                  // [29, p. 162]
                        ch[r] = empty_slot;                                     // [29, p. 162]
                        count[r + delta] = count[r];                            // [29, p. 162]
                        link[r + delta] = link[r];                              // [29, p. 162]
                        if (link[r]) link[link[r]] = r + delta;                 // [29, p. 162]
                        r = sibling[r];                                         // [29, p. 162]
                    } while (ch[r] != empty_slot);                              // [29, p. 162]
                }                                                               // [21, p. 160]
                // 28. Insert child c into p's family                           // [28, p. 161]
                h = link[p];                                                    // [28, p. 161]
                while (sibling[h] > q)                                          // [28, p. 161]
                    h = sibling[h];                                             // [28, p. 161]
                sibling[q] = sibling[h];                                        // [28, p. 161]
                sibling[h] = q;                                                 // [28, p. 161]
                ch[q] = c;                                                      // [28, p. 161]
                count[q] = 0;                                                   // [28, p. 161]
                link[q] = 0;                                                    // [28, p. 161]
            }                                                                   // [21, p. 160]
            p = q;                                                              // [21, p. 160]
        }                                                                       // [21, p. 160]
    }                                                                           // [20, p. 159]
    return p;                                                                   // [20, p. 159]
}

template<
    typename C,
    template<typename> class T,
    typename S>
bool HashTrie<C, T, S>::put(const char_t* str)
{
    if (*str == '\0')
        Sys::error<Error>("hash tries cannot contain empty words");

#ifdef CONFIG_HASH_TRIE_STATISTICS
    Sys::utime_t time;
#endif

    // 34. Input the text, maintaining a dictionary with frequency count        // [34. p. 163]
    auto p = find(str);                                                         // [34. p. 163]

#ifdef CONFIG_HASH_TRIE_STATISTICS
    stats.trie_time += time();

    if (p)
        stats.loaded_words ++;
    else
        stats.failed_words ++;
    if (p && count[p])
        stats.duplicated_words ++;
#endif

    if (!p) return false;                                                       // [34. p. 163]
    count[p] ++;                                                                // [34. p. 163]
    return true;
}

template<
    typename C,
    template<typename> class T,
    typename S>
void HashTrie<C, T, S>::print_word(
    pointer_t p, std::basic_ostream<char_t>& ost,
    bool only_word) const
{
    auto c = ch[p];
    if (c == empty_slot ||
        c == header)
        return;

    std::vector<char_t> b;

    // runs through ancestors of p                                              // [35, p. 163]
    auto q = p;                                                                 // [35, p. 163]
    do {                                                                        // [35, p. 163]
        c = ch[q];                                                              // [35, p. 163]
        SYS_ASSERT(
            c > empty_slot &&
            c < header);
        b.push_back(to_char(c));                                                // [35, p. 163]
        move_to_prefix(q);                                                      // [35, p. 163]
    } while (q);                                                                // [35, p. 163]

    std::reverse(b.begin(), b.end());
    b.push_back(0);

    auto n = count[p];
    if (n || only_word)
        ost << b.data();

    if (n && !only_word)
        ost << '\t' << n << '\n';                                               // [35, p. 163]
}

template<
    typename C,
    template<typename> class T,
    typename S>
void HashTrie<C, T, S>::print(
    std::basic_ostream<char_t>& ost) const
{
    typedef
        types_traits_t<pointer_type>
        types_traits_t;

    pointer_t p = types_traits_t::min;
    while (p < types_traits_t::max)
        print_word(p ++, ost, false);
    if (p <= types_traits_t::max)
        print_word(p, ost, false);
}

template<
    typename C,
    template<typename> class T,
    typename S>
inline void 
    HashTrie<C, T, S>::dump_empty(
        std::basic_ostream<char_t>& ost) const
{
    ost << "...\t.\t.\t.\t.";
    if (globals.print_words) ost
        << "\t.";
    ost << '\n';
}

template<
    typename C,
    template<typename> class T,
    typename S>
void HashTrie<C, T, S>::dump_word(
    pointer_t p, std::basic_ostream<char_t>& ost,
    bool& empty) const
{
    auto c = ch[p];
    if (c == empty_slot) {
        empty = true;
        return;
    }
    if (empty)
        dump_empty(ost);
    empty = false;
    ost << p << '\t'
        << link[p] << '\t';
    if (c == header) ost
        << "header";
    else
    if (globals.print_chars) ost
        << to_char(c);
    else ost
        << to_int(c);
    ost << '\t'
        << sibling[p] << '\t'
        << count[p];
    if (globals.print_words) {
        ost << '\t';
        print_word(p, ost, true);
    }
    ost << '\n';
}

template<
    typename C,
    template<typename> class T,
    typename S>
void HashTrie<C, T, S>::dump(
    std::basic_ostream<char_t>& ost) const
{
    typedef
        types_traits_t<pointer_type>
        types_traits_t;

    ost << "p\tlink[p]\tch[p]\tsibling[p]\tcount[p]";
    if (globals.print_words) ost
        << "\tword";
    ost << '\n';

    auto e = false;
    pointer_t p = types_traits_t::min;
    while (p < types_traits_t::max)
        dump_word(p ++, ost, e);
    if (p <= types_traits_t::max)
        dump_word(p, ost, e);
    if (e)
        dump_empty(ost);
}

#ifdef CONFIG_HASH_TRIE_STATISTICS
template<
    typename C,
    template<typename> class T,
    typename S>
void HashTrie<C, T, S>::print_stats(
    std::basic_ostream<char_t>& ost) const
{
    typedef
        types_traits_t<pointer_type>
        types_traits_t;

    if (stats.need_empty_slots) {
        stats.need_empty_slots = false;

        const auto is_empty = [this](pointer_t p)
        { return ch[p] == empty_slot; };

        stats.empty_slots = 0;
        pointer_t p = types_traits_t::min;
        while (p < types_traits_t::max) {
            if (is_empty(p ++))
                stats.empty_slots ++;
        }
        if (p <= types_traits_t::max) {
            if (is_empty(p))
                stats.empty_slots ++;
        }
    }

    using namespace Ext;
    using namespace Sys;
    using clock_t = Sys::clock_t;

    static const clock_t::flags_t time_types[] = {
        clock_t::real_time, // time_type_t::real
        clock_t::user_time, // time_type_t::user
        clock_t::sys_time,  // time_type_t::sys
    };
    auto time = clock(
        stats.trie_time,
        array(time_types)[globals.time_type] |
        clock_t::useconds);

    ost << "loaded-words:     " << stats.loaded_words << '\n'
        << "failed-words:     " << stats.failed_words << '\n'
        << "duplicated-words: " << stats.duplicated_words << '\n'
        << "empty-slots:      " << stats.empty_slots << '\n'
        << "trie-time:        " << time << '\n';
}
#endif

} // namespace HashTrie

class options_t :
    private global_options_t
{
public:
    static inline const options_t options(int argc, char* argv[])
    { options_t r; r.parse(argc, argv); return r; }

    enum action_t {
        print_config,
        print_types,
        load_trie_only,
        print_trie,
        dump_trie
    };

    action_t action = print_trie;

private:
    static void error(const char* msg, ...) PRINTF_FMT(1);
    static void invalid_opt_arg(const char* opt_name, const char* opt_arg);
    static void missing_opt_arg(const char* opt_name);
    static void missing_opt_arg(char opt_name);
    static void not_allowed_opt_arg(const char* opt_name);
    static void invalid_opt(const char* opt_name);
    static void invalid_opt(char opt_name);

    static size_t parse_args_optarg(
        const char* opt_name, const char* opt_arg,
        char const* const* args, size_t n_args);

#ifdef DEBUG
    static debug_t parse_debug_optarg(const char* opt_arg);
#endif
#ifdef CONFIG_HASH_TRIE_STATISTICS
    using time_type_t = global_options_t::time_type_t;
    static time_type_t parse_time_type_optarg(const char* opt_arg);
#endif

    void parse(int argc, char* argv[]);

    static void version();
    static void usage();
};

void options_t::version()
{
    Sys::cout << program << ": version " << verdate << "\n\n" << license;
}

void options_t::usage()
{
    Sys::cout <<
        "usage: " << program << " [OPTION]...\n"
        "where the options are:\n"
        "  -P|--[print-]config  action: print out the config parameters\n"
        "  -T|--[print-]types   action: print out the add/sub types table\n"
        "  -L|--load-only       action: only load the input into the hash trie\n"
        "  -p|--print[-trie]    action: print out all <word, count> pairs from\n"
        "                         the hash trie (default)\n"
        "  -d|--dump[-trie]     action: dump out the complete hash trie structure\n"
        "  -b|--[no-][dump-]backtrace\n"
        "                       dump a backtrace of the program on fatal error\n"
        "                         or otherwise do not (default not)\n"
        "  -c|--[no-][print-]chars\n"
        "                       print out characters instead of codes on structure\n"
        "                         dumps or otherwise do not (default not)\n"
        "  -w|--[no-][print-]words\n"
        "                       print out words when dumping out the hash trie\n"
        "                         or otherwise do not (default do)\n"
#ifdef CONFIG_HASH_TRIE_STATISTICS
        "  -s|--[no-][print-]stats\n"
        "                       print out some statistics information\n"
        "                         or otherwise do not (default not)\n"
        "     --time-type=TYPE  use the specified time type when printing out\n"
        "                         timings; TYPE can be one of: real, user or sys;\n"
        "                         the default is real\n"
#endif
#ifdef DEBUG
        "     --debug=WHAT      print out some debugging information; WHAT can\n"
        "                         be one of: probing\n"
        "     --no-debug        do not print debugging info at all (default)\n"
#endif
        "  -q|--[no-]quiet      be quiet or otherwise do not (default not)\n"
        "  -v|--version         print version numbers and exit\n"
        "  -?|--help            display this help info and exit\n";
}

void options_t::error(const char* msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    Sys::cerr << program << ": error: " << buf << std::endl;

    exit(1);
}

void options_t::invalid_opt_arg(const char* opt_name, const char* opt_arg)
{
    error("invalid argument for '%s' option: '%s'", opt_name, opt_arg);
}

void options_t::missing_opt_arg(const char* opt_name)
{
    error("argument for option '%s' not found", opt_name);
}

void options_t::missing_opt_arg(char opt_name)
{
    error("argument for option '-%c' not found", opt_name);
}

void options_t::not_allowed_opt_arg(const char* opt_name)
{
    error("option '%s' does not allow an argument", opt_name);
}

void options_t::invalid_opt(const char* opt_name)
{
    error("invalid command line option '%s'", opt_name);
}

void options_t::invalid_opt(char opt_name)
{
    error("invalid command line option '-%c'", opt_name);
}

size_t options_t::parse_args_optarg(
    const char* opt_name, const char* opt_arg,
    char const* const* args, size_t n_args)
{
    char const* const* end = args + n_args;
    char const* const *ptr = std::find_if(
        args, end, [=](const char* arg) {
            return strcmp(arg, opt_arg) == 0;
        });
    if (ptr >= end)
        invalid_opt_arg(opt_name, opt_arg);
    return
        Ext::size_cast(ptr - args);
}

#ifdef DEBUG
debug_t options_t::parse_debug_optarg(const char* opt_arg)
{
    static const char* debugs[] = {
        "probing", // debug_t::debug_probing
    };
    auto r = parse_args_optarg(
        "debug", opt_arg,
        debugs, Ext::array_size(debugs));
    return static_cast<debug_t>(1 << r);
}
#endif

#ifdef CONFIG_HASH_TRIE_STATISTICS
options_t::time_type_t options_t::parse_time_type_optarg(
    const char* opt_arg)
{
    static const char* time_types[] = {
        "real", // time_type_t::real
        "user", // time_type_t::user
        "sys",  // time_type_t::sys
    };
    auto r = parse_args_optarg(
        "time-type", opt_arg,
        time_types, Ext::array_size(time_types));
    return static_cast<time_type_t>(r);
}
#endif

void options_t::parse(int argc, char* argv[])
{
    struct opt_type_t
    {
        enum {
            dump_backtrace    = 'b',
            print_chars       = 'c',
            print_words       = 'w',
            print_config      = 'P',
            print_types       = 'T',
            load_trie_only    = 'L',
            dump_trie         = 'd',
            print_trie        = 'p',
#ifdef CONFIG_HASH_TRIE_STATISTICS
            print_stats       = 's',
#endif
            quiet             = 'q',
            version           = 'v',
            help              = '?',

            no_dump_backtrace = 256,
            no_print_chars,
            no_print_words,
#ifdef CONFIG_HASH_TRIE_STATISTICS
            no_print_stats,
#endif
            no_quiet,
#ifdef DEBUG
            debug,
            no_debug,
#endif
#ifdef CONFIG_HASH_TRIE_STATISTICS
            time_type,
#endif
        };
    };

    const char shorts[] =
        ":bcdLpPq"
#ifdef CONFIG_HASH_TRIE_STATISTICS
        "s"
#endif
        "Tvw";
    static const struct option longs[] = {
        { "config",              0,       0, opt_type_t::print_config },
        { "print-config",        0,       0, opt_type_t::print_config },
        { "print-types",         0,       0, opt_type_t::print_types },
        { "types",               0,       0, opt_type_t::print_types },
        { "load-only",           0,       0, opt_type_t::load_trie_only },
        { "dump-trie",           0,       0, opt_type_t::dump_trie },
        { "dump",                0,       0, opt_type_t::dump_trie },
        { "print-trie",          0,       0, opt_type_t::print_trie },
        { "print",               0,       0, opt_type_t::print_trie },
        { "chars",               0,       0, opt_type_t::print_chars },
        { "print-chars",         0,       0, opt_type_t::print_chars },
        { "no-chars",            0,       0, opt_type_t::no_print_chars },
        { "no-print-chars",      0,       0, opt_type_t::no_print_chars },
        { "words",               0,       0, opt_type_t::print_words },
        { "print-words",         0,       0, opt_type_t::print_words },
        { "no-words",            0,       0, opt_type_t::no_print_words },
        { "no-print-words",      0,       0, opt_type_t::no_print_words },
#ifdef CONFIG_HASH_TRIE_STATISTICS
        { "stats",               0,       0, opt_type_t::print_stats },
        { "print-stats",         0,       0, opt_type_t::print_stats },
        { "no-stats",            0,       0, opt_type_t::no_print_stats },
        { "no-print-stats",      0,       0, opt_type_t::no_print_stats },
        { "time-type",           1,       0, opt_type_t::time_type },
#endif
        { "dump-backtrace",      0,       0, opt_type_t::dump_backtrace },
        { "backtrace",           0,       0, opt_type_t::dump_backtrace },
        { "no-dump-backtrace",   0,       0, opt_type_t::no_dump_backtrace },
        { "no-backtrace",        0,       0, opt_type_t::no_dump_backtrace },
#ifdef DEBUG
        { "debug",               1,       0, opt_type_t::debug },
        { "no-debug",            0,       0, opt_type_t::no_debug },
#endif
        { "quiet",               0,       0, opt_type_t::quiet },
        { "no-quiet",            0,       0, opt_type_t::no_quiet },
        { "version",             0,       0, opt_type_t::version },
        { "help",                0, &optopt, opt_type_t::help },
        { 0,                     0,       0, 0 },
    };

    struct bits_t
    {
        bits_t() :
            version(false),
            usage(false)
        {}

        unsigned version : 1;
        unsigned usage : 1;
    };
    bits_t bits;

    const auto argv_optind = [=]() -> char* {
        SYS_ASSERT(
            optind > 0 &&
            Ext::size_cast(optind) - 1 <
            Ext::size_cast(argc));
        return argv[optind - 1];
    };
    const auto optopt_cast = [=]() -> char {
        SYS_ASSERT(
            isascii(optopt));
        return optopt;
    };

    int opt;
    opterr = 0;
    optind = 1;
    while ((opt = getopt_long(
        argc, argv, &shorts[0], &longs[0], 0)) != EOF) {
        switch (opt) {
        case opt_type_t::dump_backtrace:
            dump_backtrace = true;
            break;
        case opt_type_t::no_dump_backtrace:
            dump_backtrace = false;
            break;
        case opt_type_t::print_chars:
            print_chars = true;
            break;
        case opt_type_t::no_print_chars:
            print_chars = false;
            break;
        case opt_type_t::print_words:
            print_words = true;
            break;
        case opt_type_t::no_print_words:
            print_words = false;
            break;
        case opt_type_t::print_config:
            action = print_config;
            break;
        case opt_type_t::print_types:
            action = print_types;
            break;
        case opt_type_t::load_trie_only:
            action = load_trie_only;
            break;
        case opt_type_t::dump_trie:
            action = dump_trie;
            break;
        case opt_type_t::print_trie:
            action = print_trie;
            break;
#ifdef CONFIG_HASH_TRIE_STATISTICS
        case opt_type_t::print_stats:
            print_stats = true;
            break;
        case opt_type_t::no_print_stats:
            print_stats = false;
            break;
        case opt_type_t::time_type:
            time_type =
                parse_time_type_optarg(optarg);
            break;
#endif
#ifdef DEBUG
        case opt_type_t::debug:
            debug |= parse_debug_optarg(optarg);
            break;
        case opt_type_t::no_debug:
            debug = 0;
            break;
#endif
        case opt_type_t::quiet:
            quiet = true;
            break;
        case opt_type_t::no_quiet:
            quiet = false;
            break;
        case opt_type_t::version:
            bits.version = true;
            break;
        case 0:
            bits.usage = true;
            break;
        case ':': {
            const char* opt = argv_optind();
            if (opt[0] == '-' && opt[1] == '-')
                missing_opt_arg(opt);
            else
                missing_opt_arg(optopt_cast());
            break;
        }
        case '?':
        default:
            if (optopt == 0)
                invalid_opt(argv_optind());
            else
            if (optopt != '?') {
                auto opt = argv_optind();
                if (opt[0] == '-' && opt[1] == '-') {
                    if (auto end = strchr(opt, '='))
                        *end = '\0';
                    not_allowed_opt_arg(opt);
                }
                else
                    invalid_opt(optopt_cast());
            }
            else
                bits.usage = true;
            break;
        }
    }

    if (bits.version)
        version();
    if (bits.usage)
        usage();

    if (bits.version ||
        bits.usage)
        exit(0);

    globals = *this;
}

void print_config()
{
    struct config_t
    {
        const char* const name;
        const char* const val;
        size_t      const len;

        config_t(
            const char* _name,
            const char* _val) :
            name(_name),
            val(_val),
            len(strlen(name))
        {}

        void print(
            Sys::ostream& ost,
            size_t width) const
        {
            using namespace std;
            ost << name
                << left
                << setw(
                    len <= width
                    ? width - len
                    : 0)
                << ':'
                << val
                << '\n';
        }
    };

    static const config_t configs[] = {
#define EVAL_CONFIG0(VALUE) \
        # VALUE
#define EVAL_CONFIG(VALUE) \
        EVAL_CONFIG0(VALUE)
#define DECL_CONFIG_B(NAME, VALUE) \
        config_t{ # NAME, EVAL_CONFIG(VALUE) }
#define DECL_CONFIG_V(NAME) \
        DECL_CONFIG_B(NAME, CONFIG ## _HASH_TRIE_ ## NAME)
#define DECL_CONFIG_D(NAME) \
        DECL_CONFIG_B(NAME, yes)
#define DECL_CONFIG_U(NAME) \
        DECL_CONFIG_B(NAME, no)

        DECL_CONFIG_V(TOLERANCE),
        DECL_CONFIG_V(TRIE_SIZE),

        DECL_CONFIG_B(CHAR_TYPE, SYS_CHAR_TYPE_NAME),

#ifndef CONFIG_HASH_TRIE_STRICT_TYPES
        DECL_CONFIG_U(STRICT_TYPES),
#else
        DECL_CONFIG_D(STRICT_TYPES),
#endif

#ifndef CONFIG_HASH_TRIE_ARRAY_BOUNDS
        DECL_CONFIG_U(ARRAY_BOUNDS),
#else
        DECL_CONFIG_D(ARRAY_BOUNDS),
#endif

#ifndef CONFIG_HASH_TRIE_STATISTICS
        DECL_CONFIG_U(STATISTICS),
#else
        DECL_CONFIG_D(STATISTICS),
#endif
    };

    const auto n = Ext::array_size(configs);

    const auto e = std::max_element(
        configs, configs + n,
        [](const config_t& a,
           const config_t& b)
        { return a.len < b.len; });
    SYS_ASSERT(
        e != nullptr &&
        e < configs + n);
    const auto w = e->len + 2;

    std::for_each(
        configs, configs + n,
        [=](const config_t& c)
        { c.print(Sys::cout, w); });
}

template<Ext::op_type_t t>
void print_op_types()
{
    using Ext::operation_type_t;

    operation_type_t<t, char>::print(Sys::cout);
    // $ for t in char short int; do for s in signed unsigned; do printf '%s %s\n' "$s" "$t"; done; done|sed -r 's/^(.*)$/\toperation_type_t<t, \1>::print(Sys::cout);/'
    operation_type_t<t, signed char>::print(Sys::cout);
    operation_type_t<t, unsigned char>::print(Sys::cout);
    operation_type_t<t, signed short>::print(Sys::cout);
    operation_type_t<t, unsigned short>::print(Sys::cout);
    operation_type_t<t, signed int>::print(Sys::cout);
    operation_type_t<t, unsigned int>::print(Sys::cout);
}

void print_types()
{
    using Ext::op_type_t;

    Sys::cout << "op\toperand\toperator\twider\tresult\n";
    print_op_types<op_type_t::add>();
    print_op_types<op_type_t::sub>();
}

void hash_trie_error(bool exit, size_t lno, const char* msg, ...)
    PRINTF_FMT(3);

void hash_trie_error(bool exit, size_t lno, const char* msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    Sys::cerr
        << program
        << ": error:"
        << lno
        << ": "
        << buf
        << '\n';

    if (exit)
        std::exit(1);
}

using hash_trie_t = HashTrie::HashTrie<Sys::char_t>;

using print_func_t = void (hash_trie_t::*)(Sys::ostream&) const;

void exec_hash_trie(print_func_t print_func = nullptr)
{
    size_t lno = 1;
    hash_trie_t trie;
    hash_trie_t::string_t str;
    while (Ext::getline(Sys::cin, str)) {
        try {
            if (!trie.put(str) && !globals.quiet)
                hash_trie_error(
                    false, lno,
                    "failed to put '%"
                    SYS_CHAR_TYPE_FMTS
                    "s' in trie",
                    str.c_str());
        }
        catch (const HashTrie::Error& err) {
            hash_trie_error(
                true, lno, "%s", err.what());
        }
        lno ++;
    }

    if (print_func)
        (trie.*print_func)(Sys::cout);

#ifdef CONFIG_HASH_TRIE_STATISTICS
    if (globals.print_stats)
        trie.print_stats(Sys::cout);
#endif
}

int main(int argc, char* argv[])
{
    const auto opt =
        options_t::options(argc, argv);

    switch (opt.action) {
    case options_t::print_config:
        print_config();
        break;
    case options_t::print_types:
        print_types();
        break;
    case options_t::load_trie_only:
        exec_hash_trie();
        break;
    case options_t::print_trie:
        exec_hash_trie(
            &hash_trie_t::print);
        break;
    case options_t::dump_trie:
        exec_hash_trie(
            &hash_trie_t::dump);
        break;
    default:
        SYS_UNEXPECT_ERR(
            "action='%zu'",
            Ext::size_cast(opt.action));
    }

    return 0;
}


