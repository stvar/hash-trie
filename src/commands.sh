#!/bin/bash

# Copyright (C) 2016  Stefan Vargyas
# 
# This file is part of Hash-Trie.
# 
# Hash-Trie is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Hash-Trie is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Hash-Trie.  If not, see <http://www.gnu.org/licenses/>.

usage()
{
    echo "usage: $1 [$(sed 's/^://;s/-:$/\x0/;s/[^:]/|-\0/g;s/:/ <arg>/g;s/^|//;s/\x0/-<long>/' <<< "$2")]"
}

quote()
{
    local __n__
    local __v__

    [ -z "$1" -o "$1" == "__n__" -o "$1" == "__v__" ] &&
    return 1

    printf -v __n__ '%q' "$1"
    eval __v__="\"\$$__n__\""
    #!!! echo "!!! 0 __v__='$__v__'"
    test -z "$__v__" && return 0
    printf -v __v__ '%q' "$__v__"
    #!!! echo "!!! 1 __v__='$__v__'"
    printf -v __v__ '%q' "$__v__"  # double quote
    #!!! echo "!!! 2 __v__='$__v__'"
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optopt()
{
    local __n__="${1:-$opt}"       #!!!NONLOCAL
    local __v__=''
    test -n "$__n__" &&
    printf -v __v__ '%q' "$__n__"  # paranoia
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optarg()
{
    local __n__="${1:-$opt}"       #!!!NONLOCAL
    local __v__=''
    test -n "$OPTARG" &&
    printf -v __v__ '%q' "$OPTARG" #!!!NONLOCAL
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optact()
{
    local __v__="${1:-$opt}"       #!!!NONLOCAL
    printf -v __v__ '%q' "$__v__"  # paranoia
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "act=$__v__"
}

optlong()
{
    local a="$1"

    if [ "$a" == '-' ]; then
        if [ -z "$OPT" ]; then                                      #!!!NONLOCAL
            local A="${OPTARG%%=*}"                                 #!!!NONLOCAL
            OPT="-$opt$A"                                           #!!!NONLOCAL
            OPTN="${OPTARG:$((${#A})):1}"                           #!!!NONLOCAL
            OPTARG="${OPTARG:$((${#A} + 1))}"                       #!!!NONLOCAL
        else
            OPT="--$OPT"                                            #!!!NONLOCAL
        fi
    elif [ "$opt" == '-' -o \( -n "$a" -a -z "$OPT" \) ]; then      #!!!NONLOCAL
        OPT="${OPTARG%%=*}"                                         #!!!NONLOCAL
        OPTN="${OPTARG:$((${#OPT})):1}"                             #!!!NONLOCAL
        OPTARG="${OPTARG:$((${#OPT} + 1))}"                         #!!!NONLOCAL
        [ -n "$a" ] && OPT="$a-$OPT"                                #!!!NONLOCAL
    elif [ -z "$a" ]; then                                          #!!!NONLOCAL
        OPT=''                                                      #!!!NONLOCAL
        OPTN=''                                                     #!!!NONLOCAL
    fi
}

error()
{
    local __self__="$self"     #!!!NONLOCAL
    local __help__="$help"     #!!!NONLOCAL
    local __OPTARG__="$OPTARG" #!!!NONLOCAL
    local __opts__="$opts"     #!!!NONLOCAL
    local __opt__="$opt"       #!!!NONLOCAL
    local __OPT__="$OPT"       #!!!NONLOCAL

    local self="error"

    # actions: \
    #  a:argument for option -$OPTARG not found|
    #  o:when $OPTARG != '?': invalid command line option -$OPTARG, or, \
    #    otherwise, usage|
    #  i:invalid argument '$OPTARG' for option -$opt|
    #  d:option '$OPTARG' does not take arguments|
    #  e:error message|
    #  w:warning message|
    #  u:unexpected option -$opt|
    #  g:when $opt == ':': equivalent with 'a', \
    #    when $opt == '?': equivalent with 'o', \
    #    when $opt is anything else: equivalent with 'u'

    local act="e"
    local A="$__OPTARG__" # $OPTARG
    local h="$__help__"   # $help
    local m=""            # error msg
    local O="$__opts__"   # $opts
    local P="$__opt__"    # $opt
    local L="$__OPT__"    # $OPT
    local S="$__self__"   # $self

    local long=''         # short/long opts (default)

    #!!! echo "!!! A='$A'"
    #!!! echo "!!! O='$O'"
    #!!! echo "!!! P='$P'"
    #!!! echo "!!! L='$L'"
    #!!! echo "!!! S='$S'"

    local opt
    local opts=":aA:degh:iL:m:oO:P:S:uw-:"
    local OPTARG
    local OPTERR=0
    local OPTIND=1
    while getopts "$opts" opt; do
        case "$opt" in
            [adeiouwg])
                act="$opt"
                ;;
            #[])
            #	optopt
            #	;;
            [AhLmOPS])
                optarg
                ;;
            \:)	echo "$self: error: argument for option -$OPTARG not found" >&2
                return 1
                ;;
            \?)	if [ "$OPTARG" != "?" ]; then
                    echo "$self: error: invalid command line option -$OPTARG" >&2
                else
                    echo "$self: $(usage $self $opts)"
                fi
                return 1
                ;;
            -)	case "$OPTARG" in
                    long|long-opts)
                        long='l' ;;
                    short|short-opts)
                        long='' ;;
                    *)	echo "$self: error: invalid command line option --$OPTARG" >&2
                        return 1
                        ;;
                esac
                ;;
            *)	echo "$self: error: unexpected option -$OPTARG" >&2
                return 1
                ;;
        esac
    done
    #!!! echo "!!! A='$A'"
    #!!! echo "!!! O='$O'"
    #!!! echo "!!! P='$P'"
    #!!! echo "!!! L='$L'"
    #!!! echo "!!! S='$S'"
    shift $((OPTIND - 1))
    test -n "$1" && m="$1"
    local f="2"
    if [ "$act" == "g" ]; then
        if [ "$P" == ":" ]; then
            act="a"
        elif [ "$P" == "?" ]; then
            act="o"
        else 
            act="u"
        fi
    fi
    local o=''
    if [ -n "$long" -a -n "$L" ]; then
        test "${L:0:1}" != '-' && o+='--'
        o+="$L"
    elif [[ "$act" == [aod] ]]; then
        o="-$A"
    elif [[ "$act" == [iu] ]]; then
        o="-$P"
    fi
    case "$act" in
        a)	m="argument for option $o not found"
            ;;
        o)	if [ "$A" != "?" ]; then
                m="invalid command line option $o"
            else
                act="h"
                m="$(usage $S $O)"
                f="1"
            fi
            ;;
        i)	m="invalid argument for $o: '$A'"
            ;;
        u)	m="unexpected option $o"
            ;;
        d)	m="option $o does not take arguments"
            ;;
        *)	# [ew]
            if [ "$#" -ge "2" ]; then
                S="$1"
                m="$2"
            elif [ "$#" -ge "1" ]; then
                m="$1"
            fi
            ;;
    esac
    if [ "$act" == "w" ]; then
        m="warning${m:+: $m}"
    elif [ "$act" != "h" ]; then
        m="error${m:+: $m}"
    fi
    if [ -z "$S" -o "$S" == "-" ]; then
        printf "%s\n" "$m" >&$f
    else
        printf "%s: %s\n" "$S" "$m" >&$f
    fi
    if [ "$act" == "h" ]; then
        test -n "$1" && h="$1"
        test -n "$h" &&
        printf "%s\n" "$h" >&$f
    fi
    return $f
}

hash-trie-source()
{
    local self="hash-trie-source"
    local srcf="hash-trie.cpp"
    local part='@(source-lines|@(types|char|size)-traits|?(exec-)hash-trie|@(hash-trie|main)-func)'
    local defC="hash-trie"

    local M=0
    local L=()

    local N=0
    local R=()

    local x="eval"
    local act="R"       # actions: \
                        #  A: source line addresses (--[source-]address)|
                        #  R: raw source (default) (--raw[-source])|
                        #  T: TeX source (--tex[-source])
    local C="+"         # print out the named source code part: source-lines[=NUM-NUM], {types,char,size}-traits, hash-trie (default), hash-trie-func=NAME, exec-hash-trie, main-func (--source-lines[=NUM-NUM]|--types-traits|--char-traits|--size-traits|--hash-trie|--hash-trie-func=NAME|--exec-hash-trie|--main-func)
    local l=""          # append \label TeX commands (cummulative) (--label=NUM:NAME)
    local n=""          # print line numbers (--line-no)
    local r=""          # remove specified lines (cummulative) (--remove[-lines]=NUM[-NUM])
    local s=""          # insert \skipnumbering TeX commands -- when at least one '-r' option was given (--skip-numbering)
    local t=""          # trim source (--trim[-source])

    local S

    local opt
    local OPT
    local OPTN
    local opts=":AC:dl:nr:RstTx-:"
    local OPTARG
    local OPTERR=0
    local OPTIND=1
    while getopts "$opts" opt; do
        # discriminate long options
        optlong

        # translate long options to short ones
        test -n "$OPT" &&
        case "$OPT" in
            address|source-address)
                opt='A' ;;
            raw|raw-source)
                opt='R' ;;
            tex|tex-source)
                opt='T' ;;
            trim|trim-source)
                opt='t' ;;
            line-no)
                opt='n' ;;
            label)
                opt='l' ;;
            remove|remove-lines)
                opt='r' ;;
            skip-numbering)
                opt='s' ;;
            $part)
                opt='C' ;;
            *)	error --long -o
                return 1
                ;;
        esac

        # handle short options
        case "$opt" in
            d)	x="echo"
                ;;
            x)	x="eval"
                ;;
            [ART])
                optact
                ;;
            [nst])
                optopt
                ;;
            [])
                optarg
                ;;
            C)	optlong -

                [[ "${OPT:2}" != @(+|$part) ]] && {
                    error --long -o
                    return 1
                }
                case "${OPT:2}" in
                    source-lines)
                        [[ "$OPTARG" =~ ^\+?$|^[0-9]+-[0-9]+$ ]] || {
                            error --long -i
                            return 1
                        }
                        ;;
                    hash-trie-func)
                        [[ "$OPTARG" =~ ^[a-zA-Z][a-zA-Z0-9_]*$ ]] || {
                            error --long -i
                            return 1
                        }
                        ;;
                    *)	[ -z "$OPTN" ] || {
                            error --long -d
                            return 1
                        }
                        ;;
                esac 
                C="${OPT:2}"
                S="$OPTARG"
                ;;
            l)	[[ "$OPTARG" =~ ^[0-9]+:[a-z](([a-z0-9]|[-])*[a-z0-9])?$ ]] || {
                    error --long -i
                    return 1
                }
                L[((M ++))]="$OPTARG"
                ;;
            r)	[[ "$OPTARG" =~ ^[0-9]+(-[0-9]+)?$ ]] || {
                    error --long -i
                    return 1
                }
                R[((N ++))]="$OPTARG"
                ;;
            *)	error --long -g
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    test "$C" == '+' && C="$defC"

    # action '-T|--tex' implies '--skip-numbering'
    test "$act" == 'T' && s='s'

    local s1=''
    local s2=''
    if [ "$act" == 'A' ]; then
        [ "$C" == 'source-lines' ] && {
            [ -z "$S" -o "$S" == '+' ] &&
            S='1-$'
            S=(${S/-/ })
        }
        [ "$C" == 'source-lines' ] && s1="
            ${S[0]} {
                i>
                =
            }
            ${S[1]} {
                i<
                =
            }"
        [ "$C" == 'types-traits' ] && s1='
            # template<typename T>
            # struct types_traits_t<T, count_type>
            # {
            # ...
            # };
            :0
            n
            :0b
            /^\s*template\s*<\s*typename\s+T\s*>\s*$/!b0
            i>
            =
            n
            /^\s*struct\s+types_traits_t\s*<\s*T\s*,\s*[a-z_]+\s*>\s*$/!b0b
            n
            /^\{\s*$/!q
            :1
            n
            /^\}\s*;\s*$/!b1

            # skip empty lines
            :2
            n
            /^\s*$/b2
            b3b

            # template<typename T>
            # struct types_traits_t<T, pointer_type>
            # {
            # ...
            # };
            :3
            n
            :3b
            /^\s*template\s*<\s*typename\s+T\s*>\s*$/!b3
            n
            /^\s*struct\s+types_traits_t\s*<\s*T\s*,\s*[a-z_]+\s*>\s*$/!b3b
            n
            /^\{\s*$/!q
            :4
            n
            /^\}\s*;\s*$/!b4

            # skip empty lines
            :5
            n
            /^\s*$/b5
            b6b

            # template<typename T>
            # struct types_traits_t<T, letter_type>
            # {
            # ...
            # };
            :6
            n
            :6b
            /^\s*template\s*<\s*typename\s+T\s*>\s*$/!b6
            n
            /^\s*struct\s+types_traits_t\s*<\s*T\s*,\s*[a-z_]+\s*>\s*$/!b6b
            n
            /^\{\s*$/!q
            :7
            n
            /^\}\s*;\s*$/!b7

            # skip empty lines
            :8
            n
            /^\s*$/b8
            b9b

            # template<typename T>
            # struct types_traits_t<T, cell_type>
            # {
            # ...
            # };
            :9
            n
            :9b
            /^\s*template\s*<\s*typename\s+T\s*>\s*$/!b9
            n
            /^\s*struct\s+types_traits_t\s*<\s*T\s*,\s*[a-z_]+\s*>\s*$/!b9b
            n
            /^\{\s*$/!q
            :10
            n
            /^\}\s*;\s*$/!b10

            i<
            =
            q'
        [ "$C" == 'char-traits' ] && s1='
            # template<typename C = char>
            # struct char_traits_t
            # {
            # ...
            # };
            :0
            n
            :0b
            /^\s*template\s*<\s*typename\s+C\s*=\s*char\s*>\s*$/!b0
            i>
            =
            n
            /^\s*struct\s+char_traits_t\s*$/!b0b
            n
            /^\{\s*$/!q
            :1
            n
            /^\}\s*;\s*$/!b1

            i<
            =
            q'
        [ "$C" == 'size-traits' ] && s1='
            # struct size_traits_t
            # {
            # ...
            # };
            :0
            n
            /^\s*struct\s+size_traits_t\s*$/!b0
            i>
            =
            n
            /^\{\s*$/!q
            :1
            n
            /^\}\s*;\s*$/!b1

            i<
            =
            q'
        [ "$C" == 'hash-trie' ] && s1='
            # template<
            #	typename C = char,
            #	template<typename> class T = char_traits_t,
            #	typename S = size_traits_t>
            # class HashTrie :
            # ...
            # {
            # ...
            # };
            :0
            n
            /^\s*template\s*<\s*$/!b0
            i>
            =
            n
            /^\s*typename\s+C\s*=\s*char\s*,\s*$/!b0
            n
            /^\s*template\s*<\s*typename\s*>\s*class\s+T\s*=\s*char_traits_t\s*,\s*$/!b0
            n
            /^\s*typename\s+S\s*=\s*size_traits_t\s*>\s*$/!b0
            n
            /^\s*class\s+HashTrie\s*:\s*$/!b0
            :1
            n
            /^\{\s*$/!b1
            :2
            n
            /^\}\s*;\s*$/!b2

            i<
            =
            q'
        [ "$C" == 'hash-trie-func' ] && s1='
            # template<
            # 	typename C,
            # 	template<typename> class T,
            # 	typename S>
            # typename
            # 	HashTrie<C, T, S>::pointer_t
            # 	HashTrie<C, T, S>::find(const char_t* str)       // [20, p. 159]
            # ...
            # {
            # ...
            # }
            :0
            n
            /^\s*template\s*<\s*$/!b0
            i>
            =
            n
            /^\s*typename\s+C\s*,\s*$/!b0
            n
            /^\s*template\s*<\s*typename\s*>\s*class\s+T\s*,\s*$/!b0
            n
            /^\s*typename\s+S\s*>\s*$/!b0
            :1
            n
            /\bHashTrie\s*<\s*C\s*,\s*T\s*,\s*S\s*>\s*::'"$S"'\s*\(/b2
            /\bHashTrie\s*<\s*C\s*,\s*T\s*,\s*S\s*>\s*::[a-zA-Z][a-zA-Z0-9_]*\s*\(/b0
            b1
            :2
            n
            /^\{\s*$/!b2
            :3
            n
            /^\}\s*$/!b3

            i<
            =
            q'
        [ "$C" == 'exec-hash-trie' ] && s1='
            # using hash_trie_t = HashTrie::HashTrie<Sys::char_t>;
            # ...
            # using print_func_t = void (hash_trie_t::*)(Sys::ostream&) const;
            # ...
            # void exec_hash_trie(print_func_t print_func = nullptr)
            # {
            # ...
            # }
            :0
            n
            /^\s*using\s+hash_trie_t\s*=/!b0
            i>
            =
            :1
            n
            /^\s*$/b1
            b2b

            :2
            n
            :2b
            /^\s*using\s+print_func_t\s*=/!b2
            :3
            n
            /^\s*$/b3
            b4b

            :4
            n
            :4b
            /^\s*void\s+exec_hash_trie\s*\([^\(\)]+\)\s*$/!b4
            n
            /^\{\s*$/!q
            :5
            n
            /^\}\s*$/!b5

            i<
            =
            q'
        [ "$C" == 'main-func' ] && s1='
            # int main(int argc, char* argv[])
            # {
            # ...
            # }
            :0
            n
            /^\s*int\s+main\s*\([^\(\)]*\)\s*$/!b0
            i>
            =
            n
            /^\{\s*$/!q
            :1
            n
            /^\}\s*$/!b1

            i<
            =
            q'
        s2='
            /^>/b0
            /^</b1
            q
            :0
            n
            /^\d+$/!q
            h
            b
            :1
            n
            /^\d+$/!q
            H
            $!q
            g
            s/\n/ /
            p'
    elif [[ "$act" == [RT] ]]; then
        local A
        A=($($self -C $C${S:+=$S} -A)) && [ "${#A[@]}" -eq 2 ] || {
            error "inner command failed: $self -C $C${S:+=$S} -A"
            return 1
        }
        s1="
            ${A[0]},${A[1]}"
        [ -z "$t" ] && s1+='p'
        [ -n "$t" ] && s1+=' {
                s|\s*//\s*\[\d+.*$||'
        [ -n "$t" -a "$C" == 'hash-trie-func' ] && s1+='
                s|\s*//!!!(?![A-Z]).*$||
                s|\s*//.*?errata.*$||'
        [ -n "$t" ] && s1+='
                p
            }'
    else
        error "internal: unexpected act='$act'"
        return 1
    fi

    local c="\
ssed -nR '$s1' \\
$srcf${s2:+|
ssed -nR '$s2'}"

    if [[ "$act" == [RT] ]]; then
        [ -n "$n" ] && c+="|
nl -nrn -s\  -w4 -ba -v ${A[0]}"
        local s3=''
        [ "$act" == 'T' ] && s3+='
                    1i\\\\begin{hashtrielisting}['"${A[0]}"']'
        [ "$M" -gt 0 ] && {
            local k
            local a
            local a2
            for((k=0; k<M; k++)); do
                a="${L[k]}"
                a2=(${a/:/ })
                ((a2[0] -= A[0] - 1))
                s3+="
                    $((a2[0]))s/$/@\\\\label{${a2[1]}}@/"
            done
        }
        [ "$N" -gt 0 ] && {
            local k
            local a
            local a2
            local a3
            for((k=0; k<N; k++)); do
                a="${R[k]}"
                [ -n "$s" ] &&
                a2=(${a/-/ })
                a3=(${a/-/ })
                [ "${#a3[@]}" -eq 1 ] && {
                    [ -n "$s" ] &&
                    a2[1]="${a2[0]}"
                    a3[1]="${a3[0]}"
                }
                ((a3[0] -= A[0] - 1))
                ((a3[1] -= A[0] - 1))
                [ -n "$s" ] && s3+="
                    $((a3[0] - 1))s/$/@\\\\skipnumbering{$((a2[1] + 1))}@"
                [ -n "$s" -a \( "$act" == 'R' -o -n "$n" \) ] && s3+='\n...'
                [ -n "$s" ] && s3+="/"
                s3+="
                    ${a3[0]},${a3[1]}d"
                [ -z "$s" ] && s3+="
                    $((a3[1] + 1))i..."
            done
        }
        [ "$act" == 'T' ] && s3+='
                    $a\\\\end{hashtrielisting}'
        [ -n "$s3" ] && c+="|
ssed -R '$s3'"
    fi

    $x "$c"
}

