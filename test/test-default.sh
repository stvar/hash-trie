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

#
# File generated by a command like:
# $ hash-trie-gen-test -g test -T default
#
[[ "$1" =~ ^-u[0-9]+$ ]] &&
u="${1:2}" ||
u=""

diff -u$u -L default.old <(echo \
'$ test -x ./hash-trie
$ print() { printf '\''%s\n'\'' "$@"; }
$ hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }
$ hash-trie() { ./hash-trie "$@"; }
$ hash-trie-config
TOLERANCE:    1000
TRIE_SIZE:    32767
$ print a b c be bet ben bent af|hash-trie -Lq --debug=probing
!!! constants: trie_size=32767 tolerance=1000 alpha=20219 mod_x=32715 max_h=32741 max_x=12496
!!! probing:27: str="be" c='\''e'\'' x=20219 last_h=21246 h=20246-20247 [2]
!!! probing:27: str="bet" c='\''t'\'' x=7723 last_h=8750 h=7750-7751 [2]
!!! probing:27: str="bent" c='\''t'\'' x=27942 last_h=28969 h=27969-27970 [2]
!!! probing:27: str="af" c='\''f'\'' x=15446 last_h=16473 h=15473-15474 [2]
$ print a b c be bet ben bent af|hash-trie -d
p	link[p]	ch[p]	sibling[p]	count[p]	word
0	0	header	26	0	
1	15474	1	0	1	a
2	20247	2	1	1	b
3	0	3	2	1	c
4	0	4	3	0	d
5	0	5	4	0	e
6	0	6	5	0	f
7	0	7	6	0	g
8	0	8	7	0	h
9	0	9	8	0	i
10	0	10	9	0	j
11	0	11	10	0	k
12	0	12	11	0	l
13	0	13	12	0	m
14	0	14	13	0	n
15	0	15	14	0	o
16	0	16	15	0	p
17	0	17	16	0	q
18	0	18	17	0	r
19	0	19	18	0	s
20	0	20	19	0	t
21	0	21	20	0	u
22	0	22	21	0	v
23	0	23	22	0	w
24	0	24	23	0	x
25	0	25	24	0	y
26	0	26	25	0	z
...	.	.	.	.	.
7751	20252	header	7771	0	
...	.	.	.	.	.
7765	27970	14	7751	1	ben
...	.	.	.	.	.
7771	0	20	7765	1	bet
...	.	.	.	.	.
15474	1	header	15480	0	
...	.	.	.	.	.
15480	0	6	15474	1	af
...	.	.	.	.	.
20247	2	header	20252	0	
...	.	.	.	.	.
20252	7751	5	20247	1	be
...	.	.	.	.	.
27970	7765	header	27990	0	
...	.	.	.	.	.
27990	0	20	27970	1	bent
...	.	.	.	.	.
$ print a b c be bet ben bent af|hash-trie -p
a	1
b	1
c	1
ben	1
bet	1
af	1
be	1
bent	1'
) -L default.new <(
echo '$ test -x ./hash-trie'
test -x ./hash-trie 2>&1 ||
echo 'command failed: test -x ./hash-trie'

echo '$ print() { printf '\''%s\n'\'' "$@"; }'
print() { printf '%s\n' "$@"; } 2>&1 ||
echo 'command failed: print() { printf '\''%s\n'\'' "$@"; }'

echo '$ hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }'
hash-trie-config() { hash-trie -P|sed -r '/^(TOLERANCE|TRIE_SIZE):/!d'; } 2>&1 ||
echo 'command failed: hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }'

echo '$ hash-trie() { ./hash-trie "$@"; }'
hash-trie() { ./hash-trie "$@"; } 2>&1 ||
echo 'command failed: hash-trie() { ./hash-trie "$@"; }'

echo '$ hash-trie-config'
hash-trie-config 2>&1 ||
echo 'command failed: hash-trie-config'

echo '$ print a b c be bet ben bent af|hash-trie -Lq --debug=probing'
print a b c be bet ben bent af|hash-trie -Lq --debug=probing 2>&1 ||
echo 'command failed: print a b c be bet ben bent af|hash-trie -Lq --debug=probing'

echo '$ print a b c be bet ben bent af|hash-trie -d'
print a b c be bet ben bent af|hash-trie -d 2>&1 ||
echo 'command failed: print a b c be bet ben bent af|hash-trie -d'

echo '$ print a b c be bet ben bent af|hash-trie -p'
print a b c be bet ben bent af|hash-trie -p 2>&1 ||
echo 'command failed: print a b c be bet ben bent af|hash-trie -p'
)
