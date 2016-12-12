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
# $ hash-trie-gen-test -g test -T types
#
[[ "$1" =~ ^-u[0-9]+$ ]] &&
u="${1:2}" ||
u=""

diff -u$u -L types.old <(echo \
'$ test -x ./hash-trie
$ print() { printf '\''%s\n'\'' "$@"; }
$ hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }
$ ./hash-trie -T
op	operand	operator	wider	result
add	char	int	int	int
add	signed char	int	int	int
add	unsigned char	int	int	int
add	short	int	int	int
add	unsigned short	int	int	int
add	int	int	long long	long long
add	unsigned int	unsigned int	unsigned long long	long long
sub	char	int	int	int
sub	signed char	int	int	int
sub	unsigned char	int	int	int
sub	short	int	int	int
sub	unsigned short	int	int	int
sub	int	int	long long	long long
sub	unsigned int	unsigned int	unsigned long long	long long'
) -L types.new <(
echo '$ test -x ./hash-trie'
test -x ./hash-trie 2>&1 ||
echo 'command failed: test -x ./hash-trie'

echo '$ print() { printf '\''%s\n'\'' "$@"; }'
print() { printf '%s\n' "$@"; } 2>&1 ||
echo 'command failed: print() { printf '\''%s\n'\'' "$@"; }'

echo '$ hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }'
hash-trie-config() { hash-trie -P|sed -r '/^(TOLERANCE|TRIE_SIZE):/!d'; } 2>&1 ||
echo 'command failed: hash-trie-config() { hash-trie -P|sed -r '\''/^(TOLERANCE|TRIE_SIZE):/!d'\''; }'

echo '$ ./hash-trie -T'
./hash-trie -T 2>&1 ||
echo 'command failed: ./hash-trie -T'
)

