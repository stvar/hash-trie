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

PROG := hash-trie

ifdef PROF
PROG := ${PROG}-profiled
endif

GCC := gcc
GCC_STD := gnu++11

CFLAGS := -std=${GCC_STD} -Wall -Wextra

ifndef OPT
CFLAGS += -DDEBUG
ifdef DEBUG
CFLAGS += $(addprefix -DDEBUG_,${DEBUG})
endif
endif

ifdef CONFIG
CFLAGS += \
	$(addprefix -DCONFIG_HASH_TRIE_,\
		$(subst CHAR_TYPE=char,CHAR_TYPE=0,\
			$(subst CHAR_TYPE=wchar,CHAR_TYPE=1,\
				${CONFIG})))
endif

CFLAGS += -DPROGRAM=${PROG}

ifdef PROF
CFLAGS += -pg
ifdef OPT
CFLAGS += -g -gdwarf-3
endif
endif

ifdef OPT
CFLAGS += -O${OPT}
else
CFLAGS += -g -gdwarf-3
endif

ifeq (${GIMPLE},yes)
CFLAGS += -fdump-tree-gimple
endif

LIBS := -lstdc++

SRC := hash-trie.cpp

default: ${PROG} ${PROG}-trie-size-55-tolerance-1

${PROG}: ${SRC}
	${GCC} ${CFLAGS} $< -o $@ ${LIBS}
ifeq (${GIMPLE},yes)
	mv ${SRC}.004t.gimple ${PROG}.gimple
endif

${PROG}-trie-size-55-tolerance-1: ${SRC}
	${GCC} ${CFLAGS} -DCONFIG_HASH_TRIE_TRIE_SIZE=55 -DCONFIG_HASH_TRIE_TOLERANCE=1 $< -o $@ ${LIBS}


