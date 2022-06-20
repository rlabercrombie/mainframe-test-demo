#!/bin/bash

#################################################################
# This program will pre/compile a cobol program with embedded sql
# 
# Usage: 
# 	./comp <file(without-extension)>
#
# Input: 
# 			../src/<file>.cbl
# Output: 
#			../comp/<file>.cob
# 			../exe/<file>
#
# Run the executable with '..exe/<file>'
#################################################################

ocesql "../src/$1.cbl" "../comp/$1.cob"
if [ $? -ne 0 ]; then
	echo "failed to precompile sql..."
fi

cobc -x "../comp/$1".cob -o "../exe/$1" \
		/ocesql/dblib/ocdblog.o \
		/ocesql/dblib/ocdbutil.o \
		/ocesql/dblib/ocesql.o \
		/ocesql/dblib/ocpgsql.o \
		-I$COPY -I$SCR -locesql -lpq

if [ $? -ne 0 ]; then
	echo "failed to compile..."
fi
