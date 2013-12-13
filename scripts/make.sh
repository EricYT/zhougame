#!/bin/bash

#set -x

MAKE_FILE=make.erl

if [ -f $MAKE_FILE ]
then
	escript $MAKE_FILE;echo "Make All Files"
else
	echo "Not found file $MAKE_FILE"
fi
