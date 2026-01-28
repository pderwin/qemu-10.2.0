#!/bin/sh

mkdir -p build

while [ 1 ] ; do
    inotifywait \
	-q \
	-r \
	--exclude build \
	--timeout 60 \
	--event close_write \
	.

    make  > /tmp/compile.log
    head -20 /tmp/compile.log
    date
done
