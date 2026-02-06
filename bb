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

    set-title qemu build start
    make -j 20  > /tmp/compile.log
    rc=$?

    if [ $rc -ne 0 ]; then
	head -20 /tmp/compile.log
	set-title qemu build error
    else
	set-title qemu build success
    fi

    date
done
