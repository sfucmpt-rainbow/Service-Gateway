#!/bin/sh

# Ensure daemon is running
START=`rc.d restart rsg`
if [ -z "`rc.d status rsg | grep STARTED`" ]; then
	echo "Could not start the RSG service, start log:"
	echo $START
	exit 1
fi

exit 0

