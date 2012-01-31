#!/bin/sh

STOP=`rc.d stop rsg`
if [ -z "`rc.d status rsg | grep STOPPED`" ]; then
	echo "Could not gracefully stop the RSG service, stop log:"
	echo $STOP
	exit 1
fi

exit 0

