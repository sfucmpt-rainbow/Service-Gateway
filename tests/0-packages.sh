#!/bin/bash

PACKAGES="erlang python2 ruby"
INSTALLED=`/usr/bin/pacman -Q`
for package in $PACKAGES; do
	if [ -z "`echo $INSTALLED | grep $package`" ]; then
		RESULT=`/usr/bin/pacman -Sy $package --noconfirm 2> /dev/null`
		if [ -z "`echo $RESULT | grep installing`" ]; then
			echo "Could not install $package, aborting..."
			exit 1
		fi
	fi
done

exit 0
