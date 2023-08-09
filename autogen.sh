#! /bin/sh
# This script is used to automate the process of running some of the autotools
# against their input files (configure.in, Makefile.am) after _any_ of them have
# been updated. The commands and parameters were taken based on a similar script
# found via google and seeing the same commands issued in GNU autotool tutorials.
libtoolize -cv
aclocal
automake -c --add-missing --force-missing
autoconf
