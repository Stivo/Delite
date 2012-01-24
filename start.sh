#!/bin/sh

export JAVA_OPTS="-Xmn10m -Xms200m -Xmx2g "

set -e
set -x

sbt compile
set +e
pkill gedit
set -e
bin/delitec epfl.apps.distributed.ToyRunner

gedit generated/scala/kernels/* &
