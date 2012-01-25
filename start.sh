#!/bin/sh

export JAVA_OPTS="-Xmn10m -Xms200m -Xmx2g "

set -e
set -x

sbt "project distributed-apps" compile

set +e
pkill gedit
set -e

bin/delitec epfl.apps.distributed.ToyRunner

gedit generated/scala/kernels/* &

cp epfl.apps.distributed.ToyRunner.deg out.deg

cd ../DegVisualizer/; sbt "run"; cd -;

dot -Tpng -o out.png out.dot && eog out.png &
