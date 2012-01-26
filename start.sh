#!/bin/sh

export JAVA_OPTS="-Xmn10m -Xms200m -Xmx2g "
#export PROG="ppl.apps.ml.gda.GDARunner"
#export PROG="HelloWorldRunner"
#export PROG="ppl.apps.ml.linreg.LinRegRunner"
#export PROG="epfl.apps.distributed.ToyRunner"
#export PROG="ppl.apps.ml.ToyRunner"
export PROG="epfl.apps.distributed.WebLogAnalyzerRunner"
set -e
set -x

sbt "project distributed-apps" compile

set +e
pkill gedit
set -e

#bin/delitec epfl.apps.distributed.ToyRunner
bin/delitec $PROG
#
gedit generated/scala/kernels/* &

cp $PROG.deg out.deg

cd ../DegVisualizer/; sbt "run"; cd -; # -controlDeps

dot -Tpng -o out.png out.dot && eog out.png &

