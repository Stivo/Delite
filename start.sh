#!/bin/sh

export JAVA_OPTS="-Xmn10m -Xms200m -Xmx2g "
#export PROG="ppl.apps.ml.gda.GDARunner"
#export PROG="HelloWorldRunner"
#export PROG="ppl.apps.ml.linreg.LinRegRunner"
#export PROG="epfl.apps.distributed.ToyRunner"
#export PROG="ppl.apps.ml.ToyRunner"
#export PROG="ppl.apps.ml.WebLogAnalyzerRunner"
export PROG="epfl.apps.distributed.WebLogAnalyzerRunner"

set -e
set -x

#sbt "project distributed-apps" compile
#sbt "project optiml-apps" compile

pkill gedit || true

#bin/delitec epfl.apps.distributed.ToyRunner
bin/delitec $PROG

! grep "warning\|error" delitec.log 

#
gedit $PROG.scala delitec.log generated/scala/kernels/* start.sh &


cp $PROG.deg out.deg

pkill eog || true

cd ../DegVisualizer/; sbt "run"; cd -; # -controlDeps


dot -Tpng -o out.png out.dot && eog out.png &

