#!/bin/sh

set -e
set -x

cd ../virtualization-lms-core; sbt publish-local; cd -

cp ~/.ivy2/local/EPFL/lms/0.1/jars/lms.jar ./lib_managed/jars/EPFL/lms/lms-0.1.jar 

sbt "project distributed-apps" "~ compile"

