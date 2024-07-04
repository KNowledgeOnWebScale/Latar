#!/bin/bash

FILE=$1

if [[ "${FILE}" == "" ]]; then
   echo "usage: $0 file"
   exit 1
fi

EYE="${EYE_BIN:-eye}"

${EYE} --nope --quiet --no-bnode-relabeling --ignore-inference-fuse $1 --intermediate /dev/stdout | \
    grep "^\'" | sed -e 's/<https:\/\/eyereasoner[^#]\{1,\}#e_\([^>]\{1,\}\)>/_:\1/g'