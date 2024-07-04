#!/bin/bash

FILE=$1

if [[ "${FILE}" == "" ]]; then
   echo "usage: $0 file"
   exit 1
fi

EYE="${EYE_BIN:-eye}"

${EYE} --nope --quiet --pass --n3p ${FILE}