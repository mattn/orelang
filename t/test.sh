#!/bin/bash

DIR=$(cd $(dirname $0);pwd)

ls $DIR/*.ore |\
while read f; do
  $DIR/../ore $DIR/lib/tester.ore $f
done
