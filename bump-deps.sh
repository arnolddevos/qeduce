#! /bin.bash

for f in *.sbt
do 
  b=../${f%%.sbt}/target/$f
  if [ -f $b ]
  then 
    cp $b $f
  fi
done
