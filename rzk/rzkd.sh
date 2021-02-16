#!/bin/sh

FILE=$1

old="first time"

while true; do
  new=$(stat -f %m "$FILE")
  if [ "$old" != "$new" ]; then
    old=$new
    clear
    stack build rzk && stack exec -- rzk typecheck "$FILE"
  fi
  sleep 1
done
