#!/bin/bash
while getopts ':V:L' flag; do
  case "${flag}" in
    V) VERSION=$OPTARG ;;
    L) LIST="YES" ;;
  esac
done

if [[ $VERSION ]]; then
    candidates=$(find $(dirname $BASH_SOURCE) -maxdepth 1 -name "sss$VERSION*")
else
    candidates=$(find $(dirname $BASH_SOURCE) -maxdepth 1 -name "sss[0-9]*")
fi

if [[ $LIST ]]; then
    echo "$candidates"
    exit
fi

if [[ $candidates ]]; then
    exec "$(echo "$candidates" | sort -V | tail -n 1)" "$@"
else
    echo "Failed to find a SSS version matching: \"sss$VERSION\""
    echo "The versions available are:"
    ls $(dirname $BASH_SOURCE) | grep "^sss[0-9.]\+$" | sed 's/^/    * /'
fi
