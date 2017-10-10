#!/bin/bash

DIR=production_build
TEMP_DIR=$DIR/tmp/upload
EXE=$DIR/output-static/sgHomePage-exe

function print_help {
	echo "usage: $0 HOST"
	echo
	echo "arguments:"
	echo -e "\tHOST: must be a valid argument for ssh/rsync, e.g.: user@hostname:directory"
}

if [[ "$1" == "" ]]; then
	print_help
	exit 1
fi
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
	print_help
	exit 0
fi


DEST="$1"

if [[ ! -f $EXE ]]; then
	echo "executable not found. Please run "./production_build/build.sh --static" first!"
	exit 1
fi

rm -r -f $TEMP_DIR
mkdir -p $TEMP_DIR

echo "packing data for upload..."
rsync -r \
	--exclude="$DIR" \
	--exclude-from="$DIR/.exclude" \
	"./" "$TEMP_DIR/"

BIN="$TEMP_DIR/bin"

mkdir -p "$BIN"
cp "$EXE" "$BIN/"

rsync -rzv --update --delete \
	"$TEMP_DIR/" "$DEST"
