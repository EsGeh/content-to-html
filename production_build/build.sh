#!/usr/bin/env bash

BASE_DIR=production_build

BASE_IMG=fcpo/base
BASE_DOCKERFILE=$BASE_DIR/build/dockerfiles/Dockerfile.base

REBUILD=0

while [[ $# -gt 0 ]]; do

	key="$1"
	case $key in
		--rebuild)
			REBUILD=1
			shift
			;;
		--static)
			STATIC=1
			shift
			;;
		*)
			exit 1
			;;
	esac
done

if [[ "$STATIC" == "1" ]]; then
	echo "static compilation..."
	BUILD_IMG=fcpo/static
	DOCKERFILE=$BASE_DIR/build/dockerfiles/Dockerfile.static
	OUTDIR=$BASE_DIR/output-static
else
	BUILD_IMG=fcpo/def
	DOCKERFILE=$BASE_DIR/build/dockerfiles/Dockerfile.default
	OUTDIR=$BASE_DIR/output-default
fi

if [[ "$(docker images -q $BASE_IMG 2> /dev/null)" == "" || $REBUILD == 1 ]]; then
	docker image build --tag $BASE_IMG -f $BASE_DOCKERFILE .
fi

if [[ "$(docker images -q $BUILD_IMG 2> /dev/null)" == "" || $REBUILD == 1 ]]; then
	docker image build --tag $BUILD_IMG -f $DOCKERFILE .
fi

docker container run \
	-v $(pwd):/usr/src \
	$BUILD_IMG $OUTDIR
