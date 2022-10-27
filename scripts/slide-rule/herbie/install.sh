#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# configuration
if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=herbie/
fi

# check for toolchain
which racket
if [ "$?" != "0" ]; then
    echo "Racket not found"
    exit 1
fi

which cargo
if [ "$?" != "0" ]; then
    echo "Rust not found"
    exit 1
fi

# Install Herbie
echo "Installing Herbie"
mkdir -p $BUILD_DIR
git clone https://github.com/herbie-fp/herbie.git $BUILD_DIR ||    \
    echo "Herbie already checked out"

# copy hacked datafile to extract frontier
cp ruler-datafile.rkt $BUILD_DIR/src/datafile.rkt

pushd $BUILD_DIR

git pull
git checkout $HERBIE_COMMIT
make install

popd    # BUILD_DIR


