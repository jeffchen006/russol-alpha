#!/bin/bash

# Define the source and destination directories
SOURCE_DIR="/home/zhiychen/.rustup/toolchains/nightly-2022-09-14-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/"
DEST_DIR="/home/zhiychen/Documents/russol-alpha/target/debug/deps"

# Check if the source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Source directory does not exist: $SOURCE_DIR"
    exit 1
fi

# Check if the destination directory exists
if [ ! -d "$DEST_DIR" ]; then
    echo "Destination directory does not exist: $DEST_DIR"
    exit 1
fi

# Copy all .so files from the source to the destination
echo "Copying .so files from $SOURCE_DIR to $DEST_DIR"
cp -v "$SOURCE_DIR"/*.so "$DEST_DIR"

# Completion message
echo "Move completed."
