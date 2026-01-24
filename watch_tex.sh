#!/bin/bash

WATCH_FILE="whitepaper.tex"

# Check if inotifywait is installed
if ! command -v inotifywait &> /dev/null; then
    echo "Error: 'inotifywait' command not found."
    echo "Please install inotify-tools (e.g., sudo apt-get install inotify-tools)."
    exit 1
fi

echo "Watching $WATCH_FILE for changes..."

while true; do
    # Wait for the file to be written to and closed
    inotifywait -q -e close_write "$WATCH_FILE"
    
    echo "Change detected. Recompiling..."
    make
    make clean
    echo "--------------------------------------"
done