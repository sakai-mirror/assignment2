#!/bin/sh

echo "Reverting Spring 2->3 changes..."
patch -R -p0 < patches/springUpdate.patch

    
