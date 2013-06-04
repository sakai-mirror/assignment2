#!/bin/sh

echo "Stubbing out taggable for ..."
patch -p0 < patches/remove-taggable-functionality-stub-false.patch
echo "Removing edit points functionality ..."
patch -p0 < patches/editPoints.patch
echo "Reverting Spring 2->3 changes..."
patch -R -p0 < patches/springUpdate.patch    

