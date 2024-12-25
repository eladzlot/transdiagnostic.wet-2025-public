#!/bin/bash

#################################
# Generate progress image
#################################

cd images || exit # Navigate to the images directory, exit if it doesn't exist

# Step 1: Compile LaTeX to PDF
pdflatex -interaction=batchmode -no-shell-escape progress

# Step 2: Crop the PDF to remove unnecessary whitespace
pdfcrop --margin 0 progress.pdf progress-crop.pdf

# Step 3: Convert the cropped PDF to a JPEG image
pdftoppm -png -r 600 -singlefile progress-crop.pdf progress

# Step 4: Resize the image to ensure it fits within the paper width
# Example: Resize to a maximum width of 1200 pixels
convert progress.png -resize 3600x progress-resized.png

cd ..
