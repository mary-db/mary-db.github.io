#!/bin/bash

# Copy latest HTML from your analysis folder
cp "/Users/zukini/Library/Mobile Documents/com~apple~CloudDocs/Downloads/LCAVulnerability/LCAVulPooled.html" ~/Documents/mary-db.github.io/lca_vulnerability.html

# Push to GitHub
cd ~/Documents/mary-db.github.io
git add .
git commit -m "Update site $(date '+%Y-%m-%d %H:%M')"
git push

echo "Site updated!"
