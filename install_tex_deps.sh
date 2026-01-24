#!/bin/bash

# Script to install missing LaTeX packages for whitepaper.tex

echo "Checking for package managers..."

if command -v apt-get &> /dev/null; then
    echo "APT package manager found (Debian/Ubuntu). Installing texlive-latex-extra..."
    # titlesec and many formatting packages are in latex-extra
    # mathptmx is in fonts-recommended
    sudo apt-get update
    sudo apt-get install -y texlive-latex-extra texlive-fonts-recommended
elif command -v dnf &> /dev/null; then
    echo "DNF package manager found (Fedora/RHEL). Installing packages..."
    sudo dnf install -y texlive-titlesec texlive-geometry texlive-xcolor texlive-fancyhdr texlive-graphics texlive-hyperref texlive-booktabs texlive-setspace texlive-caption texlive-float texlive-psnfss
elif command -v pacman &> /dev/null; then
    echo "Pacman package manager found (Arch). Installing texlive-latexextra..."
    sudo pacman -S --noconfirm texlive-latexextra
elif command -v tlmgr &> /dev/null; then
    echo "TeX Live Manager (tlmgr) found. Installing packages..."
    # Install specific packages used in whitepaper.tex
    # psnfss contains mathptmx, graphics contains graphicx
    sudo tlmgr install titlesec geometry xcolor fancyhdr graphics hyperref booktabs setspace caption float psnfss
else
    echo "No supported package manager found. Please install the 'titlesec' package manually."
    exit 1
fi

echo "Installation complete. You can now compile the document."