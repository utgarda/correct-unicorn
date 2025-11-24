#!/bin/bash
# Update AUR package for correct-unicorn
# Usage: ./scripts/update-aur.sh <version>
# Example: ./scripts/update-aur.sh 0.2.0

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 0.2.0"
    exit 1
fi

VERSION="$1"
# Convert version format: 0.2.0 -> 0.2.0, 0.2.0-rc1 -> 0.2.0_rc1
PKGVER="${VERSION//-/_}"

echo "Updating AUR package to version $VERSION (pkgver: $PKGVER)"

# Setup directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
AUR_DIR="$PROJECT_ROOT/../aur-correct-unicorn"

# Clone or update AUR repository
if [ ! -d "$AUR_DIR" ]; then
    echo "Cloning AUR repository..."
    cd "$(dirname "$AUR_DIR")"
    git clone ssh://aur@aur.archlinux.org/correct-unicorn.git aur-correct-unicorn
else
    echo "Pulling latest changes from AUR..."
    cd "$AUR_DIR"
    git pull
fi

cd "$AUR_DIR"

# Update PKGBUILD
echo "Updating PKGBUILD..."
sed -i "s/^pkgver=.*/pkgver=$PKGVER/" PKGBUILD
sed -i "s/^_tagver=.*/_tagver=$VERSION/" PKGBUILD
sed -i "s/^pkgrel=.*/pkgrel=1/" PKGBUILD

echo ""
echo "=== Updated PKGBUILD ==="
cat PKGBUILD
echo ""

# Generate .SRCINFO
echo "Generating .SRCINFO..."
makepkg --printsrcinfo > .SRCINFO

echo ""
echo "=== Generated .SRCINFO ==="
cat .SRCINFO
echo ""

# Show diff
echo "=== Changes ==="
git diff

# Commit and push
read -p "Commit and push these changes? (y/N) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    git add PKGBUILD .SRCINFO
    git commit -m "Update to version $VERSION"
    git push origin master
    echo ""
    echo "✓ AUR package updated successfully!"
    echo "Check: https://aur.archlinux.org/packages/correct-unicorn"
else
    echo "Changes not committed. You can review and commit manually:"
    echo "  cd $AUR_DIR"
    echo "  git add PKGBUILD .SRCINFO"
    echo "  git commit -m 'Update to version $VERSION'"
    echo "  git push origin master"
fi
