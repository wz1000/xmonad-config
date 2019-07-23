#!/bin/bash
set -x
cd ~/.xmonad
cabal new-build
if [ $? != "0" ]; then
    read
else
  x=$(find ./dist-newstyle/ -type f -executable -name xmonad -print -quit)
  if [ ! -f "$x" ]; then
    echo "Couldn't find executable xmonad in dist-newstyle"
    read
    exit
  fi
  rm xmonad-x86_64-linux
  cp "$x" xmonad-x86_64-linux
  cp "$x" ~/.local/bin/xmonad
  xmonad --restart
  if [ $? != "0" ]; then
      read
  fi
fi
    
