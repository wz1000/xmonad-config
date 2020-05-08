#!/bin/bash
set -x
cd ~/.xmonad
cabal new-build
if [ $? != "0" ]; then
    read
else
  x=$(cabal new-exec --verbose=0 --offline sh -- -c 'which xmonad')
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
    
