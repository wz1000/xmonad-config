#!/bin/bash
set -x
cd ~/.xmonad
stack install
if [ $? != "0" ]; then
    read
else
  rm xmonad-x86_64-linux
  cp ~/.local/bin/xmonad xmonad-x86_64-linux
  cd ~;
  xmonad --restart;
  if [ $? != "0" ]; then
      read
  fi
fi
    
