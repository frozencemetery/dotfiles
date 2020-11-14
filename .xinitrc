#!/usr/bin/zsh

# I would like my env vars please
source .zshrc

emacs --daemon &
xrdb -override .Xresources
xsetroot -solid black &
xscreensaver -no-splash &
xss-lock -- xscreensaver-command --lock &
/home/bos/rharwood/.fehbg &
setxkbmap -option compose:caps &
xset r rate 200 30 &
unclutter -not QEMU &
xsetroot -cursor_name X_cursor &
rm -f ~/.ssh/master* &
redshift &
xmodmap .Xmodmap

exec xmonad
