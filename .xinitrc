#!/usr/bin/zsh

emacs --daemon &
xrdb -merge .Xresources &
xsetroot -solid black &
xscreensaver -no-splash &
xss-lock -- xscreensaver-command --lock &
/home/bos/rharwood/.fehbg &
(setxkbmap -option compose:caps ; xmodmap ~/.Xmodmap.apple) &
xset r rate 200 30 &
unclutter -not QEMU &
xsetroot -cursor_name X_cursor &
rm -f ~/.ssh/master* &
redshift &

exec xmonad
