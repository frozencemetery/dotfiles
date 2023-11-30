#!/usr/bin/zsh

# I would like my env vars please
source .zshrc

xrdb -override .Xresources
xsetroot -solid black &
# xss-lock -- slock &
~/.fehbg &
setxkbmap -option compose:caps &
xset r rate 200 30 &
unclutter -not QEMU &
xsetroot -cursor_name X_cursor &
rm -f ~/.ssh/master* &

# If you don't do this, gtk applications hang for minutes on launch
exec dbus-launch xmonad
