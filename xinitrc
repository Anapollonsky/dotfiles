mpd ~/.config/mpdconf&
compton&
xscreensaver&
autocutsel&
urxvtd&
source ~/.localxinitrc
xhost +local:
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
exec xmonad
