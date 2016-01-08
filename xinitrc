mpd ~/.config/mpdconf&
compton&
xscreensaver&
autocutsel&
urxvtd&
dunst&
source ~/.localxinitrc
xhost +local:
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
exec xmonad
