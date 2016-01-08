mpd ~/.config/mpdconf&
compton&
xscreensaver&
autocutsel&
urxvtd&
dunst&
blueberry&
source ~/.localxinitrc
xhost +local:
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
exec xmonad
