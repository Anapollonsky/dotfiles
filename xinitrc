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
xcape -e 'Control_L=Escape'
exec xmonad
