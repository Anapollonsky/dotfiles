mpd ~/.config/mpdconf&
compton & # compton daemon
source ~/.hs
xhost +local:
xrdb ~/.Xresources
xscreensaver&
autocutsel&
urxvtd&
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
exec xmonad
