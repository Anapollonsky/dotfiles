mpd ~/.config/mpdconf&
compton & # compton daemon
source ~/.hs
xhost +local:
xrdb ~/.Xresources
xscreensaver&
autocutsel&
urxvtd&
feh --bg-scale ~/dotfiles/background.jpg
exec xmonad
