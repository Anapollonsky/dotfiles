export PATH=~/bin:/home/apple/.cabal/bin:$PATH
export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient
xrandr --dpi 96
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
