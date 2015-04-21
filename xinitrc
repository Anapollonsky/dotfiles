export PATH=~/bin:/home/apple/.cabal/bin:$PATH
export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient
mpd ~/.config/mpdconf&
compton & # compton daemon
source ~/.hs
feh --bg-scale ~/dotfiles/background.jpg
xhost +local:
xrdb ~/.Xresources
xscreensaver&
exec xmonad
