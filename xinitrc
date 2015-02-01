export PATH=~/bin:/home/apple/.cabal/bin:$PATH
export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient
feh --bg-scale ~/dotfiles/background.jpg
xrdb -merge ~/.Xresources
urxvtd&
mpd &
exec xmonad
