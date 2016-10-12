#mpd ~/.config/mpdconf&
compton&
xscreensaver -no-splash &
urxvtd&
dunst&
#blueberry&
taffybar&
nm-applet&
~/dotfiles/cvim_server.py &
#source ~/.localxinitrc
xhost +local:
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
xcape -e 'Control_L=Escape'
exec xmonad
