#mpd ~/.config/mpdconf&
compton&
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
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
xscreensaver -no-splash &
exec xmonad
