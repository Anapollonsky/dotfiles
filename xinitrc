source ~/.localxinitrc
dunst&
nm-applet&
polybar main&
~/dotfiles/cvim_server.py &
xhost +local:
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
gnome-screensaver &
exec xmonad
# exec twm

