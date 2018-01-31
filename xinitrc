source ~/.localxinitrc
dunst&
nm-applet&
~/dotfiles/cvim_server.py &
xhost +local:
export LANG=en_US.UTF-8
xrdb ~/.Xresources
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
polybar main &
gnome-screensaver &
devmon & 
exec xmonad
# exec twm

