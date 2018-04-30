dunst&
nm-applet&
~/dotfiles/cvim_server.py &
xhost +local:
export LANG=en_US.UTF-8
xrdb ~/.Xresources
autorandr --change
# xrandr --output HDMI-2 --left-of eDP-1 --mode 1920x1080
# xrandr --output DP-1 --left-of HDMI-2 --mode 1920x1080
# xrandr --output eDP-1 --mode 1366x768
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
feh --randomize --bg-fill ~/dotfiles/wallpapers/*
polybar main &
gnome-screensaver &
devmon &
xmonad --recompile
exec xmonad
# exec twm

