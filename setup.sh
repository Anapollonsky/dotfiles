#!/bin/zsh

# Script for automatically installing the configuration files.
# Andrew Apollonsky


# Get correct home directory even with sudo:
#http://stackoverflow.com/questions/7358611/bash-get-users-home-directory-when-they-run-a-script-as-root
HOME_DIR=$(getent passwd $SUDO_USER | cut -d: -f6)
SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DIR=$(dirname "$SCRIPT_PATH")

mkdir -p $HOME_DIR/.config/{dunst,termite,mopidy}
mkdir -p $HOME_DIR/.xmonad

# Associative array mapping each dotfile to its destination.
typeset -A file_dest_map
file_dest_map=($SCRIPT_DIR/zshrc $HOME_DIR/.zshrc
               $SCRIPT_DIR/xmonad.hs $HOME_DIR/.xmonad/xmonad.hs
               $SCRIPT_DIR/xprofile $HOME_DIR/.xprofile
               $SCRIPT_DIR/zpreztorc $HOME_DIR/.zpreztorc 
               $SCRIPT_DIR/tmux.conf $HOME_DIR/.tmux.conf 
               $SCRIPT_DIR/xmobarrc $HOME_DIR/.xmobarrc
               $SCRIPT_DIR/xinitrc $HOME_DIR/.xinitrc
               $SCRIPT_DIR/mopidy.conf $HOME_DIR/.config/mopidy/mopidy.conf
               $SCRIPT_DIR/mpdconf $HOME_DIR/.config/mpdconf
               $SCRIPT_DIR/termite $HOME_DIR/.config/termite/config
               $SCRIPT_DIR/pentadactylrc $HOME_DIR/.pentadactylrc
               $SCRIPT_DIR/Xresources $HOME_DIR/.Xresources
               $SCRIPT_DIR/conkyrc $HOME_DIR/.conkyrc
               $SCRIPT_DIR/dunstrc $HOME_DIR/.config/dunst/dunstrc
               $SCRIPT_DIR/rofi_dmenu /usr/bin/rofi_dmenu # Messy 
              )

# install the configuration files
if [[ $# -eq 1 && $1 = "install" ]]  || [[ $# -eq 0 ]]
then
    echo "Performing install operation..."
    echo "Treating $HOME_DIR as home and $SCRIPT_DIR as config file directory for install..."    
    for k in "${(@k)file_dest_map}"; do # http://superuser.com/questions/737350/iterating-over-keys-or-k-v-pairs-in-zsh-associative-array
        ln -sv $k $file_dest_map[$k]
    done
    if [[ ! -a $HOME_DIR/.ss ]]
    then
        touch $HOME_DIR/.ss
    fi
    exit 0
    chmod 755 /usr/bin/rofi_dmenu
    
fi

# remove configuration files...
if [[ $# -eq 1 && $1 = "clean" ]]
then
    echo "Performing clean operation..."
    echo "Treating $HOME_DIR as home and $SCRIPT_DIR as config file directory for clean..."
    if [[ ! -d $SCRIPT_DIR/backup ]]
    then
        echo "Creating $SCRIPT_DIR/backup..."
        mkdir $SCRIPT_DIR/backup
    fi
    # Loop through the files, move them to the backup folder.
    for k in $file_dest_map; do
        mv -vb $k $SCRIPT_DIR/backup/ 
    done    
    exit 0
fi

# Do both!
if [[ $# -eq 1 && $1 = "reinstall" ]]
then
    ./$0 clean && ./$0 install
fi
