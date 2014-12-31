#!/bin/bash

# Get correct home directory even with sudo:
#http://stackoverflow.com/questions/7358611/bash-get-users-home-directory-when-they-run-a-script-as-root
HOME_DIR=$(getent passwd $SUDO_USER | cut -d: -f6)
SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DIR=$(dirname "$SCRIPT_PATH")
echo "Treating $HOME_DIR as home and $SCRIPT_DIR as config file directory..."

# install the configuration files
if [[ $# -eq 1 && $1 = "install" ]]  || [[ $# -eq 0 ]]
then
    echo "Performing install operation..."
    ln -sv $SCRIPT_DIR/conkerorrc $HOME_DIR/.conkerorrc
    ln -sv $SCRIPT_DIR/zshrc $HOME_DIR/.zshrc
#    ln -sv $SCRIPT_DIR/i3config /etc/i3/config
    ln -sv $SCRIPT_DIR/xmonad.hs $HOME_DIR/.xmonad/xmonad.hs
    ln -sv $SCRIPT_DIR/Xresources $HOME_DIR/.Xresources
    ln -sv $SCRIPT_DIR/xprofile $HOME_DIR/.xprofile    
fi

# remove configuration files...
if [[ $# -eq 1 && $1 = "clean" ]]
then
    echo "Performing clean operation..."
    if [[ ! -d $SCRIPT_DIR/backup ]]
    then
	echo "Creating $SCRIPT_DIR/backup..."
	mkdir $SCRIPT_DIR/backup
    fi
    mv -vb $HOME_DIR/.conkerorrc $SCRIPT_DIR/backup/
    mv -vb $HOME_DIR/.zshrc $SCRIPT_DIR/backup/
#    mv -vb /etc/i3/config $SCRIPT_DIR/backup/
    mv -vb $HOME_DIR/.Xresources $SCRIPT_DIR/backup/
    mv -vb $HOME_DIR/.xmonad/xmonad.hs $SCRIPT_DIR/backup/
    mv -vb $HOME_DIR/.xprofile $SCRIPT_DIR/backup/        
fi

if [[ $# -eq 1 && $1 = "reinstall" ]]
then
    sudo ./$0 clean
    sudo ./$0 install
fi
