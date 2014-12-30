#!/bin/bash

# Get correct home directory even with sudo:
#http://stackoverflow.com/questions/7358611/bash-get-users-home-directory-when-they-run-a-script-as-root
HOMEDIR=$(getent passwd $SUDO_USER | cut -d: -f6) t
SCRIPTPATH=$(readlink -f "$0")
SCRIPTDIR=$(dirname "${SCRIPTPATH}")
echo "Treating ${HOMEDIR} as home and ${SCRIPTDIR} as config file directory..."

# install the configuration files
if [[ $# -eq 1 && $1 = "install" ]]  || [[ $# -eq 0 ]]
then
    echo "Performing install operation..."
    ln -sv ${SCRIPTDIR}/.conkerorrc ${HOMEDIR}/.conkerorrc
    ln -sv ${SCRIPTDIR}/.zshrc ${HOMEDIR}/.zshrc
    ln -sv ${SCRIPTDIR}/.i3config /etc/i3/config
fi

# remove configuration files...
if [[ $# -eq 1 && $1 = "clean" ]]
then
    echo "Performing clean operation..."
    if [[ ! -d $SCRIPTDIR/backup ]]
    then
	echo "Creating $SCRIPTDIR/backup..."
	mkdir $SCRIPTDIR/backup
    fi
    mv -vb ${HOMEDIR}/.conkerorrc $SCRIPTDIR/backup/
    mv -vb ${HOMEDIR}/.zshrc $SCRIPTDIR/backup/
    mv -vb /etc/i3/config $SCRIPTDIR/backup/
fi

