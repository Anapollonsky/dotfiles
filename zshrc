#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# z-jump - nifty for navigating with less typing! https://github.com/rupa/z
source /usr/lib/z.sh
source ~/.ss
eval "$(fasd  --init auto)"
#################### Obsolete

### Found out you can replace colors directly in the urxvt config
# colors for ls, modify directory color. Colors seen here:
# http://misc.flogisoft.com/bash/tip_colors_and_formatting
# Assumes 256-color capable terminal 
# eval "$(dircolors -b ~/.dotfiles/LS_COLORS)"
