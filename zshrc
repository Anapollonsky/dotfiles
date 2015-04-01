# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

source ~/.ss
eval "$(fasd  --init auto)"
eval $(dircolors ~/dotfiles/dircolors)

