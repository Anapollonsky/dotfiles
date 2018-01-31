# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

eval "$(fasd  --init auto)"
eval $(dircolors ~/dotfiles/dircolors)

# Help track directories in emacs
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() {
        print -P "\033AnSiTc %d"
    }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi

export FZF_DEFAULT_OPTS='--height 40% --reverse --margin 0%,2%'
alias zhe='fc -ln -99999 | fzf | read command; eval $command'
alias zh='fc -ln -99999 | fzf'
fpath=(~/dotfiles/zsh/completion $fpath)
autoload -Uz compinit && compinit -i
alias ec="TERM=xterm;emacsclient -nw"
source ~/.profile

setopt +o nomatch