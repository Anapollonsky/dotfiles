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
source "/opt/google-cloud-sdk/path.zsh.inc"
source "/opt/google-cloud-sdk/completion.zsh.inc"
export ANDROID_HOME=/opt/android-sdk
export ANDROID_NDK=/opt/android-sdk/ndk-bundle
export PATH=$PATH:/home/andrewa/.cabal/bin
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
autoload -Uz compinit && compinit -i
alias ec="TERM=xterm;emacsclient -nw"
export PATH=$PATH:/home/andrewa/personal/scr/python
source ~/.profile

setopt +o nomatch
