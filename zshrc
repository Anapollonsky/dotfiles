# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

source ~/.ss
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

alias ec="emacsclient -nw"
# export PATH=/workspace/env:/workspace/bin:/workspace/tools:~/.cabal/bin:$PATH
export AD_USERNAME=andrew.apollonsky
source $HOME/.yodle-dev-shell-utils/conf/sh/all
alias java6="export JAVA_HOME=/usr/lib/jvm/java-6-oracle && sudo rm /etc/alternatives/java && sudo ln -s /usr/lib/jvm/java-6-oracle/bin/java /etc/alternatives/java"
alias java8="export JAVA_HOME=/usr/lib/jvm/java-8-oracle && sudo rm /etc/alternatives/java && sudo ln -s /usr/lib/jvm/java-8-oracle/bin/java /etc/alternatives/java"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
