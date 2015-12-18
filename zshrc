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

PATH="/home/andrewapollonsky/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/home/andrewapollonsky/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/andrewapollonsky/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/andrewapollonsky/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/andrewapollonsky/perl5"; export PERL_MM_OPT;
PATH="/home/andrewapollonsky/.emacs.d/elpa/edbi-20140920.35/":$PATH; export PATH;
