echo "***** Running dotfiles/.bash_aliases"

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -hF --color=auto'
    alias grep='grep --color'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
fi

alias ll='ls -lA'
alias la='ls -A'
alias l='ls -CF'

alias more='less'
alias cls='clear'

