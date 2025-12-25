#!/bin/bash
###############################################################################
# DESCRIPTION
# ===========
# Bash login script.
#
# Per-machine customization should reside in ~/.bashlocalrc. If that file is
# present on the machine, it will get sourced at the very end of this script.
###############################################################################

### Get the git branch name
git_branch()
{
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/[\1]/'
}

## Set UTF-8 locale if available
init_locale()
{
    typeset locs_r=('en_US.UTF-?8' 'C.UTF-?8')
    for loc_r in ${locs_r[@]}; do
        typeset loc=$(locale -a | grep -E -i "${loc_r}" | head -1)
        if [[ "${loc,,}" =~ utf-?8 ]]; then
            export LANG="${loc}"
            return
        fi
    done
}

### Check if the current terminal supports color
is_color_supported()
{
    typeset has_color=0

    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	has_color=1
    else
        case "$TERM" in
            xterm-color|*-256color)
                has_color=1
                ;;
        esac
    fi
    echo ${has_color}
}

### Set common shell options
init_opts()
{
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # enable programmable completion features (you don't need to enable
    # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
    # sources).
    if ! shopt -oq posix; then
        if [ -f /usr/share/bash-completion/bash_completion ]; then
            . /usr/share/bash-completion/bash_completion
        elif [ -f /etc/bash_completion ]; then
            . /etc/bash_completion
        fi
    fi
}

### Initialize prompt
init_prompt()
{
    if [ "$(is_color_supported)" -eq 1 ]
    then
        # https://github.com/r3tex/one-dark
        # https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        F_BLUE='38;2;97;175;239'
        F_CYAN='38;2;86;182;194'
        F_GREEN='38;2;152;195;121'
        F_PURPLE='38;2;198;120;221'
        F_RED='38;2;224;108;117'
        F_DARK_RED='38;2;190;80;70'
        F_WHITE='38;2;171;178;191'
        F_YELLOW='38;2;229;192;123'
        #PS1='\[\e[${F_BLUE}m[\D{%m-%d %H:%M.%S}] \e[${F_PURPLE}m\u@\h\e[${F_WHITE}m:\e[${F_GREEN}m\w\e[${F_WHITE}m $(git_branch)\]\n$ \[\e[0m\]'
        PS1='\[\e[${F_BLUE}m[\D{%m-%d %H:%M.%S}]'
        PS1+=' \e[${F_PURPLE}m\u@\h\e[${F_WHITE}m:'
        PS1+='\e[${F_GREEN}m\w'
        PS1+='\e[${F_WHITE}m $(git_branch)\]'
        PS1+='\n$ \[\e[0m\]'
    else
        PS1='[\D{%m-%d %H:%M.%S}] \u@\h:\w $(git_branch)\n$ '
    fi
}

### Setup colors
init_colors()
{
    # enable color support of ls and also add handy aliases
    if [ -x /usr/bin/dircolors ]; then
        if [ -r ~/.dircolors ]
        then
            eval "$(dircolors -b ~/.dircolors)"
        else
            eval "$(dircolors -b)"
        fi
        #test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls='ls --color=auto'
        alias dir='dir --color=auto'
        alias vdir='vdir --color=auto'
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi

    # colored GCC warnings and errors
    #export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
}

init_aliases()
{
    alias ne='emacs -nw'
    alias nec='emacsclient -nw -a "" -c'
    alias ll='ls -ltr'
    alias la='ls -ltra'
}

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

init_locale
init_opts
init_prompt
init_colors
init_aliases

if [ -d ~/.local/bin ]
then
    LCL_BIN="$(cd ~/.local/bin; pwd)"
    PATH="${PATH}:${LCL_BIN}"
fi

# Any other customizations
if [ -f ~/.bashlocalrc ]; then
    . ~/.bashlocalrc
fi
