#!/bin/bash
###############################################################################
# DESCRIPTION
# ===========
# This script is used to install the files available within this repository
# (i.e., the dotfiles) into the current user's home directory. The end state of
# the home directory will be a union of files between this repo and the
# existing contents of the home dir. Files from repo will overwrite files in
# user's home when they both have the same relative path and name.
#
# For example
#   user home (before):
#      - ~/.local/share/fonts/my-custom-font
#      - .bashrc
#   repo:
#      - ./.local/share/fonts/dejavu
#      - .bashrc
#
# After running the script,
#   user home (after):
#      - ~/.local/share/fonts/my-custom-font
#      - ~/.local/share/fonts/dejavu
#      - .bashrc (from repo)
###############################################################################

## List all the files from this repository
find_dotfiles()
{
    typeset dir="${1}"
    find "${dir}"                                                  \
        | grep -v -E "${dir}/.git|${dir}/README.md|${dir}/install" \
        | grep -v -E "^${dir}\$"
}

## Copies file/dir to target directory recursively
make_copy()
{
    typeset INDIR="$(cd "$1"; pwd)"
    typeset OUTDIR="$2"

    if [ ! -d "$OUTDIR" ]
    then
        mkdir -p "$OUTDIR"
    fi

    while read -r inp; do
        relative_path=${inp#"${INDIR}/"}
        target_path="${OUTDIR}/${relative_path}"

        if [ -d "$inp" ]
        then
            if [ ! -d "${target_path}" ]
            then
                mkdir -p "${target_path}"
            fi
        else
            target_dir="$(dirname "${target_path}")"
            if [ ! -d "${target_dir}" ]
            then
                mkdir -p "${target_dir}"
            fi
            cp "${inp}" "${target_path}"
        fi
        echo "$inp"
    done
}

## setup git config
setup_gitconfig()
{
    if [ -x "$(command -v git)" ]; then
        if [ $(git config --global --includes --list | grep -c '~/.local/gitconfig') -eq 0 ]; then
            git config --global --add include.path '~/.local/gitconfig'
        fi
    else
        echo "unable to configure .gitconfig : git not in path"
    fi
}

SCRIPT_DIR="$(cd $(dirname "$0"); pwd)"
OUTPUT_DIR="$(cd ~ >/dev/null 2>&1; pwd)"

# Create dotfiles
find_dotfiles "${SCRIPT_DIR}" | make_copy "${SCRIPT_DIR}" "${OUTPUT_DIR}"
ln -fs "${OUTPUT_DIR}/.Xresources" "${OUTPUT_DIR}/.Xdefaults"


## Add gitconfig customizations
setup_gitconfig
