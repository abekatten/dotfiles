export PATH=/Library/Frameworks/Python.framework/Versions/3.5/bin:/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin

export ALTERNATE_EDITOR=""

function ec {
    emacsclient -nc "$@"
    which osascript > /dev/null 2>&1 && osascript -e 'tell application "Emacs" to activate'
    }

#(progn (raise-frame) (x-focus-frame (selected-frame)))
