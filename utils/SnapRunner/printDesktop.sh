#! /bin/bash
test -f ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs && source ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs
echo -n ${XDG_DESKTOP_DIR:-$HOME/Desktop}
