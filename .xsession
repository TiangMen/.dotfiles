#!/usr/bin/env bash
# Source .profile
. ~/.profile
GUIX_PROFILE="/home/user/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
# Disable access control for the current user
xhost +SI:localuser:$USER

# Some apps that have to be launched only once.
picom &
dunst &

polybar-msg hook exwm 1 &

#Playing Emacs
ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0 

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/desktop.el
