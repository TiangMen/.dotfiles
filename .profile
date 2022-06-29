export GUIX_PROFILE="/home/user/.guix-profile"
export PATH=$PATH:$HOME/go/bin # go linting, autoformatting, and lsp
export PATH=$PATH:$HOME/.local/bin 
export PATH=$PATH:$HOME/.cargo/bin
export CC=gcc # guix doesn't have a cc
export XDG_DATA_DIRS=$XDG_DATA_DIRS:'/var/lib/flatpak/exports/share'
export XDG_DATA_DIRS=$XDG_DATA_DIRS:'/home/user/.local/share/flatpak/exports/share'
export PATH=$PATH:$HOME/.local/share/flatpak/exports/bin


