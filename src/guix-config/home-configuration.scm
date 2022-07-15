;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services)
  (gnu packages shells)
  (gnu home services shells))

(home-environment
  (packages
    (map (compose list specification->package+output)
         (list "libtool"
               "ripgrep"
               "unzip"
               "zip"
               "emacs-nov-el"
               "emacs-pdf-tools"
               "imagemagick"
               "emacs-info-plus"
               "ledger"
               "ncurses"
               "emacs-log4e"
               "emacs-pyvenv"
               "emacs-python-environment"
               "python"
               "node"
               "musl"
               "emacs-ctable"
               "emacs-ledger-mode"
               "sicp"
               "network-manager"
               "openssh"
               "man-pages"
               "go"
               "font-iosevka-aile"
               "emacs-org-superstar"
               "hunspell-dict-en"
               "hunspell-dict-en-us"
               "hunspell"
               "aspell"
               "aspell-dict-en"
               "emacs-flymake-shellcheck"
               "clang"
               "emacs-ccls"
               "ccls"
               "nss-certs"
               "emacs-org-roam"
               "emacs-cdlatex"
               "emacs-company-auctex"
               "emacs-company-math"
               "emacs-flycheck"
               "emacs-outshine"
               "emacs-yasnippet-snippets"
               "emacs-blacken"
               "emacs-which-key"
               "emacs-no-littering"
               "emacs-key-chord"
               "emacs-undo-tree"
               "emacs-doom-snippets"
               "emacs-evil-collection"
               "emacs-evil"
               "fzf"
               "emacs-dashboard"
               "emacs-openwith"
               "emacs-general"
               "emacs-dired-hacks"
               "emacs-all-the-icons-dired"
               "emacs-magit"
               "emacs-elfeed"
               "emacs-flyspell-correct"
               "emacs-vertico"
               "emacs-auctex"
               "emacs-doom-modeline"
               "emacs-doom-themes"
               "emacs-lispyville"
               "emacs-lispy"
               "plocate"
               "emacs-evil-multiedit"
               "emacs-guix"
               "emacs-writegood-mode"
               "ispell"
               "shellcheck"
               "aria2"
               "ffmpeg"
               "mpv"
               "openssl"
               "zathura-pdf-mupdf"
               "zathura"
               "img2pdf"
               "curl"
               "agg"
               "python-qtpy"
               "python-matplotlib"
               "git"
               "youtube-dl"
               "gst-libav"
               "gst-plugins-ugly"
               "gst-plugins-bad"
               "gst-plugins-good"
               "pulseaudio"
               "emacs-pulseaudio-control"
               "pavucontrol"
               "flatpak"
               "xdg-desktop-portal"
               "pamixer"
               "python-lsp-server"
               "dunst"
               "ungoogled-chromium"
               "syncthing-gtk"
               "polybar"
               "libptytty"
               "bison"
               "linux-pam"
               "xsecurelock"
               "xclip"
               "mu"
               "emacs-mu4e-alert"
               "pinentry"
               "password-store"
               "isync"
               "zlib"
               "cairo"
               "pkg-config"
               "libpng"
               "poppler"
               "libpng-apng"
               "neofetch"
               "emacs-rainbow-mode"
               "pinentry-emacs"
               "emacs-pinentry"
               "libva-utils"
               "intel-vaapi-driver"
               "gst-plugins-base"
               "gstreamer"
               "shared-mime-info"
               "glib:bin"
               "gtk+:bin"
               "xdg-dbus-proxy"
               "xdg-utils"
               "emacs-geiser"
               "guile"
               "emacs"
               "alsa-utils"
               "emacs-desktop-environment"
               "emacs-python-black"
               "feh"
               "picom"
               "gnome-backgrounds"
               "python-pip"
               "libvterm"
               "xhost"
               "gcc-toolchain"
               "font-fira-code"
               "cmake"
               "emacs-vterm"
               "syncthing"
               "emacs-all-the-icons"
               "font-abattis-cantarell"
               "font-jetbrains-mono"
               "zsh-syntax-highlighting"
               "zsh-autosuggestions"
               "zsh"
               "stow")))
  (services
    (list (service
            home-bash-service-type
            (home-bash-configuration
              (aliases
                '(("grep" . "grep --color=auto")
                  ("ll" . "ls -l")
                  ("ls" . "ls -p --color=auto")))
              (bashrc
                (list (local-file
                        "/home/user/src/guix-config/.bashrc"
                        "bashrc")))
              (bash-profile
                (list (local-file
                        "/home/user/src/guix-config/.bash_profile"
                        "bash_profile")))))

          (simple-service 'some-useful-env-vars-service
          		  home-environment-variables-service-type
          		  `(("GUIX_PROFILE" . "$HOME/.guix-profile")
			    ("PATH" . "$PATH:$HOME/go/bin:$HOME/.local/bin")
                            ("CC" . "gcc")
                            ("XDG_DATA_DIRS" . "/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share:XDG_DATA_DIRS")))
	  )))

