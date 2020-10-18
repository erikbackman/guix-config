(define-module (ebn packages collections)
  :use-module (gnu)
  :use-module (gnu packages))

(define-public desktop-packages
  (map (compose list specification->package+output)
       '("stumpwm"
         "stumpwm"
         "sbcl-stumpwm-ttf-fonts"
         "font-dejavu"
         "icecat"
         "nyxt")))

(define-public development-packages
  (map (compose list specification->package+output)
       '("git"
         "make")))

(define-public command-line-packages
  (map (compose list specification->package+output)
       '("fd"
         "ripgrep"
         "stow"
         "pass")))

(define-public emacs-packages
  (map (compose list specification->package+output)
       '("emacs")))

(define-public multimedia-packages
  (map (compose list specification->package+output)
       '("mpd"
         "mpv"
         "ffmpeg"
         "imagemagick")))
