(add-to-load-path "/home/me/.config/guix/systems")

(define-module (my-desktop-system)
  #:use-module (my-minimal-system)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix store)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules
 desktop
 dns
 networking
 sddm
 sound
 web
 xorg)
(use-package-modules
 linux
 certs)

(use-modules (guix gexp))

(define-public my-desktop-system
  (operating-system
   (inherit my-minimal-system)
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))
   (keyboard-layout (keyboard-layout "se"))

   (packages
    (append
     (map specification->package
          (list
           "sbcl"
           "stumpwm"
           "font-dejavu"
           "next"
           "icecat"
           "ripgrep"
           "kitty"
           "streamlink"
           "mpv"
           "nss-certs"))
     %my-base-packages))

  (services (cons (set-xorg-configuration
                    (xorg-configuration
                      (keyboard-layout keyboard-layout)))
                  %desktop-services))))

my-desktop-system
