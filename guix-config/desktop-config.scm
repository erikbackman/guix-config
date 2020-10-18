(add-to-load-path
 "/home/eb/.config/guix")
(use-modules (config-utils))
(add-local-channels-to-load-path)

(define-module (desktop-config)
  #:use-module (config-utils)
  #:use-module (base-system-config)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix store))

(use-service-modules
 linux
 desktop
 dns
 mail
 networking
 sddm
 sound
 vpn
 web
 xorg)
(use-package-modules
 linux
 certs)

(use-modules (guix gexp))

(define-public desktop-config
  (operating-system
   (inherit base-system-config)

   (packages
    (append
     (map specification->package
          (list
           "sbcl"
           "stumpwm"
           "sbcl-stumpwm-ttf-fonts"
           "font-dejavu"
           "emacs"
           "nyxt"
           "icecat"
           "ripgrep"
           "st"
           "nss-certs"))
     %base-packages))

   (services %desktop-services)))

desktop-config
