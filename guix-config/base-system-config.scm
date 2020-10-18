(define-module (base-system-config)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu system)
  #:use-module (guix store))

(use-package-modules bash shells)
(use-service-modules desktop networking ssh) 

(define-public base-system-config
  (operating-system
   (keyboard-layout
    (keyboard-layout "se" "nodeadkeys"))
   (host-name "guix")
   (locale "en_US.utf8")
   (timezone "Europe/Stockholm")

   (users (cons* (user-account
                  (name "eb")
                  (group "users")
                  (home-directory "/home/eb")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video" "tty")))
                 %base-user-accounts))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (keyboard-layout keyboard-layout)))

;;  (swap-devices (list "/dev/sda2"))

   (file-systems
    (cons* (file-system
             (mount-point "/")
	     (device "/dev/sda2")
             (type "btrfs"))
	   (file-system
             (mount-point "/boot/efi")
	     (device "/dev/sda1")
             (type "vfat"))
           %base-file-systems))

   (packages
    (append
     (map (compose list specification->package+output)
          '("curl" "wget" "rsync" "tmux" "nss-certs"))
     %base-packages))

   (services %base-services)))

base-system-config
