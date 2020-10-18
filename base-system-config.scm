(add-to-load-path "/home/eb/.config/guix")
(use-modules (config-utils))
(define-module (base-system-config)
  #:use-module (gnu))

(use-modules (gnu) (gnu packages))
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
                (bootloader grub-bootloader)
                (target "/dev/sda")
                (keyboard-layout keyboard-layout)))

  (swap-devices (list "/dev/sda2"))

   (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "f4e1e1df-2239-4b33-b3cb-adf6225d7706"
                     'btrfs))
             (type "btrfs"))
           %base-file-systems))

   (packages
    (append
     (map (compose list specification->package+output)
          '("curl" "wget" "rsync" "tmux" "nss-certs"))
     %base-packages))

   (services 
     (append
       (list (sddm-service-type)
	     (set-xorg-configuration
	       (xorg-configuration
		 (keyboard-layout keyboard-layout))))
     %desktop-services))))

base-system-config
