;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu) (gnu packages))
(use-service-modules networking ssh)

(operating-system
  (host-name "guix")
  (timezone "Europe/Stockholm")
  (locale "en_US.utf8")
  (keyboard-layout (keyboard-layout "se"))

  ;; Boot-loader
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")))
  (file-systems (cons*
		 (file-system
		  (device (file-system-label "root"))
		  (mount-point "/")
		  (type "ext4"))
		 (file-system
		  (device (file-system-label "BOOT"))
		  (mount-point "/boot/efi")
		  (type "vfat")
		  (create-mount-point? #t)
		  (check? #f))
		 %base-file-systems))

  ;; Users
  (users (cons (user-account
                (name "me")
                (group "users")
		(supplementary-groups '("wheel" "audio" "video")))
	       %base-user-accounts))

  ;; Globally-installed packages.
  (packages (append (list 
		     (specification->package "git")
		     (specification->package "emacs")
		     (specification->package "emacs-evil")
		     (specification->package "emacs-evil-collection")
		     (specification->package "nss-certs"))
		    %base-packages))

  ;; Services
  (services (append (list
		     (service dhcp-client-service-type))
		    %base-services)))
