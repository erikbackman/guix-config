(define-module (config-utils)
	         #:use-module (srfi srfi-1)
		 #:use-module (guix profiles)
		 #:use-module (guix packages)
		 #:use-module (guix gexp)) ;; for local-file*
(define this-file
  (local-file-absolute-file-name
    (local-file
      (assoc-ref (current-source-location) 'filename))))
(define this-directory
  (dirname this-file))
(define (warncheck-path path)
  (if (file-exists? path)
      #t
      (format (current-error-port) (string-append "WARNING: couldn't find added load-path " path "\n"))))
(define (relative-add-to-load-path path-relative-to-this-file)
  (let* ((absolute-path (string-append
			  this-directory
			  path-relative-to-this-file)))
    (add-to-load-path absolute-path)
    (warncheck-path absolute-path)))
(define-public (add-local-channels-to-load-path)
	       ;; Adding these local paths to the load paths overrides the guix pull'd modules,
	       ;;   and allows me to modify the channels and instantly test building a system
	       ;;   with the changes. Since they are added using the file, root also gets
	       ;;   these load paths.
	       (add-to-load-path
		 "/home/eb/.config/guix/local-channels/eb-libre")
	       (add-to-load-path
		 "/home/eb/.config/guix/local-channels/eb-nonfree"))
(define-public (add-config-root-to-load-path)
	       (relative-add-to-load-path "")) ;; To get base-system.scm
(define-public (manifest->packages manifest) ;; copied from guix/scripts/refresh.scm, referring to it with (@@ ...) stopped working for some reason, kept saying it's an unbound variable
	       "Return the list of packages in MANIFEST."
	       (filter-map (lambda (entry)
			     (let ((item (manifest-entry-item entry)))
			       (if (package? item) item #f)))
			   (manifest-entries manifest)))
