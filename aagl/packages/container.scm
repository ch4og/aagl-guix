;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages container)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl services hosts)
  #:use-module (aagl utils name)
  #:use-module (aagl utils nvidia)
  #:use-module (aagl utils vars)
  #:use-module (aagl utils warning)
  #:export (aagl-fhs-for))

(define (aagl-container-for pkg name driver)
  (nonguix-container
   (name name)
   (wrap-package pkg)
   (run (string-append "/bin/" name))

   (packages (smart-runtime-replace-mesa %aagl-runtime-libs driver))
   (union32
    (fhs-union '()
               #:name "aagl-fhs-union-32"
               #:system "i686-linux"))
   (preserved-env `("GDK_PIXBUF_MODULE_FILE" ;; Fix loading icons
                    "XDG_DATA_DIRS"          ;; Fix GTK wrapping
                    ,@%nvidia-environment-variable-regexps))
   (link-files '("share"))
   (description
    (string-append (package-description pkg)
                   " in a container."))))

(define* (aagl-fhs-for launcher driver)
  (show-aagl-warning)

  (let* ((pkg-name (package-name launcher))
         (name (package-basename pkg-name))
         (container (aagl-container-for launcher name driver))
         (container-pkg (nonguix-container->package container)))
    (package-with-alias
     (generate-package-name pkg-name driver)
     ;; TODO: After fixes to nonguix this should just be container-pkg value.
     (package
       (inherit container-pkg)
       (inputs `(,@(package-inputs container-pkg)
                 ("bash-minimal" ,bash-minimal)))
       (arguments
        (list
         #:modules '((guix build utils))
         #:builder
         #~(begin
             (use-modules (guix build utils))
             (let* ((out          (assoc-ref %outputs "out"))
                    (orig-bin     (string-append out "/bin/" #$name))
                    (bash         (assoc-ref %build-inputs "bash-minimal"))
                    (bash-bin     (string-append bash "/bin/bash"))
                    (pixbuf-cache (string-append "/"
                                                 #$%gdk-pixbuf-loaders-cache-file-64)))
               (copy-recursively #$container-pkg out)
               (wrap-program orig-bin
                 #:sh bash-bin
                 `("GDK_PIXBUF_MODULE_FILE" = (,pixbuf-cache))
                 `("XDG_DATA_DIRS" = ("/usr/share")))))))))))
