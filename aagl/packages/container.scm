;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages container)
  #:use-module (guix build-system trivial)
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
         (wrapped-name (generate-package-name pkg-name driver))
         (container (aagl-container-for launcher name driver))
         (orig-pkg (nonguix-container->package container)))
    (package-with-alias
     wrapped-name
     ;; TODO: After fixes to nonguix this should just be orig-pkg value.
     (package
       (inherit orig-pkg)
       (source #f)
       (native-inputs '())
       (inputs (list bash-minimal orig-pkg))
       (build-system trivial-build-system)
       (arguments
        (list
         #:modules '((guix build utils))
         #:builder
         #~(begin
             (use-modules (guix build utils))
             (let* ((bin-loc (string-append "/bin/" #$name))
                    (pixbuf #$%fhs64-gdk-pixbuf-loaders-cache-file))

               (mkdir-p (string-append #$output "/bin"))

               (symlink (string-append #$orig-pkg bin-loc)
                        (string-append #$output bin-loc))

               (symlink (string-append #$orig-pkg "/share")
                        (string-append #$output "/share"))

               (wrap-program (string-append #$output bin-loc)
                 #:sh (string-append #$bash-minimal "/bin/sh")
                 `("GDK_PIXBUF_MODULE_FILE" = (,pixbuf))
                 ;; https://gitlab.com/nonguix/nonguix/-/merge_requests/827
                 `("XDG_DATA_DIRS" prefix ("/usr/share")))))))))))
