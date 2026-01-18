;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages container)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sdl)
  #:use-module (nonguix multiarch-container)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl services hosts)
  #:export (aagl-fhs-for))

(define (aagl-container-for pkg name driver)
  (nonguix-container
   (name name)
   (wrap-package pkg)
   (run (string-append "/bin/" name))
   (packages (replace-mesa %aagl-runtime-libs))
   (union32
    (fhs-union '()
               #:name "aagl-fhs-union-32"
               #:system "i686-linux"))
   (preserved-env (cons* "GDK_BACKEND"            ;; Allow overriding
                         "GDK_PIXBUF_MODULE_FILE" ;; Fix loading icons
                         "GST_PLUGIN_SYSTEM_PATH" ;; Fix GStreamer
                         %nvidia-environment-variable-regexps))
   (link-files '("share"))
   (description
    (string-append (package-description pkg)
                   " in a container."))))

(define %aagl-runtime-libs
  (append fhs-min-libs
          `(("bash" ,bash)
            ("coreutils" ,coreutils)
            ("file" ,file)
            ("xz" ,xz)
            ("zenity" ,zenity)

            ("7zip" ,7zip)
            ("cabextract" ,cabextract)
            ("git" ,git)
            ("gnutls" ,gnutls)
            ("gst-libav" ,gst-libav)
            ("gst-plugins-bad" ,gst-plugins-bad)
            ("gst-plugins-good" ,gst-plugins-good)
            ("imagemagick" ,imagemagick)
            ("libunwind" ,libunwind)
            ("libwebp" ,libwebp)
            ("mangohud" ,mangohud)
            ("nss-certs" ,nss-certs)
            ("nss" ,nss)
            ("unzip" ,unzip)
            ("xdelta" ,xdelta)

            ("font-google-noto-emoji" ,font-google-noto-emoji)
            ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
            ("font-google-noto-serif-cjk" ,font-google-noto-serif-cjk)
            ("font-dejavu" ,font-dejavu)

            ("adwaita-icon-theme" ,adwaita-icon-theme)
            ("hicolor-icon-theme" ,hicolor-icon-theme)
            ("font-adwaita" ,font-adwaita)

            ("alsa-lib" ,alsa-lib)
            ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
            ("bzip2" ,bzip2)
            ("gcc:lib" ,gcc "lib")
            ("libadwaita" ,libadwaita)
            ("mesa" ,mesa)
            ("pulseaudio" ,pulseaudio)
            ("wayland" ,wayland)

            ("sdl2" ,sdl2))))

(define show-aagl-warning
  (let ((shown? #f))
    (lambda ()
      (unless (or shown?
                  (aagl-hosts-configured?))
        (warning (G_ "AAGL launchers require blocking hosts!~%"))
        (info (G_ "More details: https://codeberg.org/ch4og/aagl-guix~%"))
        (set! shown? #t)))))

(define* (aagl-fhs-for launcher
                       #:key
                       (driver mesa)
                       (name (package-name launcher)))
  (show-aagl-warning)
  ;; Here we have a small wrapper with environment variables.
  ;; After fixes to nonguix this should just be container-pkg value.
  (let* ((container (aagl-container-for launcher name driver))
         (container-pkg (nonguix-container->package container)))
    (package
      (inherit container-pkg)
      (inputs (append (package-inputs container-pkg)
                      `(("bash-minimal" ,bash-minimal))))
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((out (assoc-ref %outputs "out"))
                   (orig-bin (string-append out "/bin/" #$name))
                   (bash (assoc-ref %build-inputs "bash-minimal"))
                   (bash-bin (string-append bash "/bin/bash"))
                   (pixbuf-cache "/lib64/gdk-pixbuf-2.0/2.10.0/loaders.cache")
                   (gst-paths "/lib64/gstreamer-1.0:/lib/gstreamer-1.0"))
              (copy-recursively #$container-pkg out)
              (wrap-program orig-bin
                #:sh bash-bin
                `("GDK_PIXBUF_MODULE_FILE" = (,pixbuf-cache))
                `("GST_PLUGIN_SYSTEM_PATH" = (,gst-paths))))))))))
