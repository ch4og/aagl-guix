;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages base)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl utils cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (make-aagl))

(define* (make-aagl #:key name version hash (repo name))
  (let* ((cargo-deps (aagl-cargo-inputs (string->symbol name)))
         (github-url (string-append "https://github.com/an-anime-team/" repo)))
    (package
      (name name)
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url github-url)
                (commit version)))
         (file-name (git-file-name repo version))
         (sha256 (base32 hash))))
      (build-system cargo-build-system)
      (arguments
       (list
        #:install-source? #f
        #:rust rust-1.88
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'use-guix-vendored-dependencies
              (lambda _
                (substitute* "Cargo.toml"
                  (("tag =.*") "version = \"*\"\n")
                  (("^git = .*") ""))))
            (add-after 'install 'install-desktop-files-and-icons
              (lambda _
                (let ((desktop-file (string-append "assets/" #$name ".desktop"))
                      (desktop-dest (string-append #$output "/share/applications/" #$name ".desktop"))
                      (icon-dest (string-append #$output "/share/icons/hicolor/512x512/apps/moe.launcher." #$name ".png"))
                      (pixmap-dest (string-append #$output "/share/pixmaps/" #$name ".png")))
                  (mkdir-p (dirname desktop-dest))
                  (copy-file desktop-file desktop-dest)
                  (substitute* desktop-dest
                    (("Exec=AppRun") (string-append "Exec=" #$name))
                    (("Icon=icon") (string-append "Icon=" #$name)))
                  (mkdir-p (dirname icon-dest))
                  (copy-file "assets/images/icon.png" icon-dest)
                  (mkdir-p (dirname pixmap-dest))
                  (copy-file "assets/images/icon.png" pixmap-dest))))
            (delete 'patch-dot-desktop-files))))
      (native-inputs (list pkg-config protobuf coreutils))
      (inputs (cons*
               gdk-pixbuf
               git-minimal
               glib
               graphene
               gtk
               libadwaita
               pango
               wayland
               (list glib "bin")
               (list zstd "lib")
               cargo-deps))
      (home-page github-url)
      (synopsis "One of Anime Team launchers.")
      (description "Prebuilt launcher with auto-patching and telemetry disabling")
      (license license:gpl3))))
