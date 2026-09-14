;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages rust-sources)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (aagl utils cargo))

(define* (make-anime-game-core #:key version commit sha)
  (let ((name "rust-anime-game-core")
        (crate-symbol (string->symbol (string-append "anime-game-core-" version)))
        (github-url "https://github.com/an-anime-team/anime-game-core"))
    (hidden-package
     (package
       (name name)
       (version (git-version version "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference (url github-url) (commit commit)))
          (file-name (git-file-name name version))
          (sha256 (base32 sha))))
       (build-system cargo-build-system)
       (arguments
        (list #:install-source? #t
              #:skip-build? #t
              #:cargo-package-crates
              ''("anime-game-core")
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'use-guix-vendored-dependencies
                    (lambda _
                      (substitute* "Cargo.toml"
                        (("^git = .*")
                         "")
                        (("^tag = .*")
                         "version=\"*\"\n")))))))
       (inputs (aagl-cargo-inputs crate-symbol))
       (home-page github-url)
       (synopsis "Unified library to control different games installations.")
       (description "Unified library to controll different games installations.
Provides basic instruments for adding support for mechanics like game updating.")
       (license license:gpl3)))))

(define* (make-anime-launcher-sdk #:key version commit sha)
  (let ((name "rust-anime-launcher-sdk")
        (crate-symbol (string->symbol (string-append "anime-launcher-sdk-" version)))
        (github-url "https://github.com/an-anime-team/anime-launcher-sdk"))
    (hidden-package
     (package
       (name name)
       (version (git-version version "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference (url github-url) (commit commit)))
          (file-name (git-file-name name version))
          (sha256 (base32 sha))))
       (build-system cargo-build-system)
       (arguments
        (list #:install-source? #t
              #:skip-build? #t
              #:cargo-package-crates
              ''("anime-launcher-sdk")
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'use-guix-vendored-dependencies
                    (lambda _
                      (substitute* "Cargo.toml"
                        (("^git = .*")
                         "")
                        (("^tag = .*")
                         "version=\"*\"\n")))))))
       (inputs (aagl-cargo-inputs crate-symbol))
       (home-page github-url)
       (synopsis "Anime Game Launcher development SDK")
       (description "SDK based on anime-game-core with basic instruments like launcher
state system and configuration file manager, written in Rust")
       (license license:gpl3)))))

(define* (make-sophon-lib #:key version commit sha)
  (let ((name "rust-sophon-lib")
        (crate-symbol (string->symbol (string-append "sophon-lib-" version)))
        (github-url "https://github.com/dawn-winery/sophon-tools"))
    (hidden-package
     (package
       (name name)
       (version (git-version version "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference (url github-url) (commit commit)))
          (file-name (git-file-name name version))
          (sha256 (base32 sha))))
       (build-system cargo-build-system)
       (arguments
        (list #:install-source? #t
              #:skip-build? #t
              #:cargo-package-crates
              ''("sophon-lib")))
       (inputs (aagl-cargo-inputs crate-symbol))
       (home-page github-url)
       (synopsis "High-performance async sophon downloader implementation written in Rust")
       (description synopsis)
       (license license:gpl3)))))

(define-public rust-anime-game-core-1.39.5.d2cc8c5
  (make-anime-game-core
   #:version "1.39.5"
   #:commit "d2cc8c50784cfb178f28832388dd328f83bab6e0"
   #:sha "1ii5cs2mxvi597m3i4wh84pzzki499r766kac4k8b082phmx9q0a"))

(define-public rust-anime-launcher-sdk-1.36.11.607c78a
  (make-anime-launcher-sdk
   #:version "1.36.11"
   #:commit "607c78ae86196dd2f881706f92d51d389a36687b"
   #:sha "16ljziag4wznbqh7p686n8ga3ajs3f8yizbh584kl1yry32kh43n"))

(define-public rust-sophon-lib-0.1.9.f422286
  (make-sophon-lib
   #:version "0.1.9"
   #:commit "f422286faec0b3e716603bd2c50cb3b38889d0f3"
   #:sha "1ixx9qwr6k2lzdj18akna2p7s7nfda0n50f3amr4fjjgr38wcw8m"))
