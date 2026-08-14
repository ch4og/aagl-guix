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

(define-public rust-anime-game-core-1.36.3.044a1e8
  (make-anime-game-core
   #:version "1.36.3"
   #:commit "044a1e83782fb853a9cfa8bcb807689a5c9d73ae"
   #:sha "156s4daz89048r7bhk8ibr5rb07maya0fypvigkyn2dxldjjj46j"))

(define-public rust-anime-game-core-1.38.8.de96f35
  (make-anime-game-core
   #:version "1.38.8"
   #:commit "de96f35b5a7e863f077d27abd50f6ee977cc92de"
   #:sha "0prrlnj95n40zmnd9bzkdrnvans8q73nsd0fcbcm0cwpb61869gk"))

(define-public rust-anime-game-core-1.38.10.ba60faf
  (make-anime-game-core
   #:version "1.38.10"
   #:commit "ba60faf15940e21fd20134c8f533fe981d313fee"
   #:sha "06qzd3m2hl0v4ygg7xsl8b5bs837kca08wxz0qk014drwvwj7cj9"))

(define-public rust-anime-game-core-1.39.1.aa8c5ce
  (make-anime-game-core
   #:version "1.39.1"
   #:commit "aa8c5ce41dbbc0ab57b49214e02d54001b83edac"
   #:sha "1b809a7qz4bg1xkp03grdmgrscrxl6ch9dnjzsk8n9zsibg51dyd"))

(define-public rust-anime-launcher-sdk-1.32.0.87c4206
  (make-anime-launcher-sdk
   #:version "1.32.0"
   #:commit "87c42064d8422a39b92efbd9035cbd38fffe8f91"
   #:sha "189nsrm41ihhbg85qxvr650nhi7s4c2cj8m5sgmjbw0k7jfrq75j"))

(define-public rust-anime-launcher-sdk-1.35.10.c0991af
  (make-anime-launcher-sdk
   #:version "1.35.10"
   #:commit "c0991afb76878f17abf754effa64d86125af8110"
   #:sha "0225wyqzf4a7xxc31ymk1w9km8jkj3bqp2pbn9sqj04fnvjplwm4"))

(define-public rust-anime-launcher-sdk-1.35.12.4b9cb6e
  (make-anime-launcher-sdk
   #:version "1.35.12"
   #:commit "4b9cb6efb04bacec57747e19e7277086d201562e"
   #:sha "11lr87xhljyk4wz1xzaqnyzz9bzi70hgmp0xfn7k84zhlwpf4ywa"))

(define-public rust-anime-launcher-sdk-1.36.4.bf66ccb
  (make-anime-launcher-sdk
   #:version "1.36.4"
   #:commit "bf66ccba2eb50f87911efdc93e8e9976eaf28102"
   #:sha "0366df6ny2aqm2dxcdmdk735pprjx791jdgl2g0djhlvlh6ls46h"))

(define-public rust-sophon-lib-0.1.6.89f4a70
  (make-sophon-lib
   #:version "0.1.6"
   #:commit "89f4a70476f7e5c24f03a6c269a8b291372cfd5e"
   #:sha "0za71m94cai7qfgyp4i29fgdrwbyrzbxxx1b33i7m0syhqdljf6r"))

(define-public rust-sophon-lib-0.1.8.58d223a
  (make-sophon-lib
   #:version "0.1.8"
   #:commit "58d223a5e1268bb7327bb31fe5c664aff56f6b6f"
   #:sha "0f7vy7ms7vmd75az7d72sg65yzyawkclgcz7g8axq5xszfc7y5dk"))
