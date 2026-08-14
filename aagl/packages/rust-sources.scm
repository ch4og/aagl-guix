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

(define-public rust-anime-game-core-1.37.4.cf01ae2
  (make-anime-game-core
   #:version "1.37.4"
   #:commit "cf01ae265602b2bfd222d560b3c9f912b82ba5e7"
   #:sha "1mlv3qsv3996zvbrkdi4frz8kqjmfjy79am35mb773jska0zwbq5"))

(define-public rust-anime-game-core-1.38.0.d3fce1c
  (make-anime-game-core
   #:version "1.38.0"
   #:commit "d3fce1cc7a8b95271f9917db73987bee9426a364"
   #:sha "1nbf5fl3i7vdlwc2hkyhc2jsfvim9gpfrmjbqavnqg5ym9pfcabh"))

(define-public rust-anime-game-core-1.38.8.de96f35
  (make-anime-game-core
   #:version "1.38.8"
   #:commit "de96f35b5a7e863f077d27abd50f6ee977cc92de"
   #:sha "0prrlnj95n40zmnd9bzkdrnvans8q73nsd0fcbcm0cwpb61869gk"))

(define-public rust-anime-launcher-sdk-1.32.0.87c4206
  (make-anime-launcher-sdk
   #:version "1.32.0"
   #:commit "87c42064d8422a39b92efbd9035cbd38fffe8f91"
   #:sha "189nsrm41ihhbg85qxvr650nhi7s4c2cj8m5sgmjbw0k7jfrq75j"))

(define-public rust-anime-launcher-sdk-1.33.0.f75593b
  (make-anime-launcher-sdk
   #:version "1.33.0"
   #:commit "f75593be9df416dd76ac6c5ce28140ed0feaef4d"
   #:sha "0jrrgpsc4syp3r7v0afnaigl2g4sj5y06fjy2anykllavsngmj55"))

(define-public rust-anime-launcher-sdk-1.34.7.159b4af
  (make-anime-launcher-sdk
   #:version "1.34.7"
   #:commit "159b4afd283a91b94c1153c89551e302cfcb3bf3"
   #:sha "17lkjwy32b7y8cvd81m0n8l7nlgs3nwgq6kxdm1g6bd7svc3dap6"))

(define-public rust-anime-launcher-sdk-1.35.1.d0ea1ae
  (make-anime-launcher-sdk
   #:version "1.35.1"
   #:commit "d0ea1ae7fa9913c791e026c285c4414103eeae89"
   #:sha "0p6dla9nfmhb932b48ggjx0sjz48agxsdwcxgvvi81gh8jm39gj5"))

(define-public rust-anime-launcher-sdk-1.35.10.c0991af
  (make-anime-launcher-sdk
   #:version "1.35.10"
   #:commit "c0991afb76878f17abf754effa64d86125af8110"
   #:sha "0225wyqzf4a7xxc31ymk1w9km8jkj3bqp2pbn9sqj04fnvjplwm4"))

(define-public rust-sophon-lib-0.1.5.898581c
  (make-sophon-lib
   #:version "0.1.5"
   #:commit "898581c4962682ab911a58ab90226095304db08a"
   #:sha "1i0jdqf0bb47rvjhcgf6128f8rcakq3c6087vh023iik4fvn8rda"))

(define-public rust-sophon-lib-0.1.6.89f4a70
  (make-sophon-lib
   #:version "0.1.6"
   #:commit "89f4a70476f7e5c24f03a6c269a8b291372cfd5e"
   #:sha "0za71m94cai7qfgyp4i29fgdrwbyrzbxxx1b33i7m0syhqdljf6r"))
