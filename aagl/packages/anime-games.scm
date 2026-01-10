;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-games)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define anime-games-launcher-real
  (make-aagl
   #:name "anime-games-launcher"
   #:version "v2.0.0-beta4"
   #:hash "1mkfgvs4659wy0hzhc6mavgx2441dj55k9v9bmj2mwcv3s04d2c1"))

(define-public anime-games-launcher
   (aagl-fhs-for anime-games-launcher-real))

(define-public anime-games-launcher-nvidia
  (package-with-alias
   "anime-games-launcher-nvidia"
   (aagl-fhs-for anime-games-launcher-real #:driver nvda)))
