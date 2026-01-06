;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-game)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define anime-game-launcher-real
  (make-aagl
   #:name "anime-game-launcher"
   #:repo "an-anime-game-launcher"
   #:version "3.18.0"
   #:hash "033wj3r7q44xspzp1wgpkg8yr75hwdfd6rfhrijllralz36dpzrf"))

(define-public anime-game-launcher
   (aagl-fhs-for anime-game-launcher-real))

(define-public anime-game-launcher-nvidia
  (package-with-alias
   "anime-game-launcher-nvidia"
   (aagl-fhs-for anime-game-launcher-real #:driver nvda)))
