;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-game)
  #:use-module (guix packages)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define an-anime-game-launcher-real
  (make-aagl
   #:name "an-anime-game-launcher"
   #:version "3.18.0"
   #:hash "033wj3r7q44xspzp1wgpkg8yr75hwdfd6rfhrijllralz36dpzrf"))

(define-public (an-anime-game-launcher-for driver)
  (aagl-fhs-for an-anime-game-launcher-real driver))

(define-public an-anime-game-launcher
  (an-anime-game-launcher-for mesa))

(define-public an-anime-game-launcher-nvidia
  (an-anime-game-launcher-for nvda))

(define-public anime-game-launcher
  (deprecated-package "anime-game-launcher" an-anime-game-launcher))

(define-public anime-game-launcher-nvidia
  (deprecated-package "anime-game-launcher-nvidia" an-anime-game-launcher-nvidia))
