;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-game)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define an-anime-game-launcher-real
  (make-aagl #:name "an-anime-game-launcher"
             #:version "3.19.8"
             #:hash "1hkm39laggn2cnb8mq4wa9862pj1amm9rhb8ns5i4l3q37kx0321"))

(define-public (an-anime-game-launcher-for driver)
  (aagl-fhs-for an-anime-game-launcher-real driver))

(define-public an-anime-game-launcher (an-anime-game-launcher-for mesa))
