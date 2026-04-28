;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-game)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define an-anime-game-launcher-real
  (make-aagl #:name "an-anime-game-launcher"
             #:version "3.19.1"
             #:hash "0c0w8gmwfpjmzkqi0bbr9pmw8qb6iqdff700py0v3bvl3226ry98"))

(define-public (an-anime-game-launcher-for driver)
  (aagl-fhs-for an-anime-game-launcher-real driver))

(define-public an-anime-game-launcher (an-anime-game-launcher-for mesa))
