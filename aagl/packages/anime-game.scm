;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages anime-game)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define an-anime-game-launcher-real
  (make-aagl #:name "an-anime-game-launcher"
             #:version "3.19.7"
             #:hash "08ymnc1w6r384r0k4s8n1cifyl18jaxs40bylhhmf9rjy55i5lmv"))

(define-public (an-anime-game-launcher-for driver)
  (aagl-fhs-for an-anime-game-launcher-real driver))

(define-public an-anime-game-launcher (an-anime-game-launcher-for mesa))
