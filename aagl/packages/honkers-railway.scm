;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers-railway)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define the-honkers-railway-launcher-real
  (make-aagl #:name "the-honkers-railway-launcher"
             #:version "1.15.2"
             #:hash "0r7qiix4ay6g1m48y94iyvcd73malkmma7179f969fvzhy8h1p61"))

(define-public (the-honkers-railway-launcher-for driver) (aagl-fhs-for the-honkers-railway-launcher-real driver))

(define-public the-honkers-railway-launcher (the-honkers-railway-launcher-for mesa))
