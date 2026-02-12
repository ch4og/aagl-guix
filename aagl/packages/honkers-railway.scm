;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers-railway)
  #:use-module (guix packages)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define the-honkers-railway-launcher-real
  (make-aagl #:name "the-honkers-railway-launcher"
             #:version "1.14.3"
             #:hash "0qf2yvrspvkrs041in77x4zxdh8mid3zcd5fjr2r5m20n5v5b3cb"))

(define-public (the-honkers-railway-launcher-for driver)
  (aagl-fhs-for the-honkers-railway-launcher-real driver))

(define-public the-honkers-railway-launcher
  (the-honkers-railway-launcher-for mesa))

(define-public the-honkers-railway-launcher-nvidia
  (the-honkers-railway-launcher-for nvda))

(define-deprecated-package honkers-railway-launcher
                           the-honkers-railway-launcher)

(define-deprecated-package honkers-railway-launcher-nvidia
                           the-honkers-railway-launcher-nvidia)
