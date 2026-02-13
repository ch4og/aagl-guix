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
             #:version "1.14.4"
             #:hash "04kpkb1qx72bb2sj46r4m2110ly27cib83qy6in9v9gi8vgl64sa"))

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
