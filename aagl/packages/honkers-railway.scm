;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers-railway)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define the-honkers-railway-launcher-real
  (make-aagl
   #:name "the-honkers-railway-launcher"
   #:version "1.14.2"
   #:hash "1y5j3ha8l1m2a2dy0g4z4c9s2f2w9lx8l0wy78jiaj9304asahsv"))

(define-public the-honkers-railway-launcher
  (aagl-fhs-for the-honkers-railway-launcher-real))

(define-public the-honkers-railway-launcher-nvidia
  (package-with-alias
   "the-honkers-railway-launcher-nvidia"
   (aagl-fhs-for the-honkers-railway-launcher-real #:driver nvda)))

(define-public honkers-railway-launcher
  (deprecated-package "honkers-railway-launcher" the-honkers-railway-launcher))

(define-public honkers-railway-launcher-nvidia
  (deprecated-package "honkers-railway-launcher-nvidia" the-honkers-railway-launcher-nvidia))
