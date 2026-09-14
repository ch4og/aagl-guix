;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers-railway)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define the-honkers-railway-launcher-real
  (make-aagl #:name "the-honkers-railway-launcher"
             #:version "1.15.3"
             #:hash "0cvr5pbvl2fawsvv9gfj3madjxdwbbzgwxb2f1s7qa0x9qmw9v8c"))

(define-public (the-honkers-railway-launcher-for driver) (aagl-fhs-for the-honkers-railway-launcher-real driver))

(define-public the-honkers-railway-launcher (the-honkers-railway-launcher-for mesa))
