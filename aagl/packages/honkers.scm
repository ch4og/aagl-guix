;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define honkers-launcher-real
  (make-aagl #:name "honkers-launcher"
             #:version "1.14.0"
             #:hash "0ivmvz4wihxd84672133lky98h0g8374sf3yiy29cxlw45pbsvx3"))

(define-public (honkers-launcher-for driver) (aagl-fhs-for honkers-launcher-real driver))

(define-public honkers-launcher (honkers-launcher-for mesa))
