;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages sleepy)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define sleepy-launcher-real
  (make-aagl #:name "sleepy-launcher"
             #:version "1.7.1"
             #:hash "1w1w8il6k2hlf0sgp5miac5jchvvyniwjll4lxf1sm3rf81yn833"))

(define-public (sleepy-launcher-for driver)
  (aagl-fhs-for sleepy-launcher-real driver))

(define-public sleepy-launcher
  (sleepy-launcher-for mesa))
