;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages sleepy)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define sleepy-launcher-real
  (make-aagl #:name "sleepy-launcher"
             #:version "1.7.0"
             #:hash "0dx61zpsq0y2n2c92rhqypj4ag1f6g82s2z815skzlgrxfj1p6c9"))

(define-public (sleepy-launcher-for driver)
  (aagl-fhs-for sleepy-launcher-real driver))

(define-public sleepy-launcher
  (sleepy-launcher-for mesa))
