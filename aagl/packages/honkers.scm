;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers)
  #:use-module (guix packages)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define honkers-launcher-real
  (make-aagl
   #:name "honkers-launcher"
   #:version "1.13.0"
   #:hash "1dmyh9nnngd6z2djnzkq83axrzhdn7xmbanh8ddp73fhadpgdzbm"))

(define-public honkers-launcher
  (aagl-fhs-for honkers-launcher-real))

(define-public honkers-launcher-nvidia
  (aagl-fhs-for honkers-launcher-real #:driver nvda))
