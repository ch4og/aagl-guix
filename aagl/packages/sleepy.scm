;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages sleepy)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl packages container)
  #:use-module (aagl packages base))

(define sleepy-launcher-real
  (make-aagl
   #:name "sleepy-launcher"
   #:version "1.6.2"
   #:hash "092dav32rbsiz9j2an15114p7wj8f0lsy9lpxn1n3n78wfpajrdb"))

(define-public sleepy-launcher
  (aagl-fhs-for sleepy-launcher-real))

(define-public sleepy-launcher-nvidia
  (package-with-alias
   "sleepy-launcher-nvidia"
   (aagl-fhs-for sleepy-launcher-real #:driver nvda)))
