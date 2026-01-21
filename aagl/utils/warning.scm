;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils warning)
  #:use-module (guix ui)
  #:use-module (aagl services hosts))

(define-public show-aagl-warning
  (let ((shown? #f))
    (lambda ()
      (unless (or shown?
                  (aagl-hosts-configured?))
        (warning (G_ "AAGL launchers require blocking hosts!\n"))
        (info (G_ "More details: https://codeberg.org/ch4og/aagl-guix\n"))
        (set! shown? #t)))))
