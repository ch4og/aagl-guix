;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils nvidia)
  #:use-module (gnu packages gl)
  #:use-module (ice-9 match)
  #:use-module (guix packages))

(define-public (smart-runtime-replace-mesa packages driver)
  (if (eq? driver mesa)
      packages
      (modify-inputs
          ((@ (nongnu packages nvidia) replace-mesa) packages
           #:driver driver)
        (replace "mesa" driver))))

(define-public (driver-symbol->package sym)
  (match sym
    ('nvda (@ (nongnu packages nvidia) nvda))
    ('nvdb (@ (nongnu packages nvidia) nvdb))
    ('mesa mesa)))
