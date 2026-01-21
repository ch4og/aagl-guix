;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils nvidia)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix packages))

(define-public (smart-runtime-replace-mesa packages driver)
  (if (eq? driver mesa)
      packages
      (modify-inputs
          (replace-mesa packages
                        #:driver driver)
        (replace "mesa" driver))))
