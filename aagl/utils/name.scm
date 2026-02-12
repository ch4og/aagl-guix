;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils name)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages nvidia))

(define-public (package-basename name)
  (cond
   ((string-prefix? "the-" name)
    (let ((basename (substring name 4)))
      (if (string-null? basename) name basename)))
   ((string-prefix? "an-" name)
    (let ((basename (substring name 3)))
      (if (string-null? basename) name basename)))
   (else name)))

(define-public (generate-package-name name driver)
  (string-append name
                 (cond ((eq? driver mesa) "")
                       ((eq? driver nvda) "-nvidia")
                       ((eq? driver nvdb) "-nvidia-beta")
                       (else driver))))
