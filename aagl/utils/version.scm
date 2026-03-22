;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils version))

(define-public (package-baseversion s)
  (if (and (> (string-length s) 0)
           (char=? (string-ref s 0) #\v))
      (let ((rest (substring s 1)))
        (if (string-null? rest) s rest))
      s))
