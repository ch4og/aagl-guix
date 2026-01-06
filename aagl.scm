;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((aagl packages anime-game)
        (aagl packages honkers)
        (aagl packages honkers-railway)
        (aagl packages sleepy)
        (aagl services hosts)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))
