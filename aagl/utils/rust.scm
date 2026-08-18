;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils rust)
  #:use-module (gnu packages rust)
  #:use-module (aagl utils warning))

(define-public (safe-rust symbol)
  "Return the Rust package named by SYMBOL if it exists.
If SYMBOL is not defined in this Guix revision, fall back to `rust`."
  (let* ((rust-module (resolve-interface '(gnu packages rust)))
         (rust-variable (module-variable rust-module symbol)))
    (if rust-variable
        (variable-ref rust-variable)
        (begin
          (show-aagl-fallback-warning symbol 'rust)
          rust))))
