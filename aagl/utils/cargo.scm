;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils cargo)
  #:use-module (srfi srfi-26)
  #:use-module (guix build-system cargo)
  #:export (aagl-cargo-inputs))

(define aagl-cargo-inputs
  (cut cargo-inputs <> #:module '(aagl packages rust-crates)))
