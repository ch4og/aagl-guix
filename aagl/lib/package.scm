;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl lib package)
  #:use-module (guix packages)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (nonguix multiarch-container)
  #:use-module (aagl lib fhsenv)
  #:use-module (aagl services hosts)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages gl)
  #:export (make-aagl))

(define %aagl-warning-shown? #f)

(define (show-aagl-warning)
  (unless (or %aagl-warning-shown? (aagl-hosts-configured?))
    (warning (G_ "AAGL launchers require blocking hosts!~%"))
    (info (G_ "See readme for more details:~%"))
    (info (G_ "https://codeberg.org/ch4og/aagl-guix~%"))
    (set! %aagl-warning-shown? #t)))

(define (strip-suffix suffix str)
  (let* ((len  (string-length str))
         (slen (string-length suffix)))
    (if (and (>= len slen)
             (string=? suffix (substring str (- len slen) len)))
        (substring str 0 (- len slen))
        str)))

(define* (make-aagl base-package #:key (nvidia #f))
  (show-aagl-warning)

  (let* ((base-name     (package-name base-package))
         (launcher-name (strip-suffix "-bin" base-name))
         (pkg-name      (if nvidia
                            (string-append launcher-name "-nvidia")
                            launcher-name))
         (driver        (if nvidia nvda mesa))
         (container     (aagl-container-for base-package driver
                                            #:launcher-name launcher-name))
         (container-pkg (nonguix-container->package container)))
    (package
     (inherit container-pkg)
     (name pkg-name))))
