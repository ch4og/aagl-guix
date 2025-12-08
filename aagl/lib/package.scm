;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl lib package)
  #:use-module (guix packages)
  #:use-module (nonguix multiarch-container)
  #:use-module (aagl lib fhsenv)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages gl)
  #:export (make-aagl))

(define (strip-suffix suffix str)
  (let* ((len  (string-length str))
         (slen (string-length suffix)))
    (if (and (>= len slen)
             (string=? suffix (substring str (- len slen) len)))
        (substring str 0 (- len slen))
        str)))

(define* (make-aagl base-package #:key (nvidia #f))
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
