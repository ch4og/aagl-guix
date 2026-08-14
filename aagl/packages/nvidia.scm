;;; SPDX-FileCopyrightText: 2022-2026 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages nvidia)
  #:use-module (ice-9 string-fun)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl utils name)
  #:use-module (aagl utils nvidia)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (aagl))

(define nvidia-versions
  '(nvda-390
    nvda-470
    nvda-580
    nvda-595
    nvda-new-feature
    nvda-beta))

(define nvidia-launchers
  '(an-anime-game-launcher-for
    honkers-launcher-for
    the-honkers-railway-launcher-for
    sleepy-launcher-for))

(define (define-nvidia-variant! builder-name driver-name)
  (let* ((builder (module-ref (current-module) builder-name))
         (base-name (launcher-base-name builder-name))
         (driver (module-ref (current-module) driver-name))
         (suffix (nvidia-driver-suffix driver-name))
         (variant-name
          (string->symbol (string-append base-name "-nvidia-" suffix)))
         (alias-name
          (string->symbol
           (string-append base-name "-nvidia-user-alias-" suffix)))
         (variant (builder driver)))
    (module-define! (current-module) variant-name (hidden-package variant))
    (module-define!
     (current-module) alias-name
     (make-nvidia-alias
      (nvidia-alias-name base-name suffix)
      variant
      driver))
    (module-export! (current-module) (list variant-name alias-name))))

(define (define-nvidia-default! builder-name)
  (let* ((builder (module-ref (current-module) builder-name))
         (name (string->symbol
                (string-append (launcher-base-name builder-name) "-nvidia"))))
    (module-define! (current-module) name (builder nvda))
    (module-export! (current-module) (list name))))

(map
 (lambda (builder)
   (map
    (lambda (driver)
      (define-nvidia-variant! builder driver))
    nvidia-versions)
   (define-nvidia-default! builder))
 nvidia-launchers)
