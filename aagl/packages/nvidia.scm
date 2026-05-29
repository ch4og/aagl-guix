;;; SPDX-FileCopyrightText: 2022-2026 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:use-module (aagl))

(define-syntax define-nvidia-aagl
  (syntax-rules ()
    ((_ name alias-name builder driver)
     (begin
       (define-public name
         (hidden-package (builder driver)))
       (define-public alias-name
         (package-with-alias
          (string-append
           (package-name (builder driver))
           (cond ((eq? driver nvda-beta) "-beta")
                 ((eq? driver nvda-new-feature) "-new-feature")
                 (else "")))
          (package
            (inherit (builder driver))
            (version (package-version driver)))))))))

;;; an-anime-game
(define-nvidia-aagl an-anime-game-launcher-nvidia-390 an-anime-game-launcher-nvidia-user-alias-390
  an-anime-game-launcher-for nvda-390)

(define-nvidia-aagl an-anime-game-launcher-nvidia-470 an-anime-game-launcher-nvidia-user-alias-470
  an-anime-game-launcher-for nvda-470)

(define-nvidia-aagl an-anime-game-launcher-nvidia-580 an-anime-game-launcher-nvidia-user-alias-580
  an-anime-game-launcher-for nvda-580)

(define-nvidia-aagl an-anime-game-launcher-nvda-new-feature an-anime-game-launcher-nvidia-user-alias-590
  an-anime-game-launcher-for nvda-new-feature)

(define-nvidia-aagl an-anime-game-launcher-nvidia-595 an-anime-game-launcher-nvidia-user-alias-595
  an-anime-game-launcher-for nvda-595)

(define-nvidia-aagl an-anime-game-launcher-nvidia-beta an-anime-game-launcher-nvidia-user-alias-beta
  an-anime-game-launcher-for nvda-beta)

(define-public an-anime-game-launcher-nvidia (an-anime-game-launcher-for nvda))

;;; honkers
(define-nvidia-aagl honkers-launcher-nvidia-390 honkers-launcher-nvidia-user-alias-390
  honkers-launcher-for nvda-390)

(define-nvidia-aagl honkers-launcher-nvidia-470 honkers-launcher-nvidia-user-alias-470
  honkers-launcher-for nvda-470)

(define-nvidia-aagl honkers-launcher-nvidia-580 honkers-launcher-nvidia-user-alias-580
  honkers-launcher-for nvda-580)

(define-nvidia-aagl honkers-launcher-nvda-new-feature honkers-launcher-nvidia-user-alias-590
  honkers-launcher-for nvda-new-feature)

(define-nvidia-aagl honkers-launcher-nvidia-595 honkers-launcher-nvidia-user-alias-595
  honkers-launcher-for nvda-595)

(define-nvidia-aagl honkers-launcher-nvidia-beta honkers-launcher-nvidia-user-alias-beta
  honkers-launcher-for nvda-beta)

(define-public honkers-launcher-nvidia (honkers-launcher-for nvda))

;;; the-honkers-railway
(define-nvidia-aagl the-honkers-railway-launcher-nvidia-390 the-honkers-railway-launcher-nvidia-user-alias-390
  the-honkers-railway-launcher-for nvda-390)

(define-nvidia-aagl the-honkers-railway-launcher-nvidia-470 the-honkers-railway-launcher-nvidia-user-alias-470
  the-honkers-railway-launcher-for nvda-470)

(define-nvidia-aagl the-honkers-railway-launcher-nvidia-580 the-honkers-railway-launcher-nvidia-user-alias-580
  the-honkers-railway-launcher-for nvda-580)

(define-nvidia-aagl the-honkers-railway-launcher-nvda-new-feature the-honkers-railway-launcher-nvidia-user-alias-590
  the-honkers-railway-launcher-for nvda-new-feature)

(define-nvidia-aagl the-honkers-railway-launcher-nvidia-595 the-honkers-railway-launcher-nvidia-user-alias-595
  the-honkers-railway-launcher-for nvda-595)

(define-nvidia-aagl the-honkers-railway-launcher-nvidia-beta the-honkers-railway-launcher-nvidia-user-alias-beta
  the-honkers-railway-launcher-for nvda-beta)

(define-public the-honkers-railway-launcher-nvidia (the-honkers-railway-launcher-for nvda))

;;; sleepy
(define-nvidia-aagl sleepy-launcher-nvidia-390 sleepy-launcher-nvidia-user-alias-390
  sleepy-launcher-for nvda-390)

(define-nvidia-aagl sleepy-launcher-nvidia-470 sleepy-launcher-nvidia-user-alias-470
  sleepy-launcher-for nvda-470)

(define-nvidia-aagl sleepy-launcher-nvidia-580 sleepy-launcher-nvidia-user-alias-580
  sleepy-launcher-for nvda-580)

(define-nvidia-aagl sleepy-launcher-nvda-new-feature sleepy-launcher-nvidia-user-alias-590
  sleepy-launcher-for nvda-new-feature)

(define-nvidia-aagl sleepy-launcher-nvidia-595 sleepy-launcher-nvidia-user-alias-595
  sleepy-launcher-for nvda-595)

(define-nvidia-aagl sleepy-launcher-nvidia-beta sleepy-launcher-nvidia-user-alias-beta
  sleepy-launcher-for nvda-beta)

(define-public sleepy-launcher-nvidia (sleepy-launcher-for nvda))
