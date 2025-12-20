;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages honkers-railway)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils)
  #:use-module (nongnu packages nvidia)
  #:use-module (aagl lib package)
  #:use-module (gnu packages gl)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (honkers-railway-launcher
            honkers-railway-launcher-nvidia))

(define honkers-railway-launcher-bin
  (package
   (name "honkers-railway-launcher-bin")
   (version "1.14.1")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/an-anime-team/the-honkers-railway-launcher"
       "/releases/download/" version "/honkers-railway-launcher"))
     (sha256
      (base32 "14amvagzvvk837pss784p23z88777b457940n8k14qp1ia23l8dw"))))
   (build-system copy-build-system)
   (arguments
    (list 
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'validate-runpath)
			                (replace 'install
				                       (lambda _
				                         (let* ((bin (string-append #$output "/bin"))
					                              (exe      (string-append bin "/honkers-railway-launcher")))
					                         (mkdir-p bin)
					                         (copy-file "honkers-railway-launcher" exe)
					                         (chmod exe #o755)))))))
   (home-page "https://github.com/an-anime-team/the-honkers-railway-launcher")
   (synopsis "Anime Team's Honkers Railway Launcher (binary)")
   (description "Prebuilt launcher with auto-patching and telemetry disabling.")
   (license license:gpl3)))


(define-public honkers-railway-launcher
  (make-aagl honkers-railway-launcher-bin
             #:nvidia #f))

(define-public honkers-railway-launcher-nvidia
  (make-aagl honkers-railway-launcher-bin 
             #:nvidia #t))
