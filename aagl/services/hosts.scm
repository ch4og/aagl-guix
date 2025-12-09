;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl services hosts)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (aagl-hosts-service-type
            aagl-hosts-configured?))

(define %aagl-blocked-domains
  '("overseauspider.yuanshen.com"
    "log-upload-os.hoyoverse.com"
    "log-upload-os.mihoyo.com"
    "dump.gamesafe.qq.com"
    "apm-log-upload-os.hoyoverse.com"
    "zzz-log-upload-os.hoyoverse.com"
    "log-upload.mihoyo.com"
    "devlog-upload.mihoyo.com"
    "uspider.yuanshen.com"
    "sg-public-data-api.hoyoverse.com"
    "hkrpg-log-upload-os.hoyoverse.com"
    "public-data-api.mihoyo.com"
    "prd-lender.cdp.internal.unity3d.com"
    "thind-prd-knob.data.ie.unity3d.com"
    "thind-gke-usc.prd.data.corp.unity3d.com"
    "cdp.cloud.unity3d.com"
    "remote-config-proxy-prd.uca.cloud.unity3d.com"
    "pc.crashsight.wetest.net"))

(define (domains->blocked-hosts domains)
  (list (host "0.0.0.0"
              (car domains)
              (cdr domains))))

(define %aagl-hosts
  (domains->blocked-hosts %aagl-blocked-domains))

(define (aagl-hosts-configured?)
  "Check if AAGL telemetry blocking is configured in /etc/hosts."
  (catch #t
         (lambda ()
           (let* ((hosts (call-with-input-file "/etc/hosts" read-string)))
             (any (lambda (domain)
                    (string-contains hosts domain))
                  %aagl-blocked-domains)))
         (lambda _ #t)))

(define aagl-hosts-service-type
  (service-type
   (name 'aagl-hosts)
   (extensions
    (list (service-extension
           hosts-service-type
           (const %aagl-hosts))))
   (default-value #f)
   (description
    "Add hosts entries to block HoYo telemetry and analytics servers.")))
