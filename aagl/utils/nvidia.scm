;;; SPDX-FileCopyrightText: 2022-2026 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl utils nvidia)
  #:use-module (gnu packages gl)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix utils)
  #:export (define-nvidia-container))

(define-public %nvidia-environment-variable-regexps
  '("^__NV_"
    "^__GL_"                            ; NVIDIA OpenGL settings.
    "^__GLX_VENDOR_LIBRARY_NAME$"       ; For GLVND.
    ;; NVIDIA PRIME Render Offload.
    "^__VK_LAYER_NV_optimus$"
    ;; NVIDIA NGX.
    "^__NGX_CONF_FILE$"
    "^PROTON_ENABLE_NGX_UPDATER$"
    ;; NVIDIA Smooth Motion.
    "^NVPRESENT_"
    ;; NVIDIA VDPAU settings.
    "^VDPAU_NVIDIA_"
    ;; GSYNC control for Vulkan direct-to-display applications.
    "^VKDirectGSYNC(Compatible)?Allowed$"))

(define* (replace-mesa obj #:key driver)
  (with-transformation
   (package-input-grafting
    `((,mesa . ,driver)))
      obj))

(define-public (smart-runtime-replace-mesa packages driver)
  (if (eq? driver mesa)
      packages
      (modify-inputs
          (replace-mesa packages #:driver driver)
        (replace "mesa" driver))))
