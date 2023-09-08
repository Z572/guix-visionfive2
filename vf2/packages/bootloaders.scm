;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Aleksandr Vityazev <avityazev@posteo.org>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (vf2 packages bootloaders)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages swig)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex))

(define-public u-boot-starfive-visionfive2
  (let ((opensbi (package
                   (inherit opensbi-generic)
                   (arguments
                    (substitute-keyword-arguments (package-arguments opensbi-generic)
                      ((#:make-flags flags)
                       `(cons* "FW_TEXT_START=0x40000000"
                               "FW_OPTIONS=0"
                               ,flags))))))
        (base (make-u-boot-package "starfive_visionfive2" "riscv64-linux-gnu")))
    (package-with-extra-patches
     (package
       (inherit base)
       (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           #~(modify-phases #$phases
               (add-after 'unpack 'set-environment
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "OPENSBI" (search-input-file inputs
                                                        "fw_dynamic.bin"))))))))
       (inputs
        (modify-inputs (package-inputs base)
          (append opensbi))))
     (list (local-file "patches/v2-1-2-dm-event-add-EVT_DM_POST_INIT_R-event-type.diff")
           (local-file "patches/v2-2-2-riscv-cpu-make-riscv_cpu_probe-to-EVT_DM_POST_INIT_R-callback.diff")))))

(define-public starfive-tech-tools
  (let ((commit "8c5acc4e5eb7e4ad012463b05a5e3dbbfed1c38d")
        (revision "0")
        (version "0"))
    (package
      (name "starfive-tech-tools")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/starfive-tech/Tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0y0k7pmdydl8djrl47a2qk3ly3gaa7zlxfdy3dfdawkcr7mpxzr9"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no check target
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir-elisp
              ;; spl_tool directory
              (lambda _
                (chdir "spl_tool")))
            (delete 'configure)
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin-dir (string-append out "/bin"))
                       (etc-dir (string-append out "/etc")))
                  (install-file "spl_tool" bin-dir)
                  (install-file "../uboot_its/visionfive2-uboot-fit-image.its"
                                (string-append etc-dir "/uboot_its"))))))))
      (home-page "https://github.com/starfive-tech/Tools")
      (synopsis "Starfive Tech tools")
      (description "This package provides Starfive Tech tools.")
      (license license:gpl2+))))
