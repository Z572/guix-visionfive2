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

(define-module (vf2-guix packages firmware)
  #:use-module (gnu packages)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  ;; #:use-module (vf2-guix packages bootloaders)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex))

(define-public opensbi-visionfive2
  (let ((commit "6f1fe98c2f565a394f258178b0daa3843672395c")
        (revision "0")
        (version "VF2_v2.6.0"))
    (package
      (name "opensbi-visionfive2")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/starfive-tech/opensbi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0v1dinli7l8cgd6by1k6dw5ysbhlbjbz692gvzfigqi37nyc7zzf"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no check target
        #:make-flags
        #~(list "ARCH=riscv"
                "PLATFORM=generic"
                ;; (string-append
                ;;  "FW_PAYLOAD_PATH="
                ;;  #$u-boot-starfive-visionfive2 "/libexec/u-boot.bin")
                ;; (string-append
                ;;  "FW_FDT_PATH="
                ;;  #$u-boot-starfive-visionfive2
                ;;  "/libexec/arch/riscv/dts/starfive_visionfive2.dtb")
                "FW_TEXT_START=0x40000000"
                #$@(if (not (string-prefix? "riscv64" (%current-system)))
                       ;; (and (string-prefix? "riscv64" arch))
                       `("CROSS_COMPILE=riscv64-linux-gnu-")
                       `("CC=gcc"))
                "FW_PAYLOAD=n"
                "V=1")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (bin (find-files "." "fw_.*\\.(elf|bin)$")))
                  (for-each
                   (lambda (file)
                     (install-file file out))
                   bin)))))))
      (native-inputs
       `(,@(if (not (string-prefix? "riscv64" (%current-system)))
               ;; (and (string-prefix? "riscv64" arch))
               `(("cross-gcc" ,(cross-gcc "riscv64-linux-gnu" #:xgcc gcc-7))
                 ("cross-binutils" ,(cross-binutils "riscv64-linux-gnu")))
               '())))
      (home-page "https://github.com/riscv/opensbi")
      (synopsis "RISC-V Open Source Supervisor Binary Interface")
      (description "A reference implementation of the RISC-V SBI specifications
for platform-specific firmwares executing in M-mode.")
      (license (list license:bsd-2
                     ;; lib/utils/libfdt/* is dual licensed under bsd-2 and gpl2+.
                     license:gpl2+
                     ;; platform/ariane-fpga/* is gpl2.
                     license:gpl2)))))
