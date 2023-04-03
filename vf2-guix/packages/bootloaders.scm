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

(define-module (vf2-guix packages bootloaders)
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
  #:use-module (vf2-guix packages firmware)
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

(define u-boot
  (let ((commit "688befadf1d337dee3593e6cc0fe1c737cc150bd")
        (revision "0")
        (version "VF2_v2.11.5"))
    (package
      (name "u-boot")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/starfive-tech/u-boot")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1sp4lwksa701jgpnj8a1q9wgqgl6wz8w2s2wdviyrjjrjh806q24"))))
      (build-system gnu-build-system)
      (native-inputs
       (list bc
             bison
             dtc
             flex
             lz4
             perl
             python
             openssl
             python-coverage
             python-pycryptodomex
             python-pytest
             swig))
      (home-page "https://www.denx.de/wiki/U-Boot/")
      (synopsis "ARM bootloader")
      (description "U-Boot is a bootloader used mostly for ARM boards. It
also initializes the boards (RAM etc).")
      (license license:gpl2+))))

(define-public (make-u-boot-package board triplet)
  "Returns a u-boot package for BOARD cross-compiled for TRIPLET."
  (let ((same-arch? (lambda ()
                      (string=? (%current-system)
                                (gnu-triplet->nix-system triplet)))))
    (package
      (inherit u-boot)
      (name (string-append "u-boot-"
                           (string-replace-substring (string-downcase board)
                                                     "_" "-")))
      (arguments
       (list
        #:modules '((ice-9 ftw)
                    (srfi srfi-1)
                    (guix build utils)
                    (guix build gnu-build-system))
        #:test-target "test"
        #:make-flags
        #~(list "HOSTCC=gcc"
                #$@(if (not (same-arch?))
                       #~((string-append "CROSS_COMPILE=" #$triplet "-"))
                       '()))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key outputs make-flags #:allow-other-keys)
                (let ((config-name (string-append #$board "_defconfig")))
                  (if (file-exists? (string-append "configs/" config-name))
                      (apply invoke "make" `(,@make-flags ,config-name))
                      (begin
                        (display "Invalid board name. Valid board names are:"
                                 (current-error-port))
                        (let ((suffix-len (string-length "_defconfig"))
                              (entries (scandir "configs")))
                          (for-each (lambda (file-name)
                                      (when (string-suffix? "_defconfig" file-name)
                                        (format (current-error-port)
                                                "- ~A\n"
                                                (string-drop-right file-name
                                                                   suffix-len))))
                                    (sort entries string-ci<)))
                        (error "Invalid boardname ~s." #$board))))))
            ;; Почему-то "CONFIG_TOOLS_LIBCRYPTO=n" оказывается в .config.old вместо .config
            ;; Кто мне скажет почему?
            ;; (add-after 'configure 'disable-tools-libcrypto
            ;;   ;; Disable libcrypto due to GPL and OpenSSL license
            ;;   ;; incompatibilities
            ;;   (lambda _
            ;;     (substitute* ".config"
            ;;       (("CONFIG_TOOLS_LIBCRYPTO=y") "CONFIG_TOOLS_LIBCRYPTO=n"))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (libexec (string-append out "/libexec"))
                       (uboot-files (append
                                        (remove
                                         ;; Those would not be reproducible
                                         ;; because of the randomness used
                                         ;; to produce them.
                                         ;; It's expected that the user will
                                         ;; use u-boot-tools to generate them
                                         ;; instead.
                                         (lambda (name)
                                           (string-suffix?
                                            "sunxi-spl-with-ecc.bin"
                                            name))
                                         (find-files "." ".*\\.(bin|efi|img|spl|itb|dtb|rksd)$"))
                                        (find-files "." "^(MLO|SPL)$"))))
                  (mkdir-p libexec)
                  (install-file ".config" libexec)
                  ;; Useful for "qemu -kernel".
                  (install-file "u-boot" libexec)
                  (for-each
                   (lambda (file)
                     (let ((target-file (string-append libexec "/" file)))
                       (mkdir-p (dirname target-file))
                       (copy-file file target-file)))
                   uboot-files)))))))
      (native-inputs
       `(,@(if (not (same-arch?))
               `(("cross-gcc" ,(cross-gcc triplet))
                 ("cross-binutils" ,(cross-binutils triplet)))
               `())
         ,@(package-native-inputs u-boot))))))

(define-public u-boot-starfive-visionfive2
  (make-u-boot-package "starfive_visionfive2" "riscv64-linux-gnu")
  ;; (let ((base (make-u-boot-package "starfive_visionfive2" "riscv64-linux-gnu")))
  ;;   (package
  ;;     (inherit base)
  ;;     (arguments
  ;;      (substitute-keyword-arguments (package-arguments base)
  ;;        ((#:phases phases)
  ;;         #~(modify-phases #$phases
  ;;             (add-after 'unpack 'set-environment
  ;;               (lambda* (#:key inputs #:allow-other-keys)
  ;;                 (setenv "OPENSBI" (search-input-file inputs
  ;;                                                      "fw_dynamic.bin"))))))))
  ;;     (inputs
  ;;      (modify-inputs (package-inputs base)
  ;;        (append opensbi-visionfive2)))))
  )
