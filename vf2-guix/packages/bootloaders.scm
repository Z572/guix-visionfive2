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

(define u-boot-upstream (@@ (gnu packages bootloaders) u-boot))

(define-public u-boot-tools
  (package
    (inherit u-boot-upstream)
    (name "u-boot-tools")
    (native-inputs
     (modify-inputs (package-native-inputs u-boot-upstream)
       (prepend python-coverage python-pycryptodomex python-pytest)))
    (arguments
     `(#:make-flags '("HOSTCC=gcc" "NO_SDL=1")
       #:test-target "tcheck"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/bin/pwd") (which "pwd"))
               (("/bin/false") (which "false")))
             (substitute* "tools/dtoc/fdt_util.py"
               (("'cc'") "'gcc'"))
             (substitute* "tools/patman/test_util.py"
               ;; python3-coverage is simply called coverage in guix.
               (("python3-coverage") "coverage")

               ;; Don't require 100% coverage since it's brittle and can
               ;; fail with newer versions of coverage or dependencies.
               (("raise ValueError\\('Test coverage failure'\\)")
                "print('Continuing anyway since Guix does not care :O')"))
             (substitute* "test/run"
               ;; Make it easier to find test failures.
               (("#!/bin/bash") "#!/bin/bash -x")
               ;; This test would require git.
               (("\\./tools/patman/patman") (which "true"))
               ;; FIXME: test fails, needs further investiation
               (("run_test \"binman\"") "# run_test \"binman\"")
               ;; FIXME: test_spl fails, needs further investiation
               (("test_ofplatdata or test_handoff or test_spl")
                "test_ofplatdata or test_handoff")
               ;; FIXME: code coverage not working
               (("run_test \"binman code coverage\"")
                "# run_test \"binman code coverage\"")
               ;; This test would require internet access.
               (("\\./tools/buildman/buildman") (which "true")))
             (substitute* "test/py/tests/test_sandbox_exit.py"
               (("def test_ctrl_c")
                "@pytest.mark.skip(reason='Guix has problems with SIGINT')
def test_ctrl_c"))
             ;; Test against the tools being installed rather than tools built
             ;; for "sandbox" target.
             (substitute* "test/image/test-imagetools.sh"
               (("BASEDIR=sandbox") "BASEDIR=."))
             (for-each (lambda (file)
                         (substitute* file
                           ;; Disable features that require OpenSSL due
                           ;; to GPL/Openssl license incompatibilities.
                           ;; See https://bugs.gnu.org/34717 for
                           ;; details.
                           (("CONFIG_FIT_SIGNATURE=y")
                            "CONFIG_FIT_SIGNATURE=n\nCONFIG_UT_LIB_ASN1=n\nCONFIG_TOOLS_LIBCRYPTO=n")
                           ;; This test requires a sound system, which is un-used
                           ;; in u-boot-tools.
                           (("CONFIG_SOUND=y") "CONFIG_SOUND=n")))
                       (find-files "configs" "sandbox_.*defconfig$|tools-only_defconfig"))))
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "tools-only_defconfig" make-flags)))
         (replace 'build
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (apply invoke "make" "tools-all" make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (name)
                           (install-file name bin))
                         '("tools/netconsole"
                           "tools/jtagconsole"
                           "tools/gen_eth_addr"
                           "tools/gen_ethaddr_crc"
                           "tools/img2srec"
                           "tools/mkenvimage"
                           "tools/dumpimage"
                           "tools/mkimage"
                           "tools/kwboot"
                           "tools/proftool"
                           "tools/fdtgrep"
                           "tools/env/fw_printenv"
                           "tools/sunxi-spl-image-builder")))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key make-flags test-target #:allow-other-keys)
             (invoke "test/image/test-imagetools.sh")))
         ;; Only run full test suite on x86_64 systems, as many tests
         ;; assume x86_64.
         ,@(if (string-match "^x86_64-linux"
                             (or (%current-target-system)
                                 (%current-system)))
               '((add-after 'check 'check-x86
                   (lambda* (#:key make-flags test-target #:allow-other-keys)
                     (apply invoke "make" "mrproper" make-flags)
                     (setenv "SDL_VIDEODRIVER" "dummy")
                     (setenv "PAGER" "cat")
                     (apply invoke "make" test-target make-flags))))
               '()))))
    (description (string-append
                  (package-description u-boot-upstream)
                  "  This package provides board-independent tools "
                  "of U-Boot."))))

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
