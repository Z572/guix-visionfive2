(define-module (vf2-guix packages linux)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (linux-vf2))

(define (linux-vf2-urls commit)
  "Return a git-reference for starfive Linux COMMIT."
  (git-reference
   (url "https://github.com/starfive-tech/linux")
   (commit commit)))

(define* (linux-vf2
          version
          commit
          hash
          #:key
          (name "linux-vf2")
          (revision "0")
          (defconfig "starfive_visionfive2_defconfig")
          (linux linux-libre-riscv64-generic))
  (let ((linux-package
         (customize-linux
          #:name name
          #:linux linux
          #:defconfig defconfig
          #:extra-version "starfive-visionfive2"
          #:source (origin (method git-fetch)
                           (uri (linux-vf2-urls commit))
                           (sha256 (base32 hash))
                           (file-name (git-file-name name version))))))
    (package
      (inherit linux-package)
      (version (git-version version revision commit))
      (arguments
       (substitute-keyword-arguments (package-arguments linux-package)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'install 'rename-dtb
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dtbs (string-append out "/lib/dtbs/starfive")))
                    (with-directory-excursion dtbs
                      (rename-file "jh7110-visionfive-v2.dtb" "starfive_visionfive2.dtb")))))))))
      (home-page "https://www.kernel.org/")
      (synopsis "Linux kernel with nonfree binary blobs included")
      (description
       "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function."))))

(define-public vf2-kernel
  (linux-vf2 "VF2_v2.11.5"
             "cea31b2516edef9908d43ca7d0b4c34db45cc5ac"
             "1fy9lcykwb77infkjvzidgvw3bga5k4j7pzcm341kcyy22j7gw7f"))
