(define-module (vf2-guix packages linux)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (linux-vf2))

(define (linux-vf2-upstream commit)
  (git-reference
   (url "https://github.com/starfive-tech/linux")
   (commit commit)))

(define* (linux-vf2
          commit
          hash
          version
          #:key
          (name "linux-vf2")
          (defconfig "defconfig")
          (linux linux-libre-riscv64-generic))
  (let ((linux-package
         (customize-linux
          #:name name
          #:linux linux
          #:defconfig defconfig
          #:extra-version "starfive-visionfive2"
          #:source (origin
                     (method git-fetch)
                     (uri (linux-vf2-upstream commit))
                     (file-name (git-file-name name version))
                     (sha256 (base32 hash))))))
    (package
      (inherit linux-package)
      (version version)
      (arguments
       (substitute-keyword-arguments (package-arguments linux-package)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              ;; (add-after 'unpack 'increase-memory
              ;;   (lambda _
              ;;     (substitute* "arch/riscv/boot/dts/starfive/jh7110-visionfive-v2.dtsi"
              ;;       (("0x0 0x40000000 0x1 0x0") "0x0 0x40000000 0x2 0x0"))))
              (add-after 'install 'rename-dtb
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dtbs (string-append out "/lib/dtbs/starfive")))
                    (with-directory-excursion dtbs
                      (rename-file "jh7110-starfive-visionfive-2-v1.2a.dtb"
                                   "starfive_visionfive2.dtb")))))))))
      (inputs (modify-inputs (package-inputs linux-package)
                (prepend zstd)))
      (home-page "https://www.kernel.org/")
      (synopsis "Linux kernel with nonfree binary blobs included")
      (description
       "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function."))))

(define-public vf2-kernel-upstream
  (linux-vf2 "5067e82028046501daa6e0c53e8c54343a217f45"
             "1qh53mzg6d3130ys7c7cwdazh20wzyhazz92ix96dc42nhjp49xg"
             "VF2_v6.4"
             #:defconfig (local-file "aux-files/linux/x86_64_defconfig")))
