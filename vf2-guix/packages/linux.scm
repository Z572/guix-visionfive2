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

(define (linux-vf2-urls version)
  "Return a list of URLS for Linux VERSION."
  (list
   (string-append
    "https://github.com/starfive-tech/linux/archive/refs/tags/" version ".tar.gz")))

(define* (linux-vf2
          version
          hash
          #:key
          (name "linux-vf2")
          (defconfig "starfive_visionfive2_defconfig")
          (linux linux-libre-riscv64-generic))
  (let ((linux-package
         (customize-linux
          #:name name
          #:linux linux
          #:defconfig defconfig
          #:extra-version "starfive-visionfive2"
          #:source (origin (method url-fetch)
                           (uri (linux-vf2-urls version))
                           (sha256 (base32 hash))))))
    (package
      (inherit linux-package)
      (version version)
      (home-page "https://www.kernel.org/")
      (synopsis "Linux kernel with nonfree binary blobs included")
      (description
       "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function."))))

(define-public vf2-kernel
  (linux-vf2 "VF2_v2.11.5"
             "1h0zac2szbxyv8dp0sm0bfrprjn09j3icjsl7jp35bsz336g4vla"))
