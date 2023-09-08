(define-module (vf2 packages linux)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy))

(define-public linux-visionfive2
  (customize-linux
   #:name "linux-visionfive2"
   #:linux linux-libre-riscv64-generic
   #:defconfig "starfive_visionfive2_defconfig"
   #:source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/starfive-tech/linux")
           (commit "0dfeb6ace464f0c455a9508815bb9f70760d6faf")))
     (file-name (git-file-name "linux-visionfive2" "0"))
     (sha256 (base32 "1lc1izw2nyhck9sqy5sxx5cdjknj7jqmjgyjaj1brdf93492ycv4")))
   #:configs '("# CONFIG_MODULE_COMPRESS_ZSTD is not set")))
