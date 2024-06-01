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

(define-public linux-visionfive2-upstream
  ;; XXX: USB not work, i think guix's .config miss it.
  (package
    (inherit (customize-linux
              #:name "linux-visionfive2-upstream"
              #:linux linux-6.8
              #:source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/starfive-tech/linux")
                      ;; JH7110_VisionFive2_upstream
                      (commit "076ede06c00a4069cd9f90d609eaf35bf1bdc68a")))
                (file-name (git-file-name "linux-visionfive2" "0"))
                (sha256 (base32 "1gy43a2yaz3hzk0p7hfg9bz77lhy9ny1v0rb4920igm443r3i3d0")))
              #:configs '("CONFIG_VGA_CONSOLE=y"
                          "CONFIG_STARFIVE_JH7110_TIMER=y"
                          "CONFIG_DRM_UDL=m"
                          "CONFIG_STMMAC_PCI=y"
                          "CONFIG_DRM_VERISILICON=m"
                          "CONFIG_DRM_VERISILICON_STARFIVE_HDMI=y")))))
