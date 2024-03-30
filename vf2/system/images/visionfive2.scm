;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Aleksandr Vityazev <avityazev@posteo.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (vf2 system images visionfive2)
  #:use-module (gnu)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu bootloader)
  #:use-module (vf2 bootloader u-boot)
  #:use-module (gnu packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu build image)
  #:use-module (guix platforms riscv)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:use-module (gnu services dbus)
  #:use-module (gnu services dns)
  #:use-module (gnu services avahi)
  #:use-module (gnu services networking)
  #:use-module  (gnu services cuirass)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages file)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages admin)
  #:use-module (ice-9 poe)
  #:use-module (guix channels)
  #:use-module (guix transformations)
  #:use-module (gnu packages ci)
  #:use-module (gnu packages autotools)
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (vf2 packages linux)
  #:use-module (gnu packages tmux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages freedesktop)
  #:export (visionfive2-os
            visionfive2-image-type
            visionfive2-raw-image))

(use-modules (guix packages)
             (guix)
             (ice-9 poe)
             (gnu)
             (guix git-download)

             (gnu home services desktop)
             (guix build-system trivial)
             (srfi srfi-1)
             (srfi srfi-26)
             (gnu packages video)
             (ice-9 format)

             (gnu packages linux)
             (gnu packages pulseaudio)

             (gnu home services)
             (gnu home services shepherd)
             (ice-9 pretty-print)
             (ice-9 match)
             (guix transformations))

(use-package-modules linux emacs cmake emacs-xyz guile guile-xyz gnupg rust-apps shellutils)
(use-system-modules keyboard)

(use-modules ;; (nongnu packages linux)
             (gnu packages linux))
(define visionfive2-os
  (operating-system
    (host-name "vf2")
    (timezone "Asia/Shanghai")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-starfive-visionfive2-bootloader)
                 (targets '("/dev/mmcblk0"))))
    (initrd-modules '())
    (kernel
     linux-libre-riscv64-generic
     ;; (corrupt-linux linux-libre-riscv64-generic
     ;;                #:name "linux-riscv64-generic"
     ;;                #:configs (append
     ;;                           (list "CONFIG_PHY_STARFIVE_JH7110_PCIE=y"
     ;;                                 "CONFIG_PHY_STARFIVE_JH7110_USB=y"
     ;;                                 "CONFIG_PHY_STARFIVE_JH7110_DPHY_RX=y"

     ;;                                 "CONFIG_SND_SOC_STARFIVE=y"
     ;;                                 "CONFIG_SND_SOC_JH7110_PWMDAC=y"
     ;;                                 "CONFIG_SND_SOC_JH7110_TDM=y"









     ;;                                 ;; "CONFIG_STARFIVE_JH7110_TIMER=y"
     ;;                                 )

     ;;                           (map symbol->string
     ;;                                '(;CONFIG_BT_HCIBTUSB=m
     ;;                                    ;CONFIG_BT_HCIBTUSB_BCM=y
     ;;                                    ;CONFIG_BT_HCIBTUSB_RTL=y
     ;;                                    ;CONFIG_MEDIA_USB_SUPPORT=y
     ;;                                  CONFIG_SND_USB=y
     ;;                                  CONFIG_SND_USB_AUDIO=y
     ;;                                    ;CONFIG_SND_USB_AUDIO_USE_MEDIA_CONTROLLER=y
     ;;                                  CONFIG_USB=y
     ;;                                  CONFIG_USB_ARCH_HAS_HCD=y
     ;;                                  CONFIG_USB_AUTOSUSPEND_DELAY=2
     ;;                                  CONFIG_USB_CDNS3=y
     ;;                                  CONFIG_USB_CDNS3_GADGET=y
     ;;                                  CONFIG_USB_CDNS3_HOST=y
     ;;                                    ;CONFIG_USB_CDNS3_IMX=y
     ;;                                  CONFIG_USB_CDNS3_STARFIVE=y
     ;;                                    ;CONFIG_USB_CDNS3_TI=y
     ;;                                  CONFIG_USB_CDNS_HOST=y
     ;;                                  CONFIG_USB_CDNS_SUPPORT=y
     ;;                                  CONFIG_USB_COMMON=y
     ;;                                  CONFIG_USB_CONFIGFS=y
     ;;                                  CONFIG_USB_CONFIGFS_F_FS=y
     ;;                                  CONFIG_USB_CONFIGFS_MASS_STORAGE=y
     ;;                                  CONFIG_USB_DEFAULT_PERSIST=y
     ;;                                  CONFIG_USB_EZUSB_FX2=m
     ;;                                  CONFIG_USB_F_FS=y
     ;;                                  CONFIG_USB_F_MASS_STORAGE=y
     ;;                                  CONFIG_USB_GADGET=y
     ;;                                  CONFIG_USB_GADGET_STORAGE_NUM_BUFFERS=2
     ;;                                  CONFIG_USB_GADGET_VBUS_DRAW=2
     ;;                                  CONFIG_USB_HID=y
     ;;                                  CONFIG_USB_LIBCOMPOSITE=y
     ;;                                  CONFIG_USB_NET_DRIVERS=y
     ;;                                  CONFIG_USB_OHCI_LITTLE_ENDIAN=y
     ;;                                  CONFIG_USB_PCI=y
     ;;                                  CONFIG_USB_ROLE_SWITCH=y
     ;;                                  CONFIG_USB_SERIAL=m
     ;;                                  CONFIG_USB_SERIAL_AIRCABLE=m
     ;;                                  CONFIG_USB_SERIAL_ARK3116=m
     ;;                                  CONFIG_USB_SERIAL_BELKIN=m
     ;;                                  CONFIG_USB_SERIAL_CH341=m
     ;;                                  CONFIG_USB_SERIAL_CP210X=m
     ;;                                  CONFIG_USB_SERIAL_CYBERJACK=m
     ;;                                  CONFIG_USB_SERIAL_CYPRESS_M8=m
     ;;                                  CONFIG_USB_SERIAL_DEBUG=m
     ;;                                  CONFIG_USB_SERIAL_DIGI_ACCELEPORT=m
     ;;                                  CONFIG_USB_SERIAL_EDGEPORT=m
     ;;                                  CONFIG_USB_SERIAL_EDGEPORT_TI=m
     ;;                                  CONFIG_USB_SERIAL_EMPEG=m
     ;;                                  CONFIG_USB_SERIAL_F81232=m
     ;;                                  CONFIG_USB_SERIAL_FTDI_SIO=m
     ;;                                  CONFIG_USB_SERIAL_GARMIN=m
     ;;                                  CONFIG_USB_SERIAL_GENERIC=y
     ;;                                  CONFIG_USB_SERIAL_IPAQ=m
     ;;                                  CONFIG_USB_SERIAL_IPW=m
     ;;                                  CONFIG_USB_SERIAL_IR=m
     ;;                                  CONFIG_USB_SERIAL_IUU=m
     ;;                                  CONFIG_USB_SERIAL_KEYSPAN=m
     ;;                                  CONFIG_USB_SERIAL_KEYSPAN_PDA=m
     ;;                                  CONFIG_USB_SERIAL_KLSI=m
     ;;                                  CONFIG_USB_SERIAL_KOBIL_SCT=m
     ;;                                  CONFIG_USB_SERIAL_MCT_U232=m
     ;;                                  CONFIG_USB_SERIAL_METRO=m
     ;;                                  CONFIG_USB_SERIAL_MOS7720=m
     ;;                                  CONFIG_USB_SERIAL_MOS7840=m
     ;;                                  CONFIG_USB_SERIAL_NAVMAN=m
     ;;                                  CONFIG_USB_SERIAL_OMNINET=m
     ;;                                  CONFIG_USB_SERIAL_OPTICON=m
     ;;                                  CONFIG_USB_SERIAL_OTI6858=m
     ;;                                  CONFIG_USB_SERIAL_PL2303=m
     ;;                                  CONFIG_USB_SERIAL_QCAUX=m
     ;;                                  CONFIG_USB_SERIAL_QUALCOMM=m
     ;;                                  CONFIG_USB_SERIAL_SAFE=m
     ;;                                  CONFIG_USB_SERIAL_SIERRAWIRELESS=m
     ;;                                  CONFIG_USB_SERIAL_SPCP8X5=m
     ;;                                  CONFIG_USB_SERIAL_SSU100=m
     ;;                                  CONFIG_USB_SERIAL_SYMBOL=m
     ;;                                  CONFIG_USB_SERIAL_TI=m
     ;;                                  CONFIG_USB_SERIAL_VISOR=m
     ;;                                  CONFIG_USB_SERIAL_WHITEHEAT=m
     ;;                                  CONFIG_USB_SERIAL_WISHBONE=m
     ;;                                  CONFIG_USB_SERIAL_WWAN=m
     ;;                                  CONFIG_USB_SERIAL_XSENS_MT=m
     ;;                                  CONFIG_USB_STORAGE=y
     ;;                                  CONFIG_USB_SUPPORT=y
     ;;                                  CONFIG_USB_UAS=y
     ;;                                    ;CONFIG_USB_VIDEO_CLASS=y
     ;;                                    ;CONFIG_USB_VIDEO_CLASS_INPUT_EVDEV=y
     ;;                                    ;CONFIG_USB_WIFI_ECR6600U=y
     ;;                                  CONFIG_USB_XHCI_HCD=y
     ;;                                  CONFIG_USB_XHCI_PCI=y
     ;;                                  CONFIG_USB_XHCI_PLATFORM=y
     ;;                                  ))))
     ;; linux-visionfive2
     ;; linux-visionfive2-6.1-devel
     ;; linux-visionfive2-devel
     )
    (file-systems (cons (file-system
                          (device (file-system-label "Guix_image"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (packages
     (cons* nss-certs
            cloud-utils
            htop
            tmux
            le-certs lrzsz curl
            file
            %base-packages))
    (kernel-arguments
     (list "console=ttyS0,115200"))
    (essential-services
     (modify-services (operating-system-default-essential-services
                       this-operating-system)
       (shepherd-root-service-type
        config => (shepherd-configuration
                   (inherit config)
                   (shepherd
                    (package
                      (inherit shepherd-0.10)
                      (native-inputs
                       (modify-inputs
                           (package-native-inputs shepherd-0.10)
                         (replace "guile-fibers"
                           guile-fibers-1.1)))
                      (inputs (modify-inputs
                                  (package-inputs shepherd-0.10)
                                (replace "guile-fibers"
                                  (this-package-native-input "guile-fibers"))))))))))
    (services
     (cons*
      (service ntp-service-type)
      (service avahi-service-type)
      (service wpa-supplicant-service-type)
      (service network-manager-service-type
               (network-manager-configuration
                (network-manager
                 (package (inherit network-manager)
                          (arguments
                           (substitute-keyword-arguments (package-arguments network-manager)
                             ((#:tests? test? #f)
                              #f)))))))
      ;; (service dnsmasq-service-type)
      ;; (service connman-service-type)
      (service openssh-service-type
               (openssh-configuration
                (openssh openssh-sans-x)
                (permit-root-login #t)
                (allow-empty-passwords? #t)))
      ;; (service cuirass-remote-worker-service-type
      ;;          (cuirass-remote-worker-configuration
      ;;           (workers 2)
      ;;           (substitute-urls (list "https://mirror.sjtu.edu.cn/guix"
      ;;                                  "https://bordeaux-singapore-mirror.cbaines.net"))))
      (modify-services %base-services

        (guix-service-type config =>
                           (guix-configuration
                            (inherit config)
                            (substitute-urls
                             (list "https://mirror.sjtu.edu.cn/guix"
                                   "https://bordeaux-singapore-mirror.cbaines.net"))
                            (authorized-keys
                             (append
                              (list (plain-file "ci.z572.online"
                                                "(public-key
        (ecc
         (curve Ed25519)
         (q #166927E5E329B2AF7E965A4AE07403B69177EB0B8556D3467A83E0BE5E3D27F9#)))")
                                    )
                              %default-authorized-guix-keys))
                            (discover? #t)))
        )))))

(use-modules (gnu image))

(define visionfive2-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   (partitions (list
                (partition
                 (size (* 1 (expt 2 20)))
                 (label "spl")
                 (offset (* 34 512))
                 (file-system "unformatted")
                 (uuid (uuid "2E54B353-1271-4842-806F-E436D6AF6985")))
                (partition
                 (size (* 4 (expt 2 20)))
                 (label "uboot")
                 (offset (* 2082 512))
                 (file-system "unformatted")
                 (uuid (uuid "BC13C2FF-59E6-4262-A352-B275FD6F7172")))
                root-partition))))

(define visionfive2-image-type
  (image-type
   (name 'visionfive2-raw)
   (constructor (cut image-with-os visionfive2-disk-image <>))))

(define visionfive2-raw-image
  (image
   (inherit
    (os+platform->image visionfive2-os riscv64-linux
                        #:type visionfive2-image-type))
   (name 'visionfive2-raw-image)))

visionfive2-raw-image
