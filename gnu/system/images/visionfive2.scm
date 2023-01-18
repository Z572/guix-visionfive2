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

(define-module (gnu system images visionfive2)
  #:use-module (gnu bootloader)
  #:use-module (vf2-guix bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (guix records)
  #:use-module (srfi srfi-26)
  #:export (visionfive2-barebones-os
            visionfive2-image-type
            visionfive2-barebones-raw-image))

(define-record-type* <platform> platform make-platform
  platform?
  (target               platform-target)
  (system               platform-system)
  (linux-architecture   platform-linux-architecture
                        (default #false))
  (glibc-dynamic-linker platform-glibc-dynamic-linker))

(define riscv64-linux
  (platform
   (target "riscv64-linux-gnu")
   (system "riscv64-linux")
   (linux-architecture "riscv")
   (glibc-dynamic-linker "/lib/ld-linux-riscv64-lp64d.so.1")))

(define visionfive2-barebones-os
  (operating-system
    (host-name "vf2")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-starfive-visionfive2-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '())
    (kernel linux-libre-riscv64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (cons (service agetty-service-type
                             (agetty-configuration
                              (extra-options '("-L")) ; no carrier detect
                              (baud-rate "1500000")
                              (term "vt100")
                              (tty "ttyS2")))
                    %base-services))))

(define visionfive2-image-type
  (image-type
   (name 'visionfive2-raw)
   (constructor (cut image-with-os
                     (raw-with-offset-disk-image (* 4 (expt 2 20))) ;4MiB
                     <>))))

(define visionfive2-barebones-raw-image
  (image
   (inherit
    (os+platform->image visionfive2-barebones-os riscv64-linux
                        #:type visionfive2-image-type))
   (name 'visionfive2-barebones-raw-image)))

;; Return the default image.
visionfive2-barebones-raw-image
