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
  #:use-module (gnu build image)
  #:use-module (guix platforms riscv)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:export (visionfive2-barebones-os
            visionfive2-image-type
            visionfive2-barebones-raw-image))

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

(define MiB (expt 2 20))

(define visionfive2-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   (partitions (list
                (partition
                 (size (* 2 MiB))
                 (label "spl")
                 (offset (* 4096 512)))
                (partition
                 (size (* 4 MiB))
                 (label "uboot")
                 (offset (* 8192 512)))
                (partition
                 (size 'guess)
                 (offset (* 12288 512))
                 (label "boot")
                 (flags '(boot))
                 (file-system "vfat")
                 (initializer #~(lambda* (root . rest)
                                  (mkdir root)
                                  (call-with-output-file
                                      (string-append root "/boot")
                                    (lambda (port)
                                      (format port "my-boot"))))))
                (partition
                 (size 'guess)
                 (label "root")
                 (file-system "ext4")
                 (file-system-options (list "-O" "^metadata_csum,^64bit"))
                 (initializer (gexp initialize-root-partition)))))))

(define visionfive2-image-type
  (image-type
   (name 'visionfive2-raw)
   (constructor (cut image-with-os visionfive2-disk-image <>))))

(define visionfive2-barebones-raw-image
  (image
   (inherit
    (os+platform->image visionfive2-barebones-os riscv64-linux
                        #:type visionfive2-image-type))
   (name 'visionfive2-barebones-raw-image)))

;; Return the default image.
visionfive2-barebones-raw-image
