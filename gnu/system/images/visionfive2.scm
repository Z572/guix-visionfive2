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
  #:use-module (gnu system uuid)
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

(define dummy-initializer
  #~(lambda* (root . rest)
      (mkdir root)
      (display "!")))

(define visionfive2-disk-image
  (image-without-os
   (format 'disk-image)
   (partition-table-type 'gpt)
   (partitions (list
                (partition
                 (size (* 2 MiB))
                 (label "spl")
                 ;; (file-system "ext2")
                 (type-uuid (uuid "2e54b353-1271-4842-806f-e436d6af6985"))
                 (offset (* 4096 512))
                 (initializer dummy-initializer))
                (partition
                 (size (* 4 MiB))
                 (label "uboot")
                 (offset (* 8192 512))
                 ;; (file-system "ext2")
                 (type-uuid (uuid "5b193300-fc78-40cd-8002-e86c45580b47"))
                 (initializer dummy-initializer))
                (partition
                 (size (* 292 MiB))
                 (offset (* 16384 512))
                 (label "boot")
                 (flags '(esp))
                 (file-system "vfat")
                 (type-uuid (uuid "ebd0a0a2-b9e5-4433-87c0-68b6b72699c7"))
                 (initializer #~(lambda* (root . rest)
                                  (mkdir root)
                                  (call-with-output-file
                                      (string-append root "/boot")
                                    (lambda (port)
                                      (format port "my-boot"))))))
                (partition
                 (size 'guess)
                 (label "root")
                 (flags '(boot))
                 (file-system "ext4")
                 (type-uuid (uuid "0fc63daf-8483-4772-8e79-3d69d8477de4"))
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
