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
  #:use-module (gnu bootloader)
  #:use-module (vf2 bootloader u-boot)
  #:use-module (gnu image)
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
  #:use-module (vf2 packages linux)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
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
    (kernel linux-visionfive2)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (packages
     (cons* nss-certs
            htop
            tmux
            %base-packages))
    (kernel-arguments
     (list "console=ttyS0,115200"))))

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

(define visionfive2-barebones-raw-image
  (image
   (inherit
    (os+platform->image visionfive2-barebones-os riscv64-linux
                        #:type visionfive2-image-type))
   (name 'visionfive2-barebones-raw-image)))

visionfive2-barebones-raw-image
