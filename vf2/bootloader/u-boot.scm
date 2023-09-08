;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Aleksandr Vityazev <avityazev@posteo.org>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (vf2 bootloader u-boot)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages firmware)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:use-module (vf2 packages bootloaders))

(define install-starfive-visionfive2-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl (begin
                   (copy-file (string-append
                               bootloader
                               "/libexec/spl/"
                               "u-boot-spl.bin")
                              "u-boot-spl.bin")
                   (invoke (string-append #$starfive-tech-tools "/bin/spl_tool")
                           "-c"
                           "-f"
                           "u-boot-spl.bin")
                   "u-boot-spl.bin.normal.out"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 34 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 2082 512)))))

(define-public u-boot-starfive-visionfive2-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-starfive-visionfive2)
   (disk-image-installer install-starfive-visionfive2-u-boot)))
