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

(define-module (vf2-guix bootloader u-boot)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module ((vf2-guix packages bootloaders) #:prefix vf2:)
  #:use-module (vf2-guix packages firmware)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:export (u-boot-starfive-visionfive2-bootloader))

(define (visionfive2-spl)
  (define builder
    (let ((u-boot-sf vf2:u-boot-starfive-visionfive2)
          (tools-sf vf2:starfive-tech-tools))
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (copy-file (string-append #$u-boot-sf
                                      "/libexec/spl/"
                                      "u-boot-spl.bin")
                       "u-boot-spl.bin")
            (chmod "u-boot-spl.bin" #o644)
            (invoke (string-append #$tools-sf "/bin/spl_tool")
                    "-c"
                    "-f" "u-boot-spl.bin")
            (copy-file "u-boot-spl.bin.normal.out" #$output)))))
  (computed-file "u-boot-spl.bin.normal.out" builder))

(define (visionfive2-fw-payload-img)
  (define builder
    (let ((u-boot-tools-wo-sdl vf2:u-boot-tools)
          (tools-sf vf2:starfive-tech-tools))
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (set-path-environment-variable "PATH" '("bin") (list #$dtc))
            (copy-file (string-append #$tools-sf "/etc/uboot_its/"
                                      "visionfive2-uboot-fit-image.its")
                       "visionfive2-uboot-fit-image.its")
            (chmod "visionfive2-uboot-fit-image.its" #o644)
            (copy-file (string-append #$opensbi-visionfive2 "/fw_payload.bin")
                       "fw_payload.bin")
            (chmod "fw_payload.bin" #o644)
            (invoke (string-append #$u-boot-tools-wo-sdl "/bin/mkimage")
                    "-f" "visionfive2-uboot-fit-image.its"
                    "-A" "riscv"
                    "-O" "u-boot"
                    "-T" "firmware"
                    "visionfive2_fw_payload.img")
            (copy-file "visionfive2_fw_payload.img" #$output)))))
  (computed-file "visionfive2_fw_payload.img" builder))

(define install-starfive-visionfive2-u-boot
  #~(lambda (bootloader root-index image)
      (let ((spl #$(visionfive2-spl))
            (u-boot #$(visionfive2-fw-payload-img)))
        (write-file-on-device spl (stat:size (stat spl))
                              image (* 4096 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              image (* 8192 512)))))

(define u-boot-starfive-visionfive2-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-starfive-visionfive2)
   (disk-image-installer install-starfive-visionfive2-u-boot)))
