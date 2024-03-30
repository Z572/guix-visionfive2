(define-module (vf2 packages linux)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy))

(define-public linux-visionfive2-devel
  (let ((linux ((@@ (gnu packages linux) make-linux-libre)
                "5.15" "0" "" (list "riscv64-linux")
                ;; JH7110_VisionFive2_devel branch
                ;; #:configuration-file (lambda* (x #:key variant)
                ;;                        (local-file "aux/config"))
                #:defconfig "starfive_visionfive2_defconfig")))
    (package
      (inherit linux)
      (name "linux-visionfive2-devel")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/starfive-tech/linux")
                      (commit "06ad134b6efeb393868e300058752ecfbf7258d1")))
                (file-name (git-file-name "linux-visionfive2" "0"))
                (sha256 (base32 "0awh0d74d1k1qz1sm1ianqnzla1kxian72aydi9jckwiibmp6q5l"))))
      (arguments
       (substitute-keyword-arguments (package-arguments linux)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'install 'rename-dtb
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dtbs (string-append out "/lib/dtbs/starfive")))
                    (with-directory-excursion dtbs
                      (rename-file "jh7110-visionfive-v2.dtb"
                                   "jh7110-starfive-visionfive-2-v1.3b.dtb")))))))))
      (inputs (modify-inputs (package-inputs linux)
                (prepend zstd))))))

(define-public linux-visionfive2-devel*
  (let ((linux (customize-linux
                ;; JH7110_VisionFive2_devel branch
                #:name "linux-visionfive2-devel"
                #:linux linux-libre-5.15
                #:defconfig "starfive_visionfive2_defconfig"
                #:configs
                (map symbol->string
                     '(CONFIG_CAN_8DEV_USB=m

                       CONFIG_CAN_EMS_USB=m
                       CONFIG_CAN_ESD_USB2=m
                       CONFIG_CAN_ETAS_ES58X=m
                       CONFIG_CAN_GS_USB=m
                       CONFIG_CAN_KVASER_USB=m
                       CONFIG_CAN_MCBA_USB=m
                       CONFIG_CAN_PEAK_USB=m
                       CONFIG_CAN_UCAN=m
                       CONFIG_USB_CATC=m
                       CONFIG_USB_KAWETH=m
                       CONFIG_USB_PEGASUS=m
                       CONFIG_USB_RTL8150=m
                       CONFIG_USB_RTL8152=m
                       CONFIG_USB_LAN78XX=m
                       CONFIG_USB_USBNET=m
                       CONFIG_USB_NET_AX8817X=m
                       CONFIG_USB_NET_AX88179_178A=m
                       CONFIG_USB_NET_CDCETHER=m
                       CONFIG_USB_NET_CDC_EEM=m
                       CONFIG_USB_NET_CDC_NCM=m
                       CONFIG_USB_NET_HUAWEI_CDC_NCM=m
                       CONFIG_USB_NET_CDC_MBIM=m
                       CONFIG_USB_NET_DM9601=m
                       CONFIG_USB_NET_SR9700=m
                       CONFIG_USB_NET_SR9800=m
                       CONFIG_USB_NET_SMSC75XX=m
                       CONFIG_USB_NET_SMSC95XX=m
                       CONFIG_USB_NET_GL620A=m
                       CONFIG_USB_NET_NET1080=m
                       CONFIG_USB_NET_PLUSB=m
                       CONFIG_USB_NET_MCS7830=m
                       CONFIG_USB_NET_RNDIS_HOST=m
                       CONFIG_USB_NET_CDC_SUBSET_ENABLE=m
                       CONFIG_USB_NET_CDC_SUBSET=m
                       CONFIG_USB_ALI_M5632=y
                       CONFIG_USB_AN2720=y
                       CONFIG_USB_BELKIN=y
                       CONFIG_USB_ARMLINUX=y
                       CONFIG_USB_EPSON2888=y
                       CONFIG_USB_KC2190=y
                       CONFIG_USB_NET_ZAURUS=m
                       CONFIG_USB_NET_CX82310_ETH=m
                       CONFIG_USB_NET_KALMIA=m
                       CONFIG_USB_NET_QMI_WWAN=m
                       CONFIG_USB_HSO=m
                       CONFIG_USB_NET_INT51X1=m
                       CONFIG_USB_CDC_PHONET=m
                       CONFIG_USB_IPHETH=m
                       CONFIG_USB_SIERRA_NET=m
                       CONFIG_USB_VL600=m
                       CONFIG_USB_NET_CH9200=m
                       CONFIG_USB_NET_AQC111=m
                       CONFIG_USB_RTL8153_ECM=m
                       CONFIG_WLAN=y
                       CONFIG_WLAN_VENDOR_ADMTEK=y
                       CONFIG_ADM8211=m
                       CONFIG_ATH_COMMON=m
                       CONFIG_WLAN_VENDOR_ATH=y
                       CONFIG_ATH5K_PCI=y
                       CONFIG_ATH9K_HW=m
                       CONFIG_ATH9K_COMMON=m
                       CONFIG_ATH9K_COMMON_DEBUG=y
                       CONFIG_ATH9K_BTCOEX_SUPPORT=y
                       CONFIG_ATH9K=m
                       CONFIG_ATH9K_PCI=y
                       CONFIG_ATH9K_AHB=y
                       CONFIG_ATH9K_DEBUGFS=y
                       CONFIG_ATH9K_STATION_STATISTICS=y
                       CONFIG_ATH9K_WOW=y
                       CONFIG_ATH9K_RFKILL=y
                       CONFIG_ATH9K_CHANNEL_CONTEXT=y
                       CONFIG_ATH9K_PCOEM=y
                       CONFIG_ATH9K_PCI_NO_EEPROM=m
                       CONFIG_ATH9K_HTC=m
                       CONFIG_ATH9K_HTC_DEBUGFS=y
                       CONFIG_ATH9K_HWRNG=y

                       CONFIG_CARL9170=m
                       CONFIG_CARL9170_LEDS=y
                       CONFIG_CARL9170_WPC=y
                       CONFIG_CARL9170_HWRNG=y
                       CONFIG_ATH6KL=m
                       CONFIG_ATH6KL_SDIO=m
                       CONFIG_ATH6KL_USB=m
                       CONFIG_AR5523=m
                       CONFIG_WIL6210=m
                       CONFIG_WIL6210_ISR_COR=y
                       CONFIG_WIL6210_TRACING=y
                       CONFIG_WIL6210_DEBUGFS=y
                       CONFIG_ATH10K=m
                       CONFIG_ATH10K_CE=y
                       CONFIG_ATH10K_PCI=m
                       CONFIG_ATH10K_SDIO=m
                       CONFIG_ATH10K_USB=m
                       CONFIG_ATH10K_DEBUGFS=y

                       CONFIG_ATH10K_TRACING=y
                       CONFIG_WCN36XX=m

                       CONFIG_ATH11K=m
                       CONFIG_ATH11K_PCI=m

                       CONFIG_WLAN_VENDOR_ATMEL=y
                       CONFIG_ATMEL=m
                       CONFIG_PCI_ATMEL=m
                       CONFIG_PCMCIA_ATMEL=m
                       CONFIG_AT76C50X_USB=m
                       CONFIG_WLAN_VENDOR_BROADCOM=y
                       CONFIG_B43=m
                       CONFIG_B43_BCMA=y
                       CONFIG_B43_SSB=y
                       CONFIG_B43_BUSES_BCMA_AND_SSB=y
                       CONFIG_B43_PCI_AUTOSELECT=y
                       CONFIG_B43_PCICORE_AUTOSELECT=y
                       CONFIG_B43_BCMA_PIO=y
                       CONFIG_B43_PIO=y
                       CONFIG_B43_PHY_G=y
                       CONFIG_B43_PHY_N=y
                       CONFIG_B43_PHY_LP=y
                       CONFIG_B43_PHY_HT=y
                       CONFIG_B43_LEDS=y
                       CONFIG_B43_HWRNG=y
                       CONFIG_B43LEGACY=m
                       CONFIG_B43LEGACY_PCI_AUTOSELECT=y
                       CONFIG_B43LEGACY_PCICORE_AUTOSELECT=y
                       CONFIG_B43LEGACY_LEDS=y
                       CONFIG_B43LEGACY_HWRNG=y
                       CONFIG_B43LEGACY_DMA=y
                       CONFIG_B43LEGACY_PIO=y
                       CONFIG_B43LEGACY_DMA_AND_PIO_MODE=y
                       CONFIG_BRCMUTIL=m
                       CONFIG_BRCMSMAC=m
                       CONFIG_BRCMFMAC=m
                       CONFIG_BRCMFMAC_PROTO_BCDC=y
                       CONFIG_BRCMFMAC_PROTO_MSGBUF=y
                       CONFIG_BRCMFMAC_SDIO=y
                       CONFIG_BRCMFMAC_USB=y
                       CONFIG_BRCMFMAC_PCIE=y
                       CONFIG_BRCM_TRACING=y
                       CONFIG_WLAN_VENDOR_CISCO=y
                       CONFIG_AIRO=m
                       CONFIG_AIRO_CS=m
                       CONFIG_WLAN_VENDOR_INTEL=y
                       CONFIG_IPW2100=m
                       CONFIG_IPW2100_MONITOR=y
                       CONFIG_IPW2200=m
                       CONFIG_IPW2200_MONITOR=y
                       CONFIG_IPW2200_RADIOTAP=y
                       CONFIG_IPW2200_PROMISCUOUS=y
                       CONFIG_IPW2200_QOS=y
                       CONFIG_LIBIPW=m
                       CONFIG_IWLEGACY=m
                       CONFIG_IWL4965=m
                       CONFIG_IWL3945=m

                       CONFIG_IWLEGACY_DEBUGFS=y

                       CONFIG_IWLWIFI=m
                       CONFIG_IWLWIFI_LEDS=y
                       CONFIG_IWLDVM=m
                       CONFIG_IWLMVM=m
                       CONFIG_IWLWIFI_OPMODE_MODULAR=y

                       CONFIG_IWLWIFI_DEBUGFS=y
                       CONFIG_IWLWIFI_DEVICE_TRACING=y

                       CONFIG_WLAN_VENDOR_INTERSIL=y
                       CONFIG_HOSTAP=m
                       CONFIG_HOSTAP_FIRMWARE=y
                       CONFIG_HOSTAP_FIRMWARE_NVRAM=y
                       CONFIG_HOSTAP_PLX=m
                       CONFIG_HOSTAP_PCI=m
                       CONFIG_HOSTAP_CS=m
                       CONFIG_HERMES=m
                       CONFIG_HERMES_CACHE_FW_ON_INIT=y
                       CONFIG_PLX_HERMES=m
                       CONFIG_TMD_HERMES=m
                       CONFIG_NORTEL_HERMES=m
                       CONFIG_PCMCIA_HERMES=m
                       CONFIG_PCMCIA_SPECTRUM=m
                       CONFIG_ORINOCO_USB=m
                       CONFIG_P54_COMMON=m
                       CONFIG_P54_USB=m
                       CONFIG_P54_PCI=m
                       CONFIG_P54_SPI=m
                       CONFIG_P54_LEDS=y
                       CONFIG_WLAN_VENDOR_MARVELL=y
                       CONFIG_LIBERTAS=m
                       CONFIG_LIBERTAS_USB=m
                       CONFIG_LIBERTAS_CS=m
                       CONFIG_LIBERTAS_SDIO=m
                       CONFIG_LIBERTAS_SPI=m
                       CONFIG_LIBERTAS_MESH=y
                       CONFIG_LIBERTAS_THINFIRM=m
                       CONFIG_LIBERTAS_THINFIRM_USB=m
                       CONFIG_MWIFIEX=m
                       CONFIG_MWIFIEX_SDIO=m
                       CONFIG_MWIFIEX_PCIE=m
                       CONFIG_MWIFIEX_USB=m
                       CONFIG_MWL8K=m
                       CONFIG_WLAN_VENDOR_MEDIATEK=y
                       CONFIG_MT7601U=m
                       CONFIG_WLAN_VENDOR_MICROCHIP=y
                       CONFIG_WILC1000=m
                       CONFIG_WILC1000_SDIO=m
                       CONFIG_WILC1000_SPI=m
                       CONFIG_WLAN_VENDOR_RALINK=y
                       CONFIG_RT2X00=m
                       CONFIG_RT2400PCI=m
                       CONFIG_RT2500PCI=m
                       CONFIG_RT61PCI=m
                       CONFIG_RT2800PCI=m
                       CONFIG_RT2800PCI_RT33XX=y
                       CONFIG_RT2800PCI_RT35XX=y
                       CONFIG_RT2800PCI_RT53XX=y
                       CONFIG_RT2800PCI_RT3290=y
                       CONFIG_RT2500USB=m
                       CONFIG_RT73USB=m
                       CONFIG_RT2800USB=m
                       CONFIG_RT2800USB_RT33XX=y
                       CONFIG_RT2800USB_RT35XX=y
                       CONFIG_RT2800USB_RT3573=y
                       CONFIG_RT2800USB_RT53XX=y
                       CONFIG_RT2800USB_RT55XX=y
                       CONFIG_RT2800USB_UNKNOWN=y
                       CONFIG_RT2800_LIB=m
                       CONFIG_RT2800_LIB_MMIO=m
                       CONFIG_RT2X00_LIB_MMIO=m
                       CONFIG_RT2X00_LIB_PCI=m
                       CONFIG_RT2X00_LIB_USB=m
                       CONFIG_RT2X00_LIB=m
                       CONFIG_RT2X00_LIB_FIRMWARE=y
                       CONFIG_RT2X00_LIB_CRYPTO=y
                       CONFIG_RT2X00_LIB_LEDS=y
                       CONFIG_WLAN_VENDOR_REALTEK=y
                       CONFIG_RTL8180=m
                       CONFIG_RTL8187=m
                       CONFIG_RTL8187_LEDS=y
                       CONFIG_RTL_CARDS=m
                       CONFIG_RTL8192CE=m
                       CONFIG_RTL8192SE=m
                       CONFIG_RTL8192DE=m
                       CONFIG_RTL8723AE=m
                       CONFIG_RTL8723BE=m
                       CONFIG_RTL8188EE=m
                       CONFIG_RTL8192EE=m
                       CONFIG_RTL8821AE=m
                       CONFIG_RTL8192CU=m
                       CONFIG_RTLWIFI=m
                       CONFIG_RTLWIFI_PCI=m
                       CONFIG_RTLWIFI_USB=m
                       CONFIG_RTL8192C_COMMON=m
                       CONFIG_RTL8723_COMMON=m
                       CONFIG_RTLBTCOEXIST=m
                       CONFIG_RTL8XXXU=m
                       CONFIG_RTL8XXXU_UNTESTED=y
                       CONFIG_RTW88=m
                       CONFIG_RTW88_CORE=m
                       CONFIG_RTW88_PCI=m
                       CONFIG_RTW88_8822B=m
                       CONFIG_RTW88_8822C=m
                       CONFIG_RTW88_8723D=m
                       CONFIG_RTW88_8821C=m
                       CONFIG_RTW88_8822BE=m
                       CONFIG_RTW88_8822CE=m
                       CONFIG_RTW88_8723DE=m
                       CONFIG_RTW88_8821CE=m
                       CONFIG_WLAN_VENDOR_RSI=y
                       CONFIG_RSI_91X=m
                       CONFIG_RSI_SDIO=m
                       CONFIG_RSI_USB=m
                       CONFIG_RSI_COEX=y
                       CONFIG_WLAN_VENDOR_ST=y
                       CONFIG_CW1200=m
                       CONFIG_CW1200_WLAN_SDIO=m
                       CONFIG_CW1200_WLAN_SPI=m
                       CONFIG_WLAN_VENDOR_TI=y
                       CONFIG_WL1251=m
                       CONFIG_WL1251_SPI=m
                       CONFIG_WL1251_SDIO=m
                       CONFIG_WL12XX=m
                       CONFIG_WL18XX=m
                       CONFIG_WLCORE=m
                       CONFIG_WLCORE_SDIO=m
                       CONFIG_WILINK_PLATFORM_DATA=y
                       CONFIG_WLAN_VENDOR_ZYDAS=y
                       CONFIG_USB_ZD1201=m
                       CONFIG_ZD1211RW=m
                       CONFIG_WLAN_VENDOR_QUANTENNA=y
                       CONFIG_QTNFMAC=m
                       CONFIG_QTNFMAC_PCIE=m
                       CONFIG_PCMCIA_RAYCS=m
                       CONFIG_PCMCIA_WL3501=m
                       CONFIG_MAC80211_HWSIM=m
                       CONFIG_USB_NET_RNDIS_WLAN=m
                       CONFIG_VIRT_WIFI=m
                       CONFIG_WAN=y
                       CONFIG_LANMEDIA=m
                       CONFIG_HDLC=m
                       CONFIG_HDLC_RAW=m
                       CONFIG_HDLC_RAW_ETH=m
                       CONFIG_HDLC_CISCO=m
                       CONFIG_HDLC_FR=m
                       CONFIG_HDLC_PPP=m
                       CONFIG_HDLC_X25=m
                       CONFIG_PCI200SYN=m
                       CONFIG_WANXL=m
                       CONFIG_PC300TOO=m
                       CONFIG_FARSYNC=m
                       CONFIG_LAPBETHER=m
                       CONFIG_IEEE802154_DRIVERS=m
                       CONFIG_IEEE802154_FAKELB=m
                       CONFIG_IEEE802154_AT86RF230=m
                       CONFIG_IEEE802154_AT86RF230_DEBUGFS=y
                       CONFIG_IEEE802154_MRF24J40=m
                       CONFIG_IEEE802154_CC2520=m
                       CONFIG_IEEE802154_ATUSB=m
                       CONFIG_IEEE802154_ADF7242=m
                       CONFIG_IEEE802154_CA8210=m
                       CONFIG_IEEE802154_MCR20A=m

                       CONFIG_WWAN=y
                       CONFIG_MHI_WWAN_CTRL=m
                       CONFIG_MHI_WWAN_MBIM=m
                       CONFIG_RPMSG_WWAN_CTRL=m
                       CONFIG_IOSM=m

                       CONFIG_XEN_NETDEV_FRONTEND=y
                       CONFIG_XEN_NETDEV_BACKEND=m
                       CONFIG_VMXNET3=m
                       CONFIG_FUJITSU_ES=m
                       CONFIG_USB4_NET=m
                       CONFIG_HYPERV_NET=m
                       CONFIG_NETDEVSIM=m
                       CONFIG_NET_FAILOVER=y
                       CONFIG_ISDN=y
                       CONFIG_ISDN_CAPI=y
                       CONFIG_CAPI_TRACE=y
                       CONFIG_ISDN_CAPI_MIDDLEWARE=y
                       CONFIG_MISDN=m
                       CONFIG_MISDN_DSP=m
                       CONFIG_MISDN_L1OIP=m

                       CONFIG_MISDN_HFCPCI=m
                       CONFIG_MISDN_HFCMULTI=m
                       CONFIG_MISDN_HFCUSB=m
                       CONFIG_MISDN_AVMFRITZ=m
                       CONFIG_MISDN_SPEEDFAX=m
                       CONFIG_MISDN_INFINEON=m
                       CONFIG_MISDN_W6692=m
                       CONFIG_MISDN_NETJET=m
                       CONFIG_MISDN_HDLC=m
                       CONFIG_MISDN_IPAC=m
                       CONFIG_MISDN_ISAR=m

                       CONFIG_INPUT=y
                       CONFIG_INPUT_LEDS=m
                       CONFIG_INPUT_FF_MEMLESS=m
                       CONFIG_INPUT_SPARSEKMAP=m
                       CONFIG_INPUT_MATRIXKMAP=m

                       CONFIG_INPUT_MOUSEDEV=y
                       CONFIG_INPUT_MOUSEDEV_PSAUX=y
                       CONFIG_INPUT_MOUSEDEV_SCREEN_X=1024
                       CONFIG_INPUT_MOUSEDEV_SCREEN_Y=768
                       CONFIG_INPUT_JOYDEV=m
                       CONFIG_INPUT_EVDEV=y

                       CONFIG_INPUT_KEYBOARD=y
                       CONFIG_KEYBOARD_ADC=m
                       CONFIG_KEYBOARD_ADP5520=m
                       CONFIG_KEYBOARD_ADP5588=m
                       CONFIG_KEYBOARD_ADP5589=m
                       CONFIG_KEYBOARD_APPLESPI=m
                       CONFIG_KEYBOARD_ATKBD=y
                       CONFIG_KEYBOARD_QT1050=m
                       CONFIG_KEYBOARD_QT1070=m
                       CONFIG_KEYBOARD_QT2160=m
                       CONFIG_KEYBOARD_DLINK_DIR685=m
                       CONFIG_KEYBOARD_LKKBD=m
                       CONFIG_KEYBOARD_GPIO=m
                       CONFIG_KEYBOARD_GPIO_POLLED=m
                       CONFIG_KEYBOARD_TCA6416=m
                       CONFIG_KEYBOARD_TCA8418=m
                       CONFIG_KEYBOARD_MATRIX=m
                       CONFIG_KEYBOARD_LM8323=m
                       CONFIG_KEYBOARD_LM8333=m
                       CONFIG_KEYBOARD_MAX7359=m
                       CONFIG_KEYBOARD_MCS=m
                       CONFIG_KEYBOARD_MPR121=m
                       CONFIG_KEYBOARD_NEWTON=m
                       CONFIG_KEYBOARD_OPENCORES=m
                       CONFIG_KEYBOARD_SAMSUNG=m
                       CONFIG_KEYBOARD_STOWAWAY=m
                       CONFIG_KEYBOARD_SUNKBD=m
                       CONFIG_KEYBOARD_IQS62X=m
                       CONFIG_KEYBOARD_TM2_TOUCHKEY=m
                       CONFIG_KEYBOARD_TWL4030=m
                       CONFIG_KEYBOARD_XTKBD=m
                       CONFIG_KEYBOARD_CROS_EC=m
                       CONFIG_KEYBOARD_MTK_PMIC=m
                       CONFIG_INPUT_MOUSE=y
                       CONFIG_MOUSE_PS2=m
                       CONFIG_MOUSE_PS2_ALPS=y
                       CONFIG_MOUSE_PS2_BYD=y
                       CONFIG_MOUSE_PS2_LOGIPS2PP=y
                       CONFIG_MOUSE_PS2_SYNAPTICS=y
                       CONFIG_MOUSE_PS2_SYNAPTICS_SMBUS=y
                       CONFIG_MOUSE_PS2_CYPRESS=y
                       CONFIG_MOUSE_PS2_LIFEBOOK=y
                       CONFIG_MOUSE_PS2_TRACKPOINT=y
                       CONFIG_MOUSE_PS2_ELANTECH=y
                       CONFIG_MOUSE_PS2_ELANTECH_SMBUS=y
                       CONFIG_MOUSE_PS2_SENTELIC=y
                       CONFIG_MOUSE_PS2_TOUCHKIT=y
                       CONFIG_MOUSE_PS2_FOCALTECH=y
                       CONFIG_MOUSE_PS2_VMMOUSE=y
                       CONFIG_MOUSE_PS2_SMBUS=y
                       CONFIG_MOUSE_SERIAL=m
                       CONFIG_MOUSE_APPLETOUCH=m
                       CONFIG_MOUSE_BCM5974=m
                       CONFIG_MOUSE_CYAPA=m
                       CONFIG_MOUSE_ELAN_I2C=m
                       CONFIG_MOUSE_ELAN_I2C_I2C=y
                       CONFIG_MOUSE_ELAN_I2C_SMBUS=y
                       CONFIG_MOUSE_VSXXXAA=m
                       CONFIG_MOUSE_GPIO=m
                       CONFIG_MOUSE_SYNAPTICS_I2C=m
                       CONFIG_MOUSE_SYNAPTICS_USB=m
                       CONFIG_INPUT_JOYSTICK=y
                       CONFIG_JOYSTICK_ANALOG=m
                       CONFIG_JOYSTICK_A3D=m
                       CONFIG_JOYSTICK_ADC=m
                       CONFIG_JOYSTICK_ADI=m
                       CONFIG_JOYSTICK_COBRA=m
                       CONFIG_JOYSTICK_GF2K=m
                       CONFIG_JOYSTICK_GRIP=m
                       CONFIG_JOYSTICK_GRIP_MP=m
                       CONFIG_JOYSTICK_GUILLEMOT=m
                       CONFIG_JOYSTICK_INTERACT=m
                       CONFIG_JOYSTICK_SIDEWINDER=m
                       CONFIG_JOYSTICK_TMDC=m
                       CONFIG_JOYSTICK_IFORCE=m
                       CONFIG_JOYSTICK_IFORCE_USB=m
                       CONFIG_JOYSTICK_IFORCE_232=m
                       CONFIG_JOYSTICK_WARRIOR=m
                       CONFIG_JOYSTICK_MAGELLAN=m
                       CONFIG_JOYSTICK_SPACEORB=m
                       CONFIG_JOYSTICK_SPACEBALL=m
                       CONFIG_JOYSTICK_STINGER=m
                       CONFIG_JOYSTICK_TWIDJOY=m
                       CONFIG_JOYSTICK_ZHENHUA=m
                       CONFIG_JOYSTICK_DB9=m
                       CONFIG_JOYSTICK_GAMECON=m
                       CONFIG_JOYSTICK_TURBOGRAFX=m
                       CONFIG_JOYSTICK_AS5011=m
                       CONFIG_JOYSTICK_JOYDUMP=m
                       CONFIG_JOYSTICK_XPAD=m
                       CONFIG_JOYSTICK_XPAD_FF=y
                       CONFIG_JOYSTICK_XPAD_LEDS=y
                       CONFIG_JOYSTICK_WALKERA0701=m
                       CONFIG_JOYSTICK_PSXPAD_SPI=m
                       CONFIG_JOYSTICK_PXRC=m
                       CONFIG_JOYSTICK_QWIIC=m
                       CONFIG_JOYSTICK_FSIA6B=m
                       CONFIG_INPUT_TABLET=y
                       CONFIG_TABLET_USB_ACECAD=m
                       CONFIG_TABLET_USB_AIPTEK=m
                       CONFIG_TABLET_USB_HANWANG=m
                       CONFIG_TABLET_USB_KBTAB=m
                       CONFIG_TABLET_USB_PEGASUS=m
                       CONFIG_TABLET_SERIAL_WACOM4=m
                       CONFIG_INPUT_TOUCHSCREEN=y
                       CONFIG_TOUCHSCREEN_88PM860X=m
                       CONFIG_TOUCHSCREEN_ADS7846=m
                       CONFIG_TOUCHSCREEN_AD7877=m
                       CONFIG_TOUCHSCREEN_AD7879=m
                       CONFIG_TOUCHSCREEN_AD7879_I2C=m
                       CONFIG_TOUCHSCREEN_AD7879_SPI=m

                       CONFIG_TOUCHSCREEN_ADC=m
                       CONFIG_TOUCHSCREEN_ATMEL_MXT=m))
                #:source
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/starfive-tech/linux")
                        (commit "06ad134b6efeb393868e300058752ecfbf7258d1")))
                  (file-name (git-file-name "linux-visionfive2" "0"))
                  (sha256 (base32 "0awh0d74d1k1qz1sm1ianqnzla1kxian72aydi9jckwiibmp6q5l"))))))
    (package
      (inherit linux)
      (arguments
       (substitute-keyword-arguments (package-arguments linux)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'install 'rename-dtb
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dtbs (string-append out "/lib/dtbs/starfive")))
                    (with-directory-excursion dtbs
                      (rename-file "jh7110-visionfive-v2.dtb"
                                   "jh7110-starfive-visionfive-2-v1.3b.dtb")))))))))
      (inputs (modify-inputs (package-inputs linux)
                (prepend zstd))))))

(define-public linux-visionfive2-6.1-devel
  (let ((linux (customize-linux
                #:name "linux-visionfive2-devel"
                #:linux linux-libre-6.1
                #:defconfig "starfive_visionfive2_defconfig"
                #:source
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/starfive-tech/linux")
                        (commit "a77eaf219c34156e52298a6ab2357aaa4251df8b")))
                  (file-name (git-file-name "linux-visionfive2" "6.1"))
                  (sha256 (base32 "1y6jx9aix9523rgcrkzrfsm6775lh2shz1libvn2ymb0zrblx3hj")))
                #:extra-version "starfive")))
    (package
      (inherit linux)
      (name "linux-visionfive2-devel")
      (version "6.1")
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (copy-recursively #$linux #$output)
            (with-directory-excursion (string-append #$output "/lib/dtbs/starfive")
              (rename-file "jh7110-visionfive-v2.dtb"
                           "jh7110-starfive-visionfive-2-v1.3b.dtb")))))
      (native-inputs '())
      (inputs '()))))

(define-public linux-visionfive2-upstream
  (customize-linux
   ;; JH7110_VisionFive2_upstream branch
   #:name "linux-visionfive2-upstream"
   #:linux linux-libre-riscv64-generic
   #:defconfig "starfive_visionfive2_defconfig"
   #:source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/starfive-tech/linux")
           (commit "9fe004eaf1aa5b23bd5d03b4cfe9c3858bd884c4")))
     (file-name (git-file-name "linux-visionfive2" "6.5"))
     (sha256 (base32 "0lxb045myz47935qar2pbjrmqdsfkafk0vpw0zabv5v0x2b4v5c0")))
   #:configs '("# CONFIG_MODULE_COMPRESS_ZSTD is not set"
               "CONFIG_SOC_STARFIVE=y"
               ;; "CONIFG_PINCTRL_STARFIVE_JH7110_SYS=y"
               "CONFIG_SERIAL_8250_DW=y"
               "CONFIG_MMC_DW_STARFIVE=y"
               "CONFIG_DWMAC_STARFIVE=y"

               "CONFIG_STARFIVE_JH7110_TIMER=y"
               "CONFIG_CLK_STARFIVE_JH7110_AON=y"
               "CONFIG_CLK_STARFIVE_JH7110_STG=y"
               "CONFIG_CLK_STARFIVE_JH7110_ISP=y"
               "CONFIG_CLK_STARFIVE_JH7110_VOUT=y"
               ;; "CONIFG_CLK_STARFIVE_JH7110_SYS=y"
               )
   #:extra-version "starfive"))

(define-public linux-visionfive2
  (let  ((pkg ((@@ (gnu packages linux) make-linux-libre*)
               "6.6"
               ""
               (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/starfive-tech/linux")
                       (commit "7ccbe4604c1498e880cc63c7e8b45e3c55914d6d")))
                 (file-name (git-file-name "linux-visionfive2" "6.6"))
                 (sha256 (base32 "1v211qwr3xa08ps5kdx3v0wg0cklyji69m9bdyazfzlqfngfvvl8")))
               '("riscv64-linux")
               #:configuration-file (lambda _ (local-file "aux/config"))
               #:extra-version "visionfive2"
               #:extra-options
               (append
                `(("CONFIG_PHY_STARFIVE_JH7110_USB" . #t)
                  ("CONFIG_PHY_STARFIVE_JH7110_PCIE" . #t)
                  ("CONFIG_HW_RANDOM_JH7110" . #t)
                  ("PINCTRL_STARFIVE_JH7110_SYS" . #t)
                  ("CONFIG_SERIAL_8250_DW" . #t)
                  ("CONFIG_PHY_STARFIVE_JH7110_DPHY_RX" . #t))
                (@@ (gnu packages linux) %default-extra-linux-options))
               )))
    (package (inherit pkg)
             (name "linux-visionfive2")
             (native-inputs
              (modify-inputs (package-native-inputs pkg)
                (append python-minimal-wrapper)))
             (inputs
              (modify-inputs (package-inputs pkg)
                (append zlib cpio)))))
  ;; (customize-linux
  ;;  ;; JH7110_VisionFive2_upstream branch
  ;;  #:name "linux-libre-visionfive2"
  ;;  #:linux linux-libre-riscv64-generic
  ;;  #:configs (list "CONFIG_PHY_STARFIVE_JH7110_USB=Y"
  ;;                  "CONFIG_PHY_STARFIVE_JH7110_DPHY_RX=y"
  ;;                  "CONFIG_PHY_STARFIVE_JH7110_PCIE=y")
  ;;  #:extra-version "visionfive2")
  )
