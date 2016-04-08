;;; proguard-mode.el --- ProGuard mode

;; Copyright (C) 2015 Carlo Eduardo Rodríguez Espino

;; Author: Carlo Eduardo Rodríguez Espino <carloeduardorodriguez@gmail.com>
;; Keywords: ProGuard
;; Version: 1.0.0
;; Package-Requires: ((emacs "24"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The purpose of this package is to provide a mode for proguard
;;; configuration files.

;;; Code:

(defvar proguard-mode-hook nil)

(defvar proguard-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for ProGuard major mode.")

(define-derived-mode proguard-mode java-mode "ProGuard"
  "Mode for editing ProGuard configuration files."
  (font-lock-add-keywords
    nil
    '(
       ("-\\<include\\>" . 'font-lock-keyword-face)
       ("-\\<basedirectory\\>" . 'font-lock-keyword-face)
       ("-\\<injars\\>" . 'font-lock-keyword-face)
       ("-\\<outjars\\>" . 'font-lock-keyword-face)
       ("-\\<libraryjars\\>" . 'font-lock-keyword-face)
       ("-\\<skipnonpubliclibraryclasses\\>" . 'font-lock-keyword-face)
       ("-\\<dontskipnonpubliclibraryclasses\\>" . 'font-lock-keyword-face)
       ("-\\<dontskipnonpubliclibraryclassmembers\\>" . 'font-lock-keyword-face)
       ("-\\<keepdirectories\\>" . 'font-lock-keyword-face)
       ("-\\<target\\>" . 'font-lock-keyword-face)
       ("-\\<forceprocessing\\>" . 'font-lock-keyword-face)
       ("-\\<keep\\>" . 'font-lock-keyword-face)
       ("-\\<keepclassmembers\\>" . 'font-lock-keyword-face)
       ("-\\<keepclasseswithmembers\\>" . 'font-lock-keyword-face)
       ("-\\<keepnames\\>" . 'font-lock-keyword-face)
       ("-\\<keepclassmembernames\\>" . 'font-lock-keyword-face)
       ("-\\<keepclasseswithmembernames\\>" . 'font-lock-keyword-face)
       ("-\\<printseeds\\>" . 'font-lock-keyword-face)
       ("-\\<dontshrink\\>" . 'font-lock-keyword-face)
       ("-\\<printusage\\>" . 'font-lock-keyword-face)
       ("-\\<whyareyoukeeping\\>" . 'font-lock-keyword-face)
       ("-\\<dontoptimize\\>" . 'font-lock-keyword-face)
       ("-\\<optimizations\\>" . 'font-lock-keyword-face)
       ("-\\<optimizationpasses\\>" . 'font-lock-keyword-face)
       ("-\\<assumenosideeffects\\>" . 'font-lock-keyword-face)
       ("-\\<allowaccessmodification\\>" . 'font-lock-keyword-face)
       ("-\\<mergeinterfacesaggressively\\>" . 'font-lock-keyword-face)
       ("-\\<dontobfuscate\\>" . 'font-lock-keyword-face)
       ("-\\<printmapping\\>" . 'font-lock-keyword-face)
       ("-\\<applymapping\\>" . 'font-lock-keyword-face)
       ("-\\<obfuscationdictionary\\>" . 'font-lock-keyword-face)
       ("-\\<classobfuscationdictionary\\>" . 'font-lock-keyword-face)
       ("-\\<packageobfuscationdictionary\\>" . 'font-lock-keyword-face)
       ("-\\<overloadaggressively\\>" . 'font-lock-keyword-face)
       ("-\\<useuniqueclassmembernames\\>" . 'font-lock-keyword-face)
       ("-\\<dontusemixedcaseclassnames\\>" . 'font-lock-keyword-face)
       ("-\\<keeppackagenames\\>" . 'font-lock-keyword-face)
       ("-\\<flattenpackagehierarchy\\>" . 'font-lock-keyword-face)
       ("-\\<repackageclasses\\>" . 'font-lock-keyword-face)
       ("-\\<keepattributes\\>" . 'font-lock-keyword-face)
       ("-\\<keepparameternames\\>" . 'font-lock-keyword-face)
       ("-\\<renamesourcefileattribute\\>" . 'font-lock-keyword-face)
       ("-\\<adaptclassstrings\\>" . 'font-lock-keyword-face)
       ("-\\<adaptresourcefilenames\\>" . 'font-lock-keyword-face)
       ("-\\<adaptresourcefilecontents\\>" . 'font-lock-keyword-face)
       ("-\\<dontpreverify\\>" . 'font-lock-keyword-face)
       ("-\\<microedition\\>" . 'font-lock-keyword-face)
       ("-\\<verbose\\>" . 'font-lock-keyword-face)
       ("-\\<dontnote\\>" . 'font-lock-keyword-face)
       ("-\\<dontwarn\\>" . 'font-lock-keyword-face)
       ("-\\<ignorewarnings\\>" . 'font-lock-keyword-face)
       ("-\\<printconfiguration\\>" . 'font-lock-keyword-face)
       ("-\\<dump\\>" . 'font-lock-keyword-face)
       ("!" . 'font-lock-builtin-face)
       ("\\$" . 'font-lock-builtin-face)
       ("<.*?>" . 'font-lock-type-face)
       ("@.*?\\." . 'font-lock-builtin-face)
       ("\\?" . 'font-lock-builtin-face)
       ("\\**" . 'font-lock-builtin-face)
       ("\\%" . 'font-lock-builtin-face)
       ("\\.\\.\\." . 'font-lock-builtin-face)
       ("\\<includedescriptorclasses\\>" . 'font-lock-builtin-face)
       ("\\<allowshrinking\\>" . 'font-lock-builtin-face)
       ("\\<allowoptimization\\>" . 'font-lock-builtin-face)
       ("\\<allowobfuscation\\>" . 'font-lock-builtin-face)
       ))

  (setq comment-start "#")
  (setq comment-end "")

  (modify-syntax-entry ?# "< b" proguard-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" proguard-mode-syntax-table)

  (use-local-map proguard-mode-map)

  (run-hooks 'proguard-mode-hook)
  )

(add-to-list 'auto-mode-alist '("proguard-.*?\\.\\(txt\\|pro\\)$" . proguard-mode))

(provide 'proguard-mode)

;;; proguard-mode.el ends here
