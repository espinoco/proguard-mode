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

(require 'generic-x)

(defvar proguard-indent-offset 4
  "*Indentation offset for `proguard-mode'.")

(defun proguard-indent-line ()
  "Indent current line for `proguard-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
        (while t
          (backward-up-list 1)
          (when (looking-at "[({]")
            (setq indent-col (+ indent-col proguard-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[)}]") (>= indent-col proguard-indent-offset))
        (setq indent-col (- indent-col proguard-indent-offset))))
    (indent-line-to indent-col)))

(define-generic-mode
  'proguard-mode
  '("#")
  '(
     "-include"
     "-basedirectory"
     "-injars"
     "-outjars"
     "-libraryjars"
     "-skipnonpubliclibraryclasses"
     "-dontskipnonpubliclibraryclasses"
     "-dontskipnonpubliclibraryclassmembers"
     "-keepdirectories"
     "-target"
     "-forceprocessing"
     "-keep"
     "-keepclassmembers"
     "-keepclasseswithmembers"
     "-keepnames"
     "-keepclassmembernames"
     "-keepclasseswithmembernames"
     "-printseeds"
     "-dontshrink"
     "-printusage"
     "-whyareyoukeeping"
     "-dontoptimize"
     "-optimizations"
     "-optimizationpasses"
     "-assumenosideeffects"
     "-allowaccessmodification"
     "-mergeinterfacesaggressively"
     "-dontobfuscate"
     "-printmapping"
     "-applymapping"
     "-obfuscationdictionary"
     "-classobfuscationdictionary"
     "-packageobfuscationdictionary"
     "-overloadaggressively"
     "-useuniqueclassmembernames"
     "-dontusemixedcaseclassnames"
     "-keeppackagenames"
     "-flattenpackagehierarchy"
     "-repackageclasses"
     "-keepattributes"
     "-keepparameternames"
     "-renamesourcefileattribute"
     "-adaptclassstrings"
     "-adaptresourcefilenames"
     "-adaptresourcefilecontents"
     "-dontpreverify"
     "-microedition"
     "-verbose"
     "-dontnote"
     "-dontwarn"
     "-ignorewarnings"
     "-printconfiguration"
     "-dump"
     )
  '(
     ("'.*?'" . 'font-lock-string-face)
     ("!" . 'font-lock-builtin-face)
     ("\\$" . 'font-lock-builtin-face)
     ("<init>" . 'font-lock-function-name-face)
     ("<methods>" . 'font-lock-function-name-face)
     ("<fields>" . 'font-lock-function-name-face)
     ("<.*?>" . 'font-lock-builtin-face)
     ("@" . 'font-lock-builtin-face)
     ("?" . 'font-lock-builtin-face)
     ("\\**" . 'font-lock-builtin-face)
     ("%" . 'font-lock-builtin-face)
     ("\\.\\.\\." . 'font-lock-builtin-face)
     ("\\<synthetic\\>" . 'font-lock-negation-char-face)
     ("\\<bridge\\>" . 'font-lock-negation-char-face)
     ("\\<varargs\\>" . 'font-lock-negation-char-face)
     ("\\<abstract\\>" . 'font-lock-type-face)
     ("\\<synchronized\\>" . 'font-lock-type-face)
     ("\\<boolean\\>" . 'font-lock-type-face)
     ("\\<private\\>" . 'font-lock-type-face)
     ("\\<double\\>" . 'font-lock-type-face)
     ("\\<implements\\>" . 'font-lock-type-face)
     ("\\<protected\\>" . 'font-lock-type-face)
     ("\\<byte\\>" . 'font-lock-type-face)
     ("\\<import\\>" . 'font-lock-type-face)
     ("\\<public\\>" . 'font-lock-type-face)
     ("\\<enum\\>" . 'font-lock-type-face)
     ("\\<transient\\>" . 'font-lock-type-face)
     ("\\<extends\\>" . 'font-lock-type-face)
     ("\\<int\\>" . 'font-lock-type-face)
     ("\\<short\\>" . 'font-lock-type-face)
     ("\\<char\\>" . 'font-lock-type-face)
     ("\\<final\\>" . 'font-lock-type-face)
     ("\\<interface\\>" . 'font-lock-type-face)
     ("\\<static\\>" . 'font-lock-type-face)
     ("\\<void\\>" . 'font-lock-type-face)
     ("\\<class\\>" . 'font-lock-type-face)
     ("\\<long\\>" . 'font-lock-type-face)
     ("\\<volatile\\>" . 'font-lock-type-face)
     ("\\<const\\>" . 'font-lock-type-face)
     ("\\<float\\>" . 'font-lock-type-face)
     ("\\<native\\>" . 'font-lock-type-face)
     ("\\<strictfp\\>" . 'font-lock-type-face)
     ("\\<includedescriptorclasses\\>" . 'font-lock-builtin-face)
     ("\\<allowshrinking\\>" . 'font-lock-builtin-face)
     ("\\<allowoptimization\\>" . 'font-lock-builtin-face)
     ("\\<allowobfuscation\\>" . 'font-lock-builtin-face)
     ("\\<[[:upper:]]\\w*" . 'font-lock-builtin-face)
     )
  '("proguard-.*?\\.\\(txt\\|pro\\)$")
  (list
    (lambda ()
      (make-local-variable 'proguard-indent-offset)
      (set (make-local-variable 'indent-line-function) 'proguard-indent-line)
      ))
  "A mode for ProGuard files"
)

;;; proguard-mode.el ends here
