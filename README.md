# ProGuard mode

Provides Emacs font-lock and indentation for
[ProGuard](http://proguard.sourceforge.net/) configuration files.

### Installation

Add `proguard-mode.el` to your load path and put the following in your
configuration file.

``` emacs-lisp
(require 'proguard-mode)
```

### ProGuard configuration files

By default `proguard-mode` will be enabled on files `proguard-*.txt`
and `proguard-*.pro`.

To enable in files with `.pro` extension:

``` emacs-lisp
(add-to-list 'auto-mode-alist '("\\.pro$" . proguard-mode))
```
