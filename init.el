;;; init.el --- kiwib0y's Emacs configuration

;; Copyright 2021-present, All rights reserved
;; Code licensed under the GNU GPL v.3 license

;; Author: kiwib0y
;; URL: https://github.com/kiwib0y/emacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my current Emacs configuration
;; Keep in mind that the configuration works with GNU Emacs 27+

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(add-to-list 'load-path '"~/.emacs.d/modules")

;; fonts configuration
(defvar kw/font-sizes 110)

(defun kw/font-face ()
  "Setup all fonts to Hack font."
  (set-face-attribute 'default nil
          :font "Hack" :height kw/font-sizes)
  (set-face-attribute 'fixed-pitch nil
          :font "Hack" :height kw/font-sizes))

;; initialize package source
(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))


;; check package sources
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)
;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))
(eval-when-compile
  (require 'use-package))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; ensure always -> I'm on emacs 30 and it's a native compile
(setq use-package-always-ensure t)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(require 'kw-core)
(require 'kw-dev)
(require 'kw-dired)
(require 'kw-eshell)
(require 'kw-interface)
(require 'kw-modeline)
(require 'kw-org)
(require 'kw-prog-conf)
(require 'kw-projects)
(require 'kw-theme)

;;; init.el ends here
