;;; init.el --- My Emacs init file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 David Dimagid
;;
;; Author: David Dimagid
;; Created: 29 Nov 2024
;; URL: https://github.com/Jpepetrueno/emacs-config/blob/main/init.el
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; See my Emacs configuration in:
;; https://github.com/Jpepetrueno/emacs-config
;;
;;; Code:

;; Add MELPA repository to Emacs package archives
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

;; Initialize the package system
(package-initialize)

;; Refresh package list in background only if it hasn't been
;; loaded yet
(unless package-archive-contents
  (package-refresh-contents t))

;; Gather statistics on package loading times
(setq use-package-compute-statistics t)

;; Load the README.el
(load (locate-user-emacs-file "README.el"))

;; Display Emacs initialization time, garbage collections, and
;; package information
(message
 "Emacs initialization took %s with %d garbage collections.
%d packages loaded."
 (emacs-init-time)
 gcs-done
 (length package-alist))

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)


;;; init.el ends here
