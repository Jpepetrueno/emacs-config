;;; package --- init.el --- david dimagid's init.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2024 David Dimagid
;;
;; Author: David Dimagid
;; Created: 29 Nov 2024
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

;;; Code:

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize) ; Initialize the package system

(setq use-package-compute-statistics t) ; Gather statistics on package loading times

(load (locate-user-emacs-file "README.el")) ; Load the README.el

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
