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

;; Add the MELPA package archive to the list of package repositories.

;;; Code:
(defvar package-archives nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Initialize the package system, which will load the package archives
;; and make the packages available for installation and updating.
(package-initialize)

;; Load environment variables from the shell, such as $PATH,
;; when running Emacs on a graphical display.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Load environment variables from the shell, such as $PATH,
;; when running Emacs on a Wayland display (pgtk).
(when (eq window-system 'pgtk)
  (exec-path-from-shell-initialize))

;; Load environment variables from the shell, such as $PATH,
;; when running Emacs as a daemon from systemd or similar.
;; This ensures that the daemon has access to the same environment
;; variables as the shell.
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Load the README.el file from the user's Emacs configuration directory
(load (locate-user-emacs-file "README.el"))

(provide 'init)
;;; init.el ends here
