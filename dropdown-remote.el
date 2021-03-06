;;; dropdown-remote.el --- summary -*- lexical-binding: t -*-

;; Author: Xue Qianming
;; Maintainer: Xue Qianming
;; Version: 1.0
;; Package-Requires:
;; Homepage: https://github.com/SleepyBag/dropdown-remote
;; Keywords: dropdown, terminal, yakuake, guake


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;; manipulate dropdown terminals in Emacs

;;; Code:

(require 'emacs-guake)
(require 'emacs-yakuake)

(defvar dropdown-terminal 'yakuake)

;; ----------------- Main Window Functions -------------------------------------
(defun dropdown-toggle-window ()
  "Show or hide a dropdown terminal."
  (interactive)
  (case dropdown-terminal
    ('yakuake (yakuake-toggle-window))
    ('guake (guake-show-hide))
    )
  )

(defun dropdown-close-current-tab ()
  "Close current tab of dropdown terminal."
  (interactive)
  (dropdown-close-tab (dropdown-current-tab))
  )

(defun dropdown-run-command-in-current-tab (command)
  "Run COMMAND in current tab of dropdown terminal."
  (dropdown-run-command-in-tab (dropdown-current-tab) command)
  )

;; ----------------- Tab Functions ---------------------------------------------
(defun dropdown-current-tab ()
  "Get the id of current tab."
  (case dropdown-terminal
    ('yakuake (yakuake-active-session-id))
    ('guake (guake-get-selected-uuid-tab))
    )
  )

(defun dropdown-add-tab ()
  "Create a tab and get its id."
  (interactive)
  (case dropdown-terminal
    ('yakuake (yakuake-add-session))
    ('guake (guake-add-tab))
    )
  )

(defun dropdown-close-tab (tab)
  "Close a TAB by its id."
  (case dropdown-terminal
    ('yakuake (yakuake-remove-session tab))
    ('guake (message "guake doesn't support remove tab through D-Bus interface yet"))
    )
  )

(defun dropdown-run-command-in-tab (tab command)
  "Run COMMAND in a specific TAB."
  (case dropdown-terminal
    ('yakuake (yakuake-run-command-in-session tab command))
    ('guake (guake-run-command-in-tab tab command))
    )
  )

(defun dropdown-set-tab-title (tab title)
  "Rename the TITLE of the specific TAB."
  (case dropdown-terminal
    ('yakuake (yakuake-set-tab-title tab title))
    ('guake (guake-rename-tab-uuid tab title))
    )
  )

;; ----------------- Handy Functions -------------------------------------------
(defun dropdown-terminal-here ()
  "Run a dropdown terminal from current directory"
  (interactive)
  (let ((tab (dropdown-add-tab)))
    (dropdown-run-command-in-tab tab (concat "cd '" default-directory "'"))
    (dropdown-toggle-window)
    tab
    )
  )

;; ----------------- End -------------------------------------------------------

(provide 'dropdown-remote)

;;; dropdown-remote.el ends here
