;;; emacs-guake.el --- Manipulate guake in Emacs
;;; Version: 1.0
;;; Commentary:
;;; manipulate guake in emacs
;;; Code:

(require 'emacs-guake)
(require 'emacs-yakuake)

(defvar guake-dbus-service "org.guake3.RemoteControl")
(defvar guake-dbus-interface "org.guake3.RemoteControl")
(defvar guake-dbus-path "/org/guake3/RemoteControl")

;; ================= Basic Functions ===========================================
(defun guake-call-method (method &rest args)
  (apply 'dbus-call-method
         :session guake-dbus-service guake-dbus-path guake-dbus-interface method args)
  )

;; ================= Interface Functions =======================================
;; ----------------- Main Window Functions -------------------------------------
(defun guake-show-hide ()
  "toggle guake window visibility"
  (interactive)
  (guake-call-method "show_hide")
  )

;; ----------------- Tab Functions ----------------------------------------------
(defun guake-add-tab (&optional directory)
  "add a tab in guake"
  (guake-call-method "add_tab" directory)
  (guake-get-selected-uuid-tab)
  )

(defun guake-get-selected-uuid-tab ()
  "get the uuid of the selected tab"
  (guake-call-method "get_selected_uuidtab")
  )

;TODO: guake doesn't provide d-bus interface to close tab
(defun guake-close-tab (tab)
  "close a tab in guake"
  )

(defun guake-run-command-in-tab (tab command)
  "run command in the selected terminal in a specified tab"
  (guake-call-method "execute_command_by_uuid" tab command)
  )

(defun guake-rename-tab-uuid (tab title)
  "rename a guake tab specified by tab uuid"
  (guake-call-method "rename_tab_uuid" tab title)
  )

;; ----------------- Terminal Functions ----------------------------------------
;TODO: Because guake doesn't provide terminal with uuid, we can only manipulate current terminal
(defun guake-split-current-terminal-horizontally ()
  "split current terminal top and bottom"
  (guake-call-method "h_split_current_terminal")
  )

(defun guake-split-current-terminal-vertically ()
  "split current terminal left and right"
  (guake-call-method "v_split_current_terminal")
  )

(provide 'emacs-guake)

;;; emacs-yakuake.el ends here
