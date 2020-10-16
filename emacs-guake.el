;;; emacs-guake.el --- Manipulate guake in Emacs
;;; Version: 1.0
;;; Commentary:
;;; manipulate guake in Emacs
;;; Code:

(defvar guake-dbus-service "org.guake3.RemoteControl")
(defvar guake-dbus-interface "org.guake3.RemoteControl")
(defvar guake-dbus-path "/org/guake3/RemoteControl")

;; ================= Basic Functions ===========================================
(defun guake-call-method (method &rest args)
  "Call guake d-bus METHOD.
Optional argument ARGS is passed to the d-bus function."
  (apply 'dbus-call-method
         :session guake-dbus-service guake-dbus-path guake-dbus-interface method args)
  )

;; ================= Interface Functions =======================================
;; ----------------- Main Window Functions -------------------------------------
(defun guake-show-hide ()
  "Toggle guake window visibility."
  (interactive)
  (guake-call-method "show_hide")
  )

;; ----------------- Tab Functions ----------------------------------------------
(defun guake-add-tab (&optional directory)
  "Add a tab in guake.
Optional argument DIRECTORY is the directory in which the terminal is opened."
  (guake-call-method "add_tab" directory)
  (guake-get-selected-uuid-tab)
  )

(defun guake-get-selected-uuid-tab ()
  "Get the uuid of the selected tab."
  (guake-call-method "get_selected_uuidtab")
  )

;TODO: guake doesn't provide d-bus interface to close tab
(defun guake-close-tab (tab)
  "Close a TAB in guake."
  )

(defun guake-run-command-in-tab (tab command)
  "Run COMMAND in the selected terminal in a specified TAB."
  (guake-call-method "execute_command_by_uuid" tab command)
  )

(defun guake-rename-tab-uuid (tab title)
  "Rename a guake TAB specified by tab uuid.
Argument TITLE is the new title."
  (guake-call-method "rename_tab_uuid" tab title)
  )

;; ----------------- Terminal Functions ----------------------------------------
;TODO: Because guake doesn't provide terminal with uuid, we can only manipulate current terminal
(defun guake-split-current-terminal-horizontally ()
  "Split current terminal top and bottom."
  (guake-call-method "h_split_current_terminal")
  )

(defun guake-split-current-terminal-vertically ()
  "Split current terminal left and right."
  (guake-call-method "v_split_current_terminal")
  )

(provide 'emacs-guake)

;;; emacs-guake.el ends here
