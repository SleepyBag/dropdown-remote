;;; emacs-yakuake.el --- Manipulate yakuake in Emacs
;;; Version: 1.0
;;; Commentary:
;;; manipulate yakuake in Emacs
;;; Code:

(defvar yakuake-dbus-service "org.kde.yakuake")
(defvar yakuake-dbus-interface "org.kde.yakuake")
(defvar yakuake-session-dbus-path "/yakuake/sessions")
(defvar yakuake-tab-dbus-path "/yakuake/tabs")
(defvar yakuake-window-dbus-path "/yakuake/window")

;; ================= Basic Functions ===========================================
(defun yakuake-call-session-method (method &rest args)
  "Call d-bus METHOD about session (tab).
I don't know why yakuake split session methods and tab methods.
Optional argument ARGS is passed to the d-bus function."
  (apply 'dbus-call-method
         :session yakuake-dbus-service yakuake-session-dbus-path yakuake-dbus-interface method args)
  )

(defun yakuake-call-tab-method (method &rest args)
  "Call d-bus METHOD about tab.
Optional argument ARGS is passed to the d-bus function."
  (apply 'dbus-call-method
         :session yakuake-dbus-service yakuake-tab-dbus-path yakuake-dbus-interface method args)
  )

(defun yakuake-call-window-method (method &rest args)
  "Call d-bus METHOD about window.
Optional argument ARGS is passed to the d-bus function."
  (apply 'dbus-call-method
         :session yakuake-dbus-service yakuake-window-dbus-path yakuake-dbus-interface method args)
  )

;; ================= Interface Functions =======================================
;; ----------------- Main Window Functions -------------------------------------
(defun yakuake-toggle-window ()
  "Toggle yakuake window visibility."
  (interactive)
  (yakuake-call-window-method "toggleWindowState")
  )

;; ----------------- Session(Tab) Functions ------------------------------------
(defun yakuake-add-session ()
  "Add a session (tab) in yakuake."
  (yakuake-call-session-method "addSession")
  )

(defun yakuake-remove-session (session)
  "Close a SESSION (tab) in yakuake."
  (yakuake-call-session-method "removeSession" :int32 session)
  )

(defun yakuake-run-command-in-session (session command)
  "Run COMMAND in the first terminal in a specified SESSION."
  (let ((terminal (car (yakuake-get-terminal-ids-in-session session))))
    (yakuake-run-command-in-terminal terminal command))
  )

(defun yakuake-split-session-horizontally (session)
  "Split a SESSION top and bottom."
  (yakuake-call-session-method "splitSessionTopBottom" :int32 session)
  )

(defun yakuake-split-session-vertically (session)
  "Split a SESSION left and right."
  (yakuake-call-session-method "splitSessionLeftRight" :int32 session)
  )

(defun yakuake-set-tab-title (session title)
  "Rename a yakuake tab specified by SESSION id.
Argument TITLE is the new title."
  (yakuake-call-tab-method "setTabTitle" :int32 session title)
  )

(defun yakuake-active-session-id ()
  "Get current session id."
  (yakuake-call-session-method "activeSessionId")
  )

;; ----------------- Terminal Functions ----------------------------------------
(defun yakuake-get-terminal-ids-in-session (session)
  "Get the terminals in a specified SESSION."
  (mapcar 'string-to-number
   (split-string
    (yakuake-call-session-method "terminalIdsForSessionId" :int32 session)
    ","))
  )

(defun yakuake-run-command-in-terminal (terminal command)
  "Run COMMAND in a specified TERMINAL."
  (yakuake-call-session-method "runCommandInTerminal" :int32 terminal command)
  )

(defun yakuake-split-terminal-horizontally (session)
  "Split a terminal in the specific SESSION top and bottom."
  (yakuake-call-session-method "splitTerminalTopBottom" :int32 session)
  )

(defun yakuake-split-terminal-vertically (session)
  "Split a terminal in the specific SESSION left and right."
  (yakuake-call-session-method "splitTerminalLeftRight" :int32 session)
  )

(defun yakuake-active-terminal-id ()
  "Get current terminal id."
  (yakuake-call-session-method "activeTerminalId")
  )

(provide 'emacs-yakuake)

;;; emacs-yakuake.el ends here
