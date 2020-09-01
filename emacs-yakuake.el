(defvar yakuake-dbus-service "org.kde.yakuake")
(defvar yakuake-dbus-interface "org.kde.yakuake")
(defvar yakuake-session-dbus-path "/yakuake/sessions")
(defvar yakuake-tab-dbus-path "/yakuake/tabs")

(defun yakuake-call-session-method (method &rest args)
  (apply 'dbus-call-method :session yakuake-dbus-service yakuake-session-dbus-path yakuake-dbus-interface method args)
  )

(defun yakuake-call-tab-method (method &rest args)
  (apply 'dbus-call-method :session yakuake-dbus-service yakuake-tab-dbus-path yakuake-dbus-interface method args)
  )

(defun yakuake-add-session ()
  "add a session (tab) in yakuake"
  (yakuake-call-session-method "addSession")
  )

(defun yakuake-remove-session (session)
  "close a session (tab) in yakuake"
  (yakuake-call-session-method "removeSession" :int32 session)
  )

(defun yakuake-get-terminal-ids-in-session (session)
  "get the terminals in a specified session"
  (mapcar 'string-to-number
   (split-string
    (yakuake-call-session-method "terminalIdsForSessionId" :int32 session)
    ","))
  )

(defun yakuake-run-command-in-terminal (terminal command)
  "run command in a specified terminal"
  (yakuake-call-session-method "runCommandInTerminal" :int32 terminal command)
  )

(defun yakuake-run-command-in-session (session command)
  "run command in the first terminal in a specified session"
  (let ((terminal (car (yakuake-get-terminal-ids-in-session session))))
    (yakuake-run-command-in-terminal terminal command))
  )

(defun yakuake-set-tab-title (session title)
  "rename a yakuake tab specified by session id"
  (yakuake-call-tab-method "setTabTitle" :int32 session title)
  )
