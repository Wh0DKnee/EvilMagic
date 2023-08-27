;; TCP/Socket code taken from https://gist.github.com/jclosure

;;;;;;;;;;;;;;;;;;;;;;;;;; IPC Code start ;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

(defvar evil-magic-server-clients '()
  "Alist where KEY is a client process and VALUE is the string")

(defvar evil-magic-server-servers '()
  "Alist where KEY is the port number the server is listening at")

(defvar evil-magic-server-display-buffer-on-update nil
  "If non-nil, force the process buffer to be visible whenever
new text arrives")
(make-variable-buffer-local 'evil-magic-server-display-buffer-on-update)

(defun evil-magic-server-make-process-name (port)
  "Return server name of the process listening on PORT"
  (format "evil-magic-server:%d" port))

(defun evil-magic-server-get-process (port)
  "Return the server process that is listening on PORT"
  (get-process (evil-magic-server-make-process-name port)))

(defun evil-magic-server-process-buffer (port)
  "Return buffer of the server process that is listening on PORT"
  (process-contact (evil-magic-server-get-process port) :buffer))

(defun evil-magic-server-delete-clients (server-proc)
  (let ((server-proc-name (process-contact server-proc :name)))
    (cl-loop for client in evil-magic-server-clients
             if (string= server-proc-name (process-contact client :name))
             do
             (delete-process client)
             (message "Deleted client process %s" client))
    (setq evil-magic-server-clients
          (cl-delete-if (lambda (client)
                          (string= (process-contact server-proc :name)
                                   (process-contact client :name)))
                        evil-magic-server-clients))))

(cl-defun evil-magic-server-start (port &optional (display-buffer-on-update nil)
                                        (buffer-major-mode 'text-mode))
  "Start a TCP server listening at PORT"
  (interactive
   (list (read-number "Enter the port number to listen to: " 9999)))
  (let* ((proc-name (evil-magic-server-make-process-name port))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service port
                            :sentinel 'evil-magic-server-sentinel
                            :filter 'evil-magic-server-filter :server 't)
      (with-current-buffer buffer-name
        (funcall buffer-major-mode)
        (setq evil-magic-server-display-buffer-on-update display-buffer-on-update))
      (setq evil-magic-server-clients '()))
    ;; (display-buffer buffer-name)
    ))

(defun evil-magic-server-stop (port)
  "Stop an emacs TCP server at PORT"
  (interactive
   (list (read-number "Enter the port number the server is listening to: "
                      9999)))
  (let ((server-proc (evil-magic-server-get-process port)))
    (evil-magic-server-delete-clients server-proc)
    (delete-process server-proc)))

(defun evil-magic-server-append-to-proc-buffer (proc string)
  (let ((buffer (process-contact proc :buffer))
        (inhibit-read-only t))foo bar
        (and buffer (get-buffer buffer)
             (with-current-buffer buffer
               (when evil-magic-server-display-buffer-on-update
                 (display-buffer buffer))
               (let ((moving (= (point) (point-max))))
                 (save-excursion
                   (goto-char (point-max))
                   (insert string)
                   )
                 (if moving (goto-char (point-max))))))))

(defun evil-magic-server-filter (proc string)
  (evil-magic-server-eval string))

(defun evil-magic-server-eval (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun evil-magic-server-sentinel (proc msg)
  (cond
   ((string-match "open from .*\n" msg)
    (push proc evil-magic-server-clients)
    (evil-magic-server-log proc "client connected\n")
    )
   ((string= msg "connection broken by remote peer\n")
    (setq evil-magic-server-clients (cl-delete proc evil-magic-server-clients))
    (evil-magic-server-log proc "client has quit\n")
    )
   ((eq (process-status proc) 'closed)
    (evil-magic-server-delete-clients proc))))

(defun evil-magic-server-log (client string)
  "If a server buffer exists, write STRING to it for logging purposes."
  (evil-magic-server-append-to-proc-buffer client
                                           (format "%s %s: %s"
                                                   (current-time-string)
                                                   client string)))

;;;;;;;;;;;;;;;;;;;;;;;;;; IPC Code end ;;;;;;;;;;;;;;;;;;;;;;;;;;

(window-absolute-pixel-position (point) nil)

(point)

(posn-point (posn-at-x-y 395 64))

(defun evil-magic-get-monitor-resolution()
  "Get the resolution of the active monitor.
   Active means the monitor that emacs is running on.
   Return value is a list of the form (width height)."
  (interactive)
  (let ((monitors (display-monitor-attributes-list)))
                                        ; Should probably assert that result of cl-remove-if has length one.
    (let ((active-monitor (car
                           (cl-remove-if
                            (lambda (x) (null (cdr (assoc 'frames x))))
                            monitors))))
      (let ((geometry (assoc 'geometry active-monitor)))
        (let ((resolution (list (nth 3 geometry) (nth 4 geometry))))
          resolution)))))

(defun evil-magic-visible-buffer-range ()
  "Return a list (beg, end) that holds the buffer
   range that is currently visible."
  (list (window-start) (window-end)))

(provide 'evil-magic-server)
