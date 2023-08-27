;; TCP/Socket code taken from https://gist.github.com/jclosure/cb34dbd813c6bd1e3c4e128ad87d69c7

;;;;;;;;;;;;;;;;;;;;;;;;;; IPC Code start ;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

(defvar evil-magic-server-clients '()
  "Alist where KEY is a client process and VALUE is the string")

(defvar evil-magic-server-servers '()
  "Alist where KEY is the port number the server is listening at")

(defvar evil-magic-gaze-pos '(0 . 0)
  "Most recent gaze position.")

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

(cl-defun evil-magic-server-start (&optional (display-buffer-on-update nil)
                                             (buffer-major-mode 'text-mode))
  "Start a TCP server listening at PORT"
  (interactive)
  (let* ((proc-name (evil-magic-server-make-process-name 9999))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service 9999
                            :sentinel 'evil-magic-server-sentinel
                            :filter 'evil-magic-server-filter :server 't)
      (with-current-buffer buffer-name
        (funcall buffer-major-mode)
        (setq evil-magic-server-display-buffer-on-update display-buffer-on-update))
      (setq evil-magic-server-clients '()))
    ;; (display-buffer buffer-name)
    ))

(defun evil-magic-server-stop ()
  "Stop an emacs TCP server at PORT 9999"
  (interactive)
  (let ((server-proc (evil-magic-server-get-process 9999)))
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
  (evil-magic-server-store-gaze string))

(defun evil-magic-server-store-gaze (str)
  (let ((expr (read (format "(%s)" str))))
    (setq evil-magic-gaze-pos (cons (car expr) (cadr expr)))
    (message "%s" evil-magic-gaze-pos)
    ))

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


(defun evil-magic-get-monitor-resolution()
  "Get the resolution of the active monitor.
   Active means the monitor that emacs is running on.
   Return value is a list of the form (width height)."
  (interactive) ; Should probably assert that result of cl-remove-if has length one.
  (let ((monitors (display-monitor-attributes-list)))
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

(defun evil-magic-search-matches (str)
  "Returns a list of the buffer positions at which str is found."
  (let* ((text (buffer-substring-no-properties (window-start) (window-end)))
         (matches nil)
         (start 0)
         (end (length text)))
    (while (not (null start))
      (let ((found (cl-search str (substring text start end))))
        (if (not (null found))
            (progn
                                        ; found + start to convert to pos in substring to pos in entire string
                                        ; +1 because we want to start the next search after the current match
              (setq start (+ found start 1))
              (push start matches) ; because buffer indices are 1 based, we can use the incremented start
              )
          (setq start nil)))
      )
    matches))

(defun evil-magic-matches-pixel-positions (str)
  "Returns a list of pixel positions (in the form (x . y)) where the search str was found
   in the current buffer."
  (mapcar 'window-absolute-pixel-position (evil-magic-search-matches str)))

(defun evil-magic-pixel-distance (pos1 pos2)
  "Calculate the distance between two pixel positions."
  (let ((dx (- (car pos2) (car pos1)))
        (dy (- (cdr pos2) (cdr pos1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun evil-magic-pixel-gaze-distance (pixel-pos)
  "Returns the distance between the pixel position and the gaze position."
  (evil-magic-pixel-distance pixel-pos evil-magic-gaze-pos))

(defun evil-magic-pixel-comparator (pos1 pos2)
  "Returns true if pos1 is closer to the gaze position than pos2."
  (< (evil-magic-pixel-gaze-distance pos1) (evil-magic-pixel-gaze-distance pos2)))

(defun evil-magic-closest-match-to-gaze (str)
  "Returns the pixel position of the match of str in the current buffer
   that is closest to the gaze position."
  (let ((match-positions (evil-magic-matches-pixel-positions str)))
    (apply #'min match-positions :predicate #'evil-magic-pixel-comparator)))

(posn-point (posn-at-x-y 395 64))

(provide 'evil-magic-server)
