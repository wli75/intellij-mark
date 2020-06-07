(require 'ring)

;; follow global mark ring size - https://www.gnu.org/software/emacs/manual/html_node/emacs/Global-Mark-Ring.html
(defvar intellij-mark-ring-length 16 "Size of mark ring.")
(defvar intellij-mark-ring-cmds
  '(evil-mouse-drag-region
    evil-goto-line)
  "List of commands that push current cursor to the mark ring.")

(defvar intellij-mark-ring)
(defvar intellij-mark-ring-pos)

(defun set-cursor (file point)
  "Reuse/create buffer that points to file, and set cursor position to point."
  (let ((buffer (find-buffer-visiting file)))
    (when (equal buffer nil)
      (setq buffer (find-file-noselect file)))
    (let ((window (get-buffer-window buffer)))
      (when (not (equal window nil))
	(select-window window))
      (switch-to-buffer buffer)))
  (goto-char point))

(defun cursor ()
  "Return current buffer file name and point position."
  (vector (buffer-file-name) (point)))

(defun curr-buffer-is-visiting-file ()
  "Returns t if the current buffer is visiting a file, nil otherwise."
  (not (equal (buffer-file-name) nil)))

(defun prev-curr-cursors-are-the-same (prev-cursor curr-cursor)
  "Returns t if the previous and current cursors visit the same file, and are on the same line, nil otherwise."
  (let ((prev-file (aref prev-cursor 0)) (prev-pos (aref prev-cursor 1)))
    (let ((curr-file (aref curr-cursor 0)) (curr-pos (aref curr-cursor 1)))
      (and (equal prev-file curr-file)
	   (equal (line-number-at-pos prev-pos) (line-number-at-pos curr-pos))))))

(defun should-push-mark-ring (index curr-cursor)
  "Compare cursor with the element in the mark ring at index, and return t if cursor should be pushed to the mark ring, nil otherwise."
  (and (curr-buffer-is-visiting-file)
       (or (< index 0)
	   (let ((prev-cursor (ring-ref intellij-mark-ring index)))
             (not (prev-curr-cursors-are-the-same prev-cursor curr-cursor))))))

(defun push-mark-ring ()
  "Insert cursor to the mark ring at (intellij-mark-ring-pos + 1), delete elements after the inserted position, and update intellij-mark-ring-pos."
  (let ((cur (cursor)))
    (when (should-push-mark-ring intellij-mark-ring-pos cur)
      (remove-mark-ring (- intellij-mark-ring-pos 1))
      (ring-insert intellij-mark-ring cur)
      (setq intellij-mark-ring-pos 0))))

(defun remove-mark-ring (index)
  "Remove elements with position <= index in the mark ring."
  (let ((pos 0))
    (while (<= pos index)
      (ring-remove intellij-mark-ring 0)
      (setq pos (+ pos 1)))))

(defun print-intellij-mark-ring ()
  "Print contents in intellij mark ring."
  (interactive)
  (let ((pos 0))
    (while (< pos (ring-length intellij-mark-ring))
      (message "pos: ")
      (prin1 pos)
      (message "elem: ")
      (prin1 (ring-ref intellij-mark-ring pos))
      (setq pos (+ pos 1)))))
      
(defun intellij-mark-post-command-handler ()
  (when (member this-command intellij-mark-ring-cmds)
    (push-mark-ring)))

(defun intellij-mark-ring-prev ()
  "Go to the previous cursor position."
  (interactive)
  (when (and intellij-mark-mode
	     (< intellij-mark-ring-pos (- (ring-length intellij-mark-ring) 1)))
    (let ((file-point (ring-ref intellij-mark-ring (+ intellij-mark-ring-pos 1))))
      (let ((file (aref file-point 0)) (point (aref file-point 1)))
	(set-cursor file point)
	(setq intellij-mark-ring-pos (+ intellij-mark-ring-pos 1))))))
	
(defun intellij-mark-ring-next ()
  "Go to the next cursor position."
  (interactive)
  (when (and intellij-mark-mode
	     (> intellij-mark-ring-pos 0))
    (let ((file-point (ring-ref intellij-mark-ring (- intellij-mark-ring-pos 1))))
      (let ((file (aref file-point 0)) (point (aref file-point 1)))
	(set-cursor file point)
	(setq intellij-mark-ring-pos (- intellij-mark-ring-pos 1))))))
	
(define-minor-mode intellij-mark-mode
  "Intellij mark mode."
  :group 'intellij-mark
  :global t
  (if intellij-mark-mode
      (progn
        (setq intellij-mark-ring (make-ring intellij-mark-ring-length))
        (setq intellij-mark-ring-pos -1)
	(add-hook 'post-command-hook 'intellij-mark-post-command-handler))
    (remove-hook 'post-command-hook 'intellij-mark-post-command-handler)))

(global-set-key (kbd "M-b") 'intellij-mark-ring-prev)
(global-set-key (kbd "M-f") 'intellij-mark-ring-next)

(provide 'intellij-mark)

