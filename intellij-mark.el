(require 'ring)

;; follow global mark ring size - https://www.gnu.org/software/emacs/manual/html_node/emacs/Global-Mark-Ring.html
(defvar intellij-mark-ring-length 16 "Size of mark ring")
(defvar intellij-mark-ring-cmds
  '(evil-mouse-drag-region
    evil-goto-line)
  "List of commands that push current cursor to the mark ring")
(defvar intellij-mark-ring-exclude-modes
  [neotree-mode
   help-mode]
  "Vector of modes that should not support intellij-mark")

(defvar intellij-mark-ring)
(defvar intellij-mark-ring-pos)

(defun cursor ()
  (vector (selected-window) (point)))

(defun intellij-remove-mark-ring (index)
  "Remove elements with position <= index in the mark ring"
  (let ((pos 0))
    (while (<= pos index)
      (ring-remove intellij-mark-ring 0)
      (setq pos (+ pos 1)))))

(defun intellij-should-exclude-mode (modes)
  (let ((pos 0) (should-exclude nil))
    (while (< pos (length intellij-mark-ring-exclude-modes))
      (when (derived-mode-p (aref intellij-mark-ring-exclude-modes pos))
	(setq should-exclude t))
      (setq pos (+ pos 1)))
    should-exclude))

(defun intellij-should-push-mark-ring (index curr-cursor)
  "Compare cursor with the element in the mark ring at index, and return true if cursor should be pushed to the mark ring, false otherwise."
  (message "intellij-should-exclude-mode intellij-mark-ring-exclude-modes:")
  (prin1 (intellij-should-exclude-mode intellij-mark-ring-exclude-modes))
  (and (not (intellij-should-exclude-mode intellij-mark-ring-exclude-modes))
       (or (< index 0)
	   (let ((prev-cursor (ring-ref intellij-mark-ring index)))
	     (let ((prev-win (aref prev-cursor 0)) (prev-pos (aref prev-cursor 1)))
	       (let ((curr-win (aref curr-cursor 0)) (curr-pos (aref curr-cursor 1)))
		 (or (not (equal (line-number-at-pos curr-pos) (line-number-at-pos prev-pos)))
		     (not (equal curr-win prev-win)))))))))

(defun print-intellij-mark-ring ()
  (interactive)
  (let ((pos 0))
    (while (< pos (ring-length intellij-mark-ring))
      (message "pos: ")
      (prin1 pos)
      (message "elem: ")
      (prin1 (ring-ref intellij-mark-ring pos))
      (setq pos (+ pos 1)))))

(defun intellij-push-mark-ring ()
  "Insert cursor to the mark ring at (intellij-mark-ring-pos + 1), delete elements after the inserted position, and update intellij-mark-ring-pos."
  (interactive)
;;  (message "in intellij-push-mark-ring")
;;  (message "intellij-mark-ring-pos:")
;;  (prin1 intellij-mark-ring-pos)
  (let ((cur (cursor)))
    (when (intellij-should-push-mark-ring intellij-mark-ring-pos cur)
      (intellij-remove-mark-ring (- intellij-mark-ring-pos 1))
      (ring-insert intellij-mark-ring cur)
      (setq intellij-mark-ring-pos 0)
;;      (message "intellij-mark-ring:")
;;      (print-intellij-mark-ring)
;;      (message "intellij-mark-ring-pos:")
;;      (prin1 intellij-mark-ring-pos))))
)))
      
(defun intellij-mark-post-command-handler ()
  (interactive)
;;  (message "in intellij-mark-post-command-handler")
;;  (message "this-command:")
;;  (prin1 this-command)
  (when (member this-command intellij-mark-ring-cmds)
    (intellij-push-mark-ring)))

(defun intellij-set-window-point (win pt)
  (select-window win)
  (set-window-point win pt))

(defun intellij-mark-ring-prev ()
  "Go to the previous cursor position."
  (interactive)
  (message "in intellij-mark-ring-prev")
  (when (and intellij-mark-mode
	     (< intellij-mark-ring-pos (- (ring-length intellij-mark-ring) 1)))
    (let ((win_pt (ring-ref intellij-mark-ring (+ intellij-mark-ring-pos 1))))
      (let ((win (aref win_pt 0)) (pt (aref win_pt 1)))
	(intellij-set-window-point win pt)
	(setq intellij-mark-ring-pos (+ intellij-mark-ring-pos 1))
;;	(message "intellij-mark-ring-pos:")
;;	(prin1 intellij-mark-ring-pos)))))
))))
	
(defun intellij-mark-ring-next ()
  "Go to the next cursor position."
  (interactive)
;;  (message "in intellij-mark-ring-next")
  (when (and intellij-mark-mode
	     (> intellij-mark-ring-pos 0))
    (let ((win_pt (ring-ref intellij-mark-ring (- intellij-mark-ring-pos 1))))
      (let ((win (aref win_pt 0)) (pt (aref win_pt 1)))
	(intellij-set-window-point win pt)
	(setq intellij-mark-ring-pos (- intellij-mark-ring-pos 1))
;;	(message "intellij-mark-ring-pos:")
;;	(prin1 intellij-mark-ring-pos)))))
))))
	
(define-minor-mode intellij-mark-mode
  "Intellij mark mode."
  :group 'intellij-mark
  :global t
  (if intellij-mark-mode
      (progn
;;	(message "in intellij-mark-mode")
        (setq intellij-mark-ring (make-ring intellij-mark-ring-length))
        (setq intellij-mark-ring-pos -1)
	(add-hook 'post-command-hook 'intellij-mark-post-command-handler)
	(intellij-push-mark-ring))
    (remove-hook 'post-command-hook 'intellij-mark-post-command-handler)))

(global-set-key (kbd "M-b") 'intellij-mark-ring-prev)
(global-set-key (kbd "M-f") 'intellij-mark-ring-next)

(provide 'intellij-mark)

