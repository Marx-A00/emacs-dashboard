;;; dashboard.el --- Personal Dashboard -*- lexical-binding: t -*-

(defvar dashboard-buffer-name "*Dashboard*"
  "Name of the dashboard buffer.")

(defun dashboard ()
  "Open Personal Dashboard."
  (interactive)
  (let((buf (get-buffer-create dashboard-buffer-name)))
    (switch-to-buffer buf)
    (dashboard--render)
    (dashboard-mode)
    (goto-char (point-min))))

(defun dashboard--render ()
  "Render dashboard content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "welcome back\n\n"
			'face '(:height 1.8 :weight bold)))

    (my-dashboard--insert-section "Guitar" "Practice streak: 5 days")
    (my-dashboard--insert-section "Fitness" "3 workouts this week")
    (my-dashboard--insert-section "Projects" "Finish Rec... etc")
    (my-dashboard--insert-section "Agenda" "some agenda tasks or whatever")
    (my-dashboard--insert-section "Today" (format-time-string "%A, %B %d"))))


(defun my-dashboard--insert-section (key title content target-file)
  "Insert section with KEY shortcut, TITLE, CONTENT, linking to TARGET-FILE."
;; Display: "Guitar (g)"
  (insert (propertize (concat title "\n")
                      'face '(:height 1.2 :weight semi-bold)))
  (insert content)
  (insert "\n\n"))



(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Major mode for personal dashboard."
  (setq cursor-type nil)
  (setq buffer-read-only t))

(provide 'dashboard)





