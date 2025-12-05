;;; dashboard.el --- Personal Dashboard -*- lexical-binding: t -*-

(require 'page-break-lines)

(defvar dashboard-workout-log-file
  "~/Library/Mobile Documents/com~apple~CloudDocs/workout-log.org"
  "Path to the workout log file.")

(defvar dashboard-buffer-name "*Dashboard*"
  "Name of the dashboard buffer.")

(defvar dashboard-banner-buffer-name "*Dashboard-Banner*"
  "Name of the dashboard banner buffer.")

(defvar dashboard-banner-height 3
  "Height of the banner window in lines.")

(defun dashboard ()
  "Open Personal Dashboard."
  (interactive)
  (delete-other-windows)
  ;; Start with sections in main window
  (let ((buf (get-buffer-create dashboard-buffer-name)))
    (switch-to-buffer buf)
    (dashboard--render)
    (dashboard-mode)
    (goto-char (point-min)))
  ;; Create banner at top (negative = lines for new window)
  (let ((banner-win (split-window nil -8 'above)))
    (with-selected-window banner-win
      (switch-to-buffer (get-buffer-create dashboard-banner-buffer-name))
      (dashboard--render-banner)
      (dashboard-mode)))
  ;; Split right for agenda
  (split-window-right)
  (other-window 1)
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda nil "a"))
  (other-window -1))

(defun dashboard--render-banner ()
  "Render the banner in the top window."
  (let* ((inhibit-read-only t)
         (text "welcome back")
         (text-width (length text)))
    (erase-buffer)
    (insert "\n")
    (insert (propertize " " 'display `(space :align-to (- center ,(/ text-width 2)))))
    (insert (propertize text 'face '(:height 1.8 :weight bold)))))

(defun dashboard--render ()
  "Render dashboard content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (my-dashboard--insert-section "1" "Guitar" "getting started" "~/roaming/notes/20240508091703-guitar_learning.org")
    (insert "\f\n")
    (my-dashboard--insert-section "2" "Fitness"
                                  (concat "Last workout: "
                                          (propertize (dashboard--get-last-workout-date)
                                                      'face '(:foreground "orange")))
                                  dashboard-workout-log-file)




    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (my-dashboard--insert-section "p" "Projects" "Finish Rec... etc")	   ;;
    ;; (my-dashboard--insert-section "a" "Agenda" "some agenda tasks or whatever") ;;
    ;; (my-dashboard--insert-section "Today" (format-time-string "%A, %B %d"))	   ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


))


(defun my-dashboard--insert-section (key title content target-file)
  "Insert section with KEY shortcut, TITLE, CONTENT, linking to TARGET-FILE."
;; Display: "Guitar (g)"
  (insert (propertize title 'face '(:height 1.2 :weight semi-bold)))
(insert (propertize (format " (%s)" key) 'face 'shadow))
  (insert "\n" content "\n\n")
  (insert "\n\n")

(local-set-key (kbd key) (lambda () (interactive)
(find-file target-file))))

(defun dashboard--get-last-workout-date ()
  "Get the last workout date from icloud workout-log.org."
  (with-temp-buffer
    (insert-file-contents dashboard-workout-log-file)
    (goto-char (point-max))
    (if (re-search-backward "^\\* \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
        (match-string 1)
      "No Workouts Found")))






(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Major mode for personal dashboard."
  (setq cursor-type nil)
  (setq buffer-read-only t)
  (page-break-lines-mode 1))



(provide 'dashboard)





