;; paimon-search-results.el --- Search results mode -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Search results mode

;;; Code:

(require 'aio)
(require 'ht)
(require 'paimon-search-job)
(require 'paimon-search-result)
(require 'paimon-search-results-layout)
(require 'tabulated-list)

(defvar paimon-search-results-buffer-name
  "*paimon-search-results*"
  "The name of the search result buffer.")

(defvar-local paimon-search-results-job nil
  "The search results job.")

(put 'paimon-search-results-job 'permanent-local t)

(defvar-local paimon-search-results-layout-selected nil
  "The selected layout of the search results buffer.")

(defun paimon-search-results-list-entries (job layout)
  "Return a function that return the tabulated list entries for JOB in LAYOUT."
  (lambda ()
    (let ((entry-fn (paimon-search-results-entry-fn job layout)))
      (seq-map (lambda (result)
                 (funcall entry-fn result))
               (paimon-search-results-by-job job)))))

(defun paimon-search-results-show (job)
  "Show the results of the search JOB."
  (interactive (list (paimon-search-job-under-point)))
  (with-slots (id search) job
    (let ((buffer (get-buffer-create (paimon-search-job-buffer-name job))))
      (with-current-buffer buffer
        (setq-local paimon-search-results-job job)
        (paimon-search-results-mode)
        (let ((split-width-threshold nil))
          (switch-to-buffer-other-window buffer))))))

(defun paimon-search-results--render-under-point ()
  "Render the search result under point when `paimon-search-results-buffer-name` is visible."
  (when (get-buffer-window paimon-search-result-buffer-name)
    (when-let (result (paimon-search-result-under-point))
      (paimon-search-result-show result))))

(defun paimon-search-results-next-line (&optional n)
  "Move N lines forward (backward if N is negative) and show the search result under point."
  (interactive "P")
  (forward-line n)
  (paimon-search-results--render-under-point))

(defun paimon-search-results-previous-line (&optional n)
  "Move cursor vertically up N lines and show the search result under point."
  (interactive "P")
  (forward-line (- (or n 1)))
  (paimon-search-results--render-under-point))

(defun paimon-search-results--load-p (job results offset limit)
  "Return t if the search RESULTS of JOB should be loaded, otherwise nil."
  (< (+ offset (length results))
     (min (+ offset limit)
          (paimon-search-job-event-count job))))

(defun paimon-search-results-summary (job &optional offset limit)
  "Show the pagination summary of the search JOB using OFFSET and LIMIT."
  (let ((limit (or limit paimon-search-results-limit))
        (offset (or offset paimon-search-results-offset))
        (event-count (paimon-search-job-event-count job)))
    (message "Showing search result %s - %s of %s in total."
             (paimon--bold (+ 1 offset))
             (paimon--bold (min event-count (+ offset limit)))
             (paimon--bold (paimon-search-job-event-count job)))))

(aio-defun paimon-search-results-view (job &optional offset limit)
  "Show the search results for JOB at OFFSET using LIMIT."
  (interactive (list paimon-search-results-job))
  (when job
    (revert-buffer)
    (paimon-search-results-summary job)
    (let ((results (paimon-search-results-by-job job offset limit)))
      (when (paimon-search-results--load-p job results offset limit)
        (aio-await (paimon-search-job-load-results-preview job offset limit))
        (revert-buffer)
        (paimon-search-results-summary job)))))

(defun paimon-search-results-post-command-hook ()
  "Called after each command to trigger pagination when necessary."
  ;; TODO: At end of buffer. Implement auto loading.
  ;; (when (> (line-number-at-pos (point)) (- (line-number-at-pos (point-max)) 10)))
  )

(defun paimon-search-results-next (job)
  "Show the next results of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (let ((offset (+ paimon-search-results-offset paimon-search-results-limit)))
      (when (< offset (paimon-search-job-event-count job))
        (setq-local paimon-search-results-offset offset)
        (paimon-search-results-view job offset paimon-search-results-limit)))))

(defun paimon-search-results-previous (job)
  "Show the previous results of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (let ((offset (- paimon-search-results-offset paimon-search-results-limit)))
      (when (>= offset 0)
        (setq-local paimon-search-results-offset offset)
        (paimon-search-results-view job offset paimon-search-results-limit)))))

(defun paimon-search-results--setup-list (job layout)
  "Setup the search results for JOB using LAYOUT."
  (setq paimon-search-results-layout-selected layout)
  (setq tabulated-list-entries (paimon-search-results-list-entries job layout))
  (setq tabulated-list-format (paimon-search-results-layout-format job layout))
  (setq tabulated-list-sort-key (paimon-search-results-layout-sort-key job layout))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun paimon-search-results-switch-layout (job)
  "Switch the search results layout of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (when-let (layout (paimon-search-results-layout-completing-read job))
      (paimon-search-results--setup-list job layout))))

(defvar paimon-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "L") 'paimon-search-results-switch-layout)
    (define-key map (kbd "N") 'paimon-search-results-next)
    (define-key map (kbd "P") 'paimon-search-results-previous)
    (define-key map (kbd "RET") 'paimon-search-result-show)
    (define-key map (kbd "c") 'paimon-search)
    (define-key map (kbd "l") 'paimon-search-jobs-list)
    (define-key map (kbd "n") 'paimon-search-results-next-line)
    (define-key map (kbd "p") 'paimon-search-results-previous-line)
    (define-key map (kbd "w") 'paimon-profiles-list)
    map)
  "The key map for the `paimon-search-results-mode'.")

(define-derived-mode paimon-search-results-mode tabulated-list-mode "Search Results"
  "Special mode for search results buffers."
  (let* ((job paimon-search-results-job)
         (layout (or paimon-search-results-layout-selected (paimon-search-results-layout-find job))))
    (cl-check-type job paimon-search-job)
    (setq major-mode 'paimon-search-results-mode)
    (setq mode-name "Search Results")
    (use-local-map paimon-search-results-mode-map)
    (hl-line-mode 1)
    (paimon-search-results--setup-list job layout)
    (add-hook 'post-command-hook 'paimon-search-results-post-command-hook nil t)
    (run-mode-hooks 'paimon-search-results-mode-hook)))

(provide 'paimon-search-results)

;;; paimon-search-results.el ends here
