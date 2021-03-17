;;;  -*- lexical-binding: t; lisp-indent-offset: 1; indent-tabs-mode: nil -*-
;; A minor emacs mode for showing "git blame" data  in an
;; unobtrusive way. (At the end of a line like this) <|2018|11|vg|

;; Config

(defvar compact-blame-format "%Y%m%.%#")
(defvar compact-blame-separators-enabled nil)
(defvar compact-blame-bg1 "#E0FFE0")
(defvar compact-blame-bg2 "#FFFFC0")
(defvar compact-blame-light-coeff 650)

;; End of config
;; Using capitalized prefix for private functions/vars so they don't
;; clutter describe-variable etc.

(defvar compact-blame-mode nil)
;; revert-buffer erases all buffer-local vars except marked. We must
;; keep them to clean up process and overlays
(defvar-local Compact-blame-process nil)
(put 'Compact-blame-process 'permanent-local t)
(defvar-local Compact-blame-overlays nil)
(put 'Compact-blame-overlays 'permanent-local t)
(defvar-local Compact-blame-separators nil)
(defvar-local Compact-blame-file-info nil)
(defvar-local Compact-blame-total-lines 0)
(defvar-local Compact-blame-progress-percentage-str "... ")
;; Put this here to allow properties to mode line text
(put 'Compact-blame-progress-percentage-str 'risky-local-variable t)

;; (emacs-lisp-byte-compile-and-load)

(defun Compact-blame-get-light-coeff () "TODO")

(defun Compact-blame-get-bg-color (id config)
 (if (not (string-equal "rainbow" config))
  config (Compact-blame-bg-color-from-id id)))

(defun Compact-blame-bg-color-from-id (id)
 (let* ((r (string-to-number (substring id 0 2) 16))
        (g (string-to-number (substring id 2 4) 16))
        (b (string-to-number (substring id 4 6) 16))
        ;; 600 is more contrast but darker...
        (lc (max 1 (/ (- (min 765 compact-blame-light-coeff) (+ r g b)) 50)))
        (up (lambda (x) (- 255 (/ (- 255 x) lc)))))
  (apply 'format "#%02x%02x%02x" (mapcar up (list r g b)))))

(defmacro Compact-blame-defun-subst
 (name arglist docstring additional-lists form)
 "Insert variable lists into function defs. That should allow us to 
pass object contexts around or store them to variables as a single unit"
 (let ((l-e '((commit-vars time author id)
              (region-vars ov number length))))
  (setq l-e (nconc (eval additional-lists l-e) l-e))
  (list 'defun name (eval arglist l-e) docstring
   (eval form l-e))))

(define-inline Compact-blame-propertize-face (str &rest props)
 (propertize str 'face (cons :height (cons 0.85 props))))

(Compact-blame-defun-subst Compact-blame-update-overlay-local
 `(,@region-vars ,@commit-vars)
 "Print data onto the overlay and save them to file-info" '() 
 `(let* ((str compact-blame-format)
         (id (overlay-get ov 'Compact-blame-rev))
         (b (Compact-blame-get-bg-color id compact-blame-bg1))
         (b2 (Compact-blame-get-bg-color
              (substring id 6) compact-blame-bg2))
         (f "#303030") (f2 "#111111"))
   (setq length-indication
    (if (< (length length) 2) "" (concat "\x2193" length)))
   (setq str (replace-regexp-in-string "%#" length-indication str))
   (setq str (replace-regexp-in-string "%[.]" (or author "...") str))
   (setq str
    (replace-regexp-in-string "%Y" (format-time-string "%Y" time) str))
   ;;(setq str (propertize
   ;;        str 'face (apply 'list :background b :foreground f defprops)))
   (setq str
    (Compact-blame-propertize-face str :background b :foreground f))
   (setq str
    (replace-regexp-in-string "%m"
     (Compact-blame-propertize-face (format-time-string "%m" time)
      :background b2 :foreground f2 :box t) str))
   ;;(propertize (format-time-string "%m" time) 'face
   ;; (apply 'list :background b2 :foreground f2    :box t defprops)) str))
   (setq str (replace-regexp-in-string "^\s+\\|\s+$" "" str))
   (setq str
    (concat (propertize " \x25c4" 'face (list :foreground b)) str))
   (overlay-put ov 'before-string str)
   (overlay-put ov 'Compact-blame-ov-data
    (list ,@region-vars))
   (puthash id
    (list ,@commit-vars) Compact-blame-file-info)))

;;(format "---\n\n%s" (symbol-function 'Compact-blame-update-overlay-local))
;;(byte-compile 'Compact-blame-update-overlay-local)

(defun Compact-blame-make-status ()
 (set (make-local-variable 'Compact-blame-total-lines)
  (count-lines (point-min) (point-max)))
 (let ((pos (Compact-blame-find-pos (current-buffer) 1)))
  (push (make-overlay pos pos (current-buffer) t t)
   Compact-blame-overlays)
  (Compact-blame-update-status (current-buffer) t 0)))

(defun Compact-blame-get-status-ov-local ()
 (car (last Compact-blame-overlays)))

(defun Compact-blame-update-status (b show line-number)
 (with-current-buffer b
  (let ((str "Loading 'git blame' data %d/%d (%d%%)...")
        (b "#404040") (f "#FFFFFF")
        (percentage (/ (* 100 line-number) Compact-blame-total-lines)))
   (setq str (format str line-number Compact-blame-total-lines percentage))
   (overlay-put (Compact-blame-get-status-ov-local) 'after-string
    (if show
     (Compact-blame-propertize-face str :foreground f :background b)
     ""))
   (setq Compact-blame-progress-percentage-str
    (if show
     (concat (Compact-blame-propertize-face (format "%d%% blamed" percentage)
              :foreground f :background b) " ")
     ""))
   (force-mode-line-update))))

(defvar-local Compact-blame-saved-pos 0)
(defvar-local Compact-blame-saved-pos-ln 0)

(defun Compact-blame-find-pos (b n)
 (with-current-buffer b
  (save-excursion
   (goto-char 0)
   (forward-line n)
   (1- (point)))
  ;; apparently the second is equally fast...
   ;; ---
   ;;(goto-char Compact-blame-saved-pos)
   ;;(forward-line (- n Compact-blame-saved-pos-ln))
   ;;(set (make-local-variable 'Compact-blame-saved-pos-ln) n)
   ;;(1- (set (make-local-variable 'Compact-blame-saved-pos) (point))))
   ;; ---
   ;;(let ((lines (split-string (buffer-string) "\n")))
   ;; (- (length (buffer-string))
   ;;   (length (mapconcat 'identity (nthcdr n lines) " ")))
   ;; )
  ))

(defun Compact-blame-get-overlay-local (line-number id)
 (let (ov)
  (setq ov
   (if (= line-number 1)
    (Compact-blame-get-status-ov-local)
    (let* ((pos (Compact-blame-find-pos (current-buffer) line-number))
           (ov (make-overlay pos pos (current-buffer) t t)))
     (push ov Compact-blame-overlays)
     ov)))
  (overlay-put ov 'Compact-blame-rev id)
  ov))

(defun Compact-blame-get-body-ov-local (line-number id)
 (let* ((start (Compact-blame-find-start-local line-number)) ov
        (end (+ start 4)))
  ;; (setq end (min end 
  (setq ov (make-overlay start end))
  (push ov Compact-blame-separators)
  (push ov Compact-blame-overlays)
  (overlay-put ov 'face (list :overline compact-blame-separators-enabled))
  ))

(defun Compact-blame-find-start-local (line-number)
 (save-excursion
  (goto-char 0)
  (forward-line (1- line-number))
  (point)))

(defun Compact-blame-spawn-local (name &rest cmd)
 (message "Running %s" cmd)
 ;; Only in Aquamacs 3.4 
 ;; (make-process
 ;;  :command command :buffer nil
 ;;  :filter
 ;;
 ;; In case someone changed it from default value...
 (let ((default-directory (file-name-directory (buffer-file-name))))
  (set (make-local-variable name)
   (apply 'start-process (symbol-name name) nil cmd))))

(defun Compact-blame-filter-lines (process b pattern cb)
 (let ((ac "") consumed)
  (set-process-filter process
   (lambda (proc str)
    (if (not (buffer-live-p b))
     (progn
      (message "Buffer '%s' gone, killing process '%s'" b proc)
      (delete-process proc))
     (setq ac (concat ac str))
     (while (string-match pattern ac)
      (setq consumed (match-end 0))
      (funcall cb ac)
      (setq ac (substring ac consumed))))))))
                                
(defconst Compact-blame-pattern
 (let ((parts
        '("\\(?1:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\) \\(?3:[0-9]+\\)"
          "\\(?99:[0-9a-fA-F]+\\) [0-9]+ \\(?2:[0-9]+\\)"
          "author-mail <\\(?4:.+?\\)[@>].*"
          "author-time \\(?5:.+\\)"
          "\\(?99:[a-zA-Z0-9_-]+\\) .*"
          "\\(?99:[a-zA-Z0-9_-]+\\)"
          "\t\\(?99:.*\\)"
          "fatal:\\(?6:.+?\\)"
          "\\(?98:.*?\\)")))
  (format "^\\(?:%s\\)\n" (mapconcat 'identity parts "\\|"))))
  
(Compact-blame-defun-subst Compact-blame-install-output-handler ()
 "All output line processing here"
 `((call-update
    Compact-blame-update-overlay-local ,@region-vars ,@commit-vars))
 `(let* ((b (current-buffer)) n commit-data (count 0)
         ,@region-vars ,@commit-vars)
   (Compact-blame-filter-lines
    Compact-blame-process b Compact-blame-pattern
    (lambda (ac)
     ;;(message "a='%s' m=%s" (match-string 0 ac) (match-data))
     (cond
      ((setq n (match-string 1 ac))
       (setq number (string-to-number (match-string 2 ac)))
       (setq length (match-string 3 ac) id n)
       (Compact-blame-update-status b t
        (setq count (+ count (string-to-number length))))
       (with-current-buffer b
        (setq ov (Compact-blame-get-overlay-local number id))
        (Compact-blame-get-body-ov-local number id)
        (if (setq commit-data (gethash id Compact-blame-file-info))
         (progn
          (apply 'Compact-blame-update-overlay-local
           ,@region-vars commit-data))
         (setq time nil author nil)
         ,call-update)))
      ((setq n (match-string 5 ac))
       ;;(message "new-time='%s'" n)
       (setq time (seconds-to-time (string-to-number n)))
       (with-current-buffer b ,call-update))
      ;;(when (setq unimportant (match-string 101 ac))
      ;;(message "unimportant='%s'" unimportant))
      ((setq n (match-string 4 ac))
       ;;(message "new-author='%s'" n)
       (setq author n)
       (with-current-buffer b ,call-update))
      ((setq fatal (match-string 6 ac))
       (message "fatal='%s'" fatal)) 
      ((setq unparsed (match-string 98 ac))
       (message "unparsed='%s'" unparsed)))))))

;;(format "----\n\n%s" (symbol-function 'Compact-blame-install-output-handler))
;;(byte-compile 'Compact-blame-install-output-handler)

(defun Compact-blame-create-process ()
 (Compact-blame-cleanup)
 (let* ((take-off (float-time)) (b (current-buffer)))
  (setq Compact-blame-overlays nil
   Compact-blame-file-info (make-hash-table :test 'equal))
  (Compact-blame-make-status)
  (Compact-blame-spawn-local 'Compact-blame-process
   "nice" "git" "blame" "-w" "--incremental"
   ;; Full path only works if it doesn't have symlinks to
   ;; somewhere inside of the repo
   (file-name-nondirectory (buffer-file-name)))
  (Compact-blame-install-output-handler)
  (set-process-sentinel Compact-blame-process
   (lambda (process event)
    (setq event (car (split-string event)))
    (Compact-blame-update-status b nil 100)
    (message
     "event=%s time=%dms" event (* 1000 (- (float-time) take-off)))))))


(defun Compact-blame-cleanup ()
 (if Compact-blame-process (delete-process Compact-blame-process))
 (message
  "#=%d cbm=%s buf=%s" (length Compact-blame-overlays) compact-blame-mode
  (current-buffer))
 (mapc 'delete-overlay Compact-blame-overlays)
 (setq Compact-blame-overlays nil)
 )

(defun Compact-blame-show-diff (all-files)
 (require 'vc)
 (let*
  ((p (save-excursion
       (skip-chars-forward "^\n")
       (point)))
   (ovs (overlays-in p p))
   (get-id (lambda (ov)
            (list (overlay-get ov 'Compact-blame-rev))))
   (ids (delq nil (mapcan get-id ovs))))
  (cond
   ((not ovs)
    (message "No overlays at pos %d" p))
   ((not ids)
    (message "Commit id not found in %d overlays" (length ovs)))
   (t
    (message "ids=%s" ids)
    (if all-files
     (Compact-blame-show-commit (car ids))
     (vc-diff-internal t ;; async
      (list 'git (if all-files nil (list (buffer-file-name))))
      ;; TODO: fix situation with root commit
      (format "%s^" (car ids))
      (car ids)
      t ;; verbose
      )))
   )))

(defun Compact-blame-get-cs-charset (cs)
 (let (first)
  (setq first
   (lambda (charsets)
    (if charsets
     (if (eq (car charsets) 'emacs)
      (funcall first (cdr charsets))
      (car charsets))
     'utf-8)))
  (symbol-name (funcall first (coding-system-charset-list cs)))))

(defun Compact-blame-show-commit (id)
 (let* ((bn (format "*Commit %s*" id)) proc
        (cod-sys buffer-file-coding-system)
        (enc (Compact-blame-get-cs-charset cod-sys)))
  (with-current-buffer (get-buffer-create bn)
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert " ")
   (setq buffer-read-only t)
   (diff-mode)
   ;; In case the file has some non utf-8 encoding:
   ;; Tell git to encode commit message and then we decode it back
   ;; Only utf-8 commit messages are supported!
   (setq proc
    (start-process bn (current-buffer) "git" "show" "--encoding" enc id))
   (set-process-coding-system proc cod-sys)
   (goto-char 1)
   (set-marker (process-mark proc) (point-max) (current-buffer))
   (pop-to-buffer bn)
   )))

(defun compact-blame-show-diff ()
 (interactive) (Compact-blame-show-diff nil))

(defun compact-blame-show-commit () (interactive)
 (Compact-blame-show-diff t))

(defun compact-blame-toggle-separators () (interactive)
 (set (make-local-variable 'compact-blame-separators-enabled)
  (not compact-blame-separators-enabled))
 (mapc
  (lambda (ov)
   (overlay-put ov 'face
    (list :overline compact-blame-separators-enabled)))
  Compact-blame-separators
  ))

(defun compact-blame-light-up () (interactive)
 (Compact-blame-light-adjust 20))

(defun compact-blame-light-down () (interactive)
 (Compact-blame-light-adjust -20))

(defun Compact-blame-light-adjust (amount)
 (setq compact-blame-light-coeff
  (min 765 (+ compact-blame-light-coeff amount)))
 ;; Refresh
 (mapc
  (lambda (ov)
   (let* (args (id (overlay-get ov 'Compact-blame-rev))
          (commit-data (gethash id Compact-blame-file-info)))
    (when id
     (setq args (append (overlay-get ov 'Compact-blame-ov-data) commit-data))
     ;;(message "id=%s args=%s" id args)
     (apply 'Compact-blame-update-overlay-local args))))
  Compact-blame-overlays)
 (message "coeff=%s" compact-blame-light-coeff))

(defconst Compact-blame-keymap (make-sparse-keymap))
(define-key Compact-blame-keymap (kbd "RET") 'compact-blame-mode)
(define-key Compact-blame-keymap "=" 'compact-blame-show-diff)
(define-key Compact-blame-keymap "/" 'compact-blame-show-commit)
(define-key Compact-blame-keymap "s" 'compact-blame-toggle-separators)
(define-key Compact-blame-keymap "-" 'compact-blame-light-up)
(define-key Compact-blame-keymap "+" 'compact-blame-light-down)

(define-minor-mode compact-blame-mode "TODO Git blame view"
 :lighter ""
 :keymap Compact-blame-keymap
 (let* ((path (buffer-file-name)))
  (if (not (buffer-file-name))
   (message "Buffer %s is not a file" (current-buffer))
   (if compact-blame-mode
    (progn
     (set (make-local-variable 'compact-blame-saved-readonly)
      buffer-read-only)
     (setq buffer-read-only t)
     (add-to-list 'mode-line-misc-info
      '(compact-blame-mode Compact-blame-progress-percentage-str))
     (Compact-blame-create-process))
    (Compact-blame-cleanup)
    (setq buffer-read-only compact-blame-saved-readonly)
    ))))
