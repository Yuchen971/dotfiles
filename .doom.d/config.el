;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Yuchen"
      user-mail-address "liyuchen971225@gmail.com")

(setq
 doom-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-unicode-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-big-font (font-spec :family "Hack Nerd Font" :size 24 :weight 'normal)
 )

(setq doom-theme 'doom-dracula)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq display-line-numbers-type nil)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
       ))

(setq-default
 delete-by-moving-to-trash t)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(remove-hook 'text-mode-hook #'spell-fu-mode)

(evil-define-command evil-scroll-line-to-almost-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter (+ 3 (max 1 scroll-margin)))))

(evil-define-command evil-scroll-line-to-almost-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter (- (+ 3 (max 1 scroll-margin))))))

(evil-define-motion evil-move-5-lines-down ()
  (evil-next-visual-line 5))

(evil-define-motion evil-move-5-lines-up ()
  (evil-previous-visual-line 5))

(map! :v "J"            #'drag-stuff-down
      :v "K"            #'drag-stuff-up
      :v "H"            #'drag-stuff-left
      :v "L"            #'drag-stuff-right
      ;; evil avy word search shortcut, note: g s j -> search line
      :n "g SPC"        #'evil-avy-goto-char-2
      ;; use 9 to move to the end of the line
      :n "-"            #'evil-end-of-line
      ;; move
      :n "C-j"          #'evil-move-5-lines-down
      :n "C-k"          #'evil-move-5-lines-up
      :n "zt"           #'evil-scroll-line-to-almost-top
      :n "zb"           #'evil-scroll-line-to-almost-bottom
      )

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "cyan")
      evil-visual-state-cursor '(hollow "orange"))

(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq
   ess-style 'RStudio
   ess-offset-continued 2
   ess-expression-offset 0
   ess-use-flymake nil)

(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:constants . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op% . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))

(map! :leader
      :desc "Org babel tangle" "m E" #'org-babel-tangle)

(setq org-directory "~/Documents/Org"
    org-hide-emphasis-markers t ;; hide markup indicators
    org-ellipsis "…"
    org-pretty-entities t
    )

(after! org
  (setq org-startup-folded 'show2levels))

(add-hook 'org-mode-hook #'org-modern-mode)

;; ;; levels font hight
;; (custom-set-faces
;;   '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
;;   '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
;;   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
;;   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
;;   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;; )

(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.doom.d/snippets")))

;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-posframe-display-at-point)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

;; (use-package! vertico-posframe
;;   :after 'vertico
;;   :config (vertico-posframe-mode 1))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 2)                            ; It's nice to maintain a little margin

(use-package! vundo
  :custom
  (vundo-glyph-alist     vundo-unicode-symbols)
  (vundo-compact-display t)
  :config
  (evil-set-initial-state 'vundo-mode 'motion)
  (add-hook! vundo-mode #'evil-normalize-keymaps)
  (map! :map vundo-mode-map
        :m "h" #'vundo-backward
        :m "l" #'vundo-forward
        :m "j" #'vundo-next
        :m "k" #'vundo-previous
        :m "H" #'vundo-stem-root
        :m "L" #'vundo-stem-end
        :m "q" #'vundo-quit
        :m "C-g" #'vundo-quit
        :m "RET" #'vundo-confirm)
  :defer t)

(map! :leader
      :desc "Visual Undo Tree" "U" #'vundo)

(after! company
  (setq company-idle-delay 0.5 ;; delay time
        company-minimum-prefix-length 2) ;; start with 2 letters
  )

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

;; (let ((alternatives '("doom-emacs-color.png"
;;                       "doom-emacs-colo2r.png"
;;                       "doom-emacs-slant-out-bw.png"
;;                       )))
;;   (setq fancy-splash-image
;;         (concat doom-private-dir "img/"
;;                 (nth (random (length alternatives)) alternatives))))

(defvar fancy-splash-image-template
  (expand-file-name "misc/img/emacs-e-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)

(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))


(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))
