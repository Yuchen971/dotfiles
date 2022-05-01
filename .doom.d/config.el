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
      :n "g SPC"        #'evil-avy-goto-word-1
      ;; use 9 to move to the end of the line
      :n "-"            #'evil-end-of-line
      ;; move
      :n "C-j"          #'evil-move-5-lines-down
      :n "C-k"          #'evil-move-5-lines-up
      :n "zt"           #'evil-scroll-line-to-almost-top
      :n "zb"           #'evil-scroll-line-to-almost-bottom
      )

(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))

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
    )

(after! org
  (setq org-startup-folded 'show2levels))

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
