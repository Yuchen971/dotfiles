;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;                                           ,---,                               
;;         ,---,                           ,--.' |                               
;;        /_ ./|          ,--,             |  |  :                      ,---,    
;;  ,---, |  ' :        ,'_ /|             :  :  :                  ,-+-. /  |   
;; /___/ \.  : |   .--. |  | :     ,---.   :  |  |,--.    ,---.    ,--.'|'   |   
;;  .  \  \ ,' ' ,'_ /| :  . |    /     \  |  :  '   |   /     \  |   |  ,"' |   
;;   \  ;  `  ,' |  ' | |  . .   /    / '  |  |   /' :  /    /  | |   | /  | |   
;;    \  \    '  |  | ' |  | |  .    ' /   '  :  | | | .    ' / | |   | |  | |   
;;     '  \   |  :  | : ;  ; |  '   ; :__  |  |  ' | : '   ;   /| |   | |  |/    
;;      \  ;  ;  '  :  `--'   \ '   | '.'| |  :  :_:,' '   |  / | |   | |--'     
;;       :  \  \ :  ,      .-./ |   :    : |  | ,'     |   :    | |   |/         
;;        \  ' ;  `--`----'      \   \  /  `--''        \   \  /  '---'          
;;         `--`                   `----'                 `----'                 
;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Yuchen"
      user-mail-address "liyuchen971225@gmail.com")
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc). You can also try 'gd' (or 'C-c c d') to jump to their definition 
;; and see how they are implemented.

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;; Set fonts
(setq
 doom-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-unicode-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'normal)
 doom-big-font (font-spec :family "Hack Nerd Font" :size 24 :weight 'normal)
 )
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord
      display-line-numbers-type 'relative ;; nill, relative, t
      ;; org mode dir
      org-directory "~/org/"
      ;; snippets dir, note: C-x C-s -> pop up snippets
      yas-snippet-dirs (append yas-snippet-dirs '("~/.doom.d/snippets"))
      )

;; Genral key bindings
(map! :v "J"            #'drag-stuff-down
      :v "K"            #'drag-stuff-up
      :v "H"            #'drag-stuff-left
      :v "L"            #'drag-stuff-right
      ;; evil avy word search shortcut, note: g s j -> search line
      :n "g SPC"        #'evil-avy-goto-word-1
      ;; use 9 to move to the end of the line
      :n "9"            #'evil-end-of-line
      )

;; R_customize
(after! ess
      (defun then_R_operator ()
            "R - %>% operator"
            (interactive)
            (just-one-space 1)
            (insert "%>%")
            (reindent-then-newline-and-indent))
      ;; (defun R_equal_sign ()
      ;;       "R <- operator"
      ;;       (interactive)
      ;;       (just-one-space 1)
      ;;       (insert "<-")
      ;;       (reindent-then-newline-and-indent))
      (map!
      :leader
      :desc "R - %>% operator" "i p" #'then_R_operator
      ;; :desc "R <- operator" "i =" #'R_equal_sign)
))
;; R data view
;; [ess-R-data-view](https://github.com/myuhe/ess-R-data-view.el)
;;
;; Emacs everywhere
;; add this to the mac automator
;; /usr/local/bin/emacsclient --eval "(emacs-everywhere)"
