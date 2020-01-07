;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; LOADING ;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'tramp)
(require 'popup) ; for magit
(require 'magit)
(require 'mml2015) ; notmuch
(require 'asm-mode)
(require 'rfcview)
(require 'auto-complete-config)
(require 'auto-complete)
(require 'notmuch)
(require 'uniquify)
(require 'elpy)

(elpy-enable)

;; MODE ALIST ;;
(add-to-list 'auto-mode-alist '("\\.S$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode)) ; cython
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . python-mode)) ; cython
(add-to-list 'auto-mode-alist '(".bash_aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("rfc" . rfcview-mode))

;; AC
(ac-config-default)
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)
(ac-flyspell-workaround)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-special-ctrl-a t)
(setq org-special-ctrl-e t)

;; haskell mode requires you to pick your poison
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; shells
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun new-shell () (interactive)
       (shell (generate-new-buffer-name "*shell*")))
(setq explicit-shell-file-name "/usr/bin/zsh")

;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (hs-minor-mode)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))
(setq c-default-style "linux" c-basic-offset 4) ;; ~OTBS
(add-hook 'c++-mode-hook
          (lambda () (setq comment-start "/* " comment-end " */")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 78)

;; use ctags, not etags
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name) "Create tags file." (interactive "DDirectory: ")
  (shell-command (format "ctags -f %s -e -R %s" path-to-ctags
                         (directory-file-name dir-name))))

;; TeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq TeX-PDF-mode t)
(setq TeX-view-program-list '(("zathura" "zathura %o")))
(setq TeX-view-program-selection '((output-pdf "zathura")))
(setq TeX-output-view-style (quote
                             (("^pdf$" "." "zathura %o")
                              ("^html?$" "." "firefox %o")
                              )))

(defun yank-to-x-clipboard () (interactive)
  (if (region-active-p)
      (progn (shell-command-on-region (region-beginning)
                                      (region-end)
                                      "xsel -i -b")
             (message "Yanked region to clipboard!")
             (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))
(global-set-key "\M-W" 'yank-to-x-clipboard)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              (list
               ".bak"
               ".CKP"
               ".aux"
               ".otl"
               ".err"
               ".lib"
               ".dvi"
               ".PS"
               ".o"
               ".pdf"
               ".log"
               )))

;; various important things
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq require-final-newline t)
(setq info-mode-hook 'visual-mode)
(setq line-move-visual 'nil)
(setq track-eol 1)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
(global-font-lock-mode 1)
(show-paren-mode 1)
(setq display-time-24hr-format 1)
(column-number-mode)
(setq visible-cursor nil)
(menu-bar-mode 0)
(setq scroll-step 1)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq uniquify-buffer-name-style 'post-forward)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))
(defun font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 "Green" t))))

;; notmuch
(setq notmuch-hello-thousands-separator ",")
(setq notmuch-crypto-process-mime t)
;; (setq mml2015-sign-with-sender t)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(setq message-send-mail-partially-limit 10000000)
(setq notmuch-multipart/alternative-discouraged ; fucking gmail
      '("text/html" "multipart/related"))
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq mail-host-address "redhat.com")
(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
        (let* ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ;; I use email address as account label in ~/.msmtprc
                 ((string-match "rharwood@redhat.com" from)
                  "rharwood@redhat.com")
                 ((string-match "rharwood@club.cc.cmu.edu" from)
                  "rharwood@club.cc.cmu.edu")
                 ((string-match "rharwood@bu.edu" from)
                  "rharwood@bu.edu")
                 )))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)
(setq message-mode-hook 'flyspell-mode)
(define-key notmuch-show-mode-map " "
  (lambda () "restore old space behavior" (interactive)
    (notmuch-show-next-thread t)))
(define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
(define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)
(define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map "R"
  'notmuch-search-reply-to-thread-sender)
(define-key notmuch-show-mode-map "d" 'epa-decrypt-region)
(setq notmuch-show-tag-macro-alist
      (list '("d" "-unread")
            '("k" "+muted")))
(setq notmuch-search-tag-macro-alist
      (list '("d" "-unread")
            '("k" "+muted")))
(define-key notmuch-search-mode-map "d"
  (lambda (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("-unread") beg end)
    (notmuch-search-next-thread)))
(define-key notmuch-search-mode-map "k"
  (lambda (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+muted") beg end)
    (notmuch-search-tag '("-unread") beg end)
    (notmuch-search-tag '("-inbox") beg end)
    (notmuch-search-next-thread)))
(define-key notmuch-search-mode-map "a"
  (lambda (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("-unread") beg end)
    (notmuch-search-tag '("-inbox") beg end)
    (notmuch-search-next-thread)))

;; I don't use mlterm, but if you do...
(defun terminal-init-mlterm ()
  (load "term/xterm")
  (xterm-register-default-colors xterm-standard-colors)
  (tty-set-up-initial-frame-faces))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-active-region nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-autodoc elpy-module-sane-defaults)))
 '(enable-local-variables t)
 '(enable-remote-dir-locals t)
 '(flyspell-default-dictionary "american")
 '(haskell-indent-offset 2)
 '(haskell-indentation-starter-offset 2)
 '(meson-indent-basic 4)
 '(mm-text-html-renderer (quote w3m-standalone))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox and not tag:rss")
     (:name "rss" :query "tag:inbox and tag:rss")
     (:name "unread" :query "tag:unread and not tag:maybe_spam")
     (:name "maybe_spam" :query "tag:maybe_spam"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-search-result-format
   (quote
    (("date" . "%12s ")
     ("count" . "%-7s ")
     ("authors" . "%-20s ")
     ("tags" . "[%s] ")
     ("subject" . "%s "))))
 '(notmuch-show-insert-text/plain-hook
   (quote
    (notmuch-wash-convert-inline-patch-to-part notmuch-wash-wrap-long-lines notmuch-wash-tidy-citations notmuch-wash-elide-blank-lines notmuch-wash-excerpt-citations)))
 '(nxml-child-indent 4)
 '(pydb-many-windows t)
 '(ruby-indent-level 4)
 '(safe-local-variable-values (quote ((meson-indent-basic . 2))))
 '(vc-follow-symlinks nil)
 '(xref-prompt-for-identifier t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "color-52"))))
 '(rfcview-headlink-face ((t (:foreground "brightblue" :weight light)))))
