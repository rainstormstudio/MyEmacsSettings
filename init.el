;;; package --- summary
;; all-the-icons
;; neotree
;; doom-themes
;; yasnippet-snippets
;; yasnippet
;; company
;; company-lsp
;; company-c-headers
;; flycheck
;; cquery
;; projectile
;; helm
;; lua-mode
;; racket-mode
;; --------------------------------------------------------------------------------
;;; Commentary:
;; Author: Hongyu Ding
;; ██████╗  █████╗ ██╗███╗   ██╗███████╗████████╗ ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗██║████╗  ██║██╔════╝╚══██╔══╝██╔═══██╗██╔══██╗████╗ ████║
;; ██████╔╝███████║██║██╔██╗ ██║███████╗   ██║   ██║   ██║██████╔╝██╔████╔██║
;; ██╔══██╗██╔══██║██║██║╚██╗██║╚════██║   ██║   ██║   ██║██╔══██╗██║╚██╔╝██║
;; ██║  ██║██║  ██║██║██║ ╚████║███████║   ██║   ╚██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝╚══════╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;;
;;              ███████╗████████╗██╗   ██╗██████╗ ██╗ ██████╗
;;              ██╔════╝╚══██╔══╝██║   ██║██╔══██╗██║██╔═══██╗
;;              ███████╗   ██║   ██║   ██║██║  ██║██║██║   ██║
;;              ╚════██║   ██║   ██║   ██║██║  ██║██║██║   ██║
;;              ███████║   ██║   ╚██████╔╝██████╔╝██║╚██████╔╝
;;              ╚══════╝   ╚═╝    ╚═════╝ ╚═════╝ ╚═╝ ╚═════╝          ANSI Shadow.
;; this init mainly focuses on c/c++ development environment
;; ================================================================================
;;; Code:
;; basic settings

;; open init file with F2
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "<f2>") 'open-my-init-file)

;; set the window size
(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 140))

(global-visual-line-mode 1) ; line wrapping
(setq make-backup-files nil) ; disable backup files
(tool-bar-mode -1) ; turn off tool-bar
(global-linum-mode 1) ; show line number
(global-hl-line-mode 1) ; highlight current line
(delete-selection-mode 1)
(show-paren-mode 1) ; show parentheses
(electric-pair-mode 1) ; automatic parentheses
(setq column-number-mode 1) ; show cursor location

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows 1
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main 1)

;; org mode color levels
(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "red")))))
(custom-theme-set-faces 'user
                        `(org-level-2 ((t (:foreground "orange")))))
(custom-theme-set-faces 'user
                        `(org-level-3 ((t (:foreground "spring green")))))
(custom-theme-set-faces 'user
                        `(org-level-4 ((t (:foreground "deep sky blue")))))
(custom-theme-set-faces 'user
                        `(org-level-5 ((t (:foreground "dark violet")))))
(custom-theme-set-faces 'user
                        `(org-level-6 ((t (:foreground "magenta")))))
(custom-theme-set-faces 'user
                        `(org-level-7 ((t (:foreground "cyan")))))
(custom-theme-set-faces 'user
                        `(org-level-8 ((t (:foreground "yellow")))))

;; show current time with formats
(display-time-mode 1)
(defvar display-time-format "%H:%M%p")

;; recent files
(recentf-mode 1)
(defvar recentf-max-menu-items 20)
(defvar recentf-max-saved-items 20)

;; ================================================================================
;; packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") 1)
(require 'cl)

(defvar rainstorm/packages '(
			     all-the-icons
			     neotree
			     doom-themes
			     yasnippet-snippets
			     yasnippet
			     company
			     company-lsp
			     company-c-headers
			     flycheck
			     cquery
			     projectile
			     helm
			     lua-mode
			     racket-mode
			     )
  "Default packages")
(defun rainstorm/packages-installed-p ()
  (loop for pkg in rainstorm/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (rainstorm/packages-installed-p)
  (message "%s" "Refreshing package database ...")
  (package-refresh-contents)
  (dolist (pkg rainstorm/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; theme
(require 'doom-themes)
(setq doom-themes-enable-bold 1)
(setq doom-themes-enable-italic 1)
(load-theme 'doom-spacegrey 1)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

(require 'all-the-icons)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 48)

(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
(setq company-idle-delay 0)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/7.4.0")

(require 'company-lsp)
(push 'company-lsp company-backends)

(require 'flycheck)
(global-flycheck-mode 1)

;; --------------------------------------------------------------------------------
(require 'cquery)
(setq cquery-executable "/home/daniel/Software/cquery/build/release/bin/cquery")
(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))
(defun cquery-root()
  (let ((root (cquery--get-root))
	(isroot nil))
    (cond (root
	   (dolist (f '("compile_commands.json" "build/compile_commands.json"))
	     (setq isroot (or isroot (file-exists-p (expand-file-name f root)))))))
    (if isroot
	root
      nil)))
(defun cquery-cache-dir (dir)
  (expand-file-name cquery-cache-dir (cquery-root)))
(setq cquery-cache-dir-function #'cquery-cache-dir)

(add-hook 'c-mode-hook #'cquery//enable)
(add-hook 'c++-mode-hook #'cquery//enable)
;;--------------------------------------------------------------------------------

(require 'projectile)
(projectile-mode 1)

(with-eval-after-load 'projectile
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring)))

;; --------------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(setq
 ;; move to end or beginning of source when reaching top or bottom of source.
 helm-move-to-line-cycle-in-source t
 ;; search for library in `require' and `declare-function' sexp.
 helm-ff-search-library-in-sexp t
 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount 8
 helm-ff-file-name-history-use-recentf t
 helm-echo-input-in-header-line t)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; TOOD: helm-semantic has not syntax coloring! How can I fix that?
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)

;; Lists all occurences of a pattern in buffer.
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)
(setq helm-autoresize-max-height 50)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

(helm-mode 1)
;; --------------------------------------------------------------------------------

(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; ================================================================================


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (flycheck company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "red"))))
 '(org-level-2 ((t (:foreground "orange"))))
 '(org-level-3 ((t (:foreground "spring green"))))
 '(org-level-4 ((t (:foreground "deep sky blue"))))
 '(org-level-5 ((t (:foreground "dark violet"))))
 '(org-level-6 ((t (:foreground "magenta"))))
 '(org-level-7 ((t (:foreground "cyan"))))
 '(org-level-8 ((t (:foreground "yellow")))))

;;; init.el ends here
