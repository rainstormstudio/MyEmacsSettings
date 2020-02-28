;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; set the window frame size
(package-initialize)

(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 140))

(tool-bar-mode -1) ; turn off tool-bar
(global-linum-mode 1) ; show linum-mode

;; open init file function as F2
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "<f2>") 'open-my-init-file)

(global-hl-line-mode 1)

(delete-selection-mode 1)

(display-time-mode 1)
(setq display-time-format "%H:%M%p")

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(require 'cl)
;; packages
(defvar rainstorm/packages '(
			     neotree
			     iedit
			     yasnippet
			     all-the-icons 
			     monokai-pro-theme
			     doom-themes
			     hungry-delete
			     racket-mode
			     irony
			     company-c-headers
			     projectile
			     lua-mode
			     ) "Default packages")
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

(require 'auto-complete-config)
(ac-config-default)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 48)
;; (load-theme 'monokai-pro t)

(require 'all-the-icons)
(require 'doom-themes)
(setq doom-themes-enable-bold 1)
(setq doom-themes-enable-italic 1)
(load-theme 'doom-spacegrey 1)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

(require 'iedit)
(require 'yasnippet)
(yas-global-mode 1)
(require 'hungry-delete)
(global-hungry-delete-mode)
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 20)

;; emms setting
(add-to-list 'load-path "~/emms/lisp/")
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")

(show-paren-mode 1)

(setq column-number-mode 1)

(require 'company)
(add-to-list 'company-backends 'company-c-headers)

;; CEDET
(require 'cedet)
;; semantic
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; EDE
;;(require 'ede)
;;(global-ede-mode)

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)

;; auto parentheses
(electric-pair-mode 1)

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

(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/daniel/Documents/workplace/2DGameEngine")))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets neotree monokai-pro-theme hungry-delete auto-complete))))
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
