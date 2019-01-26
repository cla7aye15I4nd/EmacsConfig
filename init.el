;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)

(setq inhibit-startup-message t)
(setq linum-format "%3d  ")
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "YaHei Consolas Hybrid" :foundry "MS  " :slant normal :weight normal :height 120 :width normal)))))

(defun c++-extensive ()
  "-Wall: print most warning
   -Wextra: more warning
   -pedantic: Guaranteed code style: ISO c++ or ISO c
   -Wshadow: 如果局部变量和另一个局部变量重名将会警告
   -Winline: inline 失败将会警告
   -fsanitize=address: 检查数组越界
   -ftrapv: 检查整型越界"
  (linum-mode t)
  (hl-line-mode t)
  (c-toggle-hungry-state t)
  (setq c-basic-offset 2)
  (setq-default indent-tabs-mode nil)
  
  (setq file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq code-name (file-name-nondirectory buffer-file-name))
  (setq exec-name (format "%s" file-name))

  (defun set-compile-command ()
    (set (make-local-variable 'compile-command)
	 (format "g++ %s -g -Wall -Wextra -O2 -o %s && time ./%s"
		 code-name exec-name exec-name)))
  
  (defun run ()
    (interactive)
    (compile (format "time ./%s" exec-name)))
  
  (defun compile-and-run ()
    (interactive)
    (set-compile-command)
    (compile compile-command))
  
  (set-compile-command)
  
  (local-set-key (kbd "C-c C-r") 'run)
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-c") 'compile-and-run)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'c++-mode-hook 'c++-extensive)

(defun c-extensive ()
  (linum-mode t)
  (hl-line-mode t)
  (c-toggle-hungry-state t)
  (setq c-basic-offset 2)
  (setq-default indent-tabs-mode nil)
  
  (setq file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (setq code-name (file-name-nondirectory buffer-file-name))
  (setq exec-name (format "%s" file-name))

  (defun set-compile-command ()
    (set (make-local-variable 'compile-command)
	 (format "gcc %s -o %s && time ./%s"
		 code-name exec-name exec-name)))
  
  (defun run ()
    (interactive)
    (compile (format "time ./%s" exec-name)))
  
  (defun compile-and-run ()
    (interactive)
    (set-compile-command)
    (compile compile-command))
  
  (set-compile-command)
  
  (local-set-key (kbd "C-c C-r") 'run)
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-c") 'compile-and-run)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'c-mode-hook 'c-extensive)

(defun myjava()
  (linum-mode t)
  (c-toggle-hungry-state t)
  (set (make-local-variable 'compile-command)
       (format "javac %s.java && java %s"
               (file-name-sans-extension (file-name-nondirectory buffer-file-name))
	       (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'java-mode-hook 'myjava)

(add-to-list 'custom-theme-load-path "./.emacs.d/elpa/color-theme-20080305.34")
(add-to-list 'custom-theme-load-path "./.emacs.d/elpa/color-theme-solarized-20160626.743")
(load-theme 'solarized t)

(defun mypy()
  (linum-mode t)
  (set (make-local-variable 'compile-command)
       (format "time python %s.py"
               (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'python-mode-hook 'mypy)

(autoload 'nasm-mode "~/.emacs.d/nasm-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

(defun myasm()
  (linum-mode t)
  (setq file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (set (make-local-variable 'compile-command)
       (format "nasm -felf64 %s.asm -o %s.o && ld -s %s.o -o %s && time ./%s"
	       file-name file-name file-name file-name file-name))
  (local-set-key (kbd "<C-return>") 'compile))
(add-hook 'nasm-mode-hook 'myasm)
  
  

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(setq package-archives
      '(("gnu" . "https://elpa.zilongshanren.com/gnu/")
	("melpa" . "https://elpa.zilongshanren.com/melpa/")
	("melpa-stable" . "https://elpa.zilongshanren.com/melpa-stable/")))

(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-1.5.1/")

(ac-config-default)
;; (global-auto-complete-mode t)
;; (setq ac-quick-help-delay 0.05)
;; (define-key ac-mode-map  (kbd "M-/") 'auto-complete)

(load "~/.emacs.d/clang-format.el")
(global-set-key (kbd "C-c C-f") 'clang-format-region)
