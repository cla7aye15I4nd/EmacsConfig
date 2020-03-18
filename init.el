;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)

(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

(setq inhibit-startup-message t)
(setq linum-format "%3d")
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 151 :width normal)))))

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
  (setq c-basic-offset 4)
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

(defun mypy()
  (linum-mode t)
  (set (make-local-variable 'compile-command)
       (format "time python %s.py"
               (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'python-mode-hook 'mypy)

(defun mysh()
  (linum-mode t)
  (set (make-local-variable 'compile-command)
       (format "bash %s.sh"
               (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'shell-script-mode-hook 'mysh)

(add-to-list 'custom-theme-load-path "./.emacs.d/elpa/color-theme-20080305.34")
(add-to-list 'custom-theme-load-path "./.emacs.d/elpa/color-theme-solarized-20160626.743")
(load-theme 'solarized t)

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

(defun javascript-extension()
  (linum-mode t)
  (c-toggle-hungry-state t)
  (set (make-local-variable 'compile-command)
       (format "node %s" (file-name-nondirectory buffer-file-name)))
  (local-set-key (kbd "<C-return>") 'compile)
  (local-set-key (kbd "C-c C-k") 'kill-compilation))
(add-hook 'javascript-mode-hook 'javascript-extension)
(add-hook 'js-mode-hook 'javascript-extension)

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(setq package-archives
      '(("gnu" . "https://elpa.zilongshanren.com/gnu/")
	;;("melpa" . "https://elpa.zilongshanren.com/melpa/")
	;;("melpa-stable" . "https://elpa.zilongshanren.com/melpa-stable/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-1.5.1/")

(ac-config-default)
;; (global-auto-complete-mode t)
;; (setq ac-quick-help-delay 0.05)
;; (define-key ac-mode-map  (kbd "M-/") 'auto-complete)

(defun oi-draw-graph (beg end region)
  "Draw a graph through graphviz
    Directed graph is default;
    To draw undirected graph, add C-u prefix"
  (interactive (list (mark) (point)
		     (prefix-numeric-value current-prefix-arg)))
  (let ((edge-expr (if current-prefix-arg " --" " ->"))
        (graph-type (if current-prefix-arg "graph" "digraph")))
    (copy-region-as-kill beg end region)
    (with-temp-file "~/graph_temp.dot"
      (yank)
      (flush-lines "^[[:space:]]*$" (point-min) (point-max) t)
      (replace-regexp "[ \t]+$" "" nil (point-min) (point-max))
      (replace-regexp "^[ \t]+" "" nil (point-min) (point-max))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (move-beginning-of-line 1)
        (insert "\"")
        (forward-word)
        (insert "\"")
        (insert edge-expr)
        (forward-word)
        (backward-word)
        (insert "\"")
        (forward-word)
        (insert "\" [label=\"")
        (move-end-of-line 1)
        (insert "\"];")
        (forward-line))
      (goto-char (point-min))
      (open-line 2)
      (insert (concat graph-type " tmp {"))
      (goto-char (point-max))
      (newline)
      (insert "}")))
  (shell-command "dot ~/graph_temp.dot -T png -o ~/graph_temp.png")
  (find-file "~/graph_temp.png"))
(global-set-key (kbd "C-c C-d") 'oi-draw-graph)

(load "~/.emacs.d/flex.el")
(load "~/.emacs.d/antlr-mode.el")
(load "~/.emacs.d/clang-format.el")

(add-to-list 'auto-mode-alist '("\\.\\(l\\)$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.\\(g4\\)$" . antlr-mode))
	     

(global-set-key (kbd "C-c C-f") 'clang-format-region)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(package-selected-packages
   (quote
    (## pinyinlib company color-theme-solarized auto-complete)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(global-set-key [(f8)] 'loop-alpha)
  
(setq alpha-list '((85 55) (100 100)))    
(defun loop-alpha ()  
  (interactive)  
  (let ((h (car alpha-list)))                  
    ((lambda (a ab)  
       (set-frame-parameter (selected-frame) 'alpha (list a ab))  
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))  
       ) (car h) (car (cdr h)))  
    (setq alpha-list (cdr (append alpha-list (list h))))  
    )  
  )

(setq backup-directory-alist (quote (("." . "/data/emacs-backups")))) ;; backup

(load "~/.emacs.d/llvm.el")
(load "~/.emacs.d/elpa/php-mode-1.21.4/php-mode-autoloads.el")
(require 'php-mode)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/php-mode-1.21.4"))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

(defun myphp()
  (linum-mode t)
  (setq file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (set (make-local-variable 'compile-command)
       (format "php %s.php" file-name))
  (local-set-key (kbd "<C-return>") 'compile))
(add-hook 'php-mode-hook 'myphp)

(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")

(defun myhaskell()
  (linum-mode t)
  (setq file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (set (make-local-variable 'compile-command)
       (format "ghc -o %s %s.hs && time ./%s" file-name, file-name, file-name))
  (local-set-key (kbd "<C-return>") 'compile))
(add-hook 'php-mode-hook 'myphp)
