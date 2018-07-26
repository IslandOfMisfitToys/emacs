;; Set up package repositories so M-x package-install works.
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/")
	     )

(package-initialize)

;; Auto install packages if not present
(unless (package-installed-p 'atom-one-dark-theme)
  (package-refresh-contents) (package-install 'atom-one-dark-theme))

(unless (package-installed-p 'importmagic)
  (package-refresh-contents) (package-install 'importmagic))


;; Enable the dark theme...
(load-theme 'atom-one-dark t)       ; Color theme installed via melpa

; Remove the unused tool bar which takes up space
(tool-bar-mode -1)


;; Ya, I'm an ido-mode kinda guy...
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; C-d Opens a dired buffer in the current directory. Available in Dirs / Files
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Display line numbers on the far left
(global-linum-mode t)

;; We always want company mode around in case we can have completions.
(global-company-mode t)

;; Just do it!
 (defun revert-buffer-noask ()
    "Revert buffer without confirmation."
0    (interactive) (revert-buffer t t))

(global-set-key (kbd "C-x f") 'revert-buffer-noask)


;; ------------------------------------------------------------
;; Favorite key bindings.
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C-x r") 'rename-buffer)

;; ------------------------------------------------------------
;; magit
;; https://magit.vc/
;; 
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p") 'magit-push)


;; magit
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; Company quickhelp mode!
;; https://github.com/expez/company-quickhelp
;;
(unless (package-installed-p 'company-quickhelp)
  (package-refresh-contents) (package-install 'company-quickhelp))


(company-quickhelp-mode)
;; Company quickhelp mode!
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Terraform
;; https://github.com/syohex/emacs-terraform-mode
;; https://github.com/rafalcieslak/emacs-company-terraform
;; 
(unless (package-installed-p 'terraform-mode)
  (package-refresh-contents) (package-install 'terraform-mode))


(unless (package-installed-p 'company-terraform)
  (package-refresh-contents) (package-install 'company-terraform))


(require 'company-terraform)
(company-terraform-init)



;; Terraform
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; Python stuff
(unless (package-installed-p 'ein)
  (package-refresh-contents) (package-install 'ein))

(elpy-enable)  


(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Find imports
;; Control-c l 
(add-hook 'python-mode-hook 'importmagic-mode)

;; Python stuff


;; The first part is the basic go mode operations.
;; http://arenzana.org/2015/Emacs-for-Go/
;; http://rz.scale-it.pl/2013/03/04/emacs_on_fly_syntax_checking_for_go_programming_language.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                Go Stuff                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defvar my-packages
  '(;;;; Go shit
    go-mode
    go-eldoc
    go-autocomplete

        ;;;;;; Markdown
    markdown-mode

        ;;;;;; Javascript
    json-mode
        ;;;;;; Env
    project-explorer
    smooth-scroll
    buffer-move
    window-number
	golint
	)
  "My packages!")

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
    
;;Load Go-specific language syntax
(defun go-mode-setup ()
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'go-mode-setup)

;;Format before saving
(defun go-mode-setup ()
  (go-eldoc-setup)
    (add-hook 'before-save-hook 'gofmt-before-save))
    (add-hook 'go-mode-hook 'go-mode-setup)

;;Goimports
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Godef, shows function definition when calling godef-jump
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet && golint && ./bin/errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;Project Explorer
(require 'project-explorer)
(global-set-key (kbd "M-e") 'project-explorer-toggle)



(defun my-go-mode-hook ()
  ; Godef jump key binding                                                      
  (local-set-key (kbd "C-x .") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-x s") 'go-rename)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(eval-after-load "go-mode"
  '(require 'flymake-go))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                Go Stuff                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-quickhelp company-terraform terraform-mode ido-mode yard-mode yaml-mode window-number web-mode utop use-package unicode-fonts tuareg tidy smooth-scroll scala-mode2 rvm ruby-tools ruby-refactor ruby-hash-syntax ruby-additional rubocop rspec-mode rope-read-mode robe rinari racer py-autopep8 projectile project-mode project-explorer popwin pip-requirements pg peep-dired mo-git-blame merlin markdown-toc markdown-mode+ magit-tramp magit-gh-pulls magit-find-file magit-filenotify magit-annex json-rpc json-mode importmagic imenus imenu-anywhere imenu+ idomenu gradle-mode golint go-rename go-eldoc go-autocomplete flymake-ruby flymake-python-pyflakes flymake-go flymake-d flymake flycheck-rust flycheck-gometalinter feature-mode enh-ruby-mode emacs-eclim elpy ein editorconfig-core editorconfig direx dired-narrow dired-imenu d-mode company-racer company-jedi cm-mode cider cedit buffer-move beacon atom-one-dark-theme alchemist ac-ispell ac-dcd ac-anaconda))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
