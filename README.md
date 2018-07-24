![alt text](https://github.com/IslandOfMisfitToys/emacs/blob/master/images/emacs-tag_cover_photo.png "Emacs: The One True Editor")


# Emacs Development

My emacs initialization and code repository for DevOps work. Herein is my .emacs file I use for DevOps works. It is designed such that it can self-install and initialize itself which makes it useful for emacsen starting from scratch with DevOps work.

# Company Quick help

When used with completions provides default help if available.
The help available with terraform is ,to its credit, excellent.

```elisp
;; Company quickhelp mode!
;; https://github.com/expez/company-quickhelp
;;
(unless (package-installed-p 'company-quickhelp)
  (package-refresh-contents) (package-install 'company-quickhelp))


(company-quickhelp-mode)
;; Company quickhelp mode!

```



# Python

My preferred scripting language.
So I install flycheck to check the syntax on the fly.
autopep8 to maintain the formatting: https://github.com/hhatto/autopep8
importmagic mode to manage imports:  https://github.com/anachronic/importmagic.el

Note that autopep8 and importmagic require pip installs. Essentially the elisp
interfaces with these python programs.

```elisp
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
```


# Terraform

Configuration. Essentially a terraform mode and 
a company terraform mode to provide terraform completions.
Best used with Company quickhelp. 

```elisp
;; https://github.com/syohex/emacs-terraform-mode
;; https://github.com/rafalcieslak/emacs-company-terraform
;; 
(unless (package-installed-p 'terraform-mode)
  (package-refresh-contents) (package-install 'terraform-mode))


(unless (package-installed-p 'company-terraform)
  (package-refresh-contents) (package-install 'company-terraform))


(require 'company-terraform)
(company-terraform-init)
```

# Git

My preferred source control system. (obviously)
You must have git installed locally.

```elisp
;; magit
;; https://magit.vc/
;; 
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p") 'magit-push)
```


# Ansible

# Docker

# Lambda

# Kubernetes


