![alt text](https://github.com/IslandOfMisfitToys/emacs/blob/master/images/emacs-tag_cover_photo.png "Emacs: The One True Editor")


# Emacs Development for Version 26.1

My emacs initialization and code repository for DevOps work. Herein is my .emacs file I use for DevOps works. It is designed such that it can self-install and initialize itself which makes it useful for emacsen starting from scratch with DevOps work. 

*NOTE:* Extraneous installation steps such as installing git locally or various git modules per example are beyond the scope of this document. It merely addresses the elisp portion as an example.

*NOTE:* The work on this page assumes linux. Windows and MacOS are a different story entirely. 

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

# Flymd
 Essentially flycheck for markdown with browser display.
 The snippet of elisp below should do the trick and initialize flymd
 to use Mozilla which works. 

```elisp
;; ------------------------------------------------------------
;; flymd: flycheck like markdown mode with html browser viewing.
;; https://github.com/mola-T/flymd
;; 
;; One and only one interactive function in this package.
;; M-x flymd-flyit, current markdown buffer opened in a browser.
;; If you close the page accidentally, M-x flymd-flyit to reopen the page.

(unless (package-installed-p 'flymd)
  (package-refresh-contents) (package-install 'flymd))

;; Chrome doesn't work, use firefox. 
(defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
 (setq flymd-browser-open-function 'my-flymd-browser-function)
```

# Python

My preferred scripting language.
So I install flycheck to check the syntax on the fly.

autopep8 to maintain the formatting: https://github.com/hhatto/autopep8

importmagic mode to manage imports:  https://github.com/anachronic/importmagic.el

*Note* that autopep8 and importmagic require pip installs. Essentially the elisp
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

# Go Lang
*My preferred systems programming language.*
 
This is still a work in progress. I will have more complete instructions 
later. It mostly works with the exception of gorename which has always been
an iffy proposition. 

### Installation 

The first step is to install the following two scripts into your go working area
and then execute them. The first scripts is env.sh:

```shell
#!/bin/sh
export GOPATH=`pwd`
export PATH=:$GOPATH/bin:$PATH
```

Execute the above env.sh like so:

```shell
$ source ./env.sh
```

At this point you have the proper environment variables set to your current go work area.
Now create the following install.sh script:

```shell
#!/bin/sh
set -x
go get -u golang.org/x/tools/cmd/...
go get -u github.com/rogpeppe/godef
go get -u github.com/ptrv/goflycheck
go get -u github.com/dougm/goflymake
go get -u github.com/golang/lint/golint
go get -u github.com/nsf/gocode
go get -u github.com/kisielk/errcheck
go get -u github.com/jstemmer/gotags
echo 'Done'
```

When executed this script will install all the necessary tools for the elisp below.
Once that is done you should have your tools installed beneath ./bin in your go working area. You 
will need to do this for every go project you work on. The tools can and should be isolated.

Once complete you can add the following golang elisp to your .emacs file:




```golang
;; The first part is the basic go mode operations.
;; http://arenzana.org/2015/Emacs-for-Go/
;; http://rz.scale-it.pl/2013/03/04/emacs_on_fly_syntax_checking_for_go_programming_language.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                Go Stuff                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Pop windows is useful for a split window view of the structure of a go program.
(unless (package-installed-p 'popwin)
  (package-refresh-contents) (package-install 'popwin))

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(push '("^\*go-direx:" :regexp t :position left :width 0.4 :dedicated t :stick t)
      popwin:special-display-config)

;; Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defvar my-packages
  '(;;;; Go stuff
    go-mode
    go-eldoc
    go-autocomplete
    go-rename
    go-gopath
    go-imports
    go-errcheck
    go-guru
    go-direx
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
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
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
  (local-set-key (kbd "C-x j") 'go-direx-pop-to-buffer)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;  


(eval-after-load "go-mode"
  '(require 'flymake-go))


;; Make the go guru available in all go buffers
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                Go Stuff                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


