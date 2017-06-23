;;; config-general-mode.el --- Config::General config file mode

;; Copyright (C) 2016-2017, T.v.Dein <tlinden@cpan.org>

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.01
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: config file editing
;; URL: https://github.com/tlinden/config-general-mode
;; License: GNU General Public License >= 2

;;; Commentary:

;;;; Introduction

;; [Config::General](http://search.cpan.org/dist/Config-General/) is a
;; Perl   module  for   parsing  config   files  with   some  enhanced
;; features. `config-general-mode' makes it easier to edit such config
;; files with emacs.

;; It is based on `conf-mode' with the following features:

;; - good syntax highlighting for config files
;; - completion support with `<tab>' (using `dabbrev')
;; - imenu support for <blocks>
;; - electric paring mode (for quotes, parens, etc) enabled
;; - automatic indenting
;; - jump to include file with `<ret>'

;;;; Installation

;; To use, save config-general-mode.el to a directory in your load-path.

;; Add something like this to your config:

;;     (require 'config-general-mode)
;;     (add-to-list 'auto-mode-alist '("\\.conf$" . config-general-mode))

;; or load it manually, when needed:

;;     M-x config-general-mode
    
;; You can also enable it with  a buffer-local variable by adding this as
;; the first line of a config file:

;;     # -*-config-general-*-

;;;; Usage

;; Edit  your config  file as  usual.  Use `<tab>'  for completion  of
;; values and variables.  Use `C-c C-t' to toggle flags  (like true to
;; false). Use `C-c C-=' on a region to automatically align on the `=`
;; character. Use `C-c  C-/' to breakup a region with  long lines into
;; shorter ones  using backslash notation.  Use  `<C-return>' to visit
;; an included file  or (when not on  a link) insert a  new line below
;; the current one, indent and move point there.

;;;; Customize

;; You can customize the mode with:

;;      M-x customize-group RET config-general-mode RET

;; You can also use hooks to config-general  mode as a way to modify or enhance
;; its behavior.  The following hooks are available:

;;     config-general-mode-hook
    
;; For example:

;;     (add-hook 'config-general-mode-hook 'electric-indent-mode)




;;; Code:

;;;; Dependencies

(require 'sh-script)
(require 'cc-mode)

;;;; Customizables

;; our group
(defgroup config-general nil
  "Config::General config file mode."
  :prefix "config-general-"
  :group 'conf)

(defgroup config-general-faces nil
  "Config::General config file mode faces."
  :prefix "config-general-"
  :group 'faces)

;; vars
(defcustom config-general-electric-return t
  "Enable electric return and follow include files."
  :group 'config-general
  :type 'boolean)

(defcustom config-general-toggle-values
  '(("true"    . "false")
    ("false"   . "true")
    ("on"      . "off")
    ("off"     . "on")
    ("yes"     . "no")
    ("no"      . "yes")
    ("enable"  . "disable")
    ("disable" . "enable"))
  "Values which can be toggled, for simplicity's sake, add both values."
  :group 'config-general
  :type 'list)

;; faces
(defface config-general-file-face
   '((t (:inherit link)))
  "face for include files"
  :group 'config-general-faces)

(defface config-general-constant-face
  '((t (:inherit font-lock-constant-face)))
  "face for include files"
  :group 'config-general-faces)

(defface config-general-special-char-face
  '((t (:inherit font-lock-regexp-grouping-backslash)))
  "face for special characters like < or |"
  :group 'config-general-faces)

(defface config-general-keyword-face
  '((t (:inherit font-lock-keyword-face))) ;; maybe type?
  "face for special keywords like include"
  :group 'config-general-faces)

(defface config-general-blockname-face
  '((t (:inherit font-lock-function-name-face)))
  "face for block names"
  :group 'config-general-faces)

(defface config-general-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "face for variable name definitions"
  :group 'config-general-faces)

(defface config-general-interpolating-variable-face
  '((t (:inherit font-lock-constant-face)))
  "face for variable name definitions"
  :group 'config-general-faces)

(defface config-general-escape-char-face
  '((t (:inherit font-lock-warning-face)))
  "face for escape chars"
  :group 'config-general-faces)


;;;; Global Vars
(defconst config-general-mode-version "0.01" "Config::General mode version")

(defvar config-general-font-lock-keywords nil
  "Keywords to highlight in CG mode.")

(defvar config-general-mode-abbrev-table nil)

(defvar config-general-imenu-expression
  '(
    ("Blocks"  "^ *<\\([a-zA-Z0-9]+.*\\)>" 1 ))
  "Imenu generic expression for Config:General mode.  See `imenu-generic-expression'.")

;;;; Public Functions

(defun config-general-reload()
  "Simple mode reloader"
  (interactive)
  (fundamental-mode)
  (config-general-mode))

(defun config-general-align-vars (beg end)
  "Automatically align variable  assignments inside region marked
with BEG and END based on the = character."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)=" 1 1 nil))

(defun config-general-do-electric-return ()
  "If    (point)    is    on   an    include    filename,    call
`find-file-at-point'  with it,  otherwise add  a new  line below,
indent it and move (point) there."
  (interactive)
  (if (eq config-general-electric-return t)
      (if (eq (get-text-property (point)'face) 'config-general-file-face)
          (find-file-at-point)
        (config-general-open-line-below))
    (newline)))

(defun config-general-open-line-below ()
  "Add a new line below, indent it and move (point) there."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun config-general-tab-or-complete ()
  "Enter a <TAB> or do a dabbrev completion based on (point) position."
  (interactive)
  (if (looking-back "[-%$_a-zA-Z0-9]")
      (dabbrev-completion)
    (indent-for-tab-command)))

(defun config-general-toggle-flag ()
  "Toggle a value of the list `config-general-toggle-values'."
  (interactive)
  (let* ((thing (downcase (thing-at-point 'word t)))
         (flag (assoc thing config-general-toggle-values))
         (A (car (bounds-of-thing-at-point 'word)))
         (B (cdr (bounds-of-thing-at-point 'word))))
    (when (and thing flag)
        (goto-char B)
        (backward-kill-word 1)
        (insert (cdr flag)))))

;;;; Internal Functions

(defun config-general--fl-beg-eof (limit)
  (re-search-forward "<<\\([A-Z0-9]+\\)\n" limit t))

(defun config-general--fl-end-eof (limit)
  (re-search-forward "^\\([A-Z0-9]+\\)\n" limit t))

(defun config-general--init-syntax ()
  ;; we need our own syntax table for mixed C++ and Shell comment support
  (set-syntax-table
        (let ((st (make-syntax-table)))
          (modify-syntax-entry ?\/ ". 14n" st)
          (modify-syntax-entry ?\* ". 23n" st)
          (modify-syntax-entry ?# "<" st)
          (modify-syntax-entry ?\n ">" st)
          (modify-syntax-entry ?\\ "\\" st)
          (modify-syntax-entry ?$ "'" st)
          (modify-syntax-entry ?\' "\"\"") ;; make ' electric too
          (modify-syntax-entry ?< ".")
          (modify-syntax-entry ?> ".")
          st)))

;; via: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html
;; however, I removed the dash.el dependency
(defun config-general-match-variables-in-quotes (limit)
  "Match variables in double-quotes"
  (with-syntax-table config-general-mode-syntax-table
    (catch 'done
      (while (re-search-forward
              "\\(?:^\\|[^\\]\\)\\(\\$\\)\\({.+?}\\|[_[:alnum:]]+\\|[-!#$*0?@_]\\)"
              limit t)
        (let ((SS (nth 3 (syntax-ppss))))
          (when SS
            (when (= SS 34)
              (throw 'done (point)))))))))

(defun config-general--init-font-lock ()
    ;; better suited to configs
  (setq config-general-font-lock-keywords
        '(
          ;; <>
          ("\\([<>|]+\\)" 1 'config-general-special-char-face)

          ;; special handling of single or double quoted variables
          (config-general-match-variables-in-quotes
           (1 'default t)
           (2 font-lock-variable-name-face t))
          
          ;; <<include ...>>
          ("^[ \t]*<<\\(include\\) [ \t]*\\(.+?\\)>>*"
           (1 'config-general-constant-face)
           (2 'config-general-file-face)) ;; FIXME: turn into real link property!

          ;; include ...
          ("^[ \t]*\\(include\\) [ \t]*\\(.*\\)"
           (1 'config-general-constant-face)
           (2 'config-general-file-face))
          
          ;; <block ..>
          ("^\s*</*\\(.+\\)>" 1 'config-general-blockname-face)

          ;; variable definitions
          ;; FIXME: add support for -SplitPolicy and -SplitDelimiter and make
          ;; the = a customizable variable, if possible
          ("^[ \t]*\\(.+?\\)[ \t]*="
           (1 'config-general-variable-name-face))
          
          ;; interpolating variables
          ("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
           (2 'config-general-interpolating-variable-face))

          ;; escape char
          ("\\(\\\\\\)" (1 'config-general-escape-char-face))

          ))
        
  (set (make-local-variable 'font-lock-defaults)
       '(config-general-font-lock-keywords nil t nil nil))

  (font-lock-add-keywords nil
                          '((config-general--fl-beg-eof . 'config-general-constant-face)
                            (config-general--fl-end-eof . 'config-general-constant-face))))

(defun config-general--init-minors ()
  ;; enable simple outlining
  (setq outline-heading-alist '(("##" . 1)
                                ("###" . 2)
                                ("####" . 3)
                                ("#####" . 4)))
  (outline-minor-mode t)
  ;; from shell-script-mode, turn << into here-doc
  (sh-electric-here-document-mode 1)
  ;; Inserting a brace or quote automatically inserts the matching pair
  (electric-pair-mode t))

(defun config-general--init-vars ()
  ;; prepare clean startup
  (kill-all-local-variables)

  ;; support for 'comment-region et al
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  
  ;; we don't need a complicated indent strategy, relative is totally ok
  (setq-local indent-line-function #'indent-relative)

  ;; alert about trailing whitespaces, important for continuations
  (setq-local show-trailing-whitespace t))

(defun config-general--init-imenu ()
  ;; imenu config
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression config-general-imenu-expression)
  (setq imenu-case-fold-search nil)
  (require 'imenu))

;;;###autoload
(defvar config-general-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-7")              'sh-backslash-region) ;; for latin keyboards 
    (define-key map (kbd "C-c C-/")              'sh-backslash-region)
    (define-key map (kbd "C-c C-0")              'config-general-align-vars) ;; for latin keyboards
    (define-key map (kbd "C-c C-=")              'config-general-align-vars)
    (define-key map (kbd "C-c C-f")              'find-file-at-point) ;; FIXME: change to [follow-link]
    (define-key map (kbd "C-c C-t")              'config-general-toggle-flag)
    (define-key map (kbd "<C-return>")           'config-general-do-electric-return)
    (define-key map (kbd "<tab>")                'config-general-tab-or-complete)
    (define-key map [remap delete-backward-char] 'backward-delete-char-untabify)
    map)
  "Keymap used in Config::General mode."
  )

;;;###autoload
(define-derived-mode config-general-mode conf-mode "config-general"
  "Config::General config file mode.
\\{config-general-mode-map}"

  ;; initialize mode
  (config-general--init-vars)
  (config-general--init-font-lock)
  (config-general--init-minors)
  (config-general--init-syntax)
  (config-general--init-imenu)
  
  ;; load keymap
  (use-local-map config-general-mode-map)

  ;; de-activate some (for C::G) senseless bindings
  (local-unset-key (kbd "C-c C-c"))
  (local-unset-key (kbd "C-c C-j"))
  (local-unset-key (kbd "C-c C-p"))
  (local-unset-key (kbd "C-c C-u"))
  (local-unset-key (kbd "C-c C-w"))
  (local-unset-key (kbd "C-c C-x"))
  (local-unset-key (kbd "C-c :"))

  ;; make us known correctly
  (setq major-mode 'config-general-mode)
  (setq mode-name "C::G")

  ;; eval hooks, if any
  (run-mode-hooks 'config-general-mode-hooks))



;; done
(provide 'config-general-mode)

;;; config-general-mode.el ends here
