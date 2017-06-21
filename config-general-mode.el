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
;; Keywords: config
;; URL: https://github.com/tlinden/config-general-mode
;; License: GNU General Public License >= 2

;;; Commentary:

;;(add-hook 'cg-mode-hook 'electric-indent-mode)

;;; Install:
;;; Customize:

;;; Code:

(require 'sh-script)
(require 'cc-mode)

;;;; Customizables
(defconst config-general-mode-version "0.01" "Config::General mode version")

;;;; Global Vars
(defvar config-general-font-lock-keywords nil
  "Keywords to highlight in CG mode.")

(defvar config-general-mode-abbrev-table nil)

;;;; Public Functions

(defun config-general-reload()
  (interactive)
  (unload-feature 'config-general-mode)
  (require 'config-general-mode)
  (config-general-mode))

(defun config-general-align-vars (beg end)
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)=" 1 1 nil))

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

(defun config-general--init-font-lock ()
    ;; better suited to configs
  (setq config-general-font-lock-keywords
        '(
          ;; <block ..> (do this first because it may look like a parameter)
          ("^\s*</*\\(.+\\)>" 1 'font-lock-function-name-face)

          ;; var=val or var[index]=val
          ("^[ \t]*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?[ \t]*="
           (1 'font-lock-variable-name-face)
           (2 'font-lock-constant-face nil t))

          ;; variables
          ("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
           (2 'font-lock-variable-name-face))

          ;; escape char
          ("\\(\\\\\\)"
           (1 'font-lock-warning-face))

          ))
        
  (set (make-local-variable 'font-lock-defaults)
       '(config-general-font-lock-keywords nil t nil nil))

  (copy-face 'font-lock-constant-face 'cg-eof-face)
  (copy-face 'font-lock-string-face 'cg-heredoc-face)
  
  (font-lock-add-keywords nil
                          '((config-general--fl-beg-eof . 'cg-eof-face)
                            (config-general--fl-end-eof . 'cg-eof-face))))

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

;;;###autoload
(defvar config-general-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-7") 'sh-backslash-region) ;; for latin keyboards 
    (define-key map (kbd "C-c C-/") 'sh-backslash-region)
    (define-key map (kbd "C-c C-0") 'config-general-align-vars)
    (define-key map (kbd "C-c C-=") 'config-general-align-vars)
    (define-key map [remap delete-backward-char] 'backward-delete-char-untabify)
    map)
  "Keymap used in Config::General mode."
  )

;;;###autoload
(define-derived-mode config-general-mode conf-mode "config-general"
  "Config::General config file mode"

  ;; prepare clean startup
  (kill-all-local-variables)

  ;; support for 'comment-region et al
  (setq-local comment-start "; ")
  (setq-local comment-end "")

  ;; we don't need a complicated indent strategy, relative is totally ok
  (setq-local indent-line-function #'indent-relative)

  ;; initialize mode
  (config-general--init-font-lock)
  (config-general--init-minors)
  (config-general--init-syntax)
  
  ;; load keymap
  (use-local-map config-general-mode-map)

  ;; eval hooks, if any
  (run-mode-hooks 'config-general-mode-hooks))



;; see also https://futureboy.us/frinktools/emacs/frink-mode.el
;; font-lock-default-fontify-buffer

;; back-to-indentation

(provide 'config-general-mode)
