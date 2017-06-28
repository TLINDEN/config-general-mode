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
;; Keywords: files
;; URL: https://github.com/tlinden/config-general-mode
;; License: GNU General Public License >= 2

;;; Commentary:

;;;; Introduction

;; [Config::General](http://search.cpan.org/dist/Config-General/) is a
;; Perl   module  for   parsing  config   files  with   some  enhanced
;; features.  `config-general-mode' makes it easier to edit such config
;; files with Emacs.

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

;; Edit  your config  file as  usual.  Use  `<tab>' for  completion of
;; values and variables.  Use `C-c C-t'  to toggle flags (like true to
;; false). Use `C-c C-=' on a region to automatically align on the `=`
;; character.  Use `C-c  C-/' to breakup a region with  long lines into
;; shorter ones  using backslash notation.  Use  `<C-return>' to visit
;; an included file  or (when not on  a link) insert a  new line below
;; the current one, indent and move point there.  Use `<C-k>' to delete
;; lines, including continuation lines or  whole blocks.  Use `C-c C-j'
;; to  jump to  a block  definition (same  as using  `imenu' with  the
;; mouse).

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
(require 'hippie-exp)

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
    ("on"      . "off")
    ("yes"     . "no")
    ("enable"  . "disable"))
  "Values which can be toggled with <C-c C-t>. Only pairs are supported."
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

(defface config-general-string-face
  '((t (:inherit font-lock-string-face)))
  "face for strings"
  :group 'config-general-faces)

(defface config-general-value-face
  '((t (:inherit default)))
  "face for variable values"
  :group 'config-general-faces)


;;;; Global Vars
(defconst config-general-mode-version "0.01" "Config::General mode version.")

(defvar config-general-font-lock-keywords nil
  "Keywords to highlight in CG mode.")

(defvar config-general-mode-abbrev-table nil)
(abbrev-table-put config-general-mode-abbrev-table :system t)

(defvar config-general-imenu-expression
  '(
    (nil  "^ *<\\([a-zA-Z0-9]+.*\\)>" 1 )
    )
  "Imenu generic expression for Config:General mode.  See `imenu-generic-expression'.")

;;;; Public Functions

(defun config-general-align-vars (beg end)
  "Automatically align variable assignments.

Align  inside region  marked  with BEG  and END  based  on the  =
character."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)=" 1 1 nil))

(defun config-general-do-electric-return ()
  "Electric return, follows file link or add newline below.

If    (point)    is    on   an    include    filename,    call
`find-file-at-point'  with it,  otherwise add  a new  line below,
indent it and move (point) there."
  (interactive)
  (if (eq config-general-electric-return t)
      (if (eq (get-text-property (point) 'face) 'config-general-file-face)
          (find-file-at-point)
        (config-general-open-line-below))
    (newline)))

(defun config-general-open-line-below ()
  "Add a new line below, indent it and move (point) there."
  (interactive)
  (end-of-line)
  (if (eq  (get-text-property (point) 'face) 'font-lock-comment-face)
      (indent-new-comment-line)
      (newline-and-indent)))

(defun config-general-tab-or-complete ()
  "Enter a <TAB>, goto current indentation or do a dabbrev
completion based on (point) position."
  (interactive)
  (if (looking-back "[-%$_a-zA-Z0-9]")
      (if (not (eq  (get-text-property (point) 'face) 'font-lock-comment-face))
          (hippie-expand nil))
    (if (eq (point) (line-end-position))
        (indent-for-tab-command)
      (back-to-indentation))))

(defun config-general-toggle-flag ()
  "Toggle a value from the list `config-general-toggle-values' to its reverse.
Case will be preserved, the toggle list can be modified on the fly."
  (interactive)
  (let* ((thing (downcase (thing-at-point 'word t)))
         (flag (assoc thing (config-general--toggle-list)))
         (A (car (bounds-of-thing-at-point 'word)))
         (B (cdr (bounds-of-thing-at-point 'word))))
    (when (and thing flag)
      ;; idea from: https://emacs.stackexchange.com/questions/24601
      ;; /replace-word-at-point-preserving-the-case-pattern/24617
      (set-match-data (list A B))
      (replace-match (cdr flag)))))


(defun config-general-kill-line-or-block-or-continuation (&optional ARG)
  "If the  current (and the  following) line[s] ends with  a bare
backslash -  the line continuation  marker - the current  and all
continuing lines will be killed.

If (point) is on a block  begin, then kill the whole block. Named
blocks are not supported though.

Otherwise the original `kill-line' will be called with ARG.

The flag `kill-whole-line' will be followed."
  (interactive)
  (when kill-whole-line
    (beginning-of-line))
  (let* ((savepos (point))
         (end (line-end-position))
         (onblock (save-excursion
                    (when (not (bobp))
                      ;; required since re-search-forward ignores 1st char
                      (backward-char 1))
                    (re-search-forward "^\s*<\\([a-zA-Z0-9]+\\)>" end t)))
         (block nil))
    (if onblock
        ;; we are on a block begin, search a matching block end
        (save-excursion
          (setq block (match-string-no-properties 1))
          (when (re-search-forward (format "^\s*</%s>" block) nil t)
            (setq end (point))))
      ;; else
      (save-excursion
        (end-of-line)
        (while (looking-back "\\\\")
          ;; we are at a continuation
          (forward-line 1)
          (end-of-line)
          (setq end (point)))))
    ;; now, 'end is either the original end of line or somewhere below
    (if (= (count-lines savepos end) 1)
        ;; we didn't leave current line, so just forward the call to the original
        (kill-line ARG)
      ;; else, more than 1 line, a block or continuation line, do it ourselfes
      (when (> (point-max) end)
        ;; goto next line to REALLY delete the line[s]
        (setq end (+ end 1)))
      (kill-region savepos end))))

;;;; Internal Functions

(defun config-general--toggle-list ()
  "Add each entry of `config-general-toggle-values' in its reverse form
and return a new list of both forms."
  (let ((N config-general-toggle-values))
    (dolist (E config-general-toggle-values)
      (add-to-list 'N `(,(cdr E) . ,(car E)) t)
      )
    N))

(defun config-general--fl-beg-eof (limit)
  "Search for beginning of here-doc."
  (re-search-forward "<<\\([A-Z0-9]+\\)\n" limit t))

(defun config-general--fl-end-eof (limit)
  "Search for end of here-doc."
  (re-search-forward "^\\([A-Z0-9]+\\)\n" limit t))

;; via: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html
;; however, I removed the dash.el dependency and used a normal regexp
(defun config-general--match-variables-in-quotes (limit)
  "Match variables in double-quotes.
Argument LIMIT limits the search."
  (with-syntax-table config-general-mode-syntax-table
    (catch 'done
      (while (re-search-forward
              "\\(?:^\\|[^\\]\\)\\(\\$\\)\\({.+?}\\|[_[:alnum:]]+\\|[-!#$*0?@_]\\)"
              limit t)
        (let ((SS (nth 3 (syntax-ppss))))
          (when SS
            (when (= SS 34)
              (throw 'done (point)))))))))

;; FIXME: Use this  patched version for older emacsen  and the default
;; for version which contain the patch (if any, ever).
;;
;; The original  function try-expand-dabbrev-all-buffers  doesn't work
;; correctly, it ignores a buffer-local configuration of the variables
;; hippie-expand-only-buffers  and hippie-expand-ignore-buffers.  This
;; is the patched version of the function.
;;
;; Bugreport: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=27501
(defun config-general--try-expand-dabbrev-all-buffers (old)
    "Try to expand word \"dynamically\", searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ())
        (buf (current-buffer))
        (orig-case-fold-search case-fold-search)
        (heib hippie-expand-ignore-buffers)
        (heob hippie-expand-only-buffers)
        )
    (if (not old)
        (progn
          (he-init-string (he-dabbrev-beg) (point))
          (setq he-search-bufs (buffer-list))
          (setq he-searched-n-bufs 0)
          (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
        (while (and he-search-bufs
                    (not expansion)
                    (or (not hippie-expand-max-buffers)
                        (< he-searched-n-bufs hippie-expand-max-buffers)))
          (set-buffer (car he-search-bufs))
          (if (and (not (eq (current-buffer) buf))
                   (if heob
                       (he-buffer-member heob)
                     (not (he-buffer-member heib))))
              (save-excursion
                (save-restriction
                  (if hippie-expand-no-restriction
                      (widen))
                  (goto-char he-search-loc)
                  (setq expansion
                        (let ((case-fold-search orig-case-fold-search))
                          (he-dabbrev-search he-search-string nil)))
                  (set-marker he-search-loc (point))
                  (if (not expansion)
                      (progn
                        (setq he-search-bufs (cdr he-search-bufs))
                        (setq he-searched-n-bufs (1+ he-searched-n-bufs))
                        (set-marker he-search-loc 1 (car he-search-bufs))))))
            (setq he-search-bufs (cdr he-search-bufs))
            (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;;;; Init Functions

(defun config-general--init-syntax ()
  "We need our own syntax table for mixed C++ and Shell comment support."
  (set-syntax-table
        (let ((st (make-syntax-table)))
          (modify-syntax-entry ?\/ ". 14n" st)
          (modify-syntax-entry ?\* ". 23n" st)
          (modify-syntax-entry ?# "<" st)
          (modify-syntax-entry ?\n ">" st)
          (modify-syntax-entry ?\\ "\\" st)
          (modify-syntax-entry ?$ "'" st)
          (modify-syntax-entry ?\' "\"\"") ;; make ' electric too
          (modify-syntax-entry ?\` "\"\"") ;; make ` electric too
          (modify-syntax-entry ?< ".")
          (modify-syntax-entry ?> ".")
          st)))

(defun config-general--init-font-lock ()
  "Initialize font locking."
  (setq config-general-font-lock-keywords
        '(
          ;; <>
          ("\\([<>|]+\\)" 1 'config-general-special-char-face)

          ;; special handling of single or double quoted variables
          (config-general--match-variables-in-quotes
           (1 'default t)
           (2 font-lock-variable-name-face t))

          ;; EOF
          ("\\(<<\\)\\([A-Z0-9]+\\)$"
           (1 'config-general-escape-char-face)
           (2 'config-general-constant-face))
          ("^[ \t]*\\([A-Z0-9]+?\\)$"
           (1 'config-general-constant-face))

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
          ("^[ \t]*\\(.+?\\)[ \t]*=\\(.*\\)"
           (1 'config-general-variable-name-face))
          
          ;; interpolating variables
          ("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
           (2 'config-general-interpolating-variable-face))

          ;; escape char
          ("\\(\\\\\\)" (1 'config-general-escape-char-face))
          ))

  ;; activate
  (set (make-local-variable 'font-lock-defaults)
       '(config-general-font-lock-keywords nil nil nil nil))
  (font-lock-mode 1)

  ;; set default font for everything else, which can only be variable values
  (setq buffer-face-mode-face 'config-general-value-face)
  (buffer-face-mode))

(defun config-general--init-minors ()
  "Enable and configure usefull minor modes."
  ;; from shell-script-mode, turn << into here-doc
  (sh-electric-here-document-mode 1)
  ;; Inserting a brace or quote automatically inserts the matching pair
  (electric-pair-mode t))

(defun config-general--init-vars ()
  "Initialize major mode configuration."
  ;; prepare clean startup
  (kill-all-local-variables)

  ;; support for 'comment-region et al
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  
  ;; we don't need a complicated indent strategy, relative is totally ok
  (set (make-local-variable 'indent-line-function) #'indent-relative)

  ;; alert about trailing whitespaces, important for continuations
  (set (make-local-variable 'show-trailing-whitespace) t))

(defun config-general--init-hippie ()
  "Configure `hippie-expand'."
  ;; use CG mode local only
  (set (make-local-variable 'hippie-expand-only-buffers) '(config-general-mode))

  ;; tries
  (set (make-local-variable 'hippie-expand-try-functions-list)
              '(try-expand-dabbrev
                config-general--try-expand-dabbrev-all-buffers
                try-complete-file-name-partially
                try-complete-file-name)))

(defun config-general--init-imenu ()
  "Configure `imenu'."
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
    (define-key map (kbd "C-c C-j")              'imenu) ;; aka jump
    (define-key map (kbd "<C-return>")           'config-general-do-electric-return)
    (define-key map (kbd "<tab>")                'config-general-tab-or-complete)
    (define-key map (kbd "C-k")                  'config-general-kill-line-or-block-or-continuation)
    (define-key map [remap delete-backward-char] 'backward-delete-char-untabify)
    map)
  "Keymap used in Config::General mode."
  )

;;;###autoload
(define-derived-mode config-general-mode conf-mode "config-general"
  "Config::General config file mode.

Config::General is  a Perl module  for parsing config  files with
some enhanced features. `config-general-mode'  makes it easier to
edit such config files with emacs.

It is based on `conf-mode' with the following features:

- good syntax highlighting for config files
- completion support with `<tab>' (using `dabbrev')
- imenu support for <blocks>
- electric paring mode (for quotes, parens, etc) enabled
- automatic indenting
- jump to include file with `<ret>'

To  use,  save  config-general-mode.el  to a  directory  in  your
load-path.

Add something like this to your config:

    (require 'config-general-mode)
    (add-to-list 'auto-mode-alist '(\"\\.conf$\" . config-general-mode))

or load it manually, when needed:

    M-x config-general-mode
   
You can  also enable  it with a  buffer-local variable  by adding
this as the first line of a config file:

    # -*-config-general-*-

Usage

Edit your  config file as  usual.  Use `<tab>' for  completion of
values and variables.   Use `C-c C-t' to toggle  flags (like true
to false).  Use `C-c C-=' on  a region to automatically  align on
the `=`  character. Use `C-c C-/'  to breakup a region  with long
lines   into  shorter   ones  using   backslash  notation.    Use
`<C-return>' to  visit an included file  or (when not on  a link)
insert a  new line below the  current one, indent and  move point
there. Use `<C-k>' to  delete lines, including continuation lines
or   whole  blocks.   Use  `C-c   C-j'   to  jump   to  a   block
definition (same as using `imenu' with the mouse).

Customize

You can customize the mode with:

     M-x customize-group RET config-general-mode RET

You can also use hooks to  config-general mode as a way to modify
or enhance its behavior.  The following hooks are available:

    config-general-mode-hook
   
For example:

    (add-hook 'config-general-mode-hook 'electric-indent-mode)

\\{config-general-mode-map}"

  ;; initialize mode
  (config-general--init-vars)
  (config-general--init-hippie)
  (config-general--init-font-lock)
  (config-general--init-minors)
  (config-general--init-syntax)
  (config-general--init-imenu)
  
  ;; load keymap
  (use-local-map config-general-mode-map)

  ;; de-activate some (for C::G) senseless bindings
  (local-unset-key (kbd "C-c C-c"))
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
