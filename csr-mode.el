;;;; csr-mode-el -- Major mode for editing .csr files

;; Author: Greg Brown <gb@trailbound.net>
;; Created: 25 Nov 2011
;; Keywords: CSR major-mode

;; Copyright (C) 2011 Greg Brown <gb@trailbound.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; Thanks to Scott Andrew Borton for his tutorial and example on
;; Emacs mode creation. CSR-Mode was built based on and stealing
;; heavily from his wpdl-mode example.



(defvar csr-mode-hook nil)
(defvar csr-mode-map
  (let ((csr-mode-map (make-keymap)))
    (define-key csr-mode-map "\C-j" 'newline-and-indent)
    csr-mode-map)
  "Keymap for CSR major mode")

(add-to-list 'auto-mode-alist '("\\.csr\\'" . csr-mode))


(defconst csr-object-list
  '("addressmap" "group" "register" "field" "port" "memory" "component")
  "List of CSR objects.")

(defconst csr-control-list
  '("if" "else" "for" "case")
  "List of CSR control statements.")

(defconst csr-type-list
  '("enum" "userdefined" "bool" "integer" "number" "string" "reference" "var")
  "List of CSR types.")


(defconst csr-font-lock-keywords-1
  (list
   (list (concat "\\<" (regexp-opt csr-object-list t) "\\>") 1 'font-lock-function-name-face)
   '("\\('\\w*'\\)" 1 font-lock-variable-name-face)
   )
  "Minimal highlighting expressions for CSR mode.")


(defconst csr-font-lock-keywords-2
  (append csr-font-lock-keywords-1
          (list
           (list (concat "\\<" (regexp-opt csr-type-list t) "\\>") 1 'font-lock-type-face)
           ))
  "Additional Keywords to highlight in CSR mode.")

(defconst csr-font-lock-keywords-3
  (append csr-font-lock-keywords-2
		  (list
       ;; Highlights address specifications (example: [0x2000])
       '("\\(\\[[0-9x]+\\]\\)" 1 font-lock-constant-face)
       ))
  "Full highlighting in CSR mode.")

;; This is just a placeholder copied from tcl-mode as the
;; setting of font-lock-defaults was copied from that mode
;; as well. I'm not entirely sure what the syntactic function's
;; purpose is, so I need to figure that out in the future and
;; replace this with whatever makes sense.
(defvar csr-font-lock-syntactic-keywords
  ;; Mark the few `#' that are not comment-markers.
  '(("[^;[{ \t\n][ \t]*\\(#\\)" (1 ".")))
  "Syntactic keywords for csr-mode.")

(defvar csr-font-lock-keywords csr-font-lock-keywords-3
  "Default highlighting expressions for CSRCompiler mode.")


(defun csr-indent-line ()
  "Indent current line as CSRCompiler code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)		   ; First line is always non-indented
	(let ((not-indented t) cur-indent)
	  (if (looking-at ".*[ \t]*}") ; If the line we are looking at is the end of a block, then decrease the indentation
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0) ; We can't indent past the left margin
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented ; Iterate backwards until we find an indentation hint
			(forward-line -1)
			(if (looking-at ".*[ \t]*}") ; This hint indicates that we need to indent at the level of the } token
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at ".*[ \t]*{") ; This hint indicates that we need to indent an extra level
				  (progn
					(setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
					(setq not-indented nil))
				(if (bobp)
					(setq not-indented nil)))))))
	  (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation


(defvar csr-mode-syntax-table
  (let ((csr-mode-syntax-table (make-syntax-table)))

    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" csr-mode-syntax-table)

	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" csr-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" csr-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" csr-mode-syntax-table)
	csr-mode-syntax-table)
  "Syntax table for csr-mode")

(defun csr-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map csr-mode-map)
  (set-syntax-table csr-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults)
       '(csr-font-lock-keywords nil nil nil beginning-of-defun
 	 (font-lock-syntactic-keywords . csr-font-lock-syntactic-keywords)
 	 (parse-sexp-lookup-properties . t)))

;; The old setting of font-lock-defaults from wpdl-mode tutorial
;;  (set (make-local-variable 'font-lock-defaults) '(csr-font-lock-keywords))

  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'csr-indent-line)
  (setq major-mode 'csr-mode)
  (setq mode-name "CSR")
  (run-hooks 'csr-mode-hook))

(provide 'csr-mode)

;;; csr-mode.el ends here
