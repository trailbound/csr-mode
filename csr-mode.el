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
;; Emacs mode creation. CSR-Mode was built based on and borrowing
;; heavily from his wpdl-mode example.



(defvar csr-mode-hook nil)
(defvar csr-mode-map
  (let ((csr-mode-map (make-keymap)))
    (define-key csr-mode-map "\C-j" 'newline-and-indent)
    csr-mode-map)
  "Keymap for CSR major mode")

(add-to-list 'auto-mode-alist '("\\.csr\\'" . csr-mode))


;;(defconst csr-builtin-list
;;  '("addressmap" "group" "register" "field" "port" "memory")
;;  "List of CSR objects.")

(defconst csr-type-list
  '("enum" "userdefined" "bool" "integer" "number" "string" "reference" "var")
  "List of CSR types.")

(defconst csr-font-lock-keywords
  (list
   (list (concat "\\<" (regexp-opt csr-type-list t) "\\>") 1 'font-lock-type-face)
   '("\\<\\(aressmap\\|field\\|register\\)\\>" 1 font-lock-keyword-face)
   '("\\('\\w*'\\)" 1 font-lock-variable-name-face)
   )
  "Minimal highlighting expressions for CSR mode.")

;;
;;   (list (concat "\\_<" (regexp-opt csr-builtin-list t) "\\_>") 1 'font-lock-builtin-face)
;;   (list (concat "\\_<" (regexp-opt csr-type-list t) "\\_>") 2 'font-lock-type-face)))

;;   (list (concat "\\<" (regexp-opt csr-builtin-list t) "\\>") 1 'font-lock-builtin-face)
;;  '("\\<\\(addressmap\\|field\\|register\\)\\>" 1 font-lock-keyword-face)
;;  '("\\('\\w*'\\)" 1 font-lock-variable-name-face))

;;(defconst csr-font-lock-keywords-2
;;  (append csr-font-lock-keywords-1
;;          (list
;;           (list (concat "\\<" (regexp-opt csr-type-list t) "\\>") 2 'font-lock-type-face)))
;;  "Additional Keywords to highlight in CSR mode.")

;;(defconst csr-font-lock-keywords-3
;;  (append csr-font-lock-keywords-2
;;		  (list
;;		 ; These are some possible built-in values for CSR attributes
;;			 ; "ROLE" "ORGANISATIONAL_UNIT" "STRING" "REFERENCE" "AND"
;;			 ; "XOR" "WORKFLOW" "SYNCHR" "NO" "APPLICATIONS" "BOOLEAN"
;;							 ; "INTEGER" "HUMAN" "UNDER_REVISION" "OR"
;;		   '("\\<\\(A\\(ND\\|PPLICATIONS\\)\\|BOOLEAN\\|HUMAN\\|INTEGER\\|NO\\|OR\\(GANISATIONAL_UNIT\\)?\\|R\\(EFERENCE\\|OLE\\)\\|S\\(TRING\\|YNCHR\\)\\|UNDER_REVISION\\|WORKFLOW\\|XOR\\)\\>" . font-lock-constant-face)))
;;  "Balls-out highlighting in CSR mode.")

;;(defvar csr-font-lock-keywords csr-font-lock-keywords-1
;;  "Default highlighting expressions for CSRCompiler mode.")

(defun csr-indent-line ()
  "Indent current line as CSRCompiler code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)		   ; First line is always non-indented
	(let ((not-indented t) cur-indent)
	  (if (looking-at "^[ \t]*END_") ; If the line we are looking at is the end of a block, then decrease the indentation
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0) ; We can't indent past the left margin
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented ; Iterate backwards until we find an indentation hint
			(forward-line -1)
			(if (looking-at "^[ \t]*END_") ; This hint indicates that we need to indent at the level of the END_ token
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at "^[ \t]*\\(PARTICIPANT\\|MODEL\\|APPLICATION\\|WORKFLOW\\|ACTIVITY\\|DATA\\|TOOL_LIST\\|TRANSITION\\)") ; This hint indicates that we need to indent an extra level
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
  (set (make-local-variable 'font-lock-defaults) '(csr-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'csr-indent-line)
  (setq major-mode 'csr-mode)
  (setq mode-name "CSR")
  (run-hooks 'csr-mode-hook))

(provide 'csr-mode)

;;; csr-mode.el ends here
