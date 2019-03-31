;;; lsp-origami.el --- origami.el support for lsp-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant
;; Version: 0.1
;; Keywords: languages lsp-mode
;; Package-Requires: ((origami "1.0") (lsp-mode "20190326.522"))
;; URL: https://github.com/emacs-lsp/lsp-origami

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lsp-origami provides support for origami.el using language server
;; protocol's "textDocument/foldingRange" functionality.  It can be enabled
;; with
;; (require 'lsp-origami)
;; (add-hook 'origami-mode-hook #'lsp-origami-mode)

;;; Code:

(require 'origami)
(require 'lsp-mode)

(defun lsp-origami--folding-range-to-fold (range create)
  (funcall create
	   (lsp--folding-range-beg range)
	   (lsp--folding-range-end range)
	   0
	   (seq-map
	    (lambda (range) (lsp-origami--folding-range-to-fold range create))
	    (seq-remove (lambda (child-range)
			  (or (eq (lsp--folding-range-beg child-range)
				  (lsp--folding-range-beg range))
			      (eq (lsp--folding-range-end child-range)
				  (lsp--folding-range-end range))))
			(lsp--folding-range-children range)))))

(defun lsp-origami--parser (create)
  "Get a list of Folding Ranges for the current buffer."
  (lambda (_content)
    (unless (lsp--capability "foldingRangeProvider")
      (signal 'lsp-capability-not-supported (list "foldingRangeProvider")))
    (seq-map (lambda (range)
	       (lsp-origami--folding-range-to-fold range create))
	     (lsp--get-nested-folding-ranges))))

;;;###autoload
(define-minor-mode lsp-origami-mode
  "Toggle code folding support for origami."
  :group 'lsp-origami
  :global nil
  (cond
   (lsp-origami-mode
    (setq-local origami-fold-style 'lsp-mode))
   (t
    (setq-local origami-fold-style nil))))

(push '(lsp-mode . lsp-origami--parser) origami-parser-alist)

(provide 'lsp-origami)
;;; lsp-origami.el ends here
