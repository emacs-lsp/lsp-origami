;;; lsp-origami.el --- origami.el support for lsp-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant
;; Keywords: languages lsp-mode
;; Package-Requires: ((origami "1.0") (lsp-mode "6.0") (dash "2.14.1") (dash-functional "2.14.1") (ht "2.0"))
;; Version: 1.0
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
;; protocol's "textDocument/foldingRange" functionality. It can be enabled
;; with
;; (require 'lsp-origami)
;; (add-hook 'origami-mode-hook #'lsp-origami-enable)

;;; Code:

(require 'origami)
(require 'lsp-mode)
(require 'dash)
(require 'dash-functional)
(require 'ht)

(cl-defstruct lsp-origami--nested-node
  (beg)
  (end)
  (children))

(defun lsp-origami--make-node-from-range (range)
  (make-lsp-origami--nested-node
   :beg (car range)
   :end (cdr range)
   :children nil))

(defun lsp-origami--insert-range (range nodes)
  (-let (((beg . end) range))
    (if (null nodes)
	nil
      (cl-block top
	(dolist (node nodes)
	  ;; Check whether the range is within the bounds of the node.
	  (if (and (> beg (lsp-origami--nested-node-beg node))
		   (< end (lsp-origami--nested-node-end node)))
	      ;; Check whether the range nests in a child node.
	      (if (->> node lsp-origami--nested-node-children
		       (lsp-origami--insert-range range))
		  (cl-return-from top t)
		;; If not, insert this node as a child.
		(setf (lsp-origami--nested-node-children node)
		      (list (make-lsp-origami--nested-node
			     :beg beg
			     :end end
			     :children nil)))
		(cl-return-from top t))
	    (if (or (= beg (lsp-origami--nested-node-beg node))
		    (= end (lsp-origami--nested-node-end node)))
		(cl-return-from top t))))
	;; If not, insert this as a "root" node.
	(nconc nodes (list (make-lsp-origami--nested-node
			    :beg beg
			    :end end
			    :children nil)))
	(cl-return-from top t)))))

(defconst lsp-origami--map-fn
  (-lambda ((&hash "startLine" start-line
		   "startCharacter" start-character
		   "endLine" end-line
		   "endCharacter" end-character))
    (cons (lsp--position-to-point (ht ("line" start-line)
				      ("character" start-character)))
 	  (lsp--position-to-point (ht ("line" end-line)
				      ("character" end-character))))))

(defun lsp-origami--node-to-fold (node create)
  (funcall create
	   (lsp-origami--nested-node-beg node)
	   (lsp-origami--nested-node-end node)
	   0
	   (seq-map (-rpartial #'lsp-origami--node-to-fold create)
		    (lsp-origami--nested-node-children node))))

(defun lsp-origami--range-sort-pred (r1 r2)
  (or (< (car r1) (car r2))
      (and (= (car r1) (car r2))
	   (> (cdr r1) (cdr r2)))))

(defun lsp-origami--parser (create)
  "Get a list of Folding Ranges for the current buffer."
  (lambda (_content)
    (unless (lsp--capability "foldingRangeProvider")
      (signal 'lsp-capability-not-supported (list "foldingRangeProvider")))
    (let* ((ranges (lsp-request "textDocument/foldingRange"
			     `(:textDocument ,(lsp--text-document-identifier))))
	   (ranges (seq-sort
		    #'lsp-origami--range-sort-pred
		    (delete-dups (seq-map lsp-origami--map-fn ranges))))
	   nodes)
      (if (not (zerop (seq-length ranges)))
	  (progn
	    (setq nodes (list (lsp-origami--make-node-from-range (pop ranges))))
	    (seq-do (-rpartial #'lsp-origami--insert-range nodes) ranges)
	    (seq-map (-rpartial #'lsp-origami--node-to-fold create) nodes))
	nil))))

(defun lsp-origami-enable ()
  (interactive)
  (if (lsp--capability "foldingRangeProvider")
      (setq-local origami-parser-alist `((,major-mode . lsp-origami--parser)))
    (lsp-warn "Language Server doesn't support \"foldingRangeProvider\", not enabling origami support.")))

(provide 'lsp-origami)
;;; lsp-origami.el ends here
