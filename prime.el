;;; prime.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'deino)
(require 's)

(defdeino superman-prime (:color blue) "j" ("`" nil "cancel"))

(defun prime--replace-spaces (str) (s-replace " " "/" str))
(defun prime--construct-name (str) (prime--replace-spaces (concat "prime/" str)))

;;;###autoload
(defmacro prime* (parent first-call key func &optional name* &rest args)
    (let* ((name (if (stringp name*) name* (if (symbolp func) (symbol-name func) nil)))
            (keys (split-string key " "))
            (one-key (= (length keys) 1))
            (two-key (= (length keys) 2))
            (carkeys (car keys))
            (spare-keys (cadr keys))
            (current-parent (if parent
                                (concat parent " " (deino--replace-key carkeys))
                                (deino--replace-key carkeys)))
            (prime-name (if one-key name (prime--construct-name current-parent)))
            (prime-body (if one-key func (intern (concat prime-name "/body"))))
            (prime-body-plus (unless one-key (fboundp prime-body)))
            (next-prime-key (string-join (cdr keys) " "))
            (next-parent (concat current-parent " " (deino--replace-key spare-keys)))
            (next-prime-name (if two-key name (s-chop-suffix "-" (prime--construct-name next-parent))))
            (next-deino-body (if two-key func (intern (concat next-prime-name "/body"))))
            (next-deino-settings (when two-key args)))
        (when first-call (eval `(defdeino+ superman-prime nil (,carkeys ,prime-body ,prime-name))))
        (unless one-key
            (eval `(prime* ,current-parent nil ,next-prime-key ,func ,name* ,@args))
            `(,(intern (concat "defdeino" (when prime-body-plus "+")))
                ,(intern prime-name)
                ,@(unless prime-body-plus '((:color blue) nil ("`" nil "cancel")))
                (,spare-keys ,next-deino-body ,next-prime-name)))))

;;;###autoload
(defmacro prime (key func &optional name &rest args) `(prime* nil t ,key ,func ,name ,@args))

(provide 'prime)
;;; prime.el ends here
