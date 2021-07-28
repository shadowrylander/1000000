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
    (let* ((ds (deino--create-dataset
                (if (stringp name*) name* (if (symbolp func) (symbol-name func) nil))
                key
                parent
                func
                #'prime--construct-name))

            (next-key (string-join (cdr (d--g ds :keys)) " "))
            (next-deino-body (if (d--g ds :two-key) func (intern (concat (d--g ds :next-name) "/body"))))
            (next-deino-settings (when (d--g ds :two-key) args)))
        (when first-call (eval `(defdeino+ superman-prime nil
                                    (,(d--g ds :carkeys)
                                        ,(d--g ds :current-body)
                                        ,(d--g ds :current-name)))))
        (unless (d--g ds :one-key)
            (eval `(prime* ,(d--g ds :current-parent) nil ,next-key ,func ,name* ,@next-deino-settings))
            `(,(intern (concat "defdeino" (when (d--g ds :current-body-plus) "+")))
                ,(intern (d--g ds :current-name))
                ,@(unless (d--g ds :current-body-plus) '((:color blue) nil ("`" nil "cancel")))
                (,(d--g ds :spare-keys) ,next-deino-body ,(d--g ds :next-name))))))

;;;###autoload
(defmacro prime (key func &optional name &rest args) `(prime* nil t ,key ,func ,name ,@args))

(provide 'prime)
;;; prime.el ends here
