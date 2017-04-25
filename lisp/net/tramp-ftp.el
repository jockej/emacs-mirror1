;;; tramp-ftp.el --- Tramp convenience functions for Ange-FTP  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2017 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Convenience functions for calling Ange-FTP from Tramp.
;; Most of them are displaced from tramp.el.

;;; Code:

(require 'tramp)

;; Pacify byte-compiler.
(eval-when-compile
  (require 'cl)
  (require 'custom))
(defvar ange-ftp-ftp-name-arg)
(defvar ange-ftp-ftp-name-res)
(defvar ange-ftp-name-format)

;; Disable Ange-FTP from file-name-handler-alist.
(defun tramp-disable-ange-ftp ()
  "Turn Ange-FTP off.
This is useful for unified remoting.  See
`tramp-file-name-structure' for details.  Requests suitable for
Ange-FTP will be forwarded to Ange-FTP.  Also see the variables
`tramp-ftp-method', `tramp-default-method', and
`tramp-default-method-alist'.

This function is not needed in Emacsen which include Tramp, but is
present for backward compatibility."
  (let ((a1 (rassq 'ange-ftp-hook-function file-name-handler-alist))
	(a2 (rassq 'ange-ftp-completion-hook-function file-name-handler-alist)))
    (setq file-name-handler-alist
	  (delete a1 (delete a2 file-name-handler-alist)))))

(eval-after-load "ange-ftp"
  '(when (functionp 'tramp-disable-ange-ftp)
     (tramp-disable-ange-ftp)))

;;;###autoload
(defun tramp-ftp-enable-ange-ftp ()
  "Reenable Ange-FTP, when Tramp is unloaded."
  ;; The following code is commented out in Ange-FTP.

  ;;; This regexp takes care of real ange-ftp file names (with a slash
  ;;; and colon).
  ;;; Don't allow the host name to end in a period--some systems use /.:
  (or (assoc "^/[^/:]*[^/:.]:" file-name-handler-alist)
      (setq file-name-handler-alist
	    (cons '("^/[^/:]*[^/:.]:" . ange-ftp-hook-function)
		  file-name-handler-alist)))

  ;;; This regexp recognizes absolute filenames with only one component,
  ;;; for the sake of hostname completion.
  (or (assoc "^/[^/:]*\\'" file-name-handler-alist)
      (setq file-name-handler-alist
	    (cons '("^/[^/:]*\\'" . ange-ftp-completion-hook-function)
		  file-name-handler-alist)))

  ;;; This regexp recognizes absolute filenames with only one component
  ;;; on Windows, for the sake of hostname completion.
  (and (memq system-type '(ms-dos windows-nt))
       (or (assoc "^[a-zA-Z]:/[^/:]*\\'" file-name-handler-alist)
	   (setq file-name-handler-alist
		 (cons '("^[a-zA-Z]:/[^/:]*\\'" .
			 ange-ftp-completion-hook-function)
		       file-name-handler-alist)))))

(add-hook 'tramp-ftp-unload-hook 'tramp-ftp-enable-ange-ftp)

;; Define FTP method ...
;;;###tramp-autoload
(defconst tramp-ftp-method "ftp"
  "When this method name is used, forward all calls to Ange-FTP.")

;; ... and add it to the method list.
;;;###tramp-autoload
(add-to-list 'tramp-methods (cons tramp-ftp-method nil))

;; Add some defaults for `tramp-default-method-alist'.
;;;###tramp-autoload
(add-to-list 'tramp-default-method-alist
	     (list "\\`ftp\\." nil tramp-ftp-method))
;;;###tramp-autoload
(add-to-list 'tramp-default-method-alist
	     (list nil "\\`\\(anonymous\\|ftp\\)\\'" tramp-ftp-method))

;; Add completion function for FTP method.
;;;###tramp-autoload
(eval-after-load 'tramp
  '(tramp-set-completion-function
     tramp-ftp-method
     '((tramp-parse-netrc "~/.netrc"))))

;;;###tramp-autoload
(defun tramp-ftp-file-name-handler (operation &rest args)
  "Invoke the Ange-FTP handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (save-match-data
    (or (boundp 'ange-ftp-name-format)
	(let (file-name-handler-alist) (require 'ange-ftp)))
    (let ((ange-ftp-name-format
	   (list (nth 0 (tramp-file-name-structure))
		 (nth 3 (tramp-file-name-structure))
		 (nth 2 (tramp-file-name-structure))
		 (nth 4 (tramp-file-name-structure))))
	  ;; ange-ftp uses `ange-ftp-ftp-name-arg' and `ange-ftp-ftp-name-res'
	  ;; for optimization in `ange-ftp-ftp-name'. If Tramp wasn't active,
	  ;; there could be incorrect values from previous calls in case the
	  ;; "ftp" method is used in the Tramp file name. So we unset
	  ;; those values.
	  (ange-ftp-ftp-name-arg "")
	  (ange-ftp-ftp-name-res nil))
      (cond
       ;; If argument is a symlink, `file-directory-p' and
       ;; `file-exists-p' call the traversed file recursively. So we
       ;; cannot disable the file-name-handler this case.  We set the
       ;; connection property "started" in order to put the remote
       ;; location into the cache, which is helpful for further
       ;; completion.  We don't use `with-parsed-tramp-file-name',
       ;; because this returns another user but the one declared in
       ;; "~/.netrc".
       ((memq operation '(file-directory-p file-exists-p))
	(if (apply 'ange-ftp-hook-function operation args)
	    (let ((v (tramp-dissect-file-name (car args) t)))
	      (aset v 0 tramp-ftp-method)
	      (tramp-set-connection-property v "started" t))
	  nil))

       ;; If the second argument of `copy-file' or `rename-file' is a
       ;; remote file name but via FTP, ange-ftp doesn't check this.
       ;; We must copy it locally first, because there is no place in
       ;; ange-ftp for correct handling.
       ((and (memq operation '(copy-file rename-file))
	     (tramp-tramp-file-p (cadr args))
	     (not (tramp-ftp-file-name-p (cadr args))))
	(let* ((filename (car args))
	       (newname (cadr args))
	       (tmpfile (tramp-compat-make-temp-file filename))
	       (args (cddr args)))
	  ;; We must set `ok-if-already-exists' to t in the first
	  ;; step, because the temp file has been created already.
	  (if (eq operation 'copy-file)
	      (apply operation filename tmpfile t (cdr args))
	    (apply operation filename tmpfile t))
	  (unwind-protect
	      (rename-file tmpfile newname (car args))
	    ;; Cleanup.
	    (ignore-errors (delete-file tmpfile)))))

       ;; Normally, the handlers must be discarded.
       (t (let* ((inhibit-file-name-handlers
		  (list 'tramp-file-name-handler
			'tramp-completion-file-name-handler
			(and (eq inhibit-file-name-operation operation)
			     inhibit-file-name-handlers)))
		 (inhibit-file-name-operation operation))
	    (apply 'ange-ftp-hook-function operation args)))))))

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-ftp-file-name-p (filename)
  "Check if it's a filename that should be forwarded to Ange-FTP."
  (string= (tramp-file-name-method (tramp-dissect-file-name filename))
	   tramp-ftp-method))

;;;###tramp-autoload
(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-ftp-file-name-p 'tramp-ftp-file-name-handler))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-ftp 'force)))

(provide 'tramp-ftp)

;;; TODO:

;; * There are no backup files on FTP hosts.

;;; tramp-ftp.el ends here
