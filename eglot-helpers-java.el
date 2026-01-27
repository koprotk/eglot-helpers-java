;;; eglot-helpers-java.el --- Helper functions for Java with Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Muñoz

;; Version: 0.2
;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Maintainer: Daniel Muñoz <demunoz2@uc.cl>
;; URL: https://github.com/koprotk/eglot-java-helpers
;; Keywords: java, eglot, convenience, languages
;; Package-Requires: ((emacs "29.1") (eglot "1.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.   If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides helper functions for working with Java in Eglot.
;; It offers utilities for running Maven tests, debugging, and managing Java
;; projects with Eglot LSP integration.
;;
;; Features:
;; - Get fully qualified class and method names at point
;; - Run Maven tests for specific classes or methods
;; - Build Maven projects with test skipping
;; - Debug Maven tests with JDB integration
;; - Set JDB breakpoints using fully qualified names
;;
;; Usage:
;; (require 'eglot-helpers-java)
;;
;; Key functions:
;; - `eglot-helpers-java-run-mvn-test-class' - Run tests for the current class
;; - `eglot-helpers-java-run-mvn-test-method' - Run test for the method at point
;; - `eglot-helpers-java-build-mvn-project-skiptests' - Build project without tests
;; - `eglot-helpers-java-debug-mvn-test-method' - Debug test method with JDB
;; - `eglot-helpers-java-gud-jdb-break' - Set JDB breakpoint at current line
;; - `eglot-helpers-java-get-fqcn' - Get fully qualified class name
;; - `eglot-helpers-java-get-fqmn' - Get fully qualified method name

;;; Code:

(require 'eglot)
(require 'cl-lib)
(require 'gud)

(defun eglot-helpers-java--get-fqnm-at-point (with-method)
  "Get the fully qualified name of the method or class at point. 
If WITH-METHOD is non-nil, include the method name."
  (let* ((imenu-list (eglot-imenu))
         (package (substring-no-properties (car (car imenu-list))))
         (class (substring-no-properties (car (car (cdr imenu-list)))))
         (methods (cdr (car (cdr imenu-list))))
         (method-found nil))
    (cl-dolist (obj methods)
      (let* ((name (car obj))
             (kind (get-text-property 0 'breadcrumb-kind name))
             (region (get-text-property 0 'breadcrumb-region name)))
        (when (and region (string= kind "Method"))
          (let ((start (car region))
                (end (cdr region)))
            (when (and (>= (point) start) (<= (point) end))
              (setq method-found (replace-regexp-in-string "[()]" "" (substring-no-properties name)))
              (cl-return))))))
    (cond
     ((and with-method method-found)
      (concat package "." class "#" method-found))
     ((and package class)
      (concat package "." class))
     (t nil))))

(defun eglot-helpers-java-get-fqcn ()
  "Get the fully qualified class name at point."
  (eglot-helpers-java--get-fqnm-at-point nil))

(defun eglot-helpers-java-get-fqmn ()
  "Get the fully qualified method name at point."
  (eglot-helpers-java--get-fqnm-at-point t))

;;;###autoload
(defun eglot-helpers-java-run-mvn-test-class ()
  "Run the Maven test for the class at point."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile (format "mvn -Dtest=%s test" (eglot-helpers-java--get-fqnm-at-point nil))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-run-mvn-test-method ()
  "Run the Maven test for the method at point."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile (format "mvn -Dtest=%s test" (eglot-helpers-java--get-fqnm-at-point t))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-build-mvn-project-skiptests ()
  "Build the Maven project, skipping tests."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile "mvn clean package -DskipTests -U"))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-debug-mvn-test-method ()
  "Run the test at point in debug mode."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile
         (format "mvn -Dmaven.surefire.debug=-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=8000 -Dtest=%s test"
                 (eglot-helpers-java--get-fqnm-at-point t))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-gud-jdb-break ()
  "Create breakpoint for jdb at the current line."
  (interactive)
  (if-let ((class (eglot-helpers-java--get-fqnm-at-point nil)))
      (gud-call (concat "stop at " class ":%l") 1)
    (message "Could not determine class name.")))

(provide 'eglot-helpers-java)
;;; eglot-helpers-java.el ends here
