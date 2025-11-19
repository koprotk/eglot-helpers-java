;;; eglot-helper-java.el --- Helper functions for Java with Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Muñoz

;; Version: 0.2
;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Maintainer: Daniel Muñoz <demunoz2@uc.cl>
;; URL: https://github.com/koprotk/eglot-helper-java
;; Keywords: java, eglot, convenience, languages
;; URL: https://github.com/yourusername/eglot-helper-java
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides helper functions for working with Java in Eglot.
;; The function:  

;;; Code:

(require 'eglot)

(defun get-fqnm-at-point-beta (with-method)
  "Get the fully qualified name of the class at point."
  (let* ((package (substring-no-properties (car (car (eglot-imenu)))))
         (class  (substring-no-properties (car (car (cdr (eglot-imenu))))))
         (method (replace-regexp-in-string "[()]" "" (substring-no-properties (car (car (cdr (car (cdr (eglot-imenu))))))))))
    (if with-method
        (if (and package class method)
            (concat package "." class "#" method)
          (concat package "." class))
      (concat package "." class)
      )))

(defun get-fqnm-at-point (with-method)
  "Obtiene el nombre cualificado del método o clase en el punto."
  (let* (;; --- A. PREPARACIÓN ---
         (imenu-list (eglot-imenu))
         (package (substring-no-properties (car (car imenu-list))))
         (class (substring-no-properties (car (car (cdr imenu-list)))))
         (methods (cdr (car (cdr imenu-list)))) 
         (method-found nil))

    ;; --- B. BÚSQUEDA ---
    ;; Usaremos un bucle para encontrar tanto la clase como el método que contienen el punto.
    ;; --- C. BÚSQUEDA: Tu bucle, modificado para guardar el resultado ---
    (dolist (obj methods)
      ;; Extraemos los datos de cada objeto, como ya haces
      (let* ((name   (car obj))
             (kind   (get-text-property 0 'breadcrumb-kind name))
             (region (get-text-property 0 'breadcrumb-region name)))
        
        ;; Solo continuamos si la región existe y el tipo es "Method"
        (when (and region (string= kind "Method"))
          (let ((start (car region))
                (end   (cdr region)))
            ;; Si el punto está en el rango, ¡hemos encontrado nuestro método!
            (when (and (>= (point) start) (<= (point) end))
              ;; Guardamos el nombre limpio en nuestra variable y salimos del bucle.
              (setq method-found (replace-regexp-in-string "[()]" "" (substring-no-properties name)))
              ;; (cl-return) detiene el bucle dolist/cl-loop para no seguir buscando inútilmente
              ;; (esto es opcional pero más eficiente)
              )))))
    (cond
     ((and with-method method-found)
      (concat package "." class "#" method-found))
     
     ((and package class)
      (concat package "." class))
     
     (t nil))))

(defvar fqcn-custom (lambda () (defvar fqcn-custom #'(get-fqnm-at-point nil))))
(defvar fqmn-custom (lambda () (defvar fqcn-custom #'(get-fqnm-at-point t))))

;;If you want to use eglot-java functions for methods you should replace (get-fqnm-at-point t) with (eglot-java--find-nearest-method-at-point)
;;If you want to use eglot-java functions for classes you should replace (get-fqnm-at-point nil) with (eglot-java--find-nearest-method-at-point)
;;Remember to add a (require eglot-java)

(defun run-mvn-test-class ()
  "Run the mvn test class at point"
  (interactive)
  (if-let (project (project-current))
      (let ((default-directory (project-root project)))
        (compile (format "mvn -Dtest=%s test" (get-fqnm-at-point nil))))
    (message "Not inside a know project.")))


(defun run-mvn-test-method ()
  "Run the mvn test at point"
  (interactive)
  (if-let (project (project-current))
      (let ((default-directory (project-root project)))
        (compile (format "mvn -Dtest=%s test" (get-fqnm-at-point t))))
    (message "Not inside a know project."))
  )

(defun build-mvn-project-skiptests ()
  "Run the mvn test at point"
  (interactive)
  (if-let (project (project-current))
      (let ((default-directory (project-root project)))
        (compile "mvn clean package -DskipTests -U")
        (message "Not inside a know project."))
    )
  )

(defun debug-mvn-test-method ()
  "Run the test on debug mode"
  (interactive)
  (if-let (project (project-current))
      (let ((default-directory (project-root project)))
        (compile
         (format "mvn -Dmaven.surefire.debug=-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=8000 -Dtest=%s test"
                 (get-fqnm-at-point t))))
    (message "Not inside a know project."))
  )

(defun gud-jdb-break ()
  "Create breakpoint for jdb"
    (interactive)
    (if-let (class (eglot-java--class-fqcn))
        (gud-call (concat "stop at " class ":%l") 1)
      (message "Could not determine class name."))
    )

(provide 'eglot-helper-java)
;;; eglot-helper-java.el ends here
