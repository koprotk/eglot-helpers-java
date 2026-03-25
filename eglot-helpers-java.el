;;; eglot-helpers-java.el --- Helper functions for Java with Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Muñoz

;; Version: 0.2
;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Maintainer: Daniel Muñoz <demunoz2@uc.cl>
;; URL: https://github.com/koprotk/eglot-java-helpers
;; Keywords: java, eglot, convenience, languages
;; Package-Requires: ((emacs "29.1") (eglot "1.9") (flymake "1.2"))

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
;; - `eglot-helpers-java-debug-mvn-test-method-now' - Launch JDB in listen mode, then run Maven test
;; - `eglot-helpers-java-launch-jdb' - Attach JDB on port 8000 in another window
;; - `eglot-helpers-java-gud-jdb-break' - Set JDB breakpoint at current line it's mimic the behaviour of the standard 'grud-break'
;; - `eglot-helpers-java-get-fqcn' - Get fully qualified class name
;; - `eglot-helpers-java-get-fqmn' - Get fully qualified method name
;; - `eglot-helpers-java-flymake-branch-diagnostics' - Show Flymake diagnostics only for branch-changed files
;; - `eglot-helpers-java--branch-changed-files' - Get files touched by branch-only commits

;;; Code:

(require 'eglot)
(require 'cl-lib)
(require 'gud)
(require 'flymake)

(defgroup eglot-helpers-java nil
  "Helper functions for Java with Eglot."
  :group 'eglot
  :prefix "eglot-helpers-java-")

(defcustom eglot-helpers-java-lombok-jar-path nil
  "Path to the Lombok JAR file for JDTLS.
When set, Lombok will be enabled as a Java agent for the language server.
Example: \"~/.m2/repository/org/projectlombok/lombok/1.18.36/lombok-1.18.36.jar\""
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Path to lombok.jar"))
  :group 'eglot-helpers-java)

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
        (compile (format "./mvnw -Dtest=%s test" (eglot-helpers-java--get-fqnm-at-point nil))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-run-mvn-test-method ()
  "Run the Maven test for the method at point."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile (format "./mvnw -Dtest=%s test" (eglot-helpers-java--get-fqnm-at-point t))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-build-mvn-project-skiptests ()
  "Build the Maven project, skipping tests."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile "./mvnw clean package -DskipTests -U"))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-debug-mvn-test-method-now ()
  "Launch JDB in listen mode first, then run the Maven test connecting to it.
JDB listens on port 8000; Maven starts with server=n so the JVM connects to JDB."
  (interactive)
  (if-let ((project (project-current)))
      (let* ((project-dir (project-root project))
             (default-directory project-dir)
             (sourcepath (concat project-dir "src/main/java"
                                 ":"
                                 project-dir "src/test/java"))
             (fqmn (eglot-helpers-java--get-fqnm-at-point t))
             (orig-window (selected-window))
             (orig-buffer (current-buffer)))
        (jdb (format "jdb -listen 8000 -sourcepath%s" sourcepath))
        (let ((jdb-buffer (current-buffer)))
          (switch-to-buffer orig-buffer)
          (select-window orig-window)
          (display-buffer jdb-buffer '(display-buffer-use-some-window (inhibit-same-window . t)))
          (compile
           (format "./mvnw -Dmaven.surefire.debug=-agentlib:jdwp=transport=dt_socket,server=n,suspend=y,address=8000 -Dtest=%s test"
                   fqmn))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-debug-mvn-test-method ()
  "Run the test at point in debug mode."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile
         (format "./mvnw -Dmaven.surefire.debug=-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=8000 -Dtest=%s test"
                 (eglot-helpers-java--get-fqnm-at-point t))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-launch-jdb ()
  "Attach JDB to port 8000 with project source and class paths, opening in another window.
Use after starting a debug session with `eglot-helpers-java-debug-mvn-test-method'."
  (interactive)
  (if-let ((project (project-current)))
      (let* ((default-directory (project-root project))
             (sourcepath (concat (project-root project) "src/main/java"
                                 ":"
                                 (project-root project) "src/test/java"))
             (orig-window (selected-window))
             (orig-buffer (current-buffer)))
        (jdb (format "jdb -attach 8000 -sourcepath%s" sourcepath))
        (let ((jdb-buffer (current-buffer)))
          (switch-to-buffer orig-buffer)
          (select-window orig-window)
          (display-buffer jdb-buffer '(display-buffer-use-some-window (inhibit-same-window . t)))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-gud-jdb-break ()
  "Create breakpoint for jdb at the current line."
  (interactive)
  (if-let ((class (eglot-helpers-java--get-fqnm-at-point nil)))
      (gud-call (concat "stop at " class ":%l") 1)
    (message "Could not determine class name.")))

(defcustom eglot-helpers-java-base-branch "develop"
  "Base branch used to determine changed files for diagnostics filtering."
  :type 'string
  :group 'eglot-helpers-java)

(defun eglot-helpers-java--branch-changed-files ()
  "Return list of files touched by commits unique to this branch.
Uses the merge-base with `eglot-helpers-java-base-branch' to find
only commits that belong to the current branch, then collects all
files modified by those commits."
  (let* ((merge-base (string-trim
                      (shell-command-to-string
                       (format "git merge-base HEAD %s"
                               eglot-helpers-java-base-branch))))
         (files (split-string
                 (shell-command-to-string
                  (format "git log --name-only --pretty=format: %s..HEAD"
                          merge-base))
                 "\n" t)))
    (delete-dups files)))

;;;###autoload
(defun eglot-helpers-java-flymake-branch-diagnostics ()
  "Show Flymake project diagnostics filtered to files changed on this branch.
Identifies changed files via commits unique to the current branch
relative to `eglot-helpers-java-base-branch'.  Queries all Eglot
managed buffers (including those JDTLS opened for workspace
diagnostics) so files don't need to be visited manually."
  (interactive)
  (if-let ((server (eglot-current-server)))
      (let* ((project (eglot--project server))
             (root (project-root project))
             (default-directory root)
             (changed-files (eglot-helpers-java--branch-changed-files))
             (basenames (mapcar #'file-name-nondirectory changed-files)))
        (if (null changed-files)
            (message "No files changed vs %s" eglot-helpers-java-base-branch)
          (let ((diags '())
                (managed (eglot--managed-buffers server)))
            ;; Collect diagnostics from Eglot-managed buffers matching branch files
            (dolist (buf managed)
              (when (buffer-live-p buf)
                (when-let* ((file (buffer-file-name buf))
                            (name (file-name-nondirectory file))
                            (_match (member name basenames)))
                  (with-current-buffer buf
                    (when (bound-and-true-p flymake-mode)
                      (dolist (d (flymake-diagnostics))
                        (push (list d (vector
                                       (propertize name 'face 'font-lock-function-name-face)
                                       (number-to-string (line-number-at-pos
                                                          (flymake-diagnostic-beg d)))
                                       (pcase (flymake-diagnostic-type d)
                                         ('eglot-error (propertize "error" 'face 'error))
                                         ('eglot-warning (propertize "warning" 'face 'warning))
                                         ('eglot-note (propertize "note" 'face 'shadow))
                                         (_ (format "%s" (flymake-diagnostic-type d))))
                                       (flymake-diagnostic-text d)))
                              diags)))))))
            (if (null diags)
                (message "No diagnostics found in %d branch file(s) (%d managed buffers)"
                         (length changed-files) (length managed))
              (let ((buf (get-buffer-create "*Branch Flymake diagnostics*")))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (tabulated-list-mode)
                    (setq tabulated-list-format
                          [("File" 30 t) ("Line" 6 t) ("Type" 10 t) ("Message" 0 t)])
                    (setq tabulated-list-entries (reverse diags))
                    (tabulated-list-init-header)
                    (tabulated-list-print)
                    (setq-local revert-buffer-function
                                (lambda (_ignore-auto _noconfirm)
                                  (eglot-helpers-java-flymake-branch-diagnostics)))))
                (display-buffer buf)
                (message "Showing %d diagnostics from %d branch file(s)"
                         (length diags) (length changed-files)))))))
    (message "No active Eglot server. Open a Java file first.")))


;; JDTLS customization for Eglot

;; Prevent Eglot from honoring JDTLS's workspace/didChangeWatchedFiles
;; registration, which creates thousands of file-notify watches and
;; exhausts file descriptors on large projects.
(with-eval-after-load 'eglot
  (cl-defmethod eglot-register-capability
    (_server (_method (eql workspace/didChangeWatchedFiles)) _id &key _watchers)
    nil))

(add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) .
                 ("jdtls"
                  "-Xms2G"
                  "-Xmx6G"
                  "-XX:+UseZGC"
                  "-XX:+ZGenerational"
                  "-XX:+AlwaysPreTouch"
                  "-XX:+UseStringDeduplication"
                  ,@(when eglot-helpers-java-lombok-jar-path
                      (list (concat "--jvm-arg=-javaagent:" eglot-helpers-java-lombok-jar-path)))
                  "--add-modules=ALL-SYSTEM"
                  "--add-opens" "java.base/java.util=ALL-UNNAMED"
                  "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                  :initializationOptions
                  (:extendedClientCapabilities
                   (:classFileContentsSupport t
                    :resolveAdditionalTextEditsSupport t
                    :progressReportProvider t)
                   :bundles []
                   :settings
                   (:java
                    (:format
                     (:enabled t
                      :comments (:enabled t))
                     :insertSpaces t
                     :tabSize 4)
                    :completion
                    (:enabled t
                     :favoriteStaticMembers ["org.testng.Assert.*"
                                             "org.junit.Assert.*"
                                             "org.junit.Assume.*"
                                             "org.junit.jupiter.api.Assertions.*"
                                             "org.junit.jupiter.api.Assumptions.*"
                                             "org.mockito.Mockito.*"
                                             "org.mockito.ArgumentMatchers.*"
                                             "org.mockito.Answers.*"]
                     :filteredTypes ["com.sun.*"
                                    "java.awt.*"
                                    "jdk.*"
                                    "sun.*"
                                    "org.graalvm.*"]
                     :importOrder ["java" "javax" "org" "com"]
                     :guessMethodArguments t
                     :maxResults 0
                     :postfix (:enabled t))
                    :signatureHelp (:enabled t :description (:enabled t))
                    :contentProvider (:preferred "fernflower")
                    :autobuild (:enabled t)
                    :maven (:downloadSources t :updateSnapshots t)
                    :implementationsCodeLens (:enabled t)
                    :referencesCodeLens (:enabled t)
                    :references (:includeDecompiledSources t)
                    :inlayHints
                    (:parameterNames (:enabled "all"))
                    :codeGeneration
                    (:hashCodeEquals (:useJava7Objects t :useInstanceof t)
                     :useBlocks t
                     :generateComments t
                     :toString
                     (:template "${object.className} [${member.name()}=${member.value}, ${otherMembers}]"
                      :codeStyle "STRING_CONCATENATION"
                      :skipNullValues :json-false
                      :listArrayContents t
                      :limitElements 0))
                    :saveActions
                    (:organizeImports t)
                    :sources
                    (:organizeImports
                     (:starThreshold 99
                      :staticStarThreshold 99))
                    :import (:gradle (:wrapper (:enabled t))
                             :maven (:enabled t)
                             :exclusions ["**/node_modules/**"
                                         "**/.metadata/**"
                                         "**/archetype-resources/**"
                                         "**/META-INF/maven/**"])
                    :eclipse (:downloadSources t)
                    :configuration
                    (:updateBuildConfiguration "automatic"
                     :runtimes [(:name "JavaSE-21" :default t)]))))))

(provide 'eglot-helpers-java)
;;; eglot-helpers-java.el ends here
