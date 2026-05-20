;;; eglot-helpers-java.el --- Java/Eglot helpers: LSP test running and dape debugging -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Muñoz

;; Version: 0.3
;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Maintainer: Daniel Muñoz <demunoz2@uc.cl>
;; URL: https://github.com/koprotk/eglot-java-helpers
;; Keywords: java, eglot, convenience, languages
;; Package-Requires: ((emacs "29.1") (eglot "1.9") (flymake "1.2") (dape "0.1"))

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

;; Usage:
;; (require 'eglot-helpers-java)
;;
;; On first use, call `eglot-helpers-java-ensure-bundles' (or just open
;; a Java file -- the server contact calls it automatically).
;;
;; Key functions:
;; - `eglot-helpers-java-ensure-bundles'       Download plugins if missing
;; - `eglot-helpers-java-upgrade-bundles'      Upgrade plugins to latest
;; - `eglot-helpers-java-run-test-class'       Run tests for class at point
;; - `eglot-helpers-java-run-test-method'      Run test method at point
;; - `eglot-helpers-java-mvn-build-project-skiptests' Build without tests
;; - `eglot-helpers-java-debug-test-method'    Debug test method via dape
;; - `eglot-helpers-java-list-java-commands'     List JDTLS executeCommand handlers
;; - `eglot-helpers-java-reload-bundles'        Hot-reload plugins (no restart)
;; - `eglot-helpers-java-restart-server'        Restart JDTLS
;; - `eglot-helpers-java-restart-server-clean'  Restart clearing OSGi cache
;; - `eglot-helpers-java-force-rebuild'         Force JDTLS workspace rebuild
;; - `eglot-helpers-java-flymake-branch-diagnostics' Branch-scoped diagnostics
;;
;; If `vscode.java.test.junit.argument' reports no delegate handler:
;;   1) M-x eglot-helpers-java-reload-bundles     (hot-reload, no restart)
;;   2) M-x eglot-helpers-java-restart-server-clean  (clears OSGi cache)

;;; Code:

(require 'eglot)
(require 'cl-lib)
(require 'flymake)
(require 'dape)
(require 'url)


;;;; ─── Customization ─────────────────────────────────────────────────────────

(defgroup eglot-helpers-java nil
  "LSP-driven Java helpers for Eglot."
  :group 'eglot
  :prefix "eglot-helpers-java-")

(defcustom eglot-helpers-java-bundles-dir
  (expand-file-name "java-bundles" user-emacs-directory)
  "Directory where JDTLS plugin JARs are stored.
Both the debug and test plugin JARs, plus any auto-downloaded Lombok
JAR, are placed here."
  :type 'directory
  :group 'eglot-helpers-java)

(defcustom eglot-helpers-java-lombok-jar-path nil
  "Path to the Lombok JAR for JDTLS.
When nil (the default), Lombok is auto-detected from the project's
pom.xml and the JAR is resolved from ~/.m2 or downloaded automatically.
Set this to override auto-detection."
  :type '(choice (const  :tag "Auto-detect from pom.xml" nil)
                 (file   :tag "Path to lombok.jar"))
  :group 'eglot-helpers-java)

(defcustom eglot-helpers-java-debug-port 5005
  "JDWP port used when falling back to Maven surefire for test debugging.
The JVM listens on this port; dape attaches via JDTLS debug adapter."
  :type 'integer
  :group 'eglot-helpers-java)

(defcustom eglot-helpers-java-base-branch "develop"
  "Base branch used to determine changed files for diagnostics filtering."
  :type 'string
  :group 'eglot-helpers-java)


;;;; ─── Bundle management ─────────────────────────────────────────────────────

(defun eglot-helpers-java--installed-jar (prefix)
  "Return path of the first JAR matching PREFIX*.jar in `eglot-helpers-java-bundles-dir', or nil."
  (car (file-expand-wildcards
        (expand-file-name (concat prefix "*.jar")
                          eglot-helpers-java-bundles-dir))))

(defun eglot-helpers-java--jar-version (jar-path prefix)
  "Extract the version string from JAR-PATH by stripping PREFIX- and .jar."
  (when jar-path
    (let ((fname (file-name-nondirectory jar-path)))
      (when (string-match (concat (regexp-quote prefix) "-\\(.+\\)\\.jar\\'") fname)
        (match-string 1 fname)))))

(defun eglot-helpers-java--http-json (url &optional extra-headers)
  "Fetch URL with EXTRA-HEADERS, parse the JSON body, return result or nil."
  (condition-case err
      (let* ((url-request-extra-headers (append extra-headers url-request-extra-headers))
             (url-user-agent "eglot-helpers-java/0.1")
             (buf (url-retrieve-synchronously url t t 15)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil t)
                (json-parse-buffer :object-type 'plist
                                   :array-type  'array
                                   :null-object  nil
                                   :false-object nil))
            (kill-buffer buf))))
    (error
     (message "eglot-helpers-java: HTTP request failed (%s): %s"
              url (error-message-string err))
     nil)))

(defun eglot-helpers-java--fetch-latest-debug-version ()
  "Return the latest com.microsoft.java.debug.plugin version from Maven Central."
  (when-let* ((json (eglot-helpers-java--http-json
                     "https://search.maven.org/solrsearch/select?q=g:com.microsoft.java+AND+a:com.microsoft.java.debug.plugin&rows=1&wt=json&core=gav"))
              (docs (plist-get (plist-get json :response) :docs))
              ((> (length docs) 0)))
    (plist-get (aref docs 0) :v)))

(defun eglot-helpers-java--fetch-latest-test-release ()
  "Return plist (:version VERSION :url VSIX-URL) for the latest vscode-java-test release.
Version is read from the GitHub releases API; the .vsix is downloaded from
the VS Code Marketplace (the GitHub release has no binary assets)."
  (when-let* ((json (eglot-helpers-java--http-json
                     "https://api.github.com/repos/microsoft/vscode-java-test/releases/latest"
                     '(("Accept" . "application/vnd.github+json"))))
              (tag     (plist-get json :tag_name))
              (version (string-trim-left tag "v")))
    (list :version version
          :url (format "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/vscjava/vsextensions/vscode-java-test/%s/vspackage"
                       version))))

(defun eglot-helpers-java--download-debug-plugin (version)
  "Download com.microsoft.java.debug.plugin VERSION to `eglot-helpers-java-bundles-dir'."
  (make-directory eglot-helpers-java-bundles-dir t)
  (let* ((url  (format "https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/%s/com.microsoft.java.debug.plugin-%s.jar"
                       version version))
         (dest (expand-file-name
                (format "com.microsoft.java.debug.plugin-%s.jar" version)
                eglot-helpers-java-bundles-dir)))
    (message "eglot-helpers-java: downloading java-debug %s..." version)
    (url-copy-file url dest t)
    (message "eglot-helpers-java: java-debug %s installed." version)
    dest))

(defun eglot-helpers-java--gzip-file-p (file)
  "Return t if FILE starts with gzip magic bytes (\\x1f\\x8b)."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 2)
    (and (= (buffer-size) 2)
         (= (char-after 1) #x1f)
         (= (char-after 2) #x8b))))

(defun eglot-helpers-java--download-test-plugin (version vsix-url)
  "Download vscode-java-test VERSION from VSIX-URL and extract its JAR to bundles dir."
  (make-directory eglot-helpers-java-bundles-dir t)
  (let* ((tmp-vsix (make-temp-file "vscode-java-test-" nil ".vsix"))
         (tmp-dir  (make-temp-file "vscode-java-test-extract-" t))
         dest-jar)
    (message "eglot-helpers-java: downloading vscode-java-test %s..." version)
    (url-copy-file vsix-url tmp-vsix t)
    ;; The Marketplace may serve the vsix gzip-compressed.  url-copy-file
    ;; sometimes auto-decompresses and sometimes does not, depending on the
    ;; Emacs build.  Check the magic bytes and gunzip only when needed.
    (when (eglot-helpers-java--gzip-file-p tmp-vsix)
      (let ((gz-path (concat tmp-vsix ".gz")))
        (rename-file tmp-vsix gz-path)
        (call-process "gunzip" nil nil nil gz-path)))
    (call-process "unzip" nil nil nil "-q" tmp-vsix "-d" tmp-dir)
    (if-let ((jar (car (file-expand-wildcards
                        (expand-file-name
                         "extension/server/com.microsoft.java.test.plugin*.jar"
                         tmp-dir)))))
        (let ((fname (file-name-nondirectory jar)))
          (setq dest-jar (expand-file-name fname eglot-helpers-java-bundles-dir))
          (copy-file jar dest-jar t)
          (message "eglot-helpers-java: vscode-java-test %s installed." version))
      (message "eglot-helpers-java: could not find test plugin JAR inside vsix."))
    (ignore-errors (delete-file tmp-vsix))
    (ignore-errors (delete-directory tmp-dir t))
    dest-jar))

;;;###autoload
(defun eglot-helpers-java-ensure-bundles ()
  "Download java-debug and vscode-java-test JARs if not already installed."
  (interactive)
  (let ((debug-jar (eglot-helpers-java--installed-jar "com.microsoft.java.debug.plugin"))
        (test-jar  (eglot-helpers-java--installed-jar "com.microsoft.java.test.plugin")))
    (if (and debug-jar test-jar)
        (when (called-interactively-p 'interactive)
          (message "eglot-helpers-java: bundles already installed (%s, %s)."
                   (file-name-nondirectory debug-jar)
                   (file-name-nondirectory test-jar)))
      (unless debug-jar
        (if-let ((version (eglot-helpers-java--fetch-latest-debug-version)))
            (eglot-helpers-java--download-debug-plugin version)
          (message "eglot-helpers-java: could not fetch java-debug version from Maven Central.")))
      (unless test-jar
        (if-let ((release (eglot-helpers-java--fetch-latest-test-release)))
            (eglot-helpers-java--download-test-plugin
             (plist-get release :version)
             (plist-get release :url))
          (message "eglot-helpers-java: could not fetch vscode-java-test release from GitHub."))))))

;;;###autoload
(defun eglot-helpers-java-upgrade-bundles ()
  "Upgrade java-debug and vscode-java-test JARs to latest versions if outdated."
  (interactive)
  (let* ((debug-jar     (eglot-helpers-java--installed-jar "com.microsoft.java.debug.plugin"))
         (debug-version (eglot-helpers-java--jar-version debug-jar "com.microsoft.java.debug.plugin"))
         (debug-latest  (eglot-helpers-java--fetch-latest-debug-version)))
    (cond
     ((null debug-latest)
      (message "eglot-helpers-java: could not fetch java-debug version."))
     ((or (null debug-version) (version< debug-version debug-latest))
      (message "eglot-helpers-java: upgrading java-debug %s → %s"
               (or debug-version "none") debug-latest)
      (when debug-jar (delete-file debug-jar))
      (eglot-helpers-java--download-debug-plugin debug-latest))
     (t
      (message "eglot-helpers-java: java-debug %s is up to date." debug-version))))
  (let* ((test-jar     (eglot-helpers-java--installed-jar "com.microsoft.java.test.plugin"))
         (test-version (eglot-helpers-java--jar-version test-jar "com.microsoft.java.test.plugin"))
         (test-release (eglot-helpers-java--fetch-latest-test-release)))
    (cond
     ((null test-release)
      (message "eglot-helpers-java: could not fetch vscode-java-test release."))
     ((or (null test-version) (version< test-version (plist-get test-release :version)))
      (message "eglot-helpers-java: upgrading vscode-java-test %s → %s"
               (or test-version "none") (plist-get test-release :version))
      (when test-jar (delete-file test-jar))
      (eglot-helpers-java--download-test-plugin
       (plist-get test-release :version)
       (plist-get test-release :url)))
     (t
      (message "eglot-helpers-java: vscode-java-test %s is up to date." test-version)))))


;;;; ─── Lombok auto-detection ─────────────────────────────────────────────────

(defun eglot-helpers-java--lombok-version-from-pom (project-root)
  "Return the Lombok version declared in PROJECT-ROOT/pom.xml, or nil.
Handles simple property references like ${lombok.version}."
  (let ((pom (expand-file-name "pom.xml" project-root)))
    (when (file-readable-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (goto-char (point-min))
        (when (re-search-forward "<artifactId>lombok</artifactId>" nil t)
          (let* ((dep-start (save-excursion
                              (re-search-backward "<dependency" nil t)
                              (point)))
                 (dep-end   (save-excursion
                              (re-search-forward "</dependency>" nil t)
                              (point))))
            (goto-char dep-start)
            (when (re-search-forward "<version>\\([^<]+\\)</version>" dep-end t)
              (let ((version (match-string 1)))
                ;; Resolve one level of ${property} references from <properties>
                (if (string-prefix-p "${" version)
                    (let ((prop (substring version 2 (- (length version) 1))))
                      (goto-char (point-min))
                      (when (re-search-forward
                             (concat "<" (regexp-quote prop) ">\\([^<]+\\)</"
                                     (regexp-quote prop) ">")
                             nil t)
                        (match-string 1)))
                  version)))))))))

(defun eglot-helpers-java--ensure-lombok-jar (project-root)
  "Return path to Lombok JAR for PROJECT-ROOT, downloading if necessary.
Returns nil when no Lombok dependency is found or if download fails."
  (or
   ;; 1. Manual override wins.
   (and eglot-helpers-java-lombok-jar-path
        (expand-file-name eglot-helpers-java-lombok-jar-path))
   ;; 2. Auto-detect from pom.xml.
   (when-let ((version (eglot-helpers-java--lombok-version-from-pom project-root)))
     (let ((m2-jar (expand-file-name
                    (format "org/projectlombok/lombok/%s/lombok-%s.jar" version version)
                    (expand-file-name "~/.m2/repository/")))
           (cache-jar (expand-file-name
                       (format "lombok-%s.jar" version)
                       eglot-helpers-java-bundles-dir)))
       (cond
        ((file-readable-p m2-jar)   m2-jar)
        ((file-readable-p cache-jar) cache-jar)
        (t
         (condition-case err
             (progn
               (make-directory eglot-helpers-java-bundles-dir t)
               (message "eglot-helpers-java: downloading lombok %s..." version)
               (url-copy-file
                (format "https://repo1.maven.org/maven2/org/projectlombok/lombok/%s/lombok-%s.jar"
                        version version)
                cache-jar t)
               (message "eglot-helpers-java: lombok %s installed." version)
               cache-jar)
           (error
            (message "eglot-helpers-java: lombok download failed: %s"
                     (error-message-string err))
            nil))))))))


;;;; ─── FQCN / FQMN resolution ────────────────────────────────────────────────

(defun eglot-helpers-java--get-fqnm-at-point (with-method)
  "Return the fully qualified class name at point.
When WITH-METHOD is non-nil, include the method name (format: pkg.Class#method)."
  (let* ((imenu-list   (eglot-imenu))
         (package      (substring-no-properties (car (car imenu-list))))
         (class        (substring-no-properties (car (car (cdr imenu-list)))))
         (methods      (cdr (car (cdr imenu-list))))
         (method-found nil))
    (cl-dolist (obj methods)
      (let* ((name   (car obj))
             (kind   (get-text-property 0 'breadcrumb-kind name))
             (region (get-text-property 0 'breadcrumb-region name)))
        (when (and region (string= kind "Method"))
          (let ((start (car region))
                (end   (cdr region)))
            (when (and (>= (point) start) (<= (point) end))
              (setq method-found
                    (replace-regexp-in-string "[()]" "" (substring-no-properties name)))
              (cl-return))))))
    (cond
     ((and with-method method-found) (concat package "." class "#" method-found))
     ((and package class)            (concat package "." class))
     (t nil))))

;;;###autoload
(defun eglot-helpers-java-get-fqcn ()
  "Return the fully qualified class name at point."
  (eglot-helpers-java--get-fqnm-at-point nil))

;;;###autoload
(defun eglot-helpers-java-get-fqmn ()
  "Return the fully qualified method name at point."
  (eglot-helpers-java--get-fqnm-at-point t))


;;;; ─── Test running via LSP ──────────────────────────────────────────────────

;; Test levels as defined in JDTLS ITestItemConstants:
;;   ROOT=0  FOLDER=1  PACKAGE=2  CLASS=3  METHOD=4
;; testKind: JUnit=0  TestNG=1  JUnit5=2

(defun eglot-helpers-java--test-launch-args (fqcn test-level)
  "Ask JDTLS for JVM launch arguments to run FQCN at TEST-LEVEL.
TEST-LEVEL: 3 = class, 4 = method.
Returns a plist with :classpath, :mainClass, :vmArguments, :programArguments."
  (let ((server (eglot-current-server)))
    (unless server
      (user-error "No active Eglot server — open a Java file first"))
    (condition-case err
        (eglot-execute-command
         server
         "vscode.java.test.junit.argument"
         (vector (list :testLevel  test-level
                       :testNames  (vector fqcn)
                       :testKind   0)))
      (error
       (error "JDTLS test plugin unavailable: %s" (error-message-string err))))))

(defun eglot-helpers-java--run-test (fqcn test-level)
  "Run FQCN using LSP-resolved launch args in the compile buffer.
Falls back to Maven (`mvnw' or `mvn') when the test plugin is not loaded.
TEST-LEVEL: 3 = class, 4 = method."
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (condition-case _err
            (let* ((args      (eglot-helpers-java--test-launch-args fqcn test-level))
                   (classpath (mapconcat #'identity
                                         (append (plist-get args :classpath) nil) ":"))
                   (main      (plist-get args :mainClass))
                   (vm-args   (string-join (append (plist-get args :vmArguments) nil) " "))
                   (prog-args (string-join (append (plist-get args :programArguments) nil) " ")))
              (compile (string-trim
                        (format "java %s -cp %s %s %s"
                                vm-args classpath main prog-args))
                       t))
          ;; Test plugin not loaded — fall back to Maven (same format as v1).
          ;; FQCN/FQMN already uses the '#' separator that Maven -Dtest= expects.
          (error
           (compile (format "%s -Dtest=%s test" (eglot-helpers-java--mvn-command) fqcn)
                    t))))
    (message "Not inside a known project.")))

;;;###autoload
(defun eglot-helpers-java-run-test-class ()
  "Run tests for the class at point via JDTLS LSP."
  (interactive)
  (eglot-helpers-java--run-test
   (eglot-helpers-java--get-fqnm-at-point nil) 3))

;;;###autoload
(defun eglot-helpers-java-run-test-method ()
  "Run the test method at point via JDTLS LSP."
  (interactive)
  (eglot-helpers-java--run-test
   (eglot-helpers-java--get-fqnm-at-point t) 4))


;;;; ─── Maven helpers (no LSP alternative for build/test invocation) ─────────

(defun eglot-helpers-java--mvn-command ()
  "Return the Maven executable for the current project.
Prefers ./mvnw (project wrapper) over the system mvn."
  (cond
   ((file-executable-p "./mvnw") "./mvnw")
   ((executable-find "mvn")     "mvn")
   (t (user-error "No Maven executable found (no ./mvnw and mvn not on PATH)"))))

;;;###autoload
(defun eglot-helpers-java-mvn-build-project-skiptests ()
  "Build the Maven project, skipping tests.
Uses the project's ./mvnw wrapper when present, otherwise falls back to
the system `mvn'.  JDTLS has no LSP command equivalent to `mvn package'."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (compile (format "%s clean package -DskipTests -U"
                         (eglot-helpers-java--mvn-command))
                 t))
    (message "Not inside a known project.")))


;;;; ─── Debugging via dape ─────────────────────────────────────────────────────

(defun eglot-helpers-java--dape-via-start-debug-session (launch-config)
  "Call `vscode.java.startDebugSession' with LAUNCH-CONFIG, return a dape config.
The dape config prepends the DAP socket connection settings (host, port) to
LAUNCH-CONFIG so dape connects to the debug adapter while the original config
fields (:type, :request, :hostName, :port, etc.) are forwarded as the
attach/launch request body — preserving the JDWP port for attach mode."
  (let* ((server (or (eglot-current-server) (user-error "No active Eglot server")))
         (debug-session
          (condition-case err
              (eglot-execute-command server "vscode.java.startDebugSession"
                                     (vector launch-config))
            (error
             (user-error "vscode.java.startDebugSession failed: %s"
                         (error-message-string err)))))
         ;; Response format varies by debug plugin version:
         ;; {port: N} → (:port N), {debugPort: N} → (:debugPort N), or plain integer
         (dap-port (cond ((numberp debug-session) debug-session)
                         ((plist-get debug-session :port))
                         ((plist-get debug-session :debugPort))
                         (t (user-error "Unexpected startDebugSession response: %S"
                                        debug-session)))))
    ;; host/port (no colon) = where dape connects to the debug adapter.
    ;; The rest of launch-config (:type, :request, :hostName, :port …) becomes
    ;; the attach/launch request body sent to that adapter.
    (append (list 'host "127.0.0.1" 'port dap-port) launch-config)))

(defun eglot-helpers-java--debug-launch-config (fqmn)
  "Build a dape config for debugging test method FQMN via JDTLS.
Uses `vscode.java.test.junit.argument' to resolve JVM launch args, then
`vscode.java.startDebugSession' to start the DAP server."
  (let* ((launch-args (eglot-helpers-java--test-launch-args fqmn 4))
         (dap-launch  (list :type       "java"
                            :request    "launch"
                            :mainClass  (plist-get launch-args :mainClass)
                            :classPaths (vconcat (append (plist-get launch-args :classpath) nil))
                            :vmArgs     (string-join (append (plist-get launch-args :vmArguments) nil) " ")
                            :args       (string-join (append (plist-get launch-args :programArguments) nil) " "))))
    (eglot-helpers-java--dape-via-start-debug-session dap-launch)))

(defun eglot-helpers-java--port-listening-p (port)
  "Return t if PORT has a LISTEN socket on localhost.
Uses `lsof' to inspect the kernel socket table — no TCP connection is made,
so the JDWP handshake is not disturbed."
  (= 0 (call-process "lsof" nil nil nil
                      "-nP" (format "-iTCP:%d" port) "-sTCP:LISTEN")))

(defun eglot-helpers-java--wait-for-jvm-listen (port &optional attempt)
  "Poll every second until PORT is in LISTEN state, then attach dape.
Gives up after 60 attempts (60 seconds)."
  (let ((attempt (or attempt 0)))
    (cond
     ((> attempt 60)
      (message "eglot-helpers-java: timed out waiting for JVM on port %d" port))
     ((eglot-helpers-java--port-listening-p port)
      (message "JVM ready on port %d — attaching dape..." port)
      (run-with-timer
       0.5 nil
       (lambda ()
         (condition-case err
             (dape (eglot-helpers-java--dape-via-start-debug-session
                    (list :type "java" :request "attach"
                          :hostName "localhost" :port port)))
           (error (message "Dape attach failed: %s" (error-message-string err)))))))
     (t
      (run-with-timer 1 nil
                      #'eglot-helpers-java--wait-for-jvm-listen
                      port (1+ attempt))))))

(defun eglot-helpers-java--debug-with-maven (fqmn)
  "Debug FQMN by launching Maven surefire in JDWP listen mode, then attaching dape.
Polls localhost:PORT every second until the JVM opens it, then calls
`vscode.java.startDebugSession' with an attach config so dape can connect."
  (let* ((port eglot-helpers-java-debug-port)
         (debug-arg (format "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=%d" port))
         (cmd (format "%s \"-Dmaven.surefire.debug=%s\" -Dtest=%s test"
                      (eglot-helpers-java--mvn-command) debug-arg fqmn)))
    (compilation-start cmd t)
    (message "Maven debug: waiting for JVM on port %d..." port)
    (run-with-timer 1 nil #'eglot-helpers-java--wait-for-jvm-listen port)))

;;;###autoload
(defun eglot-helpers-java-debug-test-method ()
  "Debug the test method at point using dape + JDTLS DAP server.
Tries the LSP path first (`vscode.java.test.junit.argument' + startDebugSession).
Falls back to Maven surefire in JDWP listen mode when the test plugin is not
loaded, attaching dape via `vscode.java.startDebugSession' in attach mode."
  (interactive)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project))
            (fqmn (eglot-helpers-java--get-fqnm-at-point t)))
        (condition-case _err
            (dape (eglot-helpers-java--debug-launch-config fqmn))
          (error
           (eglot-helpers-java--debug-with-maven fqmn))))
    (message "Not inside a known project.")))


;;;; ─── Branch-scoped Flymake diagnostics ─────────────────────────────────────

(defun eglot-helpers-java--branch-changed-files ()
  "Return files changed by commits unique to the current branch vs `eglot-helpers-java-base-branch'."
  (let* ((merge-base (string-trim
                      (shell-command-to-string
                       (format "git merge-base HEAD %s"
                               eglot-helpers-java-base-branch))))
         (files (split-string
                 (shell-command-to-string
                  (format "git log --name-only --pretty=format: %s..HEAD" merge-base))
                 "\n" t)))
    (delete-dups files)))

;;;###autoload
(defun eglot-helpers-java-flymake-branch-diagnostics ()
  "Show Flymake diagnostics filtered to files changed on the current branch.
Queries all Eglot-managed buffers so files don't need to be visited manually."
  (interactive)
  (if-let ((server (eglot-current-server)))
      (let* ((project       (eglot--project server))
             (root          (project-root project))
             (default-directory root)
             (changed-files (eglot-helpers-java--branch-changed-files))
             (basenames     (mapcar #'file-name-nondirectory changed-files)))
        (if (null changed-files)
            (message "No files changed vs %s" eglot-helpers-java-base-branch)
          (let ((diags   '())
                (managed (eglot--managed-buffers server)))
            (dolist (buf managed)
              (when (buffer-live-p buf)
                (when-let* ((file   (buffer-file-name buf))
                            (name   (file-name-nondirectory file))
                            (_match (member name basenames)))
                  (with-current-buffer buf
                    (when (bound-and-true-p flymake-mode)
                      (dolist (d (flymake-diagnostics))
                        (push (list d
                                    (vector
                                     (propertize name 'face 'font-lock-function-name-face)
                                     (number-to-string
                                      (line-number-at-pos (flymake-diagnostic-beg d)))
                                     (pcase (flymake-diagnostic-type d)
                                       ('eglot-error   (propertize "error"   'face 'error))
                                       ('eglot-warning (propertize "warning" 'face 'warning))
                                       ('eglot-note    (propertize "note"    'face 'shadow))
                                       (_              (format "%s" (flymake-diagnostic-type d))))
                                     (flymake-diagnostic-text d)))
                              diags)))))))
            (if (null diags)
                (message "No diagnostics in %d branch file(s) (%d managed buffers)"
                         (length changed-files) (length managed))
              (let ((buf (get-buffer-create "*Branch Flymake Diagnostics*")))
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
                                (lambda (_a _n)
                                  (eglot-helpers-java-flymake-branch-diagnostics)))))
                (display-buffer buf)
                (message "Showing %d diagnostics from %d branch file(s)"
                         (length diags) (length changed-files)))))))
    (message "No active Eglot server. Open a Java file first.")))


;;;; ─── Server restart ────────────────────────────────────────────────────────

;;;###autoload
(defun eglot-helpers-java-list-java-commands ()
  "Show all workspace/executeCommand handlers registered by JDTLS.
Run this after server startup to see which commands are available.
Look for `vscode.java.test.*' entries (test plugin) and
`vscode.java.startDebugSession' (debug plugin).  If those are absent
the bundle OSGi activation failed; if present the issue is in how we
call the command."
  (interactive)
  (if-let* ((server (eglot-current-server))
            (caps   (eglot--capabilities server))
            (ecp    (plist-get caps :executeCommandProvider))
            (cmds   (append (plist-get ecp :commands) nil)))
      (let ((buf (get-buffer-create "*JDTLS Commands*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "JDTLS executeCommand handlers (%d total):\n\n" (length cmds)))
            (dolist (cmd (sort cmds #'string<))
              (insert "  " cmd "\n"))
            (goto-char (point-min))))
        (display-buffer buf))
    (message "No active Eglot server, or server reported no executeCommandProvider.")))

;;;###autoload
(defun eglot-helpers-java-restart-server ()
  "Restart the JDTLS server for the current project.
Required after `eglot-helpers-java-ensure-bundles' if the server was
already running — JDTLS loads plugin bundles only at startup."
  (interactive)
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server) t))
  (eglot-ensure))

;;;###autoload
(defun eglot-helpers-java-reload-bundles ()
  "Ask JDTLS to hot-reload plugin bundles without restarting the server.
Uses the `java/reloadBundles' JDTLS protocol method.  Call this first
when `vscode.java.test.junit.argument' reports no delegate command handler.
If it still fails, use `eglot-helpers-java-restart-server-clean'."
  (interactive)
  (if-let ((server (eglot-current-server)))
      (let ((bundles (eglot-helpers-java--bundle-vector)))
        (if (zerop (length bundles))
            (message "eglot-helpers-java: no bundles installed — run `eglot-helpers-java-ensure-bundles' first.")
          (condition-case err
              (progn
                ;; java.reloadBundles is a workspace/executeCommand whose first
                ;; argument is the array of bundle paths (arguments[0]).
                (eglot-execute-command server "java.reloadBundles" (vector bundles))
                (message "eglot-helpers-java: reload requested for %d bundle(s). Run `eglot-helpers-java-list-java-commands' to confirm, then retry."
                         (length bundles)))
            (error
             (message "eglot-helpers-java: reload failed (%s). Try `eglot-helpers-java-restart-server-clean'."
                      (error-message-string err))))))
    (message "No active Eglot server.")))

(defvar eglot-helpers-java--clean-on-next-start nil
  "When non-nil, pass `-clean' to jdtls at next startup to discard the OSGi cache.")

;;;###autoload
(defun eglot-helpers-java-restart-server-clean ()
  "Restart JDTLS with `-clean', discarding the OSGi plugin cache.
Use this when `eglot-helpers-java-reload-bundles' does not fix the
\"No delegateCommandHandler\" error.  The `-clean' flag forces JDTLS to
reinstall all bundles from scratch, at the cost of a slower startup."
  (interactive)
  (setq eglot-helpers-java--clean-on-next-start t)
  (message "eglot-helpers-java: next JDTLS start will use -clean (OSGi cache cleared).")
  (eglot-helpers-java-restart-server))


;;;; ─── JDTLS workspace rebuild ───────────────────────────────────────────────

;;;###autoload
(defun eglot-helpers-java-force-rebuild ()
  "Force JDTLS to rebuild the workspace index.
Run after branch switches, pulls, or when methods appear missing."
  (interactive)
  (if-let ((server (eglot-current-server)))
      (progn
        (jsonrpc-request server :java/buildWorkspace :json-false)
        (message "JDTLS workspace rebuild requested."))
    (message "No active Eglot server.")))

(defun eglot-helpers-java--rebuild-after-magit ()
  "Trigger a JDTLS workspace rebuild after Magit refreshes."
  (when (eglot-current-server)
    (eglot-helpers-java-force-rebuild)))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'eglot-helpers-java--rebuild-after-magit))


;;;; ─── JDTLS server configuration ────────────────────────────────────────────

;; Suppress workspace/didChangeWatchedFiles registration: on large projects
;; JDTLS tries to watch thousands of files which exhausts file descriptors.
(with-eval-after-load 'eglot
  (cl-defmethod eglot-register-capability
    (_server (_method (eql workspace/didChangeWatchedFiles)) _id &key _watchers)
    nil))

(defconst eglot-helpers-java--jdtls-settings
  '(:java
    (:format
     (:enabled t
      :comments (:enabled t))
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
      :runtimes [(:name "JavaSE-21" :default t)])))
  "Static JDTLS :settings plist, mirroring the v1 configuration.")

(defun eglot-helpers-java--apply-workspace-configuration ()
  "Expose `eglot-helpers-java--jdtls-settings' to JDTLS via `workspace/configuration'.
JDTLS pulls many settings (runtimes, inlay hints, format, completion,
favouriteStaticMembers, importOrder…) at runtime via
`workspace/configuration' requests, not only from
`initializationOptions.settings' at startup.  Eglot answers those
requests from `eglot-workspace-configuration', so we install the same
plist buffer-locally in every Java buffer."
  (setq-local eglot-workspace-configuration
              eglot-helpers-java--jdtls-settings))

(add-hook 'java-mode-hook    #'eglot-helpers-java--apply-workspace-configuration)
(add-hook 'java-ts-mode-hook #'eglot-helpers-java--apply-workspace-configuration)

(defun eglot-helpers-java--bundle-vector ()
  "Return a vector of installed JDTLS plugin JAR absolute paths."
  (vconcat
   (delq nil
         (list (eglot-helpers-java--installed-jar "com.microsoft.java.debug.plugin")
               (eglot-helpers-java--installed-jar "com.microsoft.java.test.plugin")))))

(defun eglot-helpers-java--server-contact (_interactive)
  "Build the JDTLS server contact list with dynamic bundle paths and auto-Lombok.
Called by Eglot each time a Java LSP server is started.  Ensures bundles
are downloaded before JDTLS launches so they can be loaded at startup."
  (eglot-helpers-java-ensure-bundles)
  (let* ((clean   (prog1 eglot-helpers-java--clean-on-next-start
                    (setq eglot-helpers-java--clean-on-next-start nil)))
         (bundles (eglot-helpers-java--bundle-vector))
         (project (project-current))
         (root    (and project (project-root project)))
         (lombok  (and root (eglot-helpers-java--ensure-lombok-jar root)))
         (lombok-arg (and lombok
                          (concat "--jvm-arg=-javaagent:"
                                  (expand-file-name lombok)))))
    (message "eglot-helpers-java: starting JDTLS with %d bundle(s)%s"
             (length bundles)
             (if clean " [OSGi clean]" ""))
    `("jdtls"
      "-Xms2G"
      "-Xmx6G"
      "-XX:+UseZGC"
      "-XX:+ZGenerational"
      "-XX:+AlwaysPreTouch"
      "-XX:+UseStringDeduplication"
      ,@(when lombok-arg (list lombok-arg))
      ,@(when clean (list "-clean"))
      "--add-modules=ALL-SYSTEM"
      "--add-opens" "java.base/java.util=ALL-UNNAMED"
      "--add-opens" "java.base/java.lang=ALL-UNNAMED"
      :initializationOptions
      (:extendedClientCapabilities
       (:classFileContentsSupport        t
        :resolveAdditionalTextEditsSupport t
        :progressReportProvider          t)
       :bundles  ,bundles
       :settings ,eglot-helpers-java--jdtls-settings))))

(add-to-list 'eglot-server-programs
             '((java-mode java-ts-mode) . eglot-helpers-java--server-contact))


(provide 'eglot-helpers-java)
;;; eglot-helpers-java.el ends here
