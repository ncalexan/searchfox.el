;;; searchfox.el --- A front-end for searchfox.org - The Mozilla Source Code Indexer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nicholas Alexander

;; Author: Nicholas Alexander <nalexander@mozilla.com>
;; Keywords: tools, matching
;; Version: 0.1

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

;; A front-end for searchfox.org - The Mozilla Source Code Indexer.
;;
;; Heavily (!) influenced by ag.el.

;;; Code:

;; TODO:

;; - Tests! Have to figure out how to capture input and compare output.
;; - Make "Files" section display nicer.
;; - Add searchfox-dwim that guesses if needle is a path or a regexp.
;; - Extract faces and add more configuration for how pieces are displayed.
;; - Generate the `jq` command with quoting so that it's easier to read.

(require 'compile)
(require 'seq) ; for seq-doseq.
(require 'subr-x) ; for string-empty-p.

(defgroup searchfox nil
  "A front-end for searchfox.org - The Mozilla Source Code Indexer."
  :group 'tools
  :group 'matching)

(defvar searchfox-search-url "https://searchfox.org/mozilla-central/search"
  "URL of searchfox service endpoint.")

(defvar searchfox-topsrcdir "/Users/nalexander/Mozilla/gecko/"
  "Intepret searchfox results relative to this top source directory.")

(defvar searchfox-history-list)

(defcustom searchfox-reuse-buffers nil
  "Non-nil means we reuse the existing searchfox results buffer.

Otherwise, create one buffer per unique search."
  :type 'boolean
  :group 'searchfox)

(defun searchfox/buffer-name (needle regexp path)
  "Return a buffer name formatted according to searchfox.el conventions."
  (let ((path-str (if (and path (not (string= path "./")))
                      (format " path:%s" path)
                    "")))
    (cond
     (searchfox-reuse-buffers "*searchfox*")
     (regexp (format "*searchfox regexp:%s%s*" needle path-str))
     (:else (format "*searchfox text:%s%s*" needle path-str)))))

;; Although searchfox results aren't exactly errors, we treat them as errors
;; so `next-error' and `previous-error' work. However, we ensure our
;; face inherits from `compilation-info-face' so the results are
;; styled appropriately.
(defface searchfox-hit-face '((t :inherit compilation-info))
  "Face name to use for searchfox matches."
  :group 'searchfox)

(defface searchfox-match-face '((t :inherit match))
  "Face name to use for searchfox matches."
  :group 'searchfox)

(defvar searchfox-search-finished-hook nil
  "Hook run when searchfox completes a search in a buffer.")

(defun searchfox/run-finished-hook (buffer how-finished)
  "Run the searchfox hook to signal that the search has completed."
  (with-current-buffer buffer
    (run-hooks 'searchfox-search-finished-hook)))

;; Based on ag-filter and grep-filter.
(defun searchfox-filter ()
  "Handle the searchfox process output.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part
      ;; of a match in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight searchfox matches using searchfox's own `bounds`
        ;; mechanism.
        (while (re-search-forward "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([^\u0000]*\\)\u0000\\(.*\\)" end 1)
          (let ((beg (string-to-number (match-string 2)))
                (end (string-to-number (match-string 3))))
            (replace-match (concat
                            (match-string 1)
                            ":"
                            (substring (match-string 5) 0 beg)
                            (propertize (substring (match-string 5) beg end)
                                        'face nil 'font-lock-face 'searchfox-match-face)
                            (substring (match-string 5) end)
                            (when (not (string-empty-p (match-string 4)))
                              (propertize (concat " // found in " (match-string 4))
                                          'face nil 'font-lock-face 'font-lock-preprocessor-face)))
                           t t)))
        ;; Insert search result types, add marker at start of line for
        ;; files. This is used by the match in
        ;; `compilation-error-regexp-alist' to extract the file name.
        (goto-char beg)
        (while (re-search-forward "^Type: \\(.*\\)" end 1)
          (replace-match
           (concat "" (propertize (match-string 1)
                                  'face nil 'font-lock-face 'font-lock-keyword-face)
                   "\n")
           t t))
        (goto-char beg)
        (while (re-search-forward "^File: \\(.*\\)" end 1)
          (replace-match
           (concat "File: " (propertize (match-string 1) 'face nil 'font-lock-face
                                        'compilation-info))
           t t))
        (goto-char beg)))))

(defun searchfox/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (searchfox/dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

(defvar searchfox/pattern
  "^\\([[:digit:]]+\\):"
  "A regexp pattern to match line number (with grouped output).")

(defun searchfox/compilation-match-grouped-filename ()
  "Match filename backwards when a line match is found (with grouped output)."
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(define-compilation-mode searchfox-mode "Searchfox"
  "Searchfox results compilation mode"
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(compilation-searchfox))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'compilation-searchfox  (list searchfox/pattern
                                                 'searchfox/compilation-match-grouped-filename 1))))
  (set (make-local-variable 'compilation-error-face) 'searchfox-hit-face)
  (set (make-local-variable 'compilation-finish-functions)
       #'searchfox/run-finished-hook)
  (add-hook 'compilation-filter-hook 'searchfox-filter nil t))

(define-key searchfox-mode-map (kbd "p") #'compilation-previous-error)
(define-key searchfox-mode-map (kbd "n") #'compilation-next-error)
(define-key searchfox-mode-map (kbd "k") '(lambda () (interactive)
                                            (let (kill-buffer-query-functions) (kill-buffer))))

;;;###autoload
(defun searchfox (string &optional regexp path)
  "Query searchfox for STRING, with STRING defaulting to the symbol under point.

If REGEXP is truth-y, interpret STRING as a regular expression.

If PATH is not NIL, restrict to PATH.  PATH may contain globs of the form * and **."
  (let* ((default-directory searchfox-topsrcdir)
         (params `((q ,string)
                   (regexp ,(if regexp "true" "false"))
                   (path ,(if (and path (not (string= path "./"))) path ""))))
         (url (format "%s?%s" searchfox-search-url (url-build-query-string params nil t)))
         (cmd (format
               "curl --silent '%s' -H 'Accept: application/json' | jq -j '\"\\n\", (with_entries(select(.key | startswith(\"*\") | not)) | .[] | to_entries | .[] | (\"Type: \" + .key + \"\\n\"), (.value | .[] | (\"File: \" + .path + \"\\n\"), (.lines[] // {\"lno\": 1, \"bounds\": [0, 0]} | ((.lno | tostring) + \":\" + (.bounds[0] | tostring) + \":\" + (.bounds[1] | tostring) + \":\" + .context + \"\\u0000\" + .line + \"\\n\")), \"\\n\"))'"
               url)))
    (compilation-start
     cmd
     #'searchfox-mode
     `(lambda (mode-name) ,(searchfox/buffer-name string regexp path)))))

(defun searchfox/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun searchfox/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (searchfox/dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil 'searchfox-history-list suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

;;;###autoload
(defun sf (string)
  "Query searchfox for STRING."
  (interactive (list (searchfox/read-from-minibuffer "Searchfox string")))
  (searchfox string))

;;;###autoload
(defun sfp (string path)
  "Query searchfox for STRING, restricting to PATH.

PATH may contain globs of the form * and **."
  (interactive (list (searchfox/read-from-minibuffer "Searchfox string")
                     (file-relative-name
                      (read-directory-name "Path glob: " searchfox-topsrcdir)
                      searchfox-topsrcdir)))
  (searchfox string nil path))

;;;###autoload
(defun sfr (regexp)
  "Query searchfox for REGEXP."
  (interactive (list (searchfox/read-from-minibuffer "Searchfox regexp")))
  (searchfox regexp t))

;;;###autoload
(defun sfrp (regexp path)
  "Query searchfox for REGEXP, restricting to PATH.

PATH may contain globs of the form * and **."
  (interactive (list (searchfox/read-from-minibuffer "Searchfox regexp")
                     (file-relative-name
                      (read-directory-name "Path glob: " searchfox-topsrcdir)
                      searchfox-topsrcdir)))
  (searchfox regexp t path))

;;;###autoload
(defun searchfox-kill-buffers ()
  "Kill all `searchfox-mode' buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'searchfox-mode)
      (kill-buffer buffer))))

;;;###autoload
(defun searchfox-kill-other-buffers ()
  "Kill all `searchfox-mode' buffers other than the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and
             (eq (buffer-local-value 'major-mode buffer) 'searchfox-mode)
             (not (eq buffer current-buffer)))
        (kill-buffer buffer)))))

(provide 'searchfox)
;;; searchfox.el ends here
