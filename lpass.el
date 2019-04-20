;;; lpass.el --- Lastpass mode
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Peterpaul Taekele Klein Haneveld

;; Author: Peterpaul Taekele Klein Haneveld <pp.kleinhaneveld@gmail.com>
;; URL: https://github.com/peterpaul/ansible-vault-string
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (s "1.12.0") (f "0.20.0"))

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (C) 2019 Peterpaul Taekele Klein Haneveld
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:

;; Emacs wrapper around the lastpass cli with tabulated list.

;;; Code:

(defconst lpass-identifiers (list '(:id . ("%ai" "account id"))
                                  '(:name . ("%an" "account name"))
                                  '(:name-including-path . ("%aN" "account name including path"))
                                  '(:user . ("%au" "account user"))
                                  '(:password . ("%ap" "account password"))
                                  '(:modification-time . ("%am" "account modification time"))
                                  '(:last-touch-time . ("%aU" "account last touch time"))
                                  '(:share-name . ("%as" "account share name"))
                                  '(:group-name . ("%ag" "account group name"))
                                  '(:URL . ("%al" "account URL")))
  "Column identifiers for tabulated-list.")

(defconst lpass-identifiers-customize-choice (cons 'choice
                                                   (mapcar (lambda (x) (list 'const x))
                                                           (mapcar (lambda (x) (car x))
                                                                   lpass-identifiers)))
  "Choice of identifiers for customize.")

(defvar-local lpass-current-group nil
  "Current selected account group.
When nil all groups are shown, when set only entries from the
selected group are shown.")

(defcustom lpass-list-format
  (list '(:id 20 nil)
        '(:group-name 32 t)
        '(:user 32 t)
        '(:name 64 t))
  "Format for tabulated-list."
  :group 'lpass
  :type `(repeat (list ,lpass-identifiers-customize-choice
                       (integer :tag "Column width")
                       (choice (boolean :tag "Sort by value?")
                               (function :tag "Sorting predicate")))))

(defcustom lpass-list-default-sort-column
  :group-name
  "Default sort column of tabulated-list."
  :group 'lpass
  :type lpass-identifiers-customize-choice)

(defcustom lpass-user-account-list nil
  "List of lastpass user accounts."
  :group 'lpass
  :type '(repeat (string)))

(defun lpass-mode-format-string ()
  "Generate the format string for tabulated list from `lpass-list-format'."
  (vconcat (mapcar (lambda (x) (cons
                                (nth 1 (alist-get (car x) lpass-identifiers))
                                (cdr x)))
                   lpass-list-format)))

(defun lpass-status ()
  "Checks whether a user is logged in.
Return t when a user is logged in, nil otherwise."
  (interactive)
  (eq (shell-command "lpass status")
      0))

(defun lpass-login (&optional lpass-user)
  "Login LPASS-USER."
  (interactive (list (completing-read "Lastpass user account: "
                                      lpass-user-account-list
                                      nil
                                      'confirm)))
  (unless (lpass-status)
    (async-shell-command (format "lpass login %s" (shell-quote-argument lpass-user)))))

(defun lpass-logout ()
  "Logout."
  (interactive)
  (if (lpass-status)
      (when (y-or-n-p "Are you sure you want to logout?")
        (shell-command "lpass logout -f"))
    (message "lpass: You're not logged in")))

(defun lpass-list-parse-entries-from-buffer (buffer)
  "Parse BUFFER and return a list of entries containing '(GROUP NAME ID)'.
BUFFER can be a buffer or a buffer name, and should contain the output of 'lpass ls'."
  (let (entries)
    (with-current-buffer buffer
      (goto-char (point-max))
      (while (re-search-backward (format "^\\([^~]*\\)~%s$"
                                         (s-join "~"
                                                 (mapcar (lambda (x) "\\([^~]*\\)")
                                                         lpass-list-format)))
                                 nil t)
        (setq entries (cons (list (match-string 1)
                                  (vconcat (mapcar (lambda (x) (match-string x))
                                                   (number-sequence 2 (+ -1 2 (seq-length lpass-list-format))))))
                            entries))))
    entries))

(defun lpass-list-entries ()
  "List all accounts."
  (interactive)
  (unless (lpass-status)
    (error "Not logged in"))
  (let ((cmd (format "lpass ls --format=\"%s~%s\" %s"
                     "%ai"
                     (s-join "~"
                             (mapcar (lambda (x) (nth 0
                                                      (alist-get (car x)
                                                                 lpass-identifiers)))
                                     lpass-list-format))
                     (shell-quote-argument (or lpass-current-group
                                               "")))))
    (with-temp-buffer
      (shell-command cmd
                     (current-buffer)
                     "*lpass errors*")
      (lpass-list-parse-entries-from-buffer (current-buffer)))))

(defvar lpass-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "w" #'lpass-list-copy-password)
    (define-key map "i" #'lpass-list-show)
    (define-key map "l" #'lpass-list-limit-group)
    (define-key map "r" #'lpass-list-reset-group)
    map)
  "Local keymap for `lpass-mode' buffers.")

(define-derived-mode lpass-mode tabulated-list-mode "lpass"
  "\\<lpass-mode-map>
\\{lpass-mode-map}"
  ;; TODO before refresh of contents, reset tabulated-list-format
  (setq-local tabulated-list-format (lpass-mode-format-string))
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key (cdr (alist-get lpass-list-default-sort-column lpass-identifiers)))
  (setq-local tabulated-list-entries #'lpass-list-entries)
  (tabulated-list-init-header))

(defun lpass-list ()
  "Open buffer with lastpass entries."
  (interactive)
  (with-current-buffer (get-buffer-create "*lpass list*")
    (lpass-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun lpass-list--column-value (column-index)
  "Return value column with COLUMN-INDEX for current line."
  (elt (tabulated-list-get-entry (point)) column-index))

(defun lpass-list-limit-group (pos)
  "Set `lpass-current-group' to the group for the account at POS."
  (interactive "d")
  (setq-local lpass-current-group (lpass-list--column-value 1))
  (tabulated-list-revert))

(defun lpass-list-reset-group ()
  "Clear `lpass-current-group'."
  (interactive)
  (setq-local lpass-current-group nil)
  (tabulated-list-revert))

(defun lpass-list-show (pos)
  "Show details of lastpass account at POS."
  (interactive "d")
  (let ((id (tabulated-list-get-id pos)))
    (with-current-buffer (get-buffer-create "*lpass show*")
      (shell-command (format "lpass show %s" (shell-quote-argument id))
                     (current-buffer)
                     "*lpass errors*")
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer)))))

(defun lpass-list-copy-password (pos)
  "Add the password of the account at POS to the kill-ring."
  (interactive "d")
  (let ((id (tabulated-list-get-id pos)))
    (with-temp-buffer
      (shell-command (format "lpass show --password %s" (shell-quote-argument id))
                     (current-buffer)
                     "*lpass errors*")
      (goto-char (point-min))
      (set-mark (point))
      (end-of-line)
      (kill-region (mark) (point)))))

(provide 'lpass)
;;; lpass.el ends here
