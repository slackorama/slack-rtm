;;; slack-rtm.el --- An elisp implementation of the Remember The Milk API

;; Copyright (C) 2010 Seth Mason

;; Author: Seth Mason
;; Created: 18 August 2010
;; Version: 0.0
;; Keywords: remember the milk productivity todo

;; This product uses the Remember The Milk API but is not endorsed or
;; certified by Remember The Milk

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Pull your tasks from RTM and then shove them into a buffer.

;;; Code:

(require 'rtm)
(require 'xml)
(require 'org)
(require 'ido)

(defcustom slack-rtm-default-query "list:Inbox AND status:incomplete"
  "Query to use when getting lists from RTM."
  :group 'slack-rtm
  :type 'string)

(defcustom slack-rtm-buffer-name "*todo*"
  "Name of buffer that is created"
  :group 'slack-rtm
  :type 'string)

(defconst slack-rtm-buffer-name "*todo*"
  "Name for the buffer where tasks are written")

(defun slack-rtm-get-tasks (query &optional last-sync)
  "Get the tasks from rtm."
  (rtm-tasks-get-list nil query last-sync))

(defun slack-rtm (query)
  "Create the `slack-rtm-buffer-name' and fill it with takss from RTM"
  (interactive (list (or (and (not current-prefix-arg)
                              slack-rtm-default-query)
                         (read-from-minibuffer "Enter search: " slack-rtm-default-query))))
  (let ((slack-buffer (generate-new-buffer slack-rtm-buffer-name))
        (task-list (slack-rtm-get-tasks query)))
    (with-current-buffer slack-buffer
      (insert "#+STARTUP: content\n\n"
              "* Remember The Milk -- " query "\n"
              ":PROPERTIES:\n"
              ":last-query: " query "\n"
              ":last-sync: " (format "%d" (float-time (current-time))) "\n"
              ":END:\n")
      (insert (mapconcat 'slack-rtm-tasks-to-string task-list "\n"))
      (display-buffer slack-buffer)
      (org-mode))))

(defun slack-rtm-add-task (task)
  "Add the task to Remember The Milk."
  (let ((title (slack-rtm-task-title task))
        (priority (slack-rtm-task-priority task))
        (deadline (slack-rtm-task-deadline task))
        (scheduled (slack-rtm-task-scheduled task))
        (tags (slack-rtm-task-tags task))
        (url (slack-rtm-task-url task)))
    (if (not (null title))
        (slack-rtm-task-quickadd
         (concat
          title
          (if (or deadline scheduled)
              (if (null deadline)
                  (concat " ^" scheduled " ")
                (concat " ^" deadline)))
          (when tags
            (concat " #" (mapconcat 'identity tags " #")))
          (when url
            (concat " " url))
          (when priority
            (concat " !"
                    (cond
                     ((equal priority "[#A]")
                      "1")
                     ((equal priority "[#B]")
                      "2")
                     ((equal priority "[#C]")
                      "3")))))))))

(defun slack-rtm-parse-current-task ()
  "Convert a task from org to the internal data structure"
;;;TODO: Handle repeating tasks
  (save-excursion
    (org-back-to-heading t)
    (when (and (looking-at org-complex-heading-regexp)
               (match-string 2))
      (let* (info
             (status (match-string-no-properties 2))
             (priority (match-string-no-properties 3))
             (title (match-string-no-properties 4))
             (tags (match-string-no-properties 5))
             (id (org-entry-get (point)
                                "rtm-task-id"))
             (list-id (org-entry-get (point)
                                     "rtm-list-id"))
             (taskseries-id (org-entry-get (point)
                                           "rtm-taskseries-id"))
             (url (org-entry-get (point)
                                 "url")))
        (if id (add-to-list 'info (cons "id" id)))
        (when tags
          (setq tags  (split-string tags ":" t)))
        (setq info
              (list
               (cons "id" id)
               (cons "list-id" list-id)
               (cons "taskseries-id" taskseries-id)
               (cons "title" title)
               (cons "priority" priority)
               (cons "tags" tags)
               (cons "status" status)))
        (when url
          (setq info (cons (cons "url" url)
                           info)))
        (when (org-entry-get nil "DEADLINE")
          (setq info (cons (cons "deadline"
                                 (substring (org-entry-get nil "DEADLINE")
                                            0 10)) info)))
        (when (org-entry-get nil "SCHEDULED")
          (setq info (cons (cons "scheduled"
                                 (substring (org-entry-get nil "SCHEDULED")
                                            0 10)) info)))
        info))))

(defun slack-rtm-success-p (result)
  "Did the transaction succeed?"
  (let ((attrs (xml-node-attributes (assq 'transaction result))))
    (and (not (null (cdr (assq 'id attrs))))
         (not (null (cdr (assq 'undoable attrs)))))))

(defun slack-rtm-push-task (task)
;;; TODO: so many requests just to update???  eeek
  (let ((list-id (slack-rtm-task-list-id task))
        (taskseries-id (slack-rtm-task-taskseries-id task))
        (id (slack-rtm-task-id task))
        (title (slack-rtm-task-title task))
        (tags (slack-rtm-task-tags task)))
    (if (string= (slack-rtm-task-status task)
                 "DONE")
        (rtm-tasks-complete list-id taskseries-id id)
      (rtm-tasks-uncomplete list-id taskseries-id id))
    (when tags
      (rtm-tasks-set-tags list-id taskseries-id id
                          (concat " #" (mapconcat 'identity tags " #"))))
    (rtm-tasks-set-name list-id taskseries-id id title)))

(defun slack-rtm-push (&optional processed)
  "Push all locally modified tasks but skip the ones in `processed'."
  (interactive)
  (goto-char (point-min))
  (let ((start (float-time (current-time))))
    (while (re-search-forward org-complex-heading-regexp nil t)
      (if (match-string 2)
          (let ((id (org-entry-get (point)
                                   "slack-id"))
                (modified (string-to-number (or (org-entry-get (point)
                                                                "modified")
                                                 "")))
                (last-sync (string-to-number (or (org-entry-get-with-inheritance
                                           "last-sync") ""))))
            (if (or (null id)
                    (and (> modified last-sync)
                         (not (member id processed))))
                (progn
                  (save-excursion (slack-rtm-sync-task))
                  t)
              nil))))))

(defun slack-rtm-sync-task (&optional force)
  "Update RTM or add task for the current task"
  (interactive "P")
  (save-excursion
    (let ((task (slack-rtm-parse-current-task)))
      (if (null (slack-rtm-task-id task))
          ;; new task
          (let ((result (slack-rtm-add-task task)))
            (when (slack-rtm-success-p result)
              (slack-rtm-update-current-task result)
              (message "Task added.")))
        (let ((result (slack-rtm-push-task task)))
          (when (slack-rtm-success-p result)
            (slack-rtm-update-current-task result)
            (message "Task updated.")))))))

(defun slack-rtm-delete-task ()
  "Delete the current task from RTM"
  (interactive)
  (org-back-to-heading t)
  (save-excursion
    (let ((task (slack-rtm-parse-current-task)))
      (if (null (slack-rtm-task-id task))
          (message "Task not known by RTM")
        (let* ((task-id (slack-rtm-task-id task))
               (taskseries-id (slack-rtm-task-taskseries-id task))
               (list-id (slack-rtm-task-list-id task))
               (result (rtm-tasks-delete list-id taskseries-id task-id)))
          (if (slack-rtm-success-p result)
              (progn
                (delete-region
                 (point)
                 (if (and (end-of-line)
                          (re-search-forward org-complex-heading-regexp nil t))
                     (match-beginning 0)
                   (org-end-of-subtree t t)
                   (point)))
                (message "Task deleted."))
            (message "Problem deleting task.")))))))

(defun slack-rtm-sync ()
  "Insert new tasks and update previous tasks."
  (interactive)
  (let ((tasks (slack-rtm-get-tasks (org-entry-get-with-inheritance
                                     "last-query")
                                    (slack-rtm-utc-string-to-8601-date
                                     (org-entry-get-with-inheritance
                                      "last-sync"))))
        processed deleted)
    (mapc (lambda (task-list)
            (mapc (lambda (deleted-list)
                    (mapc (lambda (taskseries)
                            (mapc (lambda (task)
                                    (let ((list-id (xml-get-attribute
                                                    task-list 'id))
                                          (taskseries-id (xml-get-attribute
                                                          taskseries 'id))
                                          (task-id (xml-get-attribute task
                                                                      'id)))
                                      (if (slack-rtm-find-task list-id
                                                               taskseries-id
                                                               task-id)
                                          (progn
                                            (org-back-to-heading t)
                                            (delete-region
                                             (point)
                                             (if (and (end-of-line)
                                                      (re-search-forward
                                                       org-complex-heading-regexp nil t))
                                                 (match-beginning 0)
                                               (org-end-of-subtree t t)
                                               (point)))
                                            (setq deleted (append (list
                                                                   (slack-rtm-get-slack-id
                                                                    list-id
                                                                    taskseries-id
                                                                    task-id))
                                                                  deleted))))))

                                  (xml-get-children taskseries 'task))
                            ) (xml-get-children deleted-list 'taskseries))

                    ) (xml-get-children task-list 'deleted))

            ;; then do the modified tasks
            (mapc (lambda (taskseries)
                    (mapc (lambda (task)
                            (let ((list-id (xml-get-attribute task-list 'id))
                                  (taskseries-id (xml-get-attribute taskseries
                                                                    'id))
                                  (task-id (xml-get-attribute task 'id)))
                              ;; if we find it update it (if needed)
                              (if (slack-rtm-find-task list-id taskseries-id
                                                       task-id)
                                  (progn
                                    (when
                                        (slack-rtm-update-task task-list
                                                         taskseries
                                                         task)
                                    (setq processed (append (list
                                                             (slack-rtm-get-slack-id
                                                              list-id taskseries-id
                                                              task-id))
                                                            processed))))
                                ;; not found so add it at end
                                (slack-rtm-create-task task-list taskseries
                                                  task)
                                (setq processed (append (list
                                                         (slack-rtm-get-slack-id
                                                          list-id taskseries-id
                                                          task-id))
                                                        processed)))))
                          (xml-get-children taskseries 'task)))
                  (xml-get-children task-list 'taskseries)))
          tasks)

    ;; now go through and handle the locally modified ones
    (slack-rtm-push processed)

    ;;; TODO: Add tasks that are pushed to the processed list for output below?

    ;; now update the last-sync
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\(" outline-regexp "\\)") nil t)
      (org-entry-put (point)
                     "last-sync"
                     (format "%d" (float-time (current-time)))))
    (if (or tasks deleted)
        (message (format "%d task(s) updated. %d task(s) deleted."
                                 (length processed) (length deleted)))
      (message "No tasks changed on RTM."))))

(defun slack-rtm-task-quickadd (text)
  "Quickly add a task to RTM"
  (interactive "MTask: ")
  (rtm-tasks-add text t))

(defun slack-rtm-tasks-to-string (task-list)
  "Return a formatted version of task.  This walks the datastructure returned
by the rtm package. `rtm-tasks-get-list'"
  (let ((taskseries (xml-get-children task-list 'taskseries)))
    (mapconcat (lambda (item)
                 (slack-rtm-taskseries-to-string task-list item))
               taskseries "\n")))

(defun slack-rtm-taskseries-to-string (task-list taskseries &optional level)
  "Return a formatted of taskseries"
  ;;         (task (car (xml-get-children taskseries 'task)))
  (mapconcat (lambda (task)
               (slack-rtm-task-to-string task-list taskseries task level))
             (xml-get-children taskseries 'task)
             "\n"))

(defun slack-rtm-task-to-string (task-list taskseries task &optional level)
  "Convert an individual RTM task to an representation in org-mode"
  (let* ((list-id (xml-get-attribute task-list 'id))
         (name (xml-get-attribute taskseries 'name))
         (priority (slack-rtm-priority-to-org (xml-get-attribute task
                                                                 'priority)))
         (taskseries-id (xml-get-attribute taskseries 'id))
         (task-id (xml-get-attribute task 'id))
         (due-date (xml-get-attribute task 'due)))
    (concat (make-string (or level 2) ?*)
            " "
            (slack-rtm-status-to-string task)
            " "
            (when (not (equal priority ""))
              (concat " " priority " "))
            name " "  (slack-rtm-taskseries-tags
                       taskseries)
            (if (not (equal due-date ""))
                (concat "\n" org-deadline-string " "
                        (slack-rtm-format-date due-date)))
            "\n"
            (mapconcat (lambda (notes)
                         (mapconcat 'slack-rtm-note-to-string
                                    (xml-get-children notes 'note)
                                    ""))
                       (xml-get-children taskseries
                                         'notes) "\n")
            ":PROPERTIES:\n"
            ":slack-id: " (slack-rtm-get-slack-id list-id taskseries-id
                                                  task-id) "\n"
            ":rtm-task-id: " task-id "\n"
            ":rtm-taskseries-id: " taskseries-id "\n"
            ":rtm-list-id: "  list-id "\n"
            ":modified: " (slack-rtm-fix-8601 (xml-get-attribute taskseries
                                                                 'modified))
            "\n"
            (when (not (string= (xml-get-attribute taskseries 'url) ""))
              (concat ":url: "  (xml-get-attribute taskseries 'url) "\n"))
            ":END:\n")))


(defun slack-rtm-taskseries-tags (taskseries)
  (let ((tags (xml-get-children (car (xml-get-children taskseries 'tags))
                                'tag)))
    (when tags
      (concat ":" (mapconcat (lambda (item)
                               (concat (car (xml-node-children item))))
                             tags ":" ) ":"))))

(defun slack-rtm-status-to-string (task)
  (let ((comp (xml-get-attribute task 'completed)))
    (cond
     ((equal comp "") "TODO")
     (t "DONE"))))

(defun slack-rtm-format-date (date)
  "Return <yyyy-mm-dd day> for DATE"
  (concat
   "<"
   (format-time-string
    "%Y-%m-%d %a"
    (cond
     ((listp date)
      date)
     (t (apply 'encode-time (org-parse-time-string date)))))
   ">"))

(defun slack-rtm-note-to-string (note)
  (concat (xml-get-attribute note 'title)
          (replace-regexp-in-string "\\*" "+" (car
                                               (xml-node-children note)))
          "\n"))

(defun slack-rtm-priority-to-org (rtm-priority)
  "Convert the RTM priority to an org priority."
  (cond
   ((string= rtm-priority "1") "[#A]")
   ((string= rtm-priority "2") "[#B]")
   ((string= rtm-priority "3") "[#C]")
   (t "")))

(defun slack-rtm-update-current-task (result)
  "Update the org task at point with properties from result"
  (let* ((task-list (assq 'list result))
         (taskseries (car (xml-get-children task-list 'taskseries)))
         (task (assq 'task taskseries))
         (list-id (xml-get-attribute task-list 'id))
         (taskseries-id (xml-get-attribute taskseries 'id))
         (task-id (xml-get-attribute task 'id)))
    (org-entry-put (point)
                   "slack-id" (slack-rtm-get-slack-id list-id taskseries-id task-id))
    (org-entry-put (point) "rtm-list-id" list-id)
    (org-entry-put (point) "rtm-taskseries-id" taskseries-id)
    (org-entry-put (point) "rtm-task-id" task-id)
    (org-entry-put (point) "modified" (slack-rtm-fix-8601 (xml-get-attribute taskseries
                                                                             'modified)))))

(defun slack-rtm-move-task ()
  "Move the current task to a different list"
  (interactive)
  (let* ((list-id (slack-rtm-get-list))
        (result (slack-rtm-move-task-to-list (slack-rtm-parse-current-task) list-id)))
  (when (slack-rtm-success-p result)
    (slack-rtm-update-current-task result)
    (message "Task moved."))))

(defun slack-rtm-move-task-to-list (task list-id)
  (let* ((result (rtm-tasks-move-to (slack-rtm-task-list-id task) list-id
                                    (slack-rtm-task-taskseries-id task)
                                    (slack-rtm-task-id task))))
    result))

(defun slack-rtm-get-list ()
  "Propmpt the user for a list"
  (let ((slack-rtm-list-lookup
         (delq nil
               (mapcar (lambda (node)
                         (if (string= (xml-get-attribute node 'smart) "0")
                             `(,(xml-get-attribute node 'name)
                               ,(xml-get-attribute node 'id))))
                                       (rtm-lists-get-list)))))
    (cadr (assoc (ido-completing-read "List? "
                                      (mapcar 'car slack-rtm-list-lookup))
                 slack-rtm-list-lookup))))

(defun slack-rtm-touch-task ()
  "Update the current task's timestamp so that it's included in the next sync."
  (interactive)
  (org-entry-put (point) "modified" (format "%d" (float-time
                                                  (current-time)))))

(defun slack-rtm-create-task (task-list taskseries task)
  "Create the org version of the RTM task"
  (goto-char (point-max))
  (if (point-at-eol)
      (insert "\n"))
  (insert (slack-rtm-task-to-string task-list taskseries task)))

(defun slack-rtm-update-task (task-list taskseries task)
  "Update the org version of the RTM task (if needed)"
  (let* ((local-modified (string-to-number (or (org-entry-get (point)
                                                              "modified")
                                              "")))
        (server-modified (string-to-number (slack-rtm-fix-8601 (xml-get-attribute taskseries
                                                                 'modified))))
        (level (car (org-heading-components)))
        (is-locally-modified-p (>= local-modified server-modified)))
    (if is-locally-modified-p
        nil
      ;; not locally modified so replace it with server version
      (delete-region (org-back-to-heading)
                     (progn (goto-char (match-end 0))
                            (if (re-search-forward org-complex-heading-regexp
                                                   nil t)
                                (goto-char (match-beginning 0))
                              (org-end-of-subtree))))
      (insert (slack-rtm-task-to-string task-list taskseries task level))
      t)))

(defun slack-rtm-fix-8601 (time-string)
  "This is admittedly goofy and doesn't really 'fix' anything."
  (let ((fixed-time-string
         (replace-regexp-in-string "T" " "
                                   (replace-regexp-in-string
                                    "Z" " Z" time-string))))
    (format "%d"
            (float-time
             (apply 'encode-time
                    (parse-time-string fixed-time-string))))))

(defun slack-rtm-utc-string-to-8601-date (value)
  "Convert a epoch time to 8601 date"
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (seconds-to-time (string-to-number
                                                             value))))
(defun slack-rtm-find-task (list-id taskseries-id task-id)
  (goto-char (point-min))
    (re-search-forward
     (concat "^[ \t]*:slack-id:[ \t]+" (slack-rtm-get-slack-id list-id
                                                             taskseries-id task-id) "$")
     nil t))

(defun slack-rtm-get-slack-id (list-id taskseries-id task-id)
  "Get the internal id"
  (concat list-id "-" taskseries-id "-" task-id))

(defun slack-rtm-get-last-query ()
  "Get the last query used for this org file"
  (org-entry-get (point)
                  "last-query" t))

(defmacro slack-rtm-task-prop-defun (field)
  `(defun ,(intern (concat "slack-rtm-task-" field)) (task)
     (cdr (assoc ,field task))))

(slack-rtm-task-prop-defun "id")
(slack-rtm-task-prop-defun "list-id")
(slack-rtm-task-prop-defun "taskseries-id")
(slack-rtm-task-prop-defun "priority")
(slack-rtm-task-prop-defun "status")
(slack-rtm-task-prop-defun "title")
(slack-rtm-task-prop-defun "deadline")
(slack-rtm-task-prop-defun "scheduled")
(slack-rtm-task-prop-defun "tags")
(slack-rtm-task-prop-defun "url")

(provide 'slack-rtm)

;;; slack-rtm.el ends here
