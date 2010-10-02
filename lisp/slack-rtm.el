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

(defun slack-rtm-get-tasks (query)
  "Get the tasks from rtm."
  (rtm-tasks-get-list nil query))

(defun slack-rtm (query)
  "Create the `slack-rtm-buffer-name' and fill it with takss from RTM"
  (interactive (list (or (and (not current-prefix-arg)
                              slack-rtm-default-query)
                         (read-from-minibuffer "Enter search: " slack-rtm-default-query))))
  (let ((slack-buffer (generate-new-buffer slack-rtm-buffer-name))
        (task-list (slack-rtm-get-tasks query)))
    (with-current-buffer slack-buffer
      (insert "* Remember The Milk -- " query "\n"
              ":PROPERTIES:\n"
              ":Last-query: " query "\n"            
              ":Last-modified: TBD"  "\n"
              ":Last-deleted: TBD"  "\n"
              ":Last-sync: TBD"  "\n"
              ":END:\n")
      (insert (mapconcat 'slack-rtm-tasks-to-string task-list "\n"))      
      (display-buffer slack-buffer)
      (org-mode))))

(defun slack-rtm-add-task (task)
  "Add the task to Remember The Milk."
  (let ((title (slack-rtm-task-title task))
        (priority (slack-rtm-task-priority task))
        (deadline (slack-rtm-task-deadline task))
        (tags (slack-rtm-task-tags task)))
    
    (if (not (null title))
        (slack-rtm-task-quickadd
         (concat
          title
          (when deadline
            (concat " ^" deadline " "))
          (when tags
            (concat " #" (mapconcat 'identity tags " #")))
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
                                           "rtm-taskseries-id")))
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
        (when (org-entry-get nil "DEADLINE")
          (setq info (cons (cons "deadline"
                                 (substring (org-entry-get nil "DEADLINE")
                                            0 10)) info)))
        info))))

(defun slack-rtm-success-p (result)
  "Did the transaction succeed?"
  (let ((attrs (xml-node-attributes (assq 'transaction result))))
    (and (not (null (cdr (assq 'id attrs))))
         (not (null (cdr (assq 'undoable attrs)))))))

(defun slack-rtm-push-task (task)
;;; TODO: so many requests just to update???
  (let ((list-id (slack-rtm-task-list-id task))
        (taskseries-id (slack-rtm-task-taskseries-id task))
        (id (slack-rtm-task-id task))
        (tags (slack-rtm-task-tags task)))
    (if (string= (slack-rtm-task-status task)
                 "DONE")        
        (rtm-tasks-complete list-id taskseries-id id)
      (rtm-tasks-uncomplete list-id taskseries-id id))
    (when tags
      (rtm-tasks-set-tags list-id taskseries-id id
                          (concat " #" (mapconcat 'identity tags " #"))))))

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
            ":rtm-task-id: " (xml-get-attribute task 'id) "\n"
            ":rtm-taskseries-id: " (xml-get-attribute taskseries 'id) "\n"
            ":rtm-list-id: "  list-id "\n" 
            ":modified: " (slack-rtm-fix-8601 (xml-get-attribute taskseries
                                                                 'modified)) "\n"
                                                                 ":sync: " (format "%d" (float-time (current-time))) "\n"
                                                                 ":END:\n"))
  )

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
         (task (assq 'task taskseries)))
    (org-entry-put (point) "rtm-list-id" (xml-get-attribute task-list 'id))
    (org-entry-put (point) "rtm-taskseries-id" (xml-get-attribute taskseries 'id))
    (org-entry-put (point) "rtm-task-id" (xml-get-attribute task 'id))
    (org-entry-put (point) "modified" (slack-rtm-fix-8601 (xml-get-attribute taskseries
                                                                             'modified)))
    (org-entry-put (point)
                   "sync" (format "%d" (float-time (current-time))))))

(defun slack-rtm-touch-task ()
  "Update the current task's timestamp so that it's included in the next sync."
  (interactive)
  (org-entry-put (point) "modified" (format "%d" (float-time
                                                  (current-time)))))

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
(slack-rtm-task-prop-defun "tags")

(provide 'slack-rtm)

;;; slack-rtm.el ends here
