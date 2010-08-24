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

(defun slack-rtm-task-quickadd (text)
  "Quickly add a task to RTM"
  (interactive "MTask: ")
  (rtm-tasks-add text t))

(defun slack-rtm-tasks-to-string (task)
  "Return a formatted version of task.  This walks the datastructure returned
by the rtm package. `rtm-tasks-get-list'"
  (let ((taskseries (xml-get-children task 'taskseries)))
    (mapconcat 'slack-rtm-taskseries-to-string taskseries "\n")))

(defun slack-rtm-taskseries-to-string (taskseries &optional level)
  "Return a formatted of taskseries"
  ;; TODO: task can be a list of more than one under taskseries
  (let* ((name (xml-get-attribute taskseries 'name))
         (task (car (xml-get-children taskseries 'task)))
         (due-date (xml-get-attribute task 'due)))
    (concat (make-string (or level 2) ?*)
            " "
            (slack-rtm-status-to-string task)
            " "
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
            ":modified: " (xml-get-attribute taskseries 'modified) "\n"
            ":sync: " (format "%d" (float-time (current-time))) "\n"
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
     (t "STARTED"))))

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

(provide 'slack-rtm)

;;; slack-rtm.el ends here
