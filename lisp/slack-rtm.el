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

(defcustom slack-rtm-query "list:Inbox_ AND status:incomplete"
  "Query to use when getting lists from RTM."
  :group 'slack-rtm
  :type 'string)

(defcustom slack-rtm-buffer-name "*todo*"
  "Name of buffer that is created"
  :group 'slack-rtm
  :type 'string)

(defconst slack-rtm-buffer-name "*todo*"
  "Name for the buffer where tasks are written")

(defun slack-rtm-get-tasks ()
  "Get the tasks from rtm."
  (rtm-tasks-get-list nil slack-rtm-query))

(defun slack-rtm ()
  "Create the `slack-rtm-buffer-name' and fill it with takss from RTM"
  (interactive)
  (let ((slack-buffer (generate-new-buffer slack-rtm-buffer-name))
        (task-list (slack-rtm-get-tasks)))
    (with-current-buffer slack-buffer
      (insert (mapconcat 'slack-rtm-tasks-to-string task-list "\n"))
      (display-buffer slack-buffer))))

(defun slack-rtm-tasks-to-string (task)
  "Return a formatted version of task.  This walks the datastructure returned
by the rtm package. `rtm-tasks-get-list'"
  (let ((taskseries (xml-get-children task 'taskseries)))
    (mapconcat 'slack-rtm-taskseries-to-string taskseries "\n")))

(defun slack-rtm-taskseries-to-string (taskseries)
  "Return a formatted of taskseries"
  (concat "* " (xml-get-attribute taskseries 'name)))

(provide 'slack-rtm)

;;; slack-rtm.el ends here
