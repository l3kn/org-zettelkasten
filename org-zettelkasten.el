;;; org-zettelkasten.el --- Zettelkasten System for Emacs org-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (tablist "0.15.0"))
;; Version: 0.0.1

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
;;
;; A setup for managing large collections of interlinked org-mode files.
;; All data used by this package is cached to ensure fast performance.
;;
;; Features:
;;
;; - Convenience functions for creating and linking to files
;; - Functions for renaming / deleting files that update links to them
;; - Query language and views for showing lists of files
;; - Alternative org-agenda using cached data
;;
;;; Code:
;;;; Dependencies

(require 'org-el-cache)
(require 'ts)
(require 'subr-x)
(require 'cl)

;;;; Submodules

(require 'org-zk-awk)
(require 'org-zk-core)
(require 'org-zk-time)
(require 'org-zk-repeat)

;;;; Footer

(provide 'org-zettelkasten)

;;; org-zettelkasten.el ends here
