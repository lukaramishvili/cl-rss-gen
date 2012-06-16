;;;; Copyright 2012, Luka Ramishvili
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun rss-skeleton
  (&key (title "") (link-self "") (link-alt "") (subtitle "")
	(class "") (updated "") (id "") entries)
  (cl-who:with-html-output-to-string
   (*standard-output*
    nil :prologue "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    :indent t)
   (:feed :xmlns "http://www.w3.org/2005/Atom"
	 :|xmlns:re| "http://purl.org/atompub/rank/1.0"
	 (:title :type "text" title)
	 (:link :rel "self" :href link-self
	       :type "application/atom+xml")
	 (:link :rel "alternate" :href link-alt :type "text/html")
	 (:subtitle subtitle)
	 (:updated updated)
	 (:id id)
	 (cl-who:str entries))))

(defun rss-entry
  (&key (id "") (title "") (category-scheme "") (categories nil)
	(author-name "") (author-uri "") (link-alt "")
	(published "") (updated "") (summary ""))
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue nil :indent t)
   (:entry
    (:id (cl-who:str id))
    (:title :type "text" (cl-who:str title))
    (loop for cat in categories
	  do (cl-who:htm
	      (:category :scheme category-scheme :term cat)))
    (:author
     (:name (cl-who:str author-name))
     (:uri (cl-who:str author-uri)))
    (:link :rel "alternate" :href link-alt)
    (:published (cl-who:str published))
    (:updated (cl-who:str updated))
    (:summary :type "html" (cl-who:str summary)))))


(defun rss-entries-from-class
  (objects &key id title category-scheme categories author-name
	   author-uri link-alt published updated summary)
  "keyword arguments passed (symbols) will be used as slot names"
  (reduce
   #'+s
   (loop
    for o in objects collecting
    (let ((arglist nil))
      (progn
	(if id (setf (getf arglist :id) (slot-value o id)))
	(if title (setf (getf arglist :title) (slot-value o title)))
	(if category-scheme
	    (setf (getf arglist :category-scheme)
		  (slot-value o category-scheme)))
	(if categories (setf (getf arglist :categories)
			     (slot-value o categories)))
	(if author-name (setf (getf arglist :author-name)
			      (slot-value o author-name)))
	(if author-uri (setf (getf arglist :author-uri)
			     (slot-value o author-uri)))
	(if link-alt (setf (getf arglist :link-alt)
			   (slot-value o link-alt)))
	(if published (setf (getf arglist :published)
			    (slot-value o published)))
	(if updated (setf (getf arglist :updated)
			  (slot-value o updated)))
	(if summary (setf (getf arglist :summary)
			  (slot-value o summary)))
	(apply #'rss-entry arglist))))))