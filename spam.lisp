;;;; Naive Bayesian Filter for Common Lisp
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :spam
  (:use :cl :lw :re)
  (:export
   #:bayesian-filter

   ;; accessor methods
   #:bayesian-filter-spam-words
   #:bayesian-filter-ham-words
   #:bayesian-filter-ignore-words
   #:bayesian-filter-count
   #:bayesian-filter-liked
   #:bayesian-filter-disliked

   ;; methods
   #:bayesian-filter-like
   #:bayesian-filter-dislike
   #:bayesian-filter-like-probability
   #:bayesian-filter-dislike-probability
   #:bayesian-filter-like-p
   #:bayesian-filter-dislike-p))

(in-package :spam)

(defconstant +word-re+ (compile-re "%a%a[%w_%-']+")
  "Pattern used to find words in text.")

(defconstant +common-words+ (list "the" "be" "to" "of" "and" "a" "in" "that" "have" "i" "it" 
                                  "for" "not" "on" "with" "he" "as" "you" "do" "at" "this" 
                                  "but" "his" "by" "from" "they" "we" "say" "her" "she" "or" 
                                  "an" "will" "my" "one" "all" "would" "there" "their" "what" 
                                  "so" "up" "out" "if" "about" "who" "get" "which" "go" "me" 
                                  "when" "make" "can" "like" "time" "no" "just" "him" "know" 
                                  "take" "people" "into" "year" "your" "good" "some" "could" 
                                  "them" "see" "other" "than" "then" "now" "look" "only" 
                                  "come" "its" "over" "think" "also" "back" "after" "use" 
                                  "two" "how" "our" "work" "first" "well" "way" "even" "new" 
                                  "want" "because" "any" "these" "give" "day" "most" "us")
  "Very common words that should be excluded from filtering.")

(defclass bayesian-filter ()
  ((|spam|     :initform (make-hash-table :test 'equal) :accessor bayesian-filter-spam-words :type cl:hash-table)
   (|ham|      :initform (make-hash-table :test 'equal) :accessor bayesian-filter-ham-words :type cl:hash-table)
   (|ignore|   :initform (copy-list +common-words+)     :accessor bayesian-filter-ignore-words)
   (|count|    :initform 0                              :accessor bayesian-filter-count)
   (|liked|    :initform nil                            :accessor bayesian-filter-liked)
   (|disliked| :initform nil                            :accessor bayesian-filter-disliked))
  (:documentation "Naive Bayesian spam filter."))

(defmethod bayesian-filter-like ((filter bayesian-filter) source texts)
  "Mark a text string as one that you dislike."
  (let ((status (bayesian-filter-marked-p filter source)))
    (unless (eq status :liked)

      ;; remove from the disliked list
      (when (eq status :disliked)
        (setf (bayesian-filter-disliked filter)
              (remove source (bayesian-filter-disliked filter) :test #'string=)))

      ;; add the source to the list of liked sources
      (push source (bayesian-filter-liked filter))

      ;; tally the number of source that have been marked
      (unless status
        (incf (bayesian-filter-count filter)))
    
      ;; mark every single word as one that you like
      (dolist (text texts)
        (loop :for match :in (find-re +word-re+ text :all t)
              :for word := (string-downcase (match-string match))
            
              ;; ignore common words
              :unless (bayesian-filter-ignore-p filter word)

              ;; remove disliked words, add liked
              :do (progn
                    (when (eq status :liked)
                      (decf (gethash word (bayesian-filter-spam-words filter))))
                    (incf (gethash word (bayesian-filter-ham-words filter) 0))))))))

(defmethod bayesian-filter-dislike ((filter bayesian-filter) source texts)
  "Mark a text string as one that you dislike."
  (let ((status (bayesian-filter-marked-p filter source)))
    (unless (eq status :disliked)

      ;; remove from the liked list
      (when (eq status :liked)
        (setf (bayesian-filter-liked filter)
              (remove source (bayesian-filter-liked filter) :test #'string=)))

      ;; add the source to the list of disliked sources
      (push source (bayesian-filter-disliked filter))

      ;; tally the number of source that have been marked
      (unless status
        (incf (bayesian-filter-count filter)))
    
      ;; mark every single word as one that you like
      (dolist (text texts)
        (loop :for match :in (find-re +word-re+ text :all t)
              :for word := (string-downcase (match-string match))
            
              ;; ignore common words
              :unless (bayesian-filter-ignore-p filter word)

              ;; remove liked words, add disliked
              :do (progn
                    (when (eq status :liked)
                      (decf (gethash word (bayesian-filter-ham-words filter))))
                    (incf (gethash word (bayesian-filter-spam-words filter) 0))))))))

(defmethod bayesian-filter-dislike-probability ((filter bayesian-filter) source texts)
  "Returns the probability that a source text is spam."
  (let ((count (bayesian-filter-count filter))
        (marked (bayesian-filter-marked-p filter source)))
    (cond ((eq marked :liked)    (values 0.0 :liked))
          ((eq marked :disliked) (values 1.0 :disliked))

          ;; not enough marked sources to make a guess
          ((zerop count)         (values 0.5))

          ;; make a guess
          (t (let ((p 0.0))
               (dolist (text texts (/ 1 (1+ (exp p))))
                 (loop :for match :in (find-re +word-re+ text :all t)
                       :for word := (match-string match)
                       
                       ;; ignore common words
                       :unless (bayesian-filter-ignore-p filter word)
                       :do (let* ((prws (/ (gethash word (bayesian-filter-spam-words filter) 0) count))
                                  (prwh (/ (gethash word (bayesian-filter-ham-words filter) 0) count))
                                  
                                  ;; the probability that a message with word is disliked
                                  (prsw (if (zerop (+ prws prwh))
                                            0
                                          (min (/ prws (+ prws prwh)) 0.99))))
                             (unless (zerop prsw)
                               (incf p (- (log (- 1.0 prsw)) (log prsw))))))))))))

(defmethod bayesian-filter-like-probability ((filter bayesian-filter) source texts)
  "Returns the probability that a set of texts is liked."
  (multiple-value-bind (prob mark)
      (bayesian-filter-dislike-probability filter source texts)
    (values (- 1.0 prob) mark)))

(defmethod bayesian-filter-like-p ((filter bayesian-filter) source texts &optional (threshold 0.97))
  "T if it is guessed that the user will like this set of texts."
  (multiple-value-bind (prob mark)
      (bayesian-filter-like-probability filter source texts)
    (values (> prob threshold) mark)))

(defmethod bayesian-filter-dislike-p ((filter bayesian-filter) source texts &optional (threshold 0.97))
  "T if it is guessed that the use< r will dislike this set of texts."
  (multiple-value-bind (prob mark)
      (bayesian-filter-dislike-probability filter source texts)
    (values (> prob threshold mark))))

(defmethod bayesian-filter-marked-p ((filter bayesian-filter) source)
  "Returns :liked, :disliked, or nil if not marked."
  (or (when (find source (bayesian-filter-liked filter) :test #'string=) :liked)
      (when (find source (bayesian-filter-disliked filter) :test #'string=) :disliked)))

(defmethod bayesian-filter-ignore-p ((filter bayesian-filter) word)
  "T if word is common and should be excluded from filtering."
  (find word (bayesian-filter-ignore-words filter) :test #'string-equal))
