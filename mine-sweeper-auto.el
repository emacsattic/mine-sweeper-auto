;;; mine-sweeper-auto.el --- automated mine sweeping for mine-sweeper.el

;; Copyright 2005 Kevin Ryde
;;
;; mine-sweeper-auto.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; mine-sweeper-auto.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; http://www.gnu.org/licenses/gpl.txt, or you should have one in the file
;; COPYING which comes with GNU Emacs and other GNU programs.  Failing that,
;; write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This package extends mine-sweeper.el with a `mine-auto' function to go
;; around the board marking or opening squares the same as you might do
;; manually, but automated.
;;
;; The algorithm used is fairly basic, it probably doesn't detect all
;; possible safe moves.

;;; Install:

;; Add the following to your .emacs, to bind to the "a" key,
;;
;;     (autoload 'mine-auto "mine-sweeper-auto" nil t)
;;     (add-hook 'mine-sweeper-mode-hook
;;               (lambda ()
;;                 (define-key mine-sweeper-mode-map "a" 'mine-auto)))

;;; History:

;; Version 1 - the first version.


;;; Code:

(require 'cl)

(defvar mine-auto-pending nil
  "List ((ACTION ROW COL) (ACTION ROW COL) ...) of pending work.
ACTION is a symbol, a function to call, either `mine-open', `mine-mark'.

This variable is only used dynamically within `mine-auto', the global value
is unused.")

(defvar mine-auto-stop nil
  "True when `mine-auto' should stop.
This variable is only bound dynamically, the global value is unused.")

(defun mine-auto-ref (row col)
  "Return the visible char at ROW,COL in the minefield.
The return is nil if ROW,COL is outside the valid range."
  (and (<= 0 row) (< row mine-field-hsize)
       (<= 0 col) (< col mine-field-wsize)
       (char-after (+ (point-min) col (* row (1+ mine-field-wsize))))))
  
(defun mine-auto-around (row col)
  "Return squares around  ROW, COL in the minefield.
The return is a list ((CHAR ROW COL) (CHAR ROW COL) ...) being each of the
eight squares surrounding the given ROW, COL.  CHAR is nil when off the edge
of the grid."
  (list (list (mine-auto-ref row (1- col))      row (1- col))      ;; N
        (list (mine-auto-ref (1+ row) (1- col)) (1+ row) (1- col)) ;; NE
        (list (mine-auto-ref (1+ row) col)      (1+ row) col)      ;; E
        (list (mine-auto-ref (1+ row) (1+ col)) (1+ row) (1+ col)) ;; SE
        (list (mine-auto-ref row (1+ col))      row (1+ col))      ;; S
        (list (mine-auto-ref (1- row) (1+ col)) (1- row) (1+ col)) ;; SW
        (list (mine-auto-ref (1- row) col)      (1- row) col)      ;; W
        (list (mine-auto-ref (1- row) (1- col)) (1- row) (1- col)) ;; NW
        ))

(defun mine-auto-check-one (row col)
  "Check ROW,COL in the minefield for safe actions that its count implies.
Actions are added to `mine-auto-pending'."
  (let ((n (position (mine-auto-ref row col) "\a12345678")))
    (when n
      (let* ((around (mine-auto-around row col))
             (bombs  (count mine-mark-bomb  around :key 'car))
             (opens  (count mine-mark-field around :key 'car))
             (remain (- n bombs)))

        ;; if no more bombs then open all remaining squares
        (when (= remain 0)
          (dolist (elem around)
            (when (eq (car elem) mine-mark-field)
              (add-to-list 'mine-auto-pending
                           (cons 'mine-open (cdr elem))))))

        ;; if opens are the full count of remaining bombs then mark them all
        (when (= remain opens)
          (dolist (elem around)
            (when (eq (car elem) mine-mark-field)
              (add-to-list 'mine-auto-pending
                           (cons 'mine-mark (cdr elem))))))))))


(defun mine-auto-check-one-harder (row col)
  "Check ROW,COL in the minefield for safe actions that its count implies.
Actions are added to `mine-auto-pending'.  This does a harder check, looking
for sets of open squares which are subsets of another."

  (let ((n (position (mine-auto-ref row col) "\a12345678")))
    (when n
      (let* ((around (mine-auto-around row col))
             (bombs  (count mine-mark-bomb  around :key 'car))
             (remain (- n bombs)))

        ;; Look for squares which are adjacent to the open squares around
        ;; ROW,COL, and which have the same remaining count, and whose
        ;; open squares are a subset of ours.  The excess squares we've
        ;; got can therefore be opened safely.
        ;;
        (when (> remain 0)
          (let* ((open-list  (remove* mine-mark-field around
                                      :key 'car :test-not 'eq))
                 (cover-list (apply 'append
                                    (mapcar (lambda (elem)
                                              (mine-auto-around
                                               (second elem) (third elem)))
                                            open-list))))
            (setq cover-list (delete-duplicates cover-list))
            (mapcar (lambda (elem)
                      (setcar elem (position (car elem) "\a12345678")))
                    cover-list)
            (setq cover-list (remove* nil cover-list :key 'car))

            ;; examine each square which might cover some of open-list
            (dolist (cover cover-list)
              (let* ((cover-n      (first cover))
                     (cover-around (mine-auto-around (second cover)
                                                     (third cover)))
                     (cover-bombs  (count mine-mark-bomb cover-around
                                          :key 'car))
                     (cover-remain (- cover-n cover-bombs)))
                (when (= remain cover-remain)
                  (let ((cover-open-list (remove* mine-mark-field
                                                  cover-around
                                                  :key 'car :test-not 'eq)))
                    (if (subsetp cover-open-list open-list :test 'equal)
                        (dolist (elem (set-difference open-list
                                                      cover-open-list
                                                      :test 'equal))
                          (when (eq (car elem) mine-mark-field)
                            (add-to-list 'mine-auto-pending
                                         (cons 'mine-open (cdr elem))))))))))))))))


(defun mine-auto-check-all ()
  "Check the whole minefield for safe actions to perform.
Actions are added to `mine-auto-pending'."

  (let ((row 0))
    (while (< row mine-field-hsize)
      (let ((col 0))
        (while (< col mine-field-wsize)
          (mine-auto-check-one row col)
          (setq col (1+ col))))
      (setq row (1+ row))))

  (unless mine-auto-pending
    (let ((row 0))
      (while (< row mine-field-hsize)
        (let ((col 0))
          (while (< col mine-field-wsize)
            (mine-auto-check-one-harder row col)
            (setq col (1+ col))))
        (setq row (1+ row))))))

(defun mine-auto-distance (x y)
  "X and Y are lists (ROW COL), return the distance between them."
  (sqrt (+ (expt (- (first x)  (first y))  2)
           (expt (- (second x) (second y)) 2))))

(defun mine-auto-find-minimum (proc lst)
  "Call PROC on each element of LST, return the element giving the smallest."
  (let* ((ret (first lst))
         (val (funcall proc ret)))
    (dolist (elem lst)
      (let ((this-val (funcall proc elem)))
        (when (< this-val val)
          (setq ret elem)
          (setq val this-val))))
    ret))

(defun mine-auto-travel (row col)
  "Move to ROW, COL in the minefield, walking square by square.
This is just for a cute display, obviously jumping straight to the target
would be possible."
  (let ((targ (list row col)))
    (while (and (not mine-auto-stop)
                (let ((curr (list (aref mine-point 0)
                                  (aref mine-point 1))))
                  (and (not (equal curr targ))
                       (let ((next (mine-auto-find-minimum
                                    (lambda (elem)
                                      (mine-auto-distance elem targ))
                                    (mapcar 'cdr
                                            (apply 'mine-auto-around curr)))))
                         (apply 'mine-move-cursor next)

                         ;; stop on keypress
                         (or (sit-for 0.15)
                             (setq mine-auto-stop t))

                         t)))))))

(defun mine-auto-diff (old new)
  "Return a list of ((ROW COL) (ROW COL) ...) of differences btwn OLD and NEW.
OLD and NEW are strings of minefield buffer contents."
  (let ((pos 0)
        ret)
    (while (< pos (length old))
      (unless (= (elt old pos) (elt new pos))
        (setq ret (cons (list (/ pos (1+ mine-field-wsize))
                              (% pos (1+ mine-field-wsize))) ret)))
      (setq pos (1+ pos)))
    ret))

(defadvice mine-over (before mine-auto-stop activate)
  "Let `mine-auto' notice when the game is over."
  (setq mine-auto-stop t))

(defun mine-auto-one ()
  "Process one action from `mine-auto-pending'."
  (let* ((curr   (list (aref mine-point 0)
                       (aref mine-point 1)))
         (elem   (mine-auto-find-minimum
                  (lambda (elem)
                    (mine-auto-distance (cdr elem) curr))
                  mine-auto-pending))
         (action (first elem))
         (row    (second elem))
         (col    (third elem)))
    ;; could be duplicates of this element, remove all of them
    (setq mine-auto-pending (delete elem mine-auto-pending))

    ;; if still a blank field
    (when (eq mine-mark-field (mine-auto-ref row col))
      (mine-auto-travel row col)

      (if (eq action 'mine-mark)
          (progn
            (mine-mark)

            ;; after marking a mine some surrounding squares might have have
            ;; their count filled, etc, examine those
            (dolist (elem (mine-auto-around row col))
              (when (first elem)
                (apply 'mine-auto-check-one (cdr elem)))))

        (let ((old (buffer-string)))
          (funcall (first elem))

          ;; examine any squares that have changed for new actions
          (dolist (rc (mine-auto-diff old (buffer-string)))
            (apply 'mine-auto-check-one rc))))

      ;; stop on keypress
      (or (sit-for 0.25)
          (setq mine-auto-stop t)))))

(defun mine-auto ()
  "Automated mine sweeping.
This function marks mines, or opens squares, the same as you might do
manually, but automated.

Only the visible board is examined, there's no cheating with the underlying
actual minefield contents."

  (interactive)
  (let (mine-auto-stop)
    (while (and (not mine-auto-stop)
                (let (mine-auto-pending)
                  (mine-auto-check-all)
                  (and mine-auto-pending
                       (progn
                         (while (and mine-auto-pending
                                     (not mine-auto-stop))
                           (mine-auto-one))
                         t)))))))

(provide 'mine-sweeper-auto)

;;; mine-sweeper-auto.el ends here
