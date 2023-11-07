;;; org-conflict.el --- Timing conflicts detector and resolver for Org Agenda

;; Copyright (C) 2019 Thomas Plass

;; Author: Thomas Plass <thomas dot plass at arcor dot de>
;; Version: 0.9
;; Keywords: agenda, calendar, timing, scheduling, validation

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package helps Org agenda users prevent creating scheduling
;; conflicts.
;; 
;; It detects and offers to resolve timing conflicts by checking a
;; test time or timestamp for potential overlaps with timestamps and
;; -ranges defined in 'org-agenda-files.
;;
;; Conflict handling is configurable and ranges from presentation over
;; auto-resolution to auto-replacment.  Checking may take into account
;; configurable intervals ("coffee breaks") around events.
;; 
;; Installation:
;; 
;; 1. add (require 'org-conflict) to your .emacs or equivalent startup
;;    file.
;; 
;; 2. it is recommended to put `org-conflict-check-timestamp-or-range'
;;    onto Org's general purpose validation/normalization hook accessed
;;    by the global Ctrl-c Ctrl-c keybinding.  To add conflict checking
;;    functionality to timestamps, add this to the startup file:
;; 
;;    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-conflict-check-timestamp-or-range)
;;             
;; 3. open org-conflict's customization options by evaluating
;; 
;;    (customize-group 'org-conflict t)
;; 
;;    or manually set any of the user variables
;;  
;;    - 'org-conflict-conflict-action
;;    - 'org-conflict-event-minimum-interval
;;    - 'org-conflict-default-event-duration
;;    - 'org-conflict-resolution-horizon-days
;; 
;; Functional requirements:                                              [2/2]
;; 
;; - [X] Org's user variable 'org-agenda-files set to a non-nil value
;; - [X] timestamps and -ranges stored in 'org-agenda-files              [3/3]
;;   - [X] are <active>, not [inactive] nor tagged as :ARCHIVE:d
;;   - [X] include a time-of-day
;;   - [X] encode a duration 
;;
;; Example usage (interactive mode):
;; 
;; Given the following entries
;; 
;; * TODO Project meeting with Dana
;;   SCHEDULED: <2019-08-02 Fri 10:00-11:30>
;; 
;; * Lunch with Sandy
;;   <2019-08-02 Fri 12:00-12:45>,
;; 
;; a 45 minute period starting at 10:30 on the same day, expressed as
;; the timestamp
;; 
;;   <2019-08-02 Fri 10:30-11:15>
;; 
;; creates cascading conflicts with both schedulings due to its start
;; time and duration.
;; 
;; Timestamps to be validated need not be as complete as their comparison
;; counterparts.  Missing information such as the start time or the
;; duration will be prompted for or can be supplied via a prefix
;; argument.  In fact, a timestamp isn't required at all for conflict
;; checking as executing `org-conflict' in any context other than a
;; timestamp will prompt for all required data.
;; 
;; C-c C-c'ing (or invoking M-x org-conflict) on the above timestamp
;; will start the comparison against all eligible timestamps and
;; -ranges.  If no conflicts are detected, a status message is printed
;; in the echo area.  Otherwise, user focus switches to an informative
;; prompt.
;; 
;; By default, org-conflict will offer to resolve conflicts by
;; computing a value representing the earliest non-conflicting start
;; time relative to the given test time with enough time for the
;; desired duration.  Barring a non-0 value of the user variable
;; 'org-conflict-event-minimum-interval, the proposed resolution to
;; the above conflict would be the end time of the second item,
;; expressed as a timestamp
;; 
;;   <2019-08-02 Fri 12:45-13:30>
;; 
;; which can be edited in the minibuffer, put on the kill ring or
;; inserted at point, replacing the conflicting timestamp.
;; 
;; Example usage (non-interactive mode):
;; 
;; The predicate `org-conflict-conflict-p' returns the conflict status
;; of its arguments as a boolean.  In its simplest form, the case
;; above could be couched in terms of a time value for the start time
;; and an integer for the duration
;; 
;;  (org-conflict-conflict-p '(0 30 10 2 8 2019) 45)
;; 
;; and will return t.  Additional arguments return information about
;; the detected conflicts or their resolution instead.

;;; Code:

(require 'org-agenda)
(declare-function org-element-type "org-element")

(defgroup org-conflict nil
  "Options concerning the Org agenda timing conflicts checker/resolver."
  :tag "Org Agenda Conflicts"
  :group 'org-agenda)

(defcustom org-conflict-conflict-action 'review
  "Action `org-conflict' should take upon detecting conflicts.
Interactively, determines how to handle conflicts.  Valid values
for this symbol are:

    review: Present conflicts for review and resolution.
   resolve: Compute and present a resolution for further action.
   replace: Insert a resolution at point, replacing the current timestamp."
  :group 'org-conflict
  :tag "Conflict Action"
  :type '(choice
	  (const :tag "Present conflicts for review and resolution" review)
	  (const :tag "Present a resolution for further action" resolve)
	  (const :tag "Insert a resolution at point" replace)))

(defcustom org-conflict-event-minimum-interval 0
  "Minimum number of minutes between two consecutive non-conflicting events.
This value enables planning for a break between meetings.

Set to 0, the following back-to-back events do not conflict:

    8:45- 9:30
    9:30-10:00
   10:00-11:00

Set to 15, the events above would need to be minimally spaced
like this:

    8:45- 9:30
    9:45-10:15
   10:30-11:30

Intervals do not extend past midnight.  Also, there are no
intervals before and after events that span multiple days."
  :group 'org-conflict
  :tag "Intervals between Events"
  :type 'integer)

(defcustom org-conflict-default-event-duration 0
  "Default duration, in minutes, assumed for rangeless timestamps.
When set to 0, invoking `org-conflict' on the rangeless timestamp

    <2019-07-19 Fr 10:00>

will prompt for a duration.  When set to, say, 45, all checks
will silently assume events to take 45 minutes.

A numeric prefix argument for `org-conflict' will temporarily
override the global value, so that

 C-u 50 M-x org-conflict 

sets duration to 50 minutes for this invocation."
  :group 'org-conflict
  :tag "Default Event Duration"
  :type 'integer)


(defcustom org-conflict-resolution-horizon-days 30
  "Maximum number of days in which a resolution must be found.
When a check exceeds this value, the following happens:

- interactively, a prompt will ask whether to continue searching
  the next N days or abort

- called from a program, nil will be returned immediately."
  :group 'org-conflict
  :tag "Resolution Horizon"
  :type 'integer)

(defvar org-conflict-resolution-agenda-buffer nil
  "Name/status marker of the private org-conflict agenda buffer.")

(defvar org-conflict-agenda-buffer-name-cache nil
  "Name of original agenda buffer to be restored.")

(defvar org-conflict-window-conf nil
  "Window configuration to be restored.")

;;;###autoload
(defalias 'org-conflict 'org-conflict-conflict-p)

(defun org-conflict-check-timestamp-or-range ()
  "Hook function suitable for adding to 'org-ctrl-c-ctrl-c-hook.
Normalizes timestamps before checking for conflicts.

A numeric prefix argument other than 0 sets the event duration
for a rangeless timestamp to that many minutes.  This overrides
the value of 'org-conflict-default-event-duration for the current
check.

A numeric prefix of 0 bypasses checking."
  (if (or (eq current-prefix-arg 'org-conflict-check-timestamp-or-range)
          (eq current-prefix-arg 0)
          (not (memq (org-element-type (org-element-context))
                     '(timestamp planning))))
      nil
    (let ((org-conflict-default-event-duration
           (if (and current-prefix-arg
                    (numberp current-prefix-arg))
               current-prefix-arg
             org-conflict-default-event-duration))
          (current-prefix-arg 'org-conflict-check-timestamp-or-range))
      (ignore-errors
        (call-interactively #'org-ctrl-c-ctrl-c))
      (call-interactively #'org-conflict-conflict-p)
      t)))


(defun org-conflict-conflict-p (&optional time
                                          rangeend
                                          return-conflicts
                                          conflict-action)
  "Check timestamps for conflicts with Org agenda items.
Interactively, offer to either resolve the conflicts, compute a
resolution or insert a resolution immediately.  Called from a
program, behave like a predicate or return information about
conflicts and resolutions.

Interactively, if point is not at a time range or timestamp,
prompt for a date+time, popping up the calendar, if configured.
Otherwise, compare the time range or timestamp at point against
existing timestamps and -ranges as used for the agenda.  Eligible
for comparison are timestamps/ranges which are active and contain
both a time-of-day and a range \(i.e. specify a start time and a
duration\).

Called programmatically, expects a start time TIME value as
returned by `decode-time' and a RANGEEND which can either be a
future time value or a positive integer denoting the duration,
measured in minutes.  RANGEEND defaults to, if set, the
calculated value of TIME + 'org-conflict-default-event-duration.

Returns nil if no conflicts were detected, t otherwise.

If flag RETURN-CONFLICTS is non-nil, returns a list of conflicts.
Items are lists containing six elements:

 DAYNUMBER integer suitable for use by org agenda functions
 START     start time value
 END       end time value
 HEADING   heading of conflict item, augmented by date and time
 MARKER    item's marker as set by Org, suitable for navigation
 SEQUENCE  cons cell encoding sequence information or nil

Symbol CONFLICT-ACTION determines what to do when conflicts were
detected.  Refer to `org-conflict-present-conflicts' for more
information on actions available in interactive mode.

When called programmatically, supported CONFLICT-ACTION symbols
are:

resolution            returns a list with data for the earliest 
                      non-conflicting time:

                      START     start time value 
                      END       end time value 
                      DURATION  duration in minutes (integer)
                      SEQUENCEP item follows multi-day sequence

resolution-as-string  returns an Org timestamp/range

CONFLICT-ACTION is honoured only when RETURN-CONFLICTS is nil.
When no resolution could be established, returns nil or the empty
string."
  (interactive
   (append
    (if (org-at-date-range-p 'lax)
        (org-conflict-get-times-at-point (point)
                                         (list (car (match-data)) (cadr (match-data)))
                                         t nil (car (match-data)))
      (if (org-at-timestamp-p 'lax)
          (org-conflict-get-times-at-point (point) (list (car (match-data))))
        (list (decode-time (org-read-date t t)) nil)))
    (list nil org-conflict-conflict-action))) ; when user action involved, prepare for action
  (let* ((skip-marker-posn (get-text-property (point) 'skip-markers))
         (return-pos (get-text-property (point) 'return-pos))
         (time (and (org-conflict-time-p time) time))
         (rangeend (or (and rangeend
                            (if (integerp rangeend)
                                (org-conflict-time-add time (* 60 rangeend))
                              (and (org-conflict-time-p rangeend) rangeend)))
                       (and current-prefix-arg
                            (integerp current-prefix-arg)
                            (org-conflict-time-add time (* 60 current-prefix-arg)))
                       (org-conflict-adjust-for-default-duration time)))
         ;; range end must be in the future
         (rangeend1 (if (<= (time-to-seconds (apply #'encode-time rangeend))
                            (time-to-seconds (apply #'encode-time time)))
                        (let ((org-conflict-default-event-duration nil)) ; force prompting
                          (org-conflict-adjust-for-default-duration time))
                      rangeend))
         (conflicts (org-conflict-get-conflicts time rangeend1 skip-marker-posn))
         (conflict-action (or conflict-action org-conflict-conflict-action)))
    (and return-pos (goto-char return-pos))
    (if (called-interactively-p 'any)
        (if (not conflicts)
            (message "No conflicts found%s." (org-conflict-restriction-p t ", "))
          (setq org-conflict-agenda-buffer-name-cache (or org-agenda-buffer-tmp-name
                                                          org-agenda-buffer-name)
                org-conflict-window-conf (current-window-configuration))
          (org-conflict-present-conflicts time rangeend1 skip-marker-posn
                                          conflicts conflict-action)
          (org-conflict-agenda-quit))
      (if (not conflicts)
          nil
        (if return-conflicts
            conflicts
          (if (memq conflict-action '(resolution resolution-as-string))
              (let ((resolution (org-conflict-resolve-conflicts time rangeend1
                                                                skip-marker-posn conflicts)))
                (if (eq conflict-action 'resolution-as-string)
                    (if resolution
                        (org-conflict-make-timestamp-or-range (car resolution) (cadr resolution))
                      "")
                  resolution))
            t))))))

(defun org-conflict-present-conflicts (time rangeend skip-marker-posn conflicts
                                            &optional conflict-action resolution)
  "Present conflicts CONFLICTS for time TIME and time RANGEEND
and solicit actions prompted for in the minibuffer.
Optional symbol CONFLICT-ACTION preselects an action to take.
Resolution RESOLUTION is a cache value used when recursing.

Valid values for CONFLICT-ACTION are:
review   print statistics about conflicts and offer to resolve them
resolve  compute a resolution and present it for further action
replace  replace the timestamp at point with the resolution or insert it"
  (let* ((singletonp (= (length conflicts) 1))
         (teaser (nth 3 (car conflicts)))
         (restriction-info (org-conflict-restriction-p t))
         (prompt (format (concat "Conflict%s detected%s %s%s\nPress "
                                 "[s]how agenda, "
                                 "[r]esolve conflict%s "
                                 "or "
                                 "[q]uit: ")
                         (if singletonp "" "s")
                         (if singletonp ":" (format " (%d), first:" (length conflicts)))
                         (substring teaser 0 (min (length teaser) 70))
                         restriction-info
                         (if singletonp "" "s")))
         (chars '(?s ?q ?r))
         (choice (if (memq conflict-action '(resolve replace)) ?r))
         (buf (current-buffer)))
    (while (null choice)
      (setq choice (read-char-choice prompt chars)))
    (pcase choice
      (?s (org-conflict-agenda-list (cadr (car conflicts)) nil skip-marker-posn)
          (set-buffer buf)
          (org-conflict-present-conflicts time rangeend skip-marker-posn conflicts 'review))
      (?q (message "Abort"))
      (?r (let* ((resolution
                  (or resolution
                      (funcall-interactively 'org-conflict-resolve-conflicts
                                             time rangeend skip-marker-posn conflicts)))
                 (s (car resolution))
                 (e (cadr resolution))
                 (period (org-conflict-format-minutes (nth 2 resolution)))
                 (timestamp (org-conflict-make-timestamp-or-range s e))
                 (region (save-match-data
                           (when (or (org-at-date-range-p 'lax) (org-at-timestamp-p 'lax))
                             (cons (car (match-data t)) (cadr (match-data t))))))
                 (date-info (if (and (= (nth 5 s) (nth 5 time))
                                     (= (nth 4 s) (nth 4 time))
                                     (= (nth 3 s) (nth 3 time)))
                                ""
                              "\nNote: dates of conflict and resolution differ."))
                 (interval-info (cond ((nth 3 resolution) "(gap: range has none)")
                                      ((= 0 org-conflict-event-minimum-interval) "(gap: none)")
                                      (t (format "(gap: %d min)"
                                                 org-conflict-event-minimum-interval))))
                 (doinsert (lambda (s)
                             (when region
                               (goto-char (car region))
                               (delete-region (car region) (cdr region)))
                             (insert s)
                             (message "Resolution inserted%s.%s"
                                      (if region ", replacing conflicting timestamp" "")
                                      date-info))))
            (setq chars '(?s ?c ?e ?q ?p ?r ?R)
                  choice (if (eq conflict-action 'replace) ?R)
                  prompt (format (concat "Earliest possible time for a %s period %s: "
                                         "%d %s %d, %d:%02d%s%s\nPress "
                                          "[s]how agenda, "
                                          "[p]review, "
                                          "[c]opy, "
                                          "[e]dit, "
                                          "[r]eplace/insert "
                                          "or "
                                          "[q]uit: ")
                                 period
                                 interval-info
                                 (nth 3 s)
                                 (calendar-month-name (nth 4 s))
                                 (nth 5 s)
                                 (nth 2 s)
                                 (nth 1 s)
                                 date-info
                                 restriction-info))
             ;; agenda is visible: update it if resolution and
             ;; conflict are on different dates (keep view when
             ;; resolution preview is showing)
            (if (and (not (equal conflict-action 'resolve))
                     org-conflict-resolution-agenda-buffer
                     (get-buffer org-conflict-resolution-agenda-buffer)
                     (not (equal date-info "")))
                (save-window-excursion
                  (org-conflict-agenda-list s nil skip-marker-posn)))
            (while (null choice)
              (setq choice (read-char-choice prompt chars)))
            (pcase choice
              ((or ?s ?p) (if (and org-conflict-resolution-agenda-buffer
                                   (get-buffer org-conflict-resolution-agenda-buffer)
                                   (not (equal date-info "")))
                              (kill-buffer org-conflict-resolution-agenda-buffer))
               (if (eq choice ?p)
                   (let ((tmp-agenda-file (org-conflict-temp-file
                                           s
                                           e
                                           ">>>>>>>>"
                                           "<<<<<<< r e s o l u t i o n <<<<<<<<<<<<<<<")))
                     (org-conflict-agenda-list s tmp-agenda-file skip-marker-posn))
                 (org-conflict-agenda-list s nil skip-marker-posn))
               (set-buffer buf)
               (org-conflict-present-conflicts time rangeend skip-marker-posn
                                               conflicts 'resolve resolution))
              (?c (kill-new timestamp)
                  (message
                   (substitute-command-keys "Resolution copied, use `\\[yank]' to insert.")))
              (?e (let ((resp (read-string "Edit timestamp, press RET to replace/insert: "
                                           timestamp)))
                    (funcall doinsert resp)))
              ((or ?r ?R) (funcall doinsert timestamp))
              (?q (message "Abort"))
              (_ nil))))
      (_ nil))))

(defun org-conflict-resolve-conflicts (time rangeend skip-marker-posn conflicts)
  "Calculate for conflicts CONFLICTS the earliest non-conflicting
time for time TIME, end time RANGEEND and buffer positions
SKIP-MARKER-POSN.

Returns a list with information about the resolved time
  START     start time value
  END       end time value
  DURATION  integer denoting the duration in minutes
  SEQUENCEP boolean indicating whether resolution follows 
            a multi-day sequence and thus should not 
            receive a leading interval

When no resolution could be established, abort with an error or
return nil when called from a program."
  (let* ((conflict (car (org-conflict-sort-conflicts conflicts 2 #'>))) ; sorting necessary?
         (seq-info (nth 5 conflict))
         (duration (truncate (- (time-to-seconds (apply #'encode-time rangeend))
                               (time-to-seconds (apply #'encode-time time)))))
         (int-min (when (and org-conflict-event-minimum-interval
                             (integerp org-conflict-event-minimum-interval)
                             (> org-conflict-event-minimum-interval 0))
                    org-conflict-event-minimum-interval))
         ;; crucially, ensure that the time cursor advances by at
         ;; least 1 minute past the end of the last conflict
         (int-min-effective (cond (seq-info 1) ; override regular interval after sequences
                                  (int-min (max int-min 1))
                                  (t 1)))
         (org-conflict-event-minimum-interval int-min-effective) ; set for recursion
         ;;  fast forward to sequence end
         (end-time (if (and seq-info (> (cdr seq-info) (car seq-info)))
                       (org-conflict-time-add (nth 2 conflict)
                                              (* 86400 (- (cdr seq-info)
                                                          (1+ (car seq-info)))))
                     (nth 2 conflict)))
         (next-time (org-conflict-time-add end-time (* int-min-effective 60)))
         (start-day (nth 3 (nth 2 conflict)))
         (days-searched 0)
         (next-days-mess "the next")
         next-rangeend last-conflict aborted)
    (while conflict
      (setq last-conflict conflict
            next-rangeend (org-conflict-time-add next-time duration))
      (put-text-property (point) (1+ (point)) 'skip-markers skip-marker-posn)
      (message "resolving...%s" (format-time-string "%F, %R" (apply #'encode-time next-time)))
      (let ((next-conflicts (org-conflict-conflict-p next-time next-rangeend t nil)))
        (setq conflict (car (org-conflict-sort-conflicts next-conflicts 2 #'>)))
        (when conflict
          (when (/= (nth 3 (nth 2 conflict)) start-day)
            (setq start-day (nth 3 (nth 2 conflict))
                  days-searched (1+ days-searched)))
          (let ((int-min-loop int-min-effective))
            ;; just to be safe: check result to prevent infinite loops
            ;; on recurring input
            (when (equal (nth 2 conflict) (nth 2 last-conflict))
              (setq int-min-loop (1+ int-min-effective)
                    int-min nil))
            (setq next-time (org-conflict-time-add (nth 2 conflict) (* int-min-loop 60))))
          ;; Panic: after having searched
          ;; 'org-conflict-resolution-horizon-days days with no
          ;; result, prompt user.  Called programmatically, abort
          ;; immediately and return nil.
          (when (> days-searched org-conflict-resolution-horizon-days)
            (if (called-interactively-p 'any)
                (if (y-or-n-p (format (concat "No resolution found within %s %d days. "
                                              "Continue? ")
                                      next-days-mess
                                      org-conflict-resolution-horizon-days))
                    (setq days-searched 0
                          next-days-mess "another")
                  (error "No resolution found. Aborted."))
              (setq conflict nil
                    aborted t))))))
    (message "resolving... done")
    ;; for consistency, subtract the effective minimum interval when
    ;; the artifical 1-minute advance was used for 0/nil-valued
    ;; interval-minimum variable or a multi-day sequence ("no
    ;; coffee-break after vacation end")
    (when (or (not int-min)
              seq-info)
      (setq next-time (org-conflict-time-add next-time (* int-min-effective -60))
            next-rangeend (org-conflict-time-add next-rangeend (* int-min-effective -60))))
    (if aborted
        nil
      (list next-time
            next-rangeend
            (/ duration 60)
            (consp seq-info)))))

(defun org-conflict-get-times-at-point (return-pos
                                        &optional marker-posn rangep range-part-p pom)
  "Converts time ranges and timestamps at position RETURN-POS.
Returns a list of decoded times.  May prompt for missing
start and/or end times."
  (let (times)
    (if pom (goto-char pom))            ; have to move point for ranges, can't save excursion
    (if (and rangep
             (not range-part-p))
        (and (setq times (org-conflict-get-times-at-point nil nil t t))
             (search-forward "--")
             (setq times (list (car times)
                               (car (org-conflict-get-times-at-point nil nil t t)))))
      (org-at-timestamp-p 'lax) ; set match data
      (let ((date (format "%s-%s-%s " (match-string 2) (match-string 3) (match-string 4)))
            (time (match-string 6))
            (end-time range-part-p))
        (if (and time
                 (save-match-data
                   (string-match "\\`[ ]*[0-9]\\{1,2\\}:[0-9]\\{2\\}" time)))
            (setq times
                  (list
                   (save-match-data
                     (decode-time (org-read-date nil t (concat date time))))
                   (progn
                     (goto-char (car (reverse (match-data))))
                     (if (and (not rangep)
                              (looking-at "-?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"))
                         (decode-time
                          (org-read-date nil t (concat date
                                                       (buffer-substring (match-beginning 1)
                                                                         (match-end 1))))
                          nil)))))
          (setq time (read-string
                      (format "Specify start time HH:MM for \"%s\": " (match-string 1))
                      "00:00"))
          (unless (string-match "^[ ]*[012][0-9]?:[0-5][0-9][ ]*$" time)
            (error "Wrong format for time: %s " time))
          (unless end-time
            (let ((resp (read-string "Specify end time HH:MM or duration INT minutes: "
                                     (if (> org-conflict-default-event-duration 0)
                                         (format "%d" org-conflict-default-event-duration)
                                       time))))
              (if (and (not (string-match ":" resp))
                       (string-match "^\\s-*\\([0-9]+\\)" resp)
                       (> (read (match-string 1 resp)) 0))
                  (setq end-time (read (match-string 1 resp)))
                (progn
                  (unless (string-match "^[ ]*[0-9]\\{1,2\\}:[0-5][0-9][ ]*$" resp)
                    (error "Wrong format: \"%s\" " resp))
                  (unless (string-greaterp (replace-regexp-in-string ":" "" resp)
                                           (replace-regexp-in-string ":" "" time))
                    (error "Malformed end time %s: must be after start time %s" resp time))
                  (setq end-time (concat date resp))))))
          (setq times (list (decode-time (org-read-date t t (concat date time))) 
                            (if (stringp end-time)
                                (decode-time (org-read-date t t end-time))
                              end-time))))))
    (if marker-posn
        (progn
          (goto-char (car marker-posn))
          (put-text-property (point) (1+ (point)) 'skip-markers
                             (mapcar 'marker-position marker-posn))))
    (put-text-property (point) (1+ (point)) 'return-pos return-pos)
    times))

(defun org-conflict-get-conflicts (time &optional rangeend skip-posn)
  "Compile a list of conflicts for time TIME.

Optional time RANGEEND determines end time.  List SKIP-POSN
contains buffer positions of items not be included in the checks
\(e.g. the tester item\).

Injects a test entry into Org's agenda workings and parses
agenda buffers containing format-controlled day listings."
  (let* ((work-buf-name " *org day schedule*")
         (work-buf (get-buffer-create work-buf-name))
         (tmp-agenda-file (org-conflict-temp-file time rangeend))
         (org-agenda-buffer-name work-buf-name)
         (org-agenda-timerange-leaders  '("" "%d/%d"))
         (org-agenda-scheduled-leaders '("" ""))
         (delim "|")
         (org-agenda-prefix-format (concat "%s" delim "%t" delim "%c" delim))
         (org-agenda-category-icon-alist nil)
         (org-agenda-persistent-filter nil)
         (org-prefix-category-max-length nil)
         (org-agenda-remove-tags t)
         (org-agenda-remove-times-when-in-prefix nil)
         (org-agenda-repeating-timestamp-show-all t)
         (org-agenda-skip-archived-trees t)
         (org-agenda-skip-comment-trees t)
         (org-agenda-ignore-properties nil)
         (org-agenda-persistent-filter nil)
         (org-agenda-include-inactive-timestamps nil)
         (org-agenda-skip-timestamp-if-done t)
         (org-agenda-skip-comment-trees t)
         (org-agenda-use-tag-inheritance nil)
         (org-agenda-sticky nil)
         (types '(:deadline* :scheduled* :timestamp :sexp))
         (agenda-files (append (org-agenda-files t nil) (list tmp-agenda-file)))
         (daynumbers (number-sequence (time-to-days (apply #'encode-time time))
                                      (time-to-days (apply #'encode-time (or rangeend time)))))
         tmp-agenda-buffer daynum dayentries allentries files file date conflicts)
    (or (setq tmp-agenda-buffer (find-file-noselect tmp-agenda-file))
        (error "Can't open temp file %s!" tmp-agenda-file))
    (unwind-protect
        (with-current-buffer work-buf
          (org-agenda-prepare)
          (org-compile-prefix-format nil)
          (catch 'onfirst
            (while (setq daynum (pop daynumbers))
              (setq date (calendar-gregorian-from-absolute daynum)
                    files agenda-files
                    allentries nil)
              (erase-buffer)
              (while (setq file (pop files))
                (catch 'nextfile
                  (org-check-agenda-file file)
                  (setq dayentries (apply 'org-agenda-get-day-entries file date types))
                  (setq allentries (append allentries dayentries))))
              (if allentries
                  (progn
                    (insert (mapconcat 'identity allentries "\n") "\n")
                    (if (setq conflicts (org-conflict-parse-daylisting-for-conflicts
                                         tmp-agenda-file delim daynum skip-posn))
                        (throw 'onfirst t)))))))
      (kill-buffer work-buf)
      (kill-buffer tmp-agenda-buffer)
      (delete-file tmp-agenda-file))
    conflicts))

(defun org-conflict-parse-daylisting-for-conflicts (tester-id field-delimiter
                                                    day-number skip-list)
  "Parse a format-controlled agenda day view to determine conflicts.
Recognize the tester item by its category TESTER-ID after
splitting lines on FIELD-DELIMITER.  Uses Org DAY-NUMBER for
inclusion in returned conflict items.  Uses buffer positions in
SKIP-LIST to skip over entries (such as the tester item).

Returns list of conflict items."
  (let ((effective-interval (if (and org-conflict-event-minimum-interval
                                     (integerp org-conflict-event-minimum-interval))
                                (1- org-conflict-event-minimum-interval)
                              -1))
        fields spans tester-start tester-end tester-category)
    (goto-char (point-min))
    (while (not (eobp))
      (let (marker start end range time category headline span seqno seqtotal seqstart seqend)
        (setq fields (split-string (buffer-substring-no-properties (point-at-bol)
                                                                   (point-at-eol))
                                   field-delimiter nil " +")
              marker (get-text-property (point-at-bol) 'org-marker)
              range (nth 0 fields)
              time (nth 1 fields)
              category  (nth 2 fields)
              headline (if (nth 4 fields) ; headline contained delimiter
                           (mapconcat (lambda (n)
                                        (nth n fields))
                                      (number-sequence 3 (1- (length fields)))
                                      (concat " " field-delimiter " "))
                         (nth 3 fields)))
        (if (and category
                 time
                 (not (equal category ""))
                 (string-match "\\`\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\(.+\\)" time))
            (setq start (+ (* (string-to-number (match-string 1 time)) 60)
                           (string-to-number (match-string 2 time)))
                  end (if (equal (aref (match-string 3 time) 0) ?-)
                          (+ (* (string-to-number (substring (match-string 3 time) 1 3)) 60)
                             (string-to-number (substring (match-string 3 time) 4 6)))
                        nil)))
        ;; Org doesn't seem to interpret timestamps intending to
        ;; encode an overnight repeater such as <1999-05-06 Mo 15:00-10:00 +1d>
        ;; Thus we split the entry and create a virtual sequence.
        (if (and start end
                 (> start end)
                 (not (string= category tester-id)))
            (save-excursion
              (setq end 1440)
              (forward-line 1)
              (insert (propertize (format "2/2|%s%s|%s|%s\n"
                                          (substring (match-string 3 time) 1)
                                          (match-string 3 time)
                                          category headline)
                                  'org-marker marker))))
        (when (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)" range)
          (setq seqno (read (match-string 1 range))
                seqtotal (read (match-string 2 range))
                seqstart (and (= seqno 1) start)
                seqend (and (= seqno seqtotal) start)))
        (cond ((and skip-list
                    (get-text-property (point-at-bol) 'org-marker)
                    (memq (marker-position (get-text-property (point-at-bol) 'org-marker))
                          skip-list))
               nil)
              ((and (not tester-start)
                    (string= category tester-id))
               (setq tester-start (if seqno (or seqstart 0) (or start 0))
                     tester-end (if seqno (or seqend 1440) (or end 1440))
                     tester-category category))
              ((and seqno
                    (not (equal category tester-category)))
               (if seqstart
                   (setq span (cons seqstart 1440))
                 (if seqend
                     (setq span (cons 0 seqend))
                   (setq span (cons 0 1440)))))
              (end
               (setq span (cons start end))))
        (when span
          (let* ((cal-date (calendar-gregorian-from-absolute day-number))
                 (day (cadr cal-date))
                 (mon (car cal-date))
                 (year (nth 2 cal-date))
                 (starth (/ (car span) 60))
                 (startm (% (car span) 60))
                 (endh (/ (cdr span) 60))
                 (endm (% (cdr span) 60))
                 (display-string (format "%d %s %d %d:%02d-%d:%02d, %s"
                                         day (calendar-month-name mon) year
                                         starth startm 
                                         endh endm 
                                         headline)))
            (setq spans
                  (append spans
                          (list
                           (list span
                                 day-number
                                 (list 0 startm starth day mon year)
                                 (list 0 endm endh day mon year)
                                 display-string
                                 marker
                                 (and seqno (cons seqno seqtotal)))))))))
      (forward-line 1))
    (unless tester-start
      (error "Cannot find test entry in day view, expected category \"%s\"" tester-id))
    (delq nil
          (mapcar 
           (lambda (span)
             (let* ((s (caar span))
                    (e (cdar span))
                    (seq (nth 6 span))
                    (pre-gap (if (and seq (/= (car seq) 1))
                                 -1   ; no gap after sequence end
                               effective-interval))
                    (post-gap (if (and seq (= (car seq) 1))
                                  -1  ; no gap before sequence start
                                effective-interval))
                    (start (- tester-start pre-gap))
                    (end (+ tester-end post-gap)))
               (if (or (and (>= start s) (<= start e)) 
                       (and (<= start s) (>= end s) (<= end e))
                       (and (<= start s) (>= end e)))
                   (setcdr span (cdr span))
                 nil)))
           spans))))

(defun org-conflict-sort-conflicts (conflicts nth &optional pred)
  "Sort conflicts CONFLICTS by comparing their NTH field.
Comparison may use optional predicate PRED, defaults to #'<."
  (let ((predicate (or pred #'<)))
    (sort conflicts
          #'(lambda (a b)
              (funcall predicate
                       (time-to-seconds (apply #'encode-time (nth nth a)))
                       (time-to-seconds (apply #'encode-time (nth nth b))))))))

(defun org-conflict-temp-file (time &optional rangeend uid heading)
  "Creates a temporary file containing an item to be injected.
Item will receive category UID and a timestamp encoding time TIME
and time RANGEEND.  May carry the optional heading HEADING.

Returns name of temp file."
  (let* ((tmp-agenda-file (org-babel-temp-file "agenda-conflict-" ".org"))
         (uid (or uid tmp-agenda-file))
         (tmp-agenda-item (org-conflict-create-conflict-item uid time rangeend heading)))
    (with-temp-file tmp-agenda-file (insert tmp-agenda-item))
    tmp-agenda-file))

(defun org-conflict-create-conflict-item (uid time &optional endtime heading) 
  "Return an Org item with category UID for time TIME and time ENDTIME.
This test item will be saved to a file to be temporarily included
in the list of Org agenda files."
  (format "* %s\nSCHEDULED: %s\n:PROPERTIES:\n:CATEGORY: %s\n:END:\n"
          (or heading "conflict tester")
          (org-conflict-make-timestamp-or-range time endtime)
          uid))

(defun org-conflict-make-timestamp-or-range (time &optional endtime)
  "Return an Org timestamp or -range for time TIME and time ENDTIME."
  (format "<%d-%02d-%02d %s %02d:%02d%s"
          (nth 5 time) (nth 4 time) (nth 3 time)
          (format-time-string "%a" (apply #'encode-time time))
          (nth 2 time) (nth 1 time)
          (if (not endtime)
              ">"
            (if (or (/= (nth 5 endtime) (nth 5 time))
                    (/= (nth 4 endtime) (nth 4 time))
                    (/= (nth 3 endtime) (nth 3 time)))
                (format ">--<%d-%02d-%02d %s %02d:%02d>"
                        (nth 5 endtime) (nth 4 endtime) (nth 3 endtime)
                        (format-time-string "%a" (apply #'encode-time endtime))
                        (nth 2 endtime) (nth 1 endtime))
              (if (and (= (nth 1 endtime) (nth 1 time))
                       (= (nth 2 endtime) (nth 2 time)))
                  ">"
                (format "-%02d:%02d>" (nth 2 endtime) (nth 1 endtime)))))))

(defun org-conflict-adjust-for-default-duration (time)
  "Apply a default duration to a rangeless time TIME.
This uses the user variable 'org-conflict-default-event-duration
when set to a value greater than 0.  Otherwise, prompt user."
  (let ((default (and org-conflict-default-event-duration ; dynamically scoped
                      (integerp org-conflict-default-event-duration)
                      (> org-conflict-default-event-duration 0)
                      org-conflict-default-event-duration)))
    (if default
        (org-conflict-time-add time (* 60 default))
      (let ((mins 0)
            (factors '(("m" . 1) ("h" . 60) ("d" . 1440)))
            resp)
        (while (<= mins 0)
          (setq resp (read-string "Specify a duration, in minutes N, hours Nh or days Nd: "))
          (if (string-match "^\\s-*\\([0-9]+\\(\\.[0-9]+\\)?\\)\\([mdh]\\)" resp)
              (setq resp (+ (* (cdr (assoc (match-string 3 resp) factors))
                            (read (match-string 1 resp)))
                         (if (match-string 2 resp)
                             (* (cdr (assoc (match-string 3 resp) factors))
                                (read (match-string 2 resp)))
                           0)))
            (setq resp (read resp)))
          (and (numberp resp)
               (setq mins (truncate resp))))
        (org-conflict-time-add time (* 60 resp))))))

(defun org-conflict-time-p (arg)
  "Return t if ARG looks like a valid time value."
  (or (and (listp arg)
           (>= (length arg) 6)
           (not (memq nil (mapcar 'integerp (seq-take arg 6)))))
      (error "Not a time value: %S" arg)))

(defun org-conflict-time-add (time seconds)
  "Returns time TIME adjusted by seconds SECONDS."
  (decode-time
   (time-add seconds
             (time-to-seconds (apply #'encode-time time)))))

(defun org-conflict-agenda-list (time &optional file-to-inject skip-marker-posn)
  "Run `org-agenda-list' for the day of time TIME.
Temporarily add file FILE-TO-INJECT to the list of agenda files.
Set up a `org-agenda-skip-function' with positions in list
SKIP-MARKER-POSN."
  (let ((org-agenda-skip-function
         (and skip-marker-posn
              `(org-conflict-agenda-skip-function '(,@skip-marker-posn)))))
    (setq org-conflict-resolution-agenda-buffer "*Org Conflict Agenda*"
          org-agenda-buffer-tmp-name org-conflict-resolution-agenda-buffer)
    (message "compiling agenda day view...")
    (if (not file-to-inject)
        (org-agenda-list nil (time-to-days (apply #'encode-time time)) 'day t)
      ;; these `org-agenda-files' options OK to use?
      (let ((org-agenda-files (append (org-agenda-files t nil) (list file-to-inject)))
            tmp-agenda-buffer)
        (unwind-protect
            (org-agenda-list nil (time-to-days (apply #'encode-time time)) 'day t)
          (if (setq tmp-agenda-buffer (find-buffer-visiting file-to-inject))
              (progn
                (kill-buffer tmp-agenda-buffer)
                (delete-file file-to-inject))))))
    (message "compiling agenda day view...done")))

(defun org-conflict-agenda-skip-function (posn)
  "Skip entries containing timestamps at buffer positions POSN."
  ;; this is called only when collecting for our private day view
  (save-excursion
    (save-match-data
      (forward-char -1)
      (if (and (or (org-at-date-range-p 'lax) (org-at-timestamp-p 'lax))
               (memq (car (match-data t)) posn))
            (cadr (match-data t))
        nil))))

(defun org-conflict-agenda-quit ()
  "Kill private agenda buffer with `org-agenda-quit'.
Restores agenda buffer name and pre-conflict window
configuration."
  (when (and org-conflict-resolution-agenda-buffer
             (get-buffer org-conflict-resolution-agenda-buffer))
    (let ((org-agenda-sticky nil))
      (set-buffer org-conflict-resolution-agenda-buffer)
      (setq org-conflict-resolution-agenda-buffer nil)
      (when org-conflict-agenda-buffer-name-cache
        (setq org-agenda-buffer-tmp-name org-conflict-agenda-buffer-name-cache
              org-conflict-agenda-buffer-name-cache nil))
      (call-interactively #'org-agenda-quit)
      ;; have to restore windows ourselves: setting
      ;; 'org-agenda-restore-windows-after-quit won't do as
      ;; switching between display agenda and preview agenda messes
      ;; up 'org-agenda-pre-window-conf
      (when org-conflict-window-conf
        (set-window-configuration org-conflict-window-conf)
        (setq org-conflict-window-conf nil)))))

(defun org-conflict-restriction-p (&optional return-as-string sep)
  "Return agenda restriction status.
If flag RETURN-AS-STRING is non-nil, as a string separated by
SEP (defaults to newline)."
  (if org-agenda-overriding-restriction
      (if return-as-string
          (format "%s%s restrictions are in effect"
                  (or sep "\n")
                  org-agenda-overriding-restriction)
        t)
    (if return-as-string "" nil)))

(defun org-conflict-format-minutes (minutes)
  "Return integer MINUTES as a human-readable string.
There are no 0-minute periods in org-conflict."
  (let (parts)
    (while minutes
      (let ((days (/ minutes 1440))
            (hours (/ minutes 60)))
        (if (> days 0)
            (setq parts (append parts (list (format "%dd" days)))
                  minutes (% minutes 1440))
          (if (> hours 0)
              (setq parts (append parts (list (format "%dh" hours)))
                    minutes (% minutes 60))
            (if (> minutes 0)
                (setq parts (append parts (list (format "%dmin" minutes)))
                      minutes nil)
              (setq minutes nil))))))
    (mapconcat 'identity parts " ")))

(defun org-conflict-check-org-agenda-files ()
  (unless org-agenda-files
    (user-error "org-conflict requires the variable 'org-agenda-files to be set")))

(org-conflict-check-org-agenda-files)

(provide 'org-conflict)

;;; org-conflict.el ends here
