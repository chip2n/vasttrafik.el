;;; vasttrafik.el --- Vasttrafik client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019 Andreas Arvidsson

;; Author: Andreas Arvidsson <andreas@arvidsson.io>
;; Keywords: tools
;; Version: 0.0.1
;; Package-Requires: ((request "0.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'request)

(defvar vasttrafik-api-key nil)
(defvar vasttrafik--known-stops nil)

(defmacro vasttrafik--with-token (token &rest body)
  (declare (indent 1))
  `(if (not vasttrafik-api-key)
       (message "You need to specify an API key for Västtrafik first (using vasttrafik-api-key).")
     (vasttrafik--get-access-token
      (lambda (,token)
        ,@body))))

(defmacro vasttrafik--with-stop (prompt stop &rest body)
  (declare (indent 2))
  `(vasttrafik--pick-stop token ,prompt (lambda (,stop) ,@body)))

(defun vasttrafik--pick-stop (token prompt handler)
  (ivy-read prompt vasttrafik--known-stops
            :action (lambda (s)
                      (if (listp s)
                          (funcall handler s)
                        (vasttrafik--search-stops token s handler)))))

(defun vasttrafik-trip ()
  (interactive)
  (vasttrafik--with-token token
    (vasttrafik--with-stop "Start: " start
      (vasttrafik--with-stop "End: " end
        (vasttrafik--calculate-trip token (cdr start) (cdr end))))))

(defun vasttrafik-table ()
  (interactive)
  (vasttrafik--with-token token
    (vasttrafik--with-stop "Choose stop: " s
      (vasttrafik--fetch-departures token (cdr s)))))

(defun vasttrafik--get-access-token (on-success)
  (request
   "https://api.vasttrafik.se:443/token"
   :type "POST"
   :headers `(("Authorization" . ,(format "Basic %s" vasttrafik-api-key)))
   :data `((grant_type . "client_credentials") 
           (scope . "device_emacs"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall on-success (vasttrafik--parse-token-response data))))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error while fething Västtrafik access token: %S" error-thrown)))))

(defun vasttrafik--parse-token-response (data)
  (alist-get 'access_token data))

(defun vasttrafik--search-stops (token query handler)
  (request
   "https://api.vasttrafik.se/bin/rest.exe/v2/location.name"
   :type "GET"
   :headers `(("Authorization" . ,(format "Bearer %s" token)))
   :params `(("input" . ,query)
             ("format" . json))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (ivy-read "Pick one: "
                         (mapcar (lambda (stop) (cons (alist-get 'name stop)
                                                      (alist-get 'id stop)))
                                 (alist-get 'StopLocation (cdar data)))
                         :action (lambda (s)
                                   (push s vasttrafik--known-stops)
                                   (funcall handler s)))))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error while fetching Västtrafik departures: %S" error-thrown)))))

(defun vasttrafik--run-shell-cmd (cmd)
  (car (split-string  (shell-command-to-string cmd) "\n")))

(defun vasttrafik--get-date ()
  (vasttrafik--run-shell-cmd "date -I"))

(defun vasttrafik--get-time ()
  (vasttrafik--run-shell-cmd "date +%H:%M"))

(defun vasttrafik--fetch-departures (token id)
  (request
   "https://api.vasttrafik.se/bin/rest.exe/v2/departureBoard"
   :type "GET"
   :headers `(("Authorization" . ,(format "Bearer %s" token)))
   :params `(("id" . ,id)
             ("date" . ,(vasttrafik--get-date))
             ("time" . ,(vasttrafik--get-time))
             ("format" . json))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message (vasttrafik--parse-data data))))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error while fetching Västtrafik departures: %S" error-thrown)))))

(defun vasttrafik--parse-data (data)
  (let ((departures (alist-get 'Departure (cdar data))))
    (string-join
     (mapcar (lambda (dep) (vasttrafik--format-departure dep))
             departures)
     "\n")))

(defun vasttrafik--format-line (name time track direction)
  (format "%s: %s towards %s (track %s)"
          (propertize (format "%s" time) 'face '(:foreground "red" :weight bold))
          (propertize (format "%s" name) 'face '(:weight bold))
          direction
          track))

(defun vasttrafik--format-time (origin-time &optional dest-time)
  (if dest-time
      (format "%s->%s" origin-time dest-time)
    origin-time))

(defun vasttrafik--format-departure (dep)
  (let ((name (alist-get 'name dep))
        (time (vasttrafik--format-time (alist-get 'time dep)))
        (track (alist-get 'track dep))
        (direction (alist-get 'direction dep)))
    (vasttrafik--format-line name time track direction)))

(defun vasttrafik--format-leg (leg)
  (let ((origin (alist-get 'Origin leg))
        (destination (alist-get 'Destination leg)))
    (let ((name (alist-get 'name leg))
          (time (vasttrafik--format-time
                 (alist-get 'time origin)
                 (alist-get 'time destination)))
          (track (alist-get 'track origin))
          (direction (alist-get 'direction leg)))
      (vasttrafik--format-line name time track direction))))

(defun vasttrafik--calculate-trip (token start end)
  (request
   "https://api.vasttrafik.se/bin/rest.exe/v2/trip"
   :type "GET"
   :headers `(("Authorization" . ,(format "Bearer %s" token)))
   :params `(("originId" . ,start)
             ("destId" . ,end)
             ("date" . ,(vasttrafik--get-date))
             ("time" . ,(vasttrafik--get-time))
             ("format" . json))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message (string-join
                         (mapcar (lambda (trip)
                                   (vasttrafik--format-leg (alist-get 'Leg trip)))
                                 (alist-get 'Trip (cdar data)))
                         "\n"))))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error while fetching Västtrafik departures: %S" error-thrown)))))

(provide 'vasttrafik)

;;; vasttrafik.el ends here
