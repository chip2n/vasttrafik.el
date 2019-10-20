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

(defun vasttrafik-search ()
  (interactive)
  (if (not vasttrafik-api-key)
      (message "You need to specify an API key for Västtrafik first (using vasttrafik-api-key).")
    (ivy-read "Choose stop: " vasttrafik--known-stops
              :action (lambda (s)
                        (vasttrafik--get-access-token
                         (lambda (token)
                           (if (listp s)
                               (vasttrafik--fetch-departures token (cdr s))
                             (vasttrafik--search-stops token s))))))))

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

(defun vasttrafik--search-stops (token query)
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
                                   (vasttrafik--fetch-departures token (cdr s))))))
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

(defun vasttrafik--format-departure (dep)
  (let ((name (alist-get 'name dep))
        (time (alist-get 'time dep))
        (track (alist-get 'track dep))
        (direction (alist-get 'direction dep)))
    (format "%s: %s towards %s (track %s)"
            (propertize time 'face '(:foreground "red" :weight bold))
            (propertize name 'face '(:weight bold))
            direction
            track)))

(provide 'vasttrafik)

;;; vasttrafik.el ends here
