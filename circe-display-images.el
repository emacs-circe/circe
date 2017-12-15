;;; circe-display-images.el --- Display images in the channel -*- lexical-binding: t -*-

;; Copyright (C) 2017 Nathan Aclander

;; Author: Nathan Aclander <nathan.aclander@gmail.com>

;; This file is part of Circe.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; This Circe modules adds the ability to display various image types when
;; they are linked in a channel. Images are inserted on new lines after
;; the message containing the URLs. This module requires ImageMagcik.

;; To use it, put the following into your .emacs:

;; (require 'circe-display-images)
;; (enable-circe-display-images)

;;; Code:

(require 'circe)
(require 'url)

;;;###autoload
(defun enable-circe-display-images ()
  "Enable the Display Images module for Circe.
This module displays various image types when they are linked in a channel"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (add-circe-display-images))))
  (add-hook 'circe-channel-mode-hook
            'add-circe-display-images))

(defun disable-circe-display-images ()
  "Disable the Display Images module for Circe.
See `enable-circe-display-images'."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'circe-channel-mode)
        (remove-circe-display-images))))
  (remove-hook 'circe-channel-mode-hook
               'add-circe-display-images))

(defun add-circe-display-images ()
  "Add `circe-display-images' to `lui-pre-output-hook'."
  (add-hook 'lui-pre-output-hook 'circe-display-images))

(defun remove-circe-display-images ()
  "Remove `circe-display-images' from `lui-pre-output-hook'."
  (remove-hook 'lui-pre-output-hook 'circe-display-images))

(defgroup circe-display-images nil
  "Image display properties for Circe"
  :prefix "circe-display-images"
  :group 'circe)

(defcustom circe-display-images-image-regex
  "\\(https?:\/\/[^ ]*?\.\\\(?:png\\|jpg\\|jpeg\\|svg\\|gif\\)\\)"
  "Regex used to find images in channel messages. This regex needs to be
greedy to match multiple images on the same line."
  :group 'circe-display-images)

(defcustom circe-display-images-max-height 400
  "The image's maximum allowed height. Images will be scaled down if they
are larger than this"
  :group 'circe-display-images)

(defcustom circe-display-images-background nil
  "Background used for the images background, if image supports transparency.
Defaults to the frame's background color."
  :group 'circe-display-images)

(defcustom circe-display-images-animate-gifs nil
  "Animate any gifs that are displayed. This might slow down Emacs."
  :group 'circe-display-images)

(defvar-local circe-display-images-text-property-map (make-hash-table
                                                      :test 'equal)
  "A hash map used to manage display transitions.

The keys are urls, and the values are a plist with an `:image-property', and a
`:display-image-p'. `:image-property' is the display property of the image, and
`:display-image-p' is a flag telling us whether the image is currently visible
or not. This map serves to keep track of display transitions, and as a mapping
between the URL and its downloaded image.

Unfortunately we can't map from URL to the image position in the buffer
because 1) the lui library can move text around when executing the
`lui-post-output-hooks' and 2) as we toggle images, that also changes other
images' position in the buffer.")

(defun circe-display-images-toggle-image-at-point ()
  "Toggle the image corresponding to the url at point.

This function iterates through all display properties in the buffer. We look
for a match with the display property we got from our property map, with the
url-at-point as the key. When we find a match, we either remove or add back
the image. See `circe-display-images-text-property-map' for more details."
  ;; Giant thank you to Malabarba who's S-O answer I slightly modified:
  ;; https://emacs.stackexchange.com/a/566
  (interactive)
  (let*
      ((inhibit-read-only t)
       (url (url-get-url-at-point))
       (image-data(gethash url circe-display-images-text-property-map))
       (display-image-p (plist-get image-data :display-image-p))
       (image-property-of-url (plist-get image-data :image-property))
       (from (if display-image-p 'display 'display-backup))
       (to (if display-image-p 'display-backup 'display))
       (current-pos (point-min))
       left current-image-property)
    (while (and current-pos (/= current-pos (point-max)))
      ;; Find the next image property in the buffer.
      (if (get-text-property current-pos from)
          (setq left current-pos)
        (setq left (next-single-property-change current-pos from)))
      (if (or (null left) (= left (point-max)))
          (setq current-pos nil)
        (setq current-image-property (get-text-property left from))
        (setq current-pos (or (next-single-property-change left from)
                              (point-max)))
        ;; Swap the images if our current image matches the image from the URL.
        (when (equal image-property-of-url current-image-property)
          (add-text-properties
           left current-pos (list from nil to current-image-property)))))
    ;; Make sure to invert the :display-image-p flag after processing all
    ;; images.
    (puthash url `(:image-property ,image-property-of-url
                   :display-image-p ,(not display-image-p))
             circe-display-images-text-property-map)))

(defun circe-display-images-insert-image-from-url (url)
  "Attempt to download the image from URL, and insert it."
  (let ((buffer (url-retrieve-synchronously url)))
    (when buffer
      (unwind-protect
          (let* ((data (with-current-buffer buffer
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (buffer-substring (point) (point-max))))
                 (img (create-image
                       data 'imagemagick t
                       :max-height circe-display-images-max-height
                       :background circe-display-images-background)))
            (when img
              (insert-image img)
              ;; Store the image so that we can toggle it on and off later. We
              ;; know the image is 1 behind us, since we just inserted it.
              (let* ((image-property
                      (get-text-property (- (point) 1) 'display)))
                (puthash url
                         `(:image-property ,image-property :display-image-p t)
                         circe-display-images-text-property-map))
              ;; This is safely a no-op if the image isn't a gif.
              (when circe-display-images-animate-gifs
                (image-animate img))))
        (kill-buffer buffer)))))

(defun circe-display-images-urls-in-body ()
  "Return all urls that match the circe-display-images-image-regex"
  (let (urls)
    (while (re-search-forward circe-display-images-image-regex nil t)
      (setq urls (cons (match-string-no-properties 1) urls)))
    (reverse urls)))

(defun circe-display-images ()
  "Replace image link with downloaded image on this lui output line"
  (let ((body (text-property-any (point-min) (point-max)
                                 'lui-format-argument 'body)))
    (when body
      (goto-char body)
      (dolist (url (circe-display-images-urls-in-body))
        (newline)
        (circe-display-images-insert-image-from-url url)
        (newline)))))

(provide 'circe-display-images)
;;; circe-display-images.el ends here
