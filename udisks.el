;;; udisks.el --- Mount/Unmount removable discs using Udisks2 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Caramel Hooves
;;
;; Author: Caramel Hooves <caramel.hooves@protonmail.com>
;; Maintainer: Caramel Hooves <caramel.hooves@protonmail.com>
;; Created: Dezember 09, 2023
;; Modified: Dezember 09, 2023
;; Version: 0.0.1
;; Keywords: convenience hardware
;; Homepage: https://github.com/caramelhooves/emacs-udisks
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;; This package provides a simple interface to udisks2.
;;
;;; Code:


(require 'dbus)

(defgroup udisks nil
  "Interface to udisks2."
  :group 'hardware)

(defconst udisks-dbus-name "org.freedesktop.UDisks2" "Dbus name for udisks2.")

(defconst udisks-dbus-path "/org/freedesktop/UDisks2" "Dbus path for udisks2.")

(defconst udisks-dbus-interface "org.freedesktop.UDisks2" "Interface for udisks2.")

(defcustom udisks-dbus-mount-options '(:array (:dict-entry ""( :variant ""))) "Mount options for udisks2.")

(defcustom udisks-dbus-unmount-options '(:array (:dict-entry ""( :variant ""))) "Unmount options for udisks2.")

(defun udisks--dbus-call (method &rest args)
  "Call METHOD on udisks2 with ARGS."
  (apply #'dbus-call-method
         :system
         udisks-dbus-name
         udisks-dbus-path
         udisks-dbus-interface
         method
         args))

(defun udisks-get-version ()
  "Get udisks2 version."
  (dbus-get-property
   :system
   "org.freedesktop.UDisks2"
   "/org/freedesktop/UDisks2/Manager"
   "org.freedesktop.UDisks2.Manager"
   "Version"))

(defun udisks--get-managed-objects ()
  "Get all connected drives(physical devices)."
  (dbus-call-method
   :system
   "org.freedesktop.UDisks2"
   "/org/freedesktop/UDisks2"
   "org.freedesktop.DBus.ObjectManager"
   "GetManagedObjects"))

(defun udisks--filter-drives (objects)
  "Filter OBJECTS, return a list of all connected drives(physical devices)."
  (seq-filter
   (lambda (object)
     (string-match-p "/org/freedesktop/UDisks2/drives/.*" (car object)))
   objects))

(defun udisks--filter-block-devices (objects)
  "Filter OBJECTS, return a list of all block devices."
  (seq-filter
   (lambda (object)
     (string-match-p "/org/freedesktop/UDisks2/block_devices/.*" (car object)))
   objects))

(defun udisks--nul-terminated-string-to-string (byte-sequence)
  "Convert nul-terminated BYTE-SEQUENCE of ASCII characters into string."

  ;; TODO error checking: throw an error if byte sequence is not null-terminated
  (let ((ret (split-string (apply #'string b) "\x0")))
    (car ret)))

(defun udisks--get-completion-affixation-function (block-device drives)
  "Return a list of three elements to serve as annotation for BLOCK-DEVICE.
The list contains: block device name, name of physical drive from
DRIVES and a comma separated list of mount points."
  (let* ((mount-points (mapcar #'udisks--nul-terminated-string-to-string (caadr (assoc "MountPoints" (cadr (assoc "org.freedesktop.UDisks2.Filesystem" (cadr block-device)))))))
         (drive-name (caadr (assoc "Drive" (cadr (assoc "org.freedesktop.UDisks2.Block" (cadr block-device))))))
         (drive (assoc drive-name drives))
         (drive-id (caadr (assoc "Id" (cadr (assoc "org.freedesktop.UDisks2.Drive" (cadr drive)))))))
    (list (string-remove-prefix "/org/freedesktop/UDisks2/block_devices/" (car block-device)) drive-id (format " -> %s" (string-join mount-points ",")))))

(defun udisks--completion (block-devices drives)
  "Return a completion function.
The completion function is suitable to be passed in
`completing-read'. It also provide annotation for BLOCK-DEVICES
using information from DRIVES."
  (lambda (str pred flag)
    (pcase flag
      ('metadata
       `(metadata (category . mount-points)
                  (affixation-function . ,(lambda (l)
                                       (mapcar (lambda(block-device) (udisks--get-completion-annotation-function block-device drives)) block-devices)))))
      (_ (all-completions str (mapcar #'car block-devices) pred)))))


(defun udisks--completion-read-block-device (prompt)
  "Read block device name from minibuffer.
PROMPT is a string to prompt with."
  (let*
      ((objects (udisks--get-managed-objects))
       (block-devices (udisks--filter-block-devices objects))
       (drives (udisks--filter-drives objects))
       (block-device-name (completing-read prompt (udisks--completion block-devices drives))))
    block-device-name))

(defun udisks-mount (block-device-name)
  "Mount a block device with BLOCK-DEVICE-NAME."
  (interactive (list (udisks--completion-read-block-device "Mount block device: ")))
  (dbus-call-method
   :system
   "org.freedesktop.UDisks2"
   block-device-name
   "org.freedesktop.UDisks2.Filesystem"
   "Mount"
   udisks-dbus-mount-options))

(defun udisks-unmount (object)
  "Un-mount OBJECT."
  (interactive (list (udisks--completion-read-block-device "Un-Mount block device")))
  (dbus-call-method
   :system
   "org.freedesktop.UDisks2"
   (car object)
   "org.freedesktop.UDisks2.Filesystem"
   "Unmount"
   udisks-dbus-unmount-options))

(provide 'udisks)
;;; udisks.el ends here
