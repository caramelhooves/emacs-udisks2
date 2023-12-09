;;; udisks.el --- Description -*- lexical-binding: t; -*-
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

(defcustom udisks-dbus-methods
  '("EnumerateDevices" "EnumerateFilesystems" "EnumerateJobs" "GetAll" "GetBlockDevices" "GetFilesystems" "GetJobs" "GetObjects" "GetVersion" "InstallMonitor" "UninstallMonitor" "UninhibitPolling" "InhibitPolling" "SetPollingInterval" "Poll" "PowerOff" "PowerOffDrive" "PowerOn" "Rescan" "RescanDrive" "RescanMedia" "SetDrivePower" "SetDriveSpindown" "SetSpindown" "Smartctl" "SmartctlExtended" "SmartctlShort" "SmartctlSelftest" "SmartctlSelftestStatus" "SmartctlSelftestUpdate" "SmartctlUpdate" "SmartctlVendorAttributes" "SmartctlVendorAttributesUpdate" "SmartctlXall" "SmartctlXallUpdate" "SmartctlXattributes" "SmartctlXattributesUpdate" "SmartctlXbadsect" "SmartctlXbadsectUpdate" "SmartctlXerror" "SmartctlXerrorUpdate" "SmartctlXselect" "SmartctlXselectUpdate" "SmartctlXselftest" "SmartctlXselftestUpdate" "SmartctlXstatus" "SmartctlXstatusUpdate" "SmartctlXupdate" "SmartctlXupdateUpdate" "SmartctlXvendorattributes" "SmartctlXvendorattributesUpdate" "SmartctlXxall" "SmartctlXxallUpdate" "SmartctlXxattributes" "SmartctlXxattributesUpdate" "SmartctlXxbadsect" "SmartctlXxbadsectUpdate" "SmartctlXxerror" "SmartctlXxerrorUpdate" "SmartctlXxselect" "SmartctlXxselectUpdate" "SmartctlXxselftest" "SmartctlXxselftestUpdate" "SmartctlXxstatus" "SmartctlXxstatusUpdate" "SmartctlXxupdate" "SmartctlXxupdateUpdate" "SmartctlXxvendorattributes" "SmartctlXxvendoratt"))


(defun udisks--dbus-call (method &rest args)
        "Call METHOD on udisks2 with ARGS."
        (apply #'dbus-call-method
                 :system
                 udisks-dbus-name
                 udisks-dbus-path
                 udisks-dbus-interface
                 method
                 args))

(defun udisks--get-version ()
  "Get udisks2 version."
  (dbus-get-property
   :system
   "org.freedesktop.UDisks2"
   "/org/freedesktop/UDisks2/Manager"
   "org.freedesktop.UDisks2.Manager"
   "Version"))

(defun udisks--get-drives ()
  "Get all connected drives(physical devices)."
  (let ((objects
         (dbus-call-method
          :system
          "org.freedesktop.UDisks2"
          "/org/freedesktop/UDisks2"
          "org.freedesktop.DBus.ObjectManager"
          "GetManagedObjects")))

    (seq-filter
     (lambda (object)
       (string-match-p "/org/freedesktop/UDisks2/drives/.*" (car object)))
     objects)))

(defun udisks--get-block-devices ()
  "Get all block devices."
  (let ((objects
         (dbus-call-method
          :system
          "org.freedesktop.UDisks2"
          "/org/freedesktop/UDisks2"
          "org.freedesktop.DBus.ObjectManager"
          "GetManagedObjects")))

    (seq-filter
     (lambda (object)
       (string-match-p "/org/freedesktop/UDisks2/block_devices/.*" (car object)))
     objects)))

(defun udisks--get-block-devices-with-filesystem ()
  "Get all block devices which implement `org.freedesktop.UDisks2.Filesystem'."
  (seq-filter (lambda (object) (member "org.freedesktop.UDisks2.Filesystem" (mapcar #'car (car (cdr object)))))
              (udisks--get-block-devices)))

;; Drive->Id
;; Device
(defun udisks--mount (object)
 "Mount OBJECT."
 (interactive (let
                  ((objects (udisks--get-block-devices-with-filesystem)))
                (list (assoc (completing-read "Mount: " (mapcar #'car objects)) objects))))
 (dbus-call-method
  :system
  "org.freedesktop.UDisks2"
  (car object)
  "org.freedesktop.UDisks2.Filesystem"
  "Mount"
  udisks-dbus-mount-options))

(defun udisks--unmount (object)
 "Un-mount OBJECT."
 (interactive (let
                  ((objects (udisks--get-block-devices-with-filesystem)))
                (list (assoc (completing-read "Un-Mount: " (mapcar #'car objects)) objects))))
 (dbus-call-method
  :system
  "org.freedesktop.UDisks2"
  (car object)
  "org.freedesktop.UDisks2.Filesystem"
  "Unmount"
  udisks-dbus-unmount-options))

(provide 'udisks)
;;; udisks.el ends here
