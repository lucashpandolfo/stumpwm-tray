(in-package :stumpwm)
(require 'simpletray)

(defun create-tray ()
  (simpletray:create))

(defun destroy-tray ()
  (simpletray:destroy))

(defcommand show-tray () ()
    "Shows the system tray."
    (simpletray:show))

(defcommand hide-tray () ()
    "Hides the system tray."
    (simpletray:hide))

(defcommand toggle-tray () ()
    "Toggles tray visibility."
    (simpletray:toggle))

