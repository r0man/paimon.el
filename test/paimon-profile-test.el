;; paimon-profile-test.el --- Paimon profile tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Paimon profile tests

;;; Code:

(require 'closql)
(require 'ert)
(require 'paimon)
(require 'paimon-profile)
(require 'paimon-test)

(ert-deftest paimon-profile-current-test ()
  (paimon-test-with-db db
    (should (null (paimon-profile-current db)))
    (let ((profile-1 (paimon-profile-set-default (closql-insert db (paimon-profile)))))
      (should (equal profile-1 (paimon-profile-current db)))
      (let ((profile-2 (paimon-profile-set-default (closql-insert db (paimon-profile)))))
        (should (equal profile-2 (paimon-profile-current db)))))))

(ert-deftest paimon-profile-insert-test ()
  (paimon-test-with-db db
    (should (closql-insert db paimon-test-profile))))

(ert-deftest paimon-profiles-test ()
  (paimon-test-with-db db
    (should (closql-insert db paimon-test-profile))
    (should (equal 1 (length (paimon-profiles db))))))

(ert-deftest paimon-profile-by-id-test ()
  (paimon-test-with-db db
    (let ((profile (closql-insert db paimon-test-profile)))
      (equal (oref profile id)
             (oref (paimon-profile-by-id db (oref profile id)) id)))))

(ert-deftest paimon-profile-set-default-test ()
  (paimon-test-with-db db
    (let* ((profile-1 (closql-insert db (paimon-profile)))
           (profile-2 (closql-insert db (paimon-profile))))
      (paimon-profile-set-default profile-1)
      (should (equal 1 (oref (closql-reload profile-1) default)))
      (should (equal 0 (oref (closql-reload profile-2) default)))
      (paimon-profile-set-default profile-2)
      (should (equal 0 (oref (closql-reload profile-1) default)))
      (should (equal 1 (oref (closql-reload profile-2) default))))))

(ert-deftest paimon-profile-setup-test ()
  (paimon-test-with-db db
    (cl-letf (((symbol-function 'paimon-profile--read)
               (lambda () (paimon-profile)))
              ((symbol-function 'paimon-profile-secret)
               (lambda (_) "SECRET")))
      (let ((profile-1 (paimon-profile-setup db)))
        (should (equal 1 (oref (closql-reload profile-1) default)))
        (let ((profile-2 (paimon-profile-setup db)))
          (should (equal 1 (oref (closql-reload profile-1) default)))
          (should (equal 0 (oref (closql-reload profile-2) default))))))))

(ert-deftest paimon-profile-secret-found-test ()
  (let ((profile (paimon-profile))
        (save-function-called-p nil))
    (with-slots (hostname identity port) profile
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest args)
                   (should (equal identity (plist-get args :user)))
                   (should (equal hostname (plist-get args :host)))
                   (should (equal (number-to-string port) (plist-get args :port)))
                   (should (equal 1 (plist-get args :max-tokens)))
                   (list (list :save-function (lambda () (setq save-function-called-p t))
                               :secret (lambda () "SECRET"))))))
        (should (equal "SECRET" (paimon-profile-secret profile)))
        (should save-function-called-p)))))

(ert-deftest paimon-profile-secret-not-found-test ()
  (let ((profile (paimon-profile))
        (save-function-called-p nil))
    (with-slots (hostname identity port) profile
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _args)
                   (list (list :save-function (lambda () (setq save-function-called-p t))
                               :secret (lambda () "SECRET"))))))
        (should (equal "SECRET" (paimon-profile-secret profile)))
        (should save-function-called-p)))))
