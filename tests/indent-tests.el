;;; indent-tests.el --- -*- lexical-binding: t -*-

(load-file "verilog3-mode.el")

(add-to-list
   'auto-mode-alist '("\\.\\(sv\\|v\\|svh\\|vh\\|vinc\\)\\'"
                      . verilog3-mode))

(setq make-backup-files nil)

(defun indent-tests ()
  (dolist (test (directory-files "tests" t "-test.sv$"))
    (message (concat "Indenting " test))
    (find-file test)
    (verilog3-indent-buffer-measure-time)
    (write-file (concat test ".result"))))

(indent-tests)
