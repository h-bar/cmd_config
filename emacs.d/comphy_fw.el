;; (defvar project-list '("~/cphy05/ws/" "~/cphyc07/ws/" "~/fw_tools/"))

(defun valid_projects (project-list)
  "Return only valid dirctories in project dir list"
  (seq-filter (lambda (dir) (file-exists-p dir)) project-list))

;;(valid_projects 'project-list)


(setq projectile-project-root-files '("ROOT.TXT"))
(setq projectile-generic-command "find -L . -type f -print0 -name '*.[ch]'")
;; (setq projectile-globally-ignored-directories )
(setq projectile-project-search-path (valid_projects '("~/cphy05/ws/" "~/cphyc07/ws/" "~/fw_tools/")))

(projectile-mode t)
(projectile-register-project-type 'comphy_fw '("ROOT.TXT")
				  :project-file "ROOT.TXT"
				  :src-dir "firmware_rev/current/"
				  :compilation-dir "firmware/release/"
				  :test-dir "phy_top/tb/"
				  :compile "fw_gen.pl"
				  )
