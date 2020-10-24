(setq comphy-projects '(
			"~/cphy05/ws/COMPHY_56G_PIPE5_X4_4PLL/"
			"~/cphy05/ws/COMPHY_56G_PIPE5_X4_4PLL_R1P0/"
			"~/cphy05/ws/COMPHY_56G_PIPE5_X1_2PLL/"
			"~/cphy05/ws/COMPHY_112G_ADC_X4_8PLL/"
			"~/cphy05/ws/COMPHY_112G_ADC_X4_8PLL_R1P0/"
			))

(setq test-project-root-dir "~/5nm/ws/COMPHY_112G_ADC_X4_8PLL/")
(setq test-dir (concat test-project-root-dir "firmware_rev/current/"))
(defvar comphy-projects nil)
(defvar comphy-project-root-file "ROOT.txt")

(defvar comphy-project-fw-dir "firmware/")
(defvar comphy-project-fw-current-dir "firmare_rev/current/")
(defvar comphy-project-sim-dir "phy_top/sim/")
(defvar comphy-project-sim-tb-dir "phy_top/tb/")

(defvar-local comphy-project-root-path nil)
(defvar-local comphy-project-fw-path nil)
(defvar-local comphy-project-fw-current-path nil)
(defvar-local comphy-project-sim-path nil)
(defvar-local comphy-project-sim-tb-path nil)
(defvar-local comphy-project-search-path nil)


(defun valid_projects (project-list)
  "Return only valid dirctories in project dir list"
  (seq-filter (lambda (dir) (file-exists-p dir)) project-list))
;; (valid_projects comphy-projects)

(defun comphy-is-project-root-p (dir)
  "Return t if dir is the root of a COMPHY project"
  (file-exists-p (concat
		  (file-name-as-directory dir)
		  comphy-project-root-file)
		 ))
(comphy-is-project-root-p test-project-root-dir)

(defun comphy-project-root (dir)
  "Find COMPHY project root directorie of a dir, return nil if the dir is not part of a COMPHY project"
  (when dir
    (locate-dominating-file dir comphy-project-root-file)))
(comphy-project-root test-dir)
(comphy-project-root buffer-file-name)

(defun comphy-project-init-buffer ()
  "Init buffer local variables related to COMPHY project"
  (interactive)
  (when (comphy-project-root buffer-file-name)
	(setq-local
	 comphy-project-root-path (file-name-as-directory (comphy-project-root buffer-file-name))
	 comphy-project-fw-path (concat comphy-project-root-path comphy-project-fw-dir)
	 comphy-project-fw-current-path (concat comphy-project-root-path comphy-project-fw-current-dir)
	 comphy-project-sim-path (concat comphy-project-root-path comphy-project-sim-dir)
	 comphy-project-sim-tb-path (concat comphy-project-root-path comphy-project-sim-tb-dir)
	 comphy-project-search-path (list comphy-project-fw-current-path comphy-project-sim-tb-path)
	 projectile-project-root comphy-project-root-path
	 )))
(comphy-project-init-buffer)

(add-hook 'projectile-mode-hook 'comphy-project-init-buffer)
