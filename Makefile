.PHONY: setup_drracket

setup_drracket:
	raco pkg install files-viewer
	# https://github.com/yjqww6/drcomplete
	raco pkg install drcomplete
	# https://github.com/yjqww6/drracket-paredit
	raco pkg install drracket-paredit
	# https://github.com/Metaxal/quickscript-extra
	raco pkg install quickscript-extra
