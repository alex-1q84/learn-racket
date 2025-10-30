.PHONY: setup_drracket install test

setup_drracket:
	raco pkg install files-viewer
	# https://github.com/yjqww6/drcomplete
	raco pkg install drcomplete
	# https://github.com/yjqww6/drracket-paredit
	raco pkg install drracket-paredit
	# https://github.com/Metaxal/quickscript-extra
	raco pkg install quickscript-extra

# Install all packages in the monorepo
install:
	raco pkg install --auto --batch aoc2020/
	raco pkg install --auto --batch exercisim/
	raco pkg install --auto --batch leet_code/

# Run tests for all packages
test:
	raco test aoc2020/
	raco test exercisim/
	raco test leet_code/
