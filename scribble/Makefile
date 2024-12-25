objects=mouse.scrbl mouse-acmart.scrbl

all: clean html

html:
	scribble --dest build --html mouse.scrbl demo.scrbl

markdown:
	scribble --dest build --markdown mouse.scrbl

pdf:
	scribble --dest build --pdf mouse-acmart.scrbl

refresh_firefox:
	osascript refresh_firefox.scpt

clean:
	-rm -rf build
	-rm -rf compiled

.PHONY: clean pdf markdown html all refresh_firefox
