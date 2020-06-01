objects=mouse.scrbl mouse-acmart.scrbl

all: clean html

html:
	scribble --dest build --htmls mouse.scrbl

pdf:
	scribble --dest build --pdf mouse-acmart.scrbl

clean:
	-rm -rf build
	-rm -rf compiled
