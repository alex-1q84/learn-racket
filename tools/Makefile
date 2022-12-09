DEST=switch-google-proxy

install: $(DEST)
	mv $(DEST) ~/bin/

$(DEST): $(DEST).rkt
	raco exe $(DEST).rkt

clean:
	rm $(DEST)

.PHONY: build clean
