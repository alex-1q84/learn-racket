DEST=switch-google-proxy
$(DEST): $(DEST).rkt
	raco exe $(DEST).rkt

install: $(DEST)
	mv $(DEST) ~/bin/

clean:
	rm $(DEST)

.PHONY: build clean