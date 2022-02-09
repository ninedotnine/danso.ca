SHELL = /bin/sh
FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files
FILES = site.hs
OUT_EXE = site

WEBRING = templates/post.html
RING_FEEDS = $(file < webring/feeds.txt)

default: build

clean:
	rm -f $(OUT_EXE)
	rm -rf _cache _site

$(OUT_EXE): $(FILES)
	ghc $(FLAGS) --make -o $(OUT_EXE) $(FILES)

watch: $(OUT_EXE) $(WEBRING)
	./$(OUT_EXE) watch

build: $(OUT_EXE) $(WEBRING)
	./$(OUT_EXE) build

rebuild: $(OUT_EXE) $(WEBRING)
	./$(OUT_EXE) rebuild

check: build $(WEBRING)
	./$(OUT_EXE) check --internal-links

deploy: $(WEBRING) build check
	rm -rf /var/www/danso.ca/*
	cp -r _site/* /var/www/danso.ca/

renewcert:
	@echo "you will need dansohost:dan's password for this."
	sudo certbot renew

$(WEBRING): webring/in.html webring/feeds.txt
	openring < webring/in.html > $(WEBRING) $(RING_FEEDS)

force_webring: $(WEBRING)
	$(MAKE) --always-make $(WEBRING)

.PHONY: clean build watch check deploy renewcert force_webring
