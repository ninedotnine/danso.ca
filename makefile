SHELL = /bin/sh
FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files
FILES = site.hs
OUT_EXE = site

default: build

clean:
	rm -f $(OUT_EXE)
	rm -rf _cache _site

$(OUT_EXE): $(FILES)
	ghc $(FLAGS) --make -o $(OUT_EXE) $(FILES)

watch: $(OUT_EXE)
	./$(OUT_EXE) watch

build: $(OUT_EXE)
	./$(OUT_EXE) build

rebuild: $(OUT_EXE)
	./$(OUT_EXE) rebuild

check: build
	./$(OUT_EXE) check --internal-links

deploy: build check
	rm -rf /var/www/danso.ca/*
	mv _site/* /var/www/danso.ca/
	rmdir _site

renewcert:
	@echo "you will need dansohost:dan's password for this."
	sudo certbot renew

.PHONY: clean build watch check deploy renewcert
