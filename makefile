FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files
FILES = site.hs
OUT_EXE = site

default: build

clean:
	rm -f $(OUT_EXE)
	rm -rf _cache _site

compile: clean
	ghc $(FLAGS) --make -o $(OUT_EXE) $(FILES)

watch: compile
	./site watch

build: compile
	./site build

rebuild: compile
	./site rebuild

check: build
	./site check --internal-links

deploy: build check
	rm -rf /var/www/danso.ca/*
	mv _site/* /var/www/danso.ca/

renewcert:
	@echo "you will need dansohost:dan's password for this."
	sudo certbot renew
