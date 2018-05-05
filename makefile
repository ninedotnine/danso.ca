FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files
FILES = site.hs
OUT_EXE = site

default: clean compile

clean:
	rm -f $(OUT_EXE)
	rm -rf _cache _site

compile:
	ghc $(FLAGS) --make -o $(OUT_EXE) $(FILES)

watch: clean compile 
	./site watch

build: clean compile
	./site build

rebuild: clean compile
	./site rebuild

check: build
	./site check --internal-links

deploy: build check
	rm -rf /var/www/danso.ca/*
	mv _site/* /var/www/danso.ca/
