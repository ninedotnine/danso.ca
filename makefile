FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files
FILES = site.hs
OUT_EXE = site

default: clean build

build:
	ghc $(FLAGS) --make -o $(OUT_EXE) $(FILES)

watch: build
	./site watch

rebuild: clean build
	./site rebuild

clean:
	rm -f $(OUT_EXE)
	rm -rf _cache _site
