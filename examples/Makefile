SRC_FILES = $(wildcard src/*.elm)
COMPILED_FILES = $(patsubst src%, output/result%, $(patsubst %.elm, %.html, $(SRC_FILES)))

.PHONY: all
all: $(COMPILED_FILES)

output/result/%.html: src/%.elm
	elm make --output=$@ $<

