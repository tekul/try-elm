SRC_FILES = $(wildcard src/*.elm)
HTML_FILES = $(patsubst %.elm, %.html, $(SRC_FILES))
RESULTS_FRAMES = $(patsubst src%, output/result%, $(HTML_FILES))
CODE_FRAMES = $(patsubst src%, output/code%, $(HTML_FILES))
FRAMESETS = $(patsubst src%, output%, $(HTML_FILES))

.PHONY: all
all: $(FRAMESETS) $(RESULTS_FRAMES) $(CODE_FRAMES)

.PHONY: clean
clean:
	@rm -r output


output/result/%.html: src/%.elm
	elm make --output=$@ $<

output/code/%.html: src/%.elm
	@mkdir -p output/code
	$(shell templates/code_frame $@ $<)

output/%.html: src/%.elm
	@mkdir -p output
	$(shell templates/editor_frameset $@ $(*F) $(notdir $@))


