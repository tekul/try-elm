.PHONY: all
all: output/editor.js output/controls.js output/editor.css output/errors.js

.PHONY: clean
clean:
	@rm -r output

output/%.js: src/%.elm
	elm make --output=$@ $<

output/%.js: src/%.js
	@mkdir -p $(@D)
	cp $< $@


output/%.css: src/%.css
	@mkdir -p $(@D)
	cp $< $@
