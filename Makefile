
.PHONY: static
static: examples
	mkdir -p static/examples
	cp -r examples/output/* static/examples

.PHONY: examples
examples:
	$(MAKE) -C examples
