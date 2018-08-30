.PHONY: static
static: examples frontend
	mkdir -p static/examples
	mkdir -p static/editor
	cp -ur examples/output/* static/examples
	cp -ur frontend/output/* static/editor

.PHONY: clean
clean:
	$(MAKE) -C examples clean
	$(MAKE) -C frontend clean

.PHONY: examples
examples:
	$(MAKE) -C examples

.PHONY: frontend
frontend:
	$(MAKE) -C frontend
