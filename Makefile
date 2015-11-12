newlisp ?= newlisp

fetch := curl -sOL
unittest_url := https://raw.github.com/kosh04/newlisp.snippet/master/unittest.lsp

default: test

unittest.lsp:
	$(fetch) $(unittest_url)

.PHONY: test
test: unittest.lsp
	$(newlisp) -n curl-test.lsp -e "(Test:run)"

.PHONY: clean
clean:
	$(RM) unittest.lsp
