newlisp := newlisp
fetch := curl -sOL

unittest_url := https://rawgit.com/kosh04/newlisp.snippet/master/unittest.lsp

default: test

unittest.lsp:
	$(fetch) $(unittest_url)

.PHONY: test
test: unittest.lsp
	$(newlisp) -n curl-test.lsp

.PHONY: clean
clean:
	$(RM) unittest.lsp
