clean_tmp:
	find /tmp -name ghc* -print0 | xargs -0 rm -r
