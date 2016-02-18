all :

check :
	gosh test.scm use-lalr-2.1.0 > test.log-2.1.0
	gosh test.scm use-lalr-2.5.0 > test.log-2.5.0
	@(if grep '^%% ' test.log-*; then exit 1; else exit 0; fi)

clean :
	rm -f fo.scm *.log-* *~
