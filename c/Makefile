all :

check :
	time gosh test.scm use-lalr-2.1.0 > test.log-2.1.0
	time gosh test.scm use-lalr-2.5.0 > test.log-2.5.0
	@(if grep '^%% ' test.log-*; then exit 1; else exit 0; fi)
	@diff test.log-2.1.0 test.log-2.5.0

clean :
	rm -f fo.scm *.log-* *~
