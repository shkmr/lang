all :

check :
	gosh test.scm > test.log
	@(if grep '^%% ' test.log; then exit 1; else exit 0; fi)

clean :
	rm -f fo.scm *.log *~
