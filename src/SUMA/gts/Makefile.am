## Process this file with automake to produce Makefile.in

SUBDIRS = src tools examples doc test

dist-hook:
	$(RM) -r `find $(distdir) -name CVS -type d -print`
	$(RM) `find $(distdir) -name ".cvsignore" -type f -print`

changelog:
	cvs2cl.pl --usermap .usermap --prune
