#! /bin/sh

rm -f errors
if awk '{
    if (NF >= 2 && NF <= 3 && substr ($1, 1, 1) != "#") {
	summary = $1 " " $2;
	command = "./set surfaces/" $1 " surfaces/" $2;
	if (NF == 3 && $3 == "XFAIL")
	    xfailed = 1;
	else
	    xfailed = 0;
	setfailed = 0;
	if (system (command " > /dev/null 2> log")) {
	    system ("echo \"======== set " summary " ========\" >> errors; cat log >> errors");
	    if (xfailed)
		print "XFAIL: set " summary;
	    else {
		print "FAIL: set " summary;
		failed++;
	    }
	    setfailed = 1;
	}
	else {
	    if (system ("grep CRITICAL log > /dev/null") &&
		!system ("grep \"duplicate edges: 0\" log > /dev/null")) {
		if (xfailed)
		    print "XPASS: set " summary;
		else
		    print "PASS: set " summary;
	    }
	    else {
		system ("echo \"======== set " summary " ========\" >> errors; cat log >> errors");
		if (xfailed)
		    print "XFAIL: set " summary;
		else {
		    print "FAIL: set " summary;
		    failed++;
		}
		setfailed = 1;
	    }
	}
	total++;
	if (!setfailed &&
	    !system ("../../tools/gtscheck -v 2> log < surfaces/" $1) &&
	    !system ("grep \"boundary edges: 0\" log > /dev/null") &&
	    !system ("../../tools/gtscheck -v 2> log < surfaces/" $2) &&
	    !system ("grep \"boundary edges: 0\" log > /dev/null")) {
	    command = "./boole surfaces/" $1 " surfaces/" $2;
	    if (system (command " > /dev/null 2> log")) {
		system ("echo \"======== boole " summary " ========\" >> errors; cat log >> errors");
		if (xfailed)
		    print "XFAIL: boole " summary;
		else {
		    print "FAIL: boole " summary;
		    failed++;
		}
	    }
	    else {
		if (system ("grep CRITICAL log > /dev/null") &&
		    !system ("grep \"duplicate edges: 0\" log > /dev/null")) {
		    if (xfailed)
			print "XPASS: boole " summary;
		    else
			print "PASS: boole " summary;
		}
		else {
		    system ("echo \"======== boole " summary " ========\" >> errors; cat log >> errors");
		    if (xfailed)
			print "XFAIL: boole " summary;
		    else {
			print "FAIL: boole " summary;
			failed++;
		    }
		}
	    }
	    total++;
        }
    }
}
END {
    if (failed > 0)
	msg = failed " of " total " tests failed";
    else
	msg = "All " total " tests passed";
    line = msg;
    gsub (".", "=", line);
    print line;
    print msg;
    print line;
    if (failed > 0)
	exit (1);
    else
	exit (0);
}' < tests; then
    ret=0;
else
    ret=1;
fi
rm -f log
exit $ret;
