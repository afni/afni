/*
   Probabilistic(+deterministic) tracking, first draft at
   AFNIfication, March 2012.  using FACTID code, from Taylor,
   Kuan-Hung, Lin and Biswal (2012)

   ----> Migrated to 3dTrackID!  (Jan., 2014)

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>
#include <3ddata.h>

void usage_ProbTrackID(int detail)
{
	printf(
"  \n"
"  FACTID-based tractography code, from Taylor, Cho, Lin and Biswal (2012),\n"
"  part of FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"  \n"
"  Estimate locations of WM associated with GM ROIs, particularly between\n"
"  pairs of GM in a network;  can process several networks in a given run.\n"
"\n"
"  !!All usage has been migrated to 3dTrackID (with modes to select for\n"
"    deterministic, mini-probabilistic and full probabilistic tracking).\n\n"
"  So get thee hence;  see '3dTrackid -help' for all options, and consider\n"
"    downloading and installing the latest version of the FATCAT demo set\n"
"    for lots of examples, scripts and descriptions: @Install_FATCAT_DEMO.\n"
"\n");
	return;
}

int main(int argc, char *argv[]) {

   int iarg;

	mainENTRY("3dProbTrackID"); machdep();

	if (argc == 1) { usage_ProbTrackID(1); exit(0); }
	iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 ||
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_ProbTrackID(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}

		ERROR_message("Program no longer supported.  It has been migrated to"
                    " '3dTrackID', with separate modes for running\n\t"
                    "deterministic, mini-probabilistic or full probabilistic"
                    " tractography. Please check it out there.");
      exit(1);
	}

	return 0;
}


