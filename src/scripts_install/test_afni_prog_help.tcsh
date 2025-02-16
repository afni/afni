#!/bin/tcsh

# ===========================================================================
# show help - can only reach via goto HELP
goto SKIP_HELP
HELP:
    cat << EOF

    ----------------------------------------------------------------------
    $progname           - run -help on AFNI programs as a simple test

    terminal options:
      -help            : get this help
      -hist            : output the program history
      -ver             : show version

    other options:

      -bin_dir BIN_DIR : specify directory of AFNI binaries
      -echo            : apply 'set echo' in script
      -prog_list PLIST : specify file to get program list from

    Test each program (PROG) in the prog_list file by running:

           bin_dir/PROG -help

    main parameters:

          BIN_DIR         : directory to run programs out of
                            default: use path this program is run from,
                               $tbin

          PLIST           : file that contains a list of programs to test
                            default: BIN_DIR/$tfile

    examples:

        $progname
        $progname -bin_dir \$HOME/abin
        $progname -prog_list my_short_list.txt

    ----------------------------------------------------------------------
    R Reynolds   Nov 2018
    distributed  Aug 2023
    ----------------------------------------------------------------------

EOF
exit
SKIP_HELP:
# ===========================================================================
# show hist - can only reach via goto HIST
goto SKIP_HIST
HIST:
  cat << EOF

  history:
     0.0   3 Dec 2018 - initial test.afni.prog.help
     0.1  24 Aug 2023 - distribute as test_afni_prog_help.tcsh
             - renamed with underscores
             - simplified, will not create prog_list file
             - added -hist, -echo and -ver options

EOF
exit
SKIP_HIST:

# ----- corresponding version
set VERSION = "0.1 August 24, 2023"

# ===========================================================================
# init user controllable variables

# destination for help output
set bin_dir = NONE
set prog_list = NONE

set progname = `basename $0`

# temp bin dir is where this program was run from
# tfile is default file to have list of programs to run -help on
set tbin = $0:h
set tfile = prog_list.txt

# ===========================================================================
# process options
set ac = 1
while ( $ac <= $#argv )
  set arg = $argv[$ac]

  # check for terminal options, first
  if ( "$arg" == "-help" ) then
    # show help and exit
    goto HELP
  else if ( "$arg" == "-hist" ) then
    # show hist and exit
    goto HIST
  else if ( "$arg" == "-ver" ) then
    # show hist and exit
    echo $VERSION
    exit 0

  # main options
  else if ( "$arg" == "-bin_dir" ) then
    @ ac += 1
    set bin_dir = "$argv[$ac]"
  else if ( "$arg" == "-prog_list" ) then
    @ ac += 1
    set prog_list = "$argv[$ac]"
  else if ( "$arg" == "-echo" ) then
    set echo
  else
    echo "** unknown option: $argv[$ac]"
    exit 1
  endif
  
  @ ac += 1
end

# ======================================================================
# verify or update main vars

# if unset, init bin_dir based on this script
if ( $bin_dir == NONE ) then
   set bin_dir = $tbin
endif

# verify
if ( ! -d $bin_dir ) then
   echo "** error: -bin_dir '$bin_dir' is not a directory"
   exit 1
endif


# if no prog_list, try to get it from src_dir or bin_dir or make one
if ( $prog_list == NONE ) then
   set prog_list = $bin_dir/$tfile
endif

# verify
if ( ! -f $prog_list ) then
   echo "** error: -prog_list '$prog_list' is not a file"
   exit 1
endif


# ======================================================================
# warn of plotting

echo "-- running programs out of directory $bin_dir"
echo "-- using prog_list file $prog_list"
echo ""


# ======================================================================
# get to work

# first add bin_dir to front of PATH, as some scripts might need it
set path = ( $bin_dir $path )
echo "++ temporarily adding $bin_dir to PATH"
rehash

echo ""

# make a list of programs to test
set all_progs = ( `grep -v \# $prog_list` )

set good_stats = ()
set bad_stats = ()
foreach prog ( $all_progs )

  printf "    %-35s " $prog
  $bin_dir/$prog -help >& /dev/null

  if ( $status ) then
     echo "                 BAD_STAT"
     set bad_stats = ( $bad_stats $prog )
  else
     echo GOOD
     set good_stats = ( $good_stats $prog )
  endif
end

echo ""
echo "found $#good_stats good progs and $#bad_stats issues out of $#all_progs"
echo ""
echo "failed progs: $bad_stats"
echo ""


##### consider writing $good_stats to a file, or even with $bad_stats
