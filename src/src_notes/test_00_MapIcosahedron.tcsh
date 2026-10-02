#!/bin/tcsh

# file for testing old/new MapIcosahedron

# 2026-10-02 : [pm] MapIcosahedron got a faster search under the hood
#              for the 3 closest original-mesh nodes of each icosahedron
#              node (a grid of node buckets, rather than a slab of nodes
#              sorted by x).  The old search can still be run with the
#              new '-classic' option.  This script runs the old program,
#              the new program and the new program with -classic on a
#              FreeSurfer subject (fsaverage by default), and checks
#              that all three give the same standard meshes, node maps
#              and mapped datasets.
#
#              Each test is 1 hemisphere at 1 mesh density, run 3 ways:
#                old : old program (any build from before 2026-10-01)
#                new : new program, default (grid) search
#                cla : new program, -classic search
#              and both 'new' and 'cla' are compared against 'old':
#                + *.MI.1D node maps: closest node indices, and
#                  interpolation weights (cols 1..3 and 4..6)
#                + std-mesh surfaces (*.gii): coords and triangles, via
#                  gifti_tool -compare_data (metadata is ignored)
#                + mapped datasets (*.niml.dset)
#              Times are reported as both the wall time of the whole
#              program and the time of the closest-node mapping step
#              (SUMA_MapSurface, as reported by -verb).
#
#              Usage (any FreeSurfer subject with a SUMA/ dir made by
#              @SUMA_Make_Spec_FS; default is ./fsaverage):
#
#                tcsh test_00_MapIcosahedron.tcsh [SUBJ_DIR [SID]]
#
#              SUBJ_DIR : FreeSurfer subject dir, holding SUMA/
#              SID      : subject ID used for the spec files, as in
#                         SUMA/SID_lh.spec (default: name of SUBJ_DIR)
#
#              Metrics from running this on 2026-10-02 (Apple M-series,
#              macOS 26; old = AFNI_26.2.03 precompiled binary; new =
#              this update, built locally with gcc-15 -O1).
#
#              Results: in all tests (fsaverage, and 1 other subject),
#              old, new and cla gave identical node maps (indices and
#              weights), identical std-mesh surfaces (7 per test, by
#              gifti_tool -compare_data) and identical mapped dsets.
#
#              Times on fsaverage (lh; rh were within ~5%):
#
#                         mapping step (s)          whole program (s)
#                mesh     old     new    cla        old    new    cla
#                -ld 60   2.40    0.17   1.86       4.5    2.2    3.9
#                -ld 141  10.69   0.83   8.21       16.3   6.6    13.9
#                -rd 7    8.07    0.64   6.02       18.1   10.5   16.1
#
#                + the mapping step is ~13-14x faster with new
#                + the whole program is ~1.7-2.5x faster (the rest is
#                  reading inputs, making the icosahedron, and
#                  interpolating and writing outputs)
#                + cla is ~20% faster than old only because of the
#                  build: the pre-update source built locally the same
#                  way as new gave -ld 141 mapping times of 8.6-9.1 s,
#                  vs 8.1-9.0 s for cla and 10.7-11.3 s for the old
#                  precompiled binary (3 runs each)
#                + on a real subject (~130k nodes per hemi) times were
#                  similar: -ld 141 mapping 11.3 s (old) vs 1.06 s (new)
#
# ===========================================================================

set prog = MapIcosahedron
set idx  = 00

# ---------------------------------------------------------------------------
# subject to test on: [SUBJ_DIR [SID]]

# (block ifs: a one-line if would expand $argv[N] even when false)
set subj_dir = fsaverage
if ( $#argv > 0 ) then
    set subj_dir = "$argv[1]"
endif
set subj_dir = `echo "${subj_dir}" | sed 's|/*$||'`
set sid      = ${subj_dir:t}
if ( $#argv > 1 ) then
    set sid = "$argv[2]"
endif

# ---------------------------------------------------------------------------
# set locations of old and new program versions, for comparisons

set path_old = ${HOME}/abin
set path_new = ${HOME}/Documents/Programming/afni/src/SUMA

set prog_old = ${path_old}/${prog}
set prog_new = ${path_new}/${prog}

# output dir for results and a text file

set dir_test = odir_test-${idx}-${prog}-${sid}
set txt_diff = ${dir_test}/all_diffs.txt
set txt_time = ${dir_test}/all_times.txt
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# generic checks to be able to run testing

if ( ! -f ${prog_old} ) then
    echo "** ERROR: cannot find prog old:"
    echo "   ${prog_old}"
    exit -1
endif

if ( ! -f ${prog_new} ) then
    echo "** ERROR: cannot find prog new:"
    echo "   ${prog_new}"
    exit -1
endif

if ( -d ${dir_test} ) then
    echo ""
    echo "** ERROR: already have output testing dir."
    echo "   Consider running the following to remove it:"
    echo ""
    echo "     \\rm -rf ${dir_test}"
    echo ""
    exit -1
endif

echo "++++ Passed first checks to be able to run test. Continuing."
# ---------------------------------------------------------------------------

# ===========================================================================
# extra checks, specific to this dset

# test data 1: findable?  MapIcosahedron has to be run from the dir
# holding the spec file, so the input is the subject's SUMA/ dir, from
# @SUMA_Make_Spec_FS (with or without its own MapIcosahedron runs)

set dir_spec = ${subj_dir}/SUMA

if ( ! -f ${dir_spec}/${sid}_lh.spec || \
     ! -f ${dir_spec}/${sid}_rh.spec ) then
cat <<EOF

** ERROR: cannot find input spec files:
     ${dir_spec}/${sid}_lh.spec
     ${dir_spec}/${sid}_rh.spec

   If SID is not the name of the subject dir, give it as the 2nd
   argument.  Otherwise, consider copy+pasting these lines to generate
   test data (needs FreeSurfer set up; takes a few minutes):

   + for fsaverage (copied, since \${FREESURFER_HOME} may not be
     writable):

     \\cp -RL \${FREESURFER_HOME}/subjects/fsaverage .
     cd fsaverage
     @SUMA_Make_Spec_FS -GIFTI -sid fsaverage -no_ld
     cd ..

   + for another subject (-no_ld skips making the std meshes, which
     this test does itself):

     cd ${subj_dir}
     @SUMA_Make_Spec_FS -GIFTI -sid ${sid} -no_ld
     cd -

EOF
    exit -1
else
    echo "++ Seem to have found the input spec files to run:"
    echo "   ${dir_spec}/${sid}_{lh,rh}.spec"
    echo "   Here we go..."
endif

# -classic only exists in the new program

${prog_new} -help | grep -q -- '-classic'
if ( ${status} ) then
    echo "** ERROR: prog new does not have the -classic option:"
    echo "   ${prog_new}"
    exit -1
endif

# ===========================================================================

# ---------------------------------------------------------------------------
# make output dir for test results (and init/clear a text file of diffs)

\mkdir -p ${dir_test}
printf '' > ${txt_diff}
printf "%-8s %-4s %-8s   %9s %9s %9s   %9s %9s %9s\n"                    \
    test hemi mesh wall_old wall_new wall_cla map_old map_new map_cla     \
    > ${txt_time}

set here    = `pwd`
set dir_out = ${here}/${dir_test}

# the old MapIcosahedron puts './' in front of -prefix, so an absolute
# path fails to write; use the output dir's path relative to the spec dir
cd ${dir_spec}
set rel_out = `python3 -c 'import os,sys; print(os.path.relpath(os.path.realpath(sys.argv[1])))' ${dir_out}`
cd ${here}
# ---------------------------------------------------------------------------

# ===========================================================================
# run tests

# each mesh is given as 2 parallel lists: the option and its value.
# -rd 7 makes an icosahedron of the same density as fsaverage itself
# (163842 nodes), so on fsaverage many nodes sit at (nearly) equal
# distances: a test of how ties are resolved.
set all_hemi = ( lh rh )
set all_mopt = ( -ld -ld  -rd )
set all_mval = ( 60  141  7   )
set nmesh    = ${#all_mopt}

@ ntest = ${#all_hemi} * ${nmesh}
set all_ii = `count_afni -digits 2 1 ${ntest}`

set hh = 0
foreach hemi ( ${all_hemi} )
foreach mm ( `seq 1 1 ${nmesh}` )
    @ hh += 1
    set ii   = ${all_ii[$hh]}
    set mopt = ${all_mopt[$mm]}
    set mval = ${all_mval[$mm]}
    set mesh = ${mopt}${mval}

    set bname = test-${ii}

    # special things done when running here:
    # + run from the spec dir, writing outputs to the test dir
    # + -verb is used to get the time of the mapping step itself
    # + want to record time of each, so this records time values as
    #   integers that count milliseconds (via python, since BSD date
    #   on macOS has no %N)

    cd ${dir_spec}

    # also map thickness, when the subject has it
    if ( -f ${hemi}.thickness.gii.dset ) then
        set dmap = ( -dset_map ${hemi}.thickness.gii.dset )
    else
        set dmap = ( )
    endif

    foreach ver ( old new cla )
        if ( ${ver} == old ) then
            set cmd = ( ${prog_old} )
        else if ( ${ver} == new ) then
            set cmd = ( ${prog_new} )
        else
            set cmd = ( ${prog_new} -classic )
        endif

        set time0 = `python3 -c 'import time; print(int(time.time()*1000))'`
        ${cmd} -verb                                       \
            -spec     ${sid}_${hemi}.spec                  \
            ${mopt}   ${mval}                              \
            ${dmap}                                        \
            -prefix   ${rel_out}/${bname}-${ver}.          \
            >& ${dir_out}/${bname}-${ver}-log.txt
        set ok    = ${status}
        set time1 = `python3 -c 'import time; print(int(time.time()*1000))'`
        @ time_ms = ${time1} - ${time0}

        # (failing to write the std surfaces does not set the exit status)
        set ngii = `find ${dir_out} -maxdepth 1 -name "${bname}-${ver}.*.gii" | wc -l`
        if ( ${ok} || ${ngii} == 0 ) then
            cd ${here}
            echo "** ERROR: failed running ${ver} for ${bname}, see:"
            echo "   ${dir_test}/${bname}-${ver}-log.txt"
            exit -1
        endif

        set tmap = `grep "SUMA_MapSurface took" \
                        ${dir_out}/${bname}-${ver}-log.txt | awk '{print $3}'`

        # keep the node map's numeric rows only: the old program's header
        # has an uncommented text line that the 1D reader cannot parse
        grep '^ *[0-9]' ${dir_out}/${bname}-${ver}.${sid}_${hemi}.MI.1D \
            > ${dir_out}/${bname}-${ver}-MI_num.1D

        set time_ms_${ver} = ${time_ms}
        set tmap_${ver}    = ${tmap}
    end

    cd ${here}

    # ----- compare new and cla against old

    echo "---- test: ${bname} (${hemi}, ${mesh}) ----" |& tee -a ${txt_diff}

    set bold = ${dir_test}/${bname}-old
    foreach ver ( new cla )
        set bver = ${dir_test}/${bname}-${ver}

        echo "-- old vs ${ver}: MI node indices" |& tee -a ${txt_diff}
        3dDiff -a "${bold}-MI_num.1D[1..3]" -b "${bver}-MI_num.1D[1..3]" \
            |& tee -a ${txt_diff}
        echo "-- old vs ${ver}: MI weights" |& tee -a ${txt_diff}
        3dDiff -a "${bold}-MI_num.1D[4..6]" -b "${bver}-MI_num.1D[4..6]" \
            |& tee -a ${txt_diff}

        set ngii = 0
        set ndiff = 0
        foreach ff ( ${bold}.*.gii )
            set gg = `echo ${ff} | sed "s/${bname}-old\./${bname}-${ver}./"`
            @ ngii += 1
            # count the match: gifti_tool exits 1 for any metadata diff
            # (e.g., date), and tcsh then gives the pipe that status
            set nsame = `gifti_tool -compare_gifti -compare_data         \
                            -infiles ${ff} ${gg}                         \
                            |& grep -c "no data differences"`
            if ( ${nsame} == 0 ) then
                @ ndiff += 1
                echo "   surface data differ: ${gg}" |& tee -a ${txt_diff}
            endif
        end
        echo "-- old vs ${ver}: surfaces with data diffs: ${ndiff} of ${ngii}" \
            |& tee -a ${txt_diff}

        foreach ff ( ${bold}.*.niml.dset )
            set gg = `echo ${ff} | sed "s/${bname}-old\./${bname}-${ver}./"`
            echo "-- old vs ${ver}: dset ${ff:t}" |& tee -a ${txt_diff}
            3dDiff -a ${ff} -b ${gg} |& tee -a ${txt_diff}
        end
    end

    # ----- times

    printf "%-8s %-4s %-8s   %9d %9d %9d   %9.3f %9.3f %9.3f\n"           \
        ${bname} ${hemi} ${mesh}                                          \
        ${time_ms_old} ${time_ms_new} ${time_ms_cla}                      \
        ${tmap_old} ${tmap_new} ${tmap_cla} >> ${txt_time}

    @ time_diff_ms = ${time_ms_new} - ${time_ms_old}

    set time_diff_frac = `echo "scale = 1; 100.0*(${time_ms_new} - ${time_ms_old})/(1.0*${time_ms_old}) " | bc`
    set map_speedup    = `echo "scale = 1; ${tmap_old} / ${tmap_new}" | bc`

cat <<EOF

++ Time info (number of ms, whole program) for run: ${ii} (${hemi}, ${mesh})
   old  : ${time_ms_old}
   new  : ${time_ms_new}
   cla  : ${time_ms_cla}
   diff : ${time_diff_ms}  (new - old)
   perc : ${time_diff_frac} %

++ Time info (sec, SUMA_MapSurface step only)
   old  : ${tmap_old}
   new  : ${tmap_new}
   cla  : ${tmap_cla}
   speedup (old/new) : ${map_speedup}x

   The 'cla' times should be similar to 'old' (same search, though a
   different build can shift them a bit), and all of the diffs above
   should say the images do NOT differ, with 0 surfaces having data
   diffs.

EOF

end
end

# ===========================================================================

echo "++ DONE.  Check diffs file:"
echo "-------------------------------------------------"
cat ${txt_diff}
echo "-------------------------------------------------"
echo "++ And times (wall: ms, whole program; map: sec, mapping step):"
echo "-------------------------------------------------"
cat ${txt_time}
echo "-------------------------------------------------"

exit 0
