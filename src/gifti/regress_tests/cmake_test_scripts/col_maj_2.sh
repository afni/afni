#!/bin/bash
if [ $# -lt 2 ]
then
echo The two arguments required are the path to the gifti tool  and to the test-data directory name
exit 1
fi

GT=$1
DATA=$2
OUT_DATA=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`
cd ${OUT_DATA}

pref=$OUT_DATA/t26


# ===== comparing 1: unconverted =====
! output=$($GT                                                                \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 3                                                               \
-infile $DATA/small.???.maj.3.gii 2>&1)


# ===== comparing 1: converted =====
$GT                                                                           \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 3                                                               \
-perm_by_iord 1                                                               \
-infile $DATA/small.???.maj.3.gii                                                   

# read cm, convert to rm (auto), compare to orig rm
$GT -infile $DATA/small.col.maj.3.gii -write_gifti $pref.2.rm.3.gii

# ===== compare 2: new rm to orig rm =====
! output=$($GT                                                                \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 4                                                               \
-infiles $DATA/small.row.maj.3.gii $pref.2.rm.3.gii 2>&1)

# read cm, convert to rm (auto), convert back (mod_ind_ord), compare to orig
! output=$($GT                                                                \
-infile $DATA/small.col.maj.3.gii                                             \
-mod_ind_ord 2                                                                \
-write_gifti $pref.3.cm.3.gii  2>&1)

# ===== compare 3: new cm to orig cm =====
! output=$($GT                                                                \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 4                                                               \
-infiles $DATA/small.col.maj.3.gii $pref.3.cm.3.gii 2>&1)

# ===== compare 3x10 1: unconverted =====
! output=$($GT                                                                \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 4                                                               \
-infiles $DATA/small.3.10.cm.gii $DATA/small.3.10.rm.gii 2>&1)

# ===== compare 3x10 2: converted =====
$GT                                                                           \
-compare_gifti                                                                \
-compare_data                                                                 \
-compare_verb 4                                                               \
-perm_by_iord 1                                                               \
-infiles $DATA/small.3.10.cm.gii $DATA/small.3.10.rm.gii                                  


rm -rf $OUT_DATA

# ===== compare 3x10 3: 3dcalc =====
# 3dcalc -a small.3.10.cm.gii -b small.3.10.rm.gii 
#        -expr a-b -prefix $pref.4.diff.3.10.gii
# 3dBrickStat -min -max -slow $pref.4.diff.3.10.gii 
