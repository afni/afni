#!/usr/bin/env bash
set -euo pipefail

mvpa=$1
undump=$2
bucket=$3
refit=$4
dinfo=$5
maskave=$6
maskdump=$7
work=$(mktemp -d "/tmp/thd_mvpa_integration.XXXXXX")
trap 'rm -rf "$work"' EXIT

make_dset() {
  local name=$1 value_file="$work/points_$1.txt"
  shift
  : > "$value_file"
  local ii=0 value
  for value in "$@"; do
    printf '%d 0 0 %s\n' "$ii" "$value" >> "$value_file"
    ii=$((ii+1))
  done
  "$undump" -overwrite -dimen 4 2 1 -ijk -datum float \
    -prefix "$work/dset_$name" "$value_file" >/dev/null 2>&1
}

make_dset mask 1 1 2 2
make_dset a1   0 0 5 5
make_dset b1  10 10 5 5
make_dset a2   1 1 5 5
make_dset b2   9 9 5 5

cat > "$work/direct.1D" <<EOF
Subj Run Sample Condition InputFile
s01 r1 a1 A $work/dset_a1+orig
s01 r1 b1 B $work/dset_b1+orig
s01 r2 a2 A $work/dset_a2+orig
s01 r2 b2 B $work/dset_b2+orig
EOF

"$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -prefix "$work/direct_result" -show_samples -show_folds \
  -save_predictions "$work/direct.predictions.1D" \
  -save_fold_scores "$work/direct.folds.1D" \
  > "$work/direct.audit.txt"

grep -q '^# samples from ' "$work/direct.audit.txt"
grep -q $'^r1\tAll\ttest\t1\ta1$' "$work/direct.audit.txt"
awk -F '\t' '
  $1 == 1 { if ($3 != 1 || $4 != 1 || $5 != 1 || $6 != 2 || $7 != 2) exit 1; one=1 }
  $1 == 2 { if ($3 != .5 || $4 != .5 || $5 != .5 || $6 != 2 || $7 != 2) exit 1; two=1 }
  END { if (!one || !two) exit 1 }
' "$work/direct_result.roi.1D"
test "$("$dinfo" -nv "$work/direct_result+orig")" = 3
awk -F '\t' '
  /^#/ { next }
  $1 == "UnitType" {
    if ($8 != "TableRow" || $9 != "Sample" || $13 != "InputFold" ||
        $14 != "CVFold" || $15 != "CVStratum" || $16 != "Observed" ||
        $17 != "Predicted" || $19 != "DecisionClass0") exit 1
    next
  }
  {
    n++
    if ($1 == "ROI" && $2 == 1 && $8 == 1 && $9 == "a1" && $10 == "s01" &&
        $11 == "r1" && $13 == "NA" && $14 == "r1" && $15 == "All" &&
        $16 == "A" && $17 == "A" && $18 == 1) good=1
    if ($1 == "ROI" && $2 == 2 && $8 == 2 && $9 == "b1" &&
        $16 == "B" && $17 == "A" && $18 == 0) miss=1
  }
  END { if (n != 8 || !good || !miss) exit 1 }
' "$work/direct.predictions.1D"
awk -F '\t' '
  /^#/ { next }
  $1 == "UnitType" { next }
  {
    n++
    if ($1 == "ROI" && $2 == 1 && $8 == "r1" && $9 == "All" &&
        $10 == 2 && $11 == 2 && $12 == 1 && $13 == 1) good=1
    if ($1 == "ROI" && $2 == 2 && $8 == "r2" &&
        $12 == .5 && $13 == .5) chance=1
  }
  END { if (n != 4 || !good || !chance) exit 1 }
' "$work/direct.folds.1D"

"$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -nperm 99 -permute_within Run -seed 42 \
  -prefix "$work/permutation_result" > "$work/permutation.audit.txt" 2>&1
grep -q '4 synchronized label relabelings (exact; 2 blocks from Run)' \
  "$work/permutation.audit.txt"
grep -q 'inference family: 2 spatial units x 4 synchronized relabelings' \
  "$work/permutation.audit.txt"
test "$("$dinfo" -nv "$work/permutation_result+orig")" = 9
for pair in 'BACC 0' 'ABOVE_CHANCE 1' 'PVAL 2' 'ZSCR 3' 'QVAL 4' \
            'PFWE 5' 'ZFWE 6' 'NFEAT 7' 'NFOLD 8'; do
  set -- $pair
  test "$("$dinfo" -label2index "$1" "$work/permutation_result+orig")" = "$2"
done
awk -F '\t' '
  $1 == 1 {
    if ($3 != 1 || $6 != .5 || $7 != .5 || $9 != 1 || $10 != .5 ||
        $12 != 2 || $13 != 2) exit 1
    signal=1
  }
  $1 == 2 {
    if ($3 != .5 || $6 != 0 || $7 != 1 || $9 != 1 || $10 != 1 ||
        $12 != 2 || $13 != 2) exit 1
    chance=1
  }
  END { if (!signal || !chance) exit 1 }
' "$work/permutation_result.roi.1D"

"$bucket" -prefix "$work/stats" \
  "$work/dset_a1+orig" "$work/dset_a2+orig" \
  "$work/dset_b1+orig" "$work/dset_b2+orig" >/dev/null 2>&1
"$refit" -sublabel 0 face#0_Coef -sublabel 1 face#1_Coef \
  -sublabel 2 house#0_Coef -sublabel 3 house#1_Coef \
  "$work/stats+orig" >/dev/null 2>&1

cat > "$work/design.xmat.1D" <<'EOF'
# <matrix
#  ni_type = "4*double"
#  ni_dimen = "4"
#  ColumnLabels = "face#0 ; face#1 ; house#0 ; house#1"
#  GoodList = "0..3"
#  NRowFull = "4"
#  RunStart = "0,2"
#  StimBots = "0,2"
#  StimTops = "1,3"
#  StimLabels = "face ; house"
# >
1 0 0 0
0 0 1 0
0 1 0 0
0 0 0 1
# </matrix>
EOF
cat > "$work/xmat.1D" <<EOF
Subj Xmat InputFile
s01 $work/design.xmat.1D $work/stats+orig
EOF

"$mvpa" -mode classify -dataTable "@$work/xmat.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -prefix "$work/xmat_result" -show_table > "$work/xmat.audit.txt"

grep -q 'face#0' "$work/xmat.audit.txt"
awk -F '\t' '
  $1 == 1 { if ($3 != 1 || $4 != 1 || $5 != 1) exit 1; one=1 }
  $1 == 2 { if ($3 != .5 || $4 != .5 || $5 != .5) exit 1; two=1 }
  END { if (!one || !two) exit 1 }
' "$work/xmat_result.roi.1D"
test "$("$dinfo" -nv "$work/xmat_result+orig")" = 3

"$mvpa" -mode classify -dataTable "@$work/xmat.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -prefix "$work/validation_only" -validate_only > "$work/xmat.validate.txt"
grep -q $'^# Samples\t4$' "$work/xmat.validate.txt"
grep -q $'^# Validation PASS\t0 warning(s)$' "$work/xmat.validate.txt"
test ! -e "$work/validation_only+orig.HEAD"

# X-matrix expansion preserves fixed partitions from the source table. Reusing
# this tiny bucket tests the adapter, not scientific independence of the sets.
cat > "$work/xmat_fixed.1D" <<EOF
Subj Split Xmat InputFile
s01 train $work/design.xmat.1D $work/stats+orig
s02 test $work/design.xmat.1D $work/stats+orig
EOF
"$mvpa" -mode classify -dataTableFile "$work/xmat_fixed.1D" \
  -target_column Condition -partition_column Split -mask "$work/dset_mask+orig" \
  -prefix "$work/xmat_fixed" -save_predictions "$work/xmat_fixed.pred.1D" \
  -save_fold_scores "$work/xmat_fixed.fold.1D"
awk -F '\t' '/^#/ {next}
  $10=="s01" {n++;if($14!="NA" || $17!="NA")exit 1}
  $10=="s02" {t++;if($14!="test" || $17=="NA")exit 1}
  END{if(n!=8 || t!=8)exit 1}' "$work/xmat_fixed.pred.1D"
awk -F '\t' '/^#/ {next} {n++;if($10!=4 || $11!=4)exit 1}
  END{if(n!=2)exit 1}' "$work/xmat_fixed.fold.1D"
"$mvpa" -mode classify -dataTableFile "$work/xmat_fixed.1D" \
  -target_column Condition -partition_column Split -mask "$work/dset_mask+orig" \
  -validate_only > "$work/xmat_fixed.validate.txt"
grep -q $'^test\tAll\t4\t4$' "$work/xmat_fixed.validate.txt"

# Two-table Xmat expansion keeps the pre-expansion source row on every beta.
"$bucket" -prefix "$work/test_stats" "$work/stats+orig" >/dev/null 2>&1
cat > "$work/xmat_train.1D" <<EOF
Subj Xmat InputFile
s01 $work/design.xmat.1D $work/stats+orig
EOF
cat > "$work/xmat_test.1D" <<EOF
InputFile Xmat Subj
$work/test_stats+orig $work/design.xmat.1D s02
EOF
"$mvpa" -mode classify -dataTable "@$work/xmat_train.1D" -testTable "@$work/xmat_test.1D" \
  -target_column Condition -mask "$work/dset_mask+orig" \
  -prefix "$work/xmat_dual" -save_predictions "$work/xmat_dual.pred.1D"
awk -F '\t' -v train="$work/xmat_train.1D" -v test="$work/xmat_test.1D" '
  /^#/ {next}
  {if($23!=1)exit 1}
  $10=="s01" {n++;if($17!="NA" || $21!="train" || $22!=train)exit 1}
  $10=="s02" {t++;if($17=="NA" || $21!="test" || $22!=test)exit 1}
  END{if(n!=8 || t!=8)exit 1}' "$work/xmat_dual.pred.1D"
cmp "$work/xmat_fixed.roi.1D" "$work/xmat_dual.roi.1D"

OMP_NUM_THREADS=2 "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -searchlight 'SPHERE(1.1)' -prefix "$work/searchlight_result" \
  -nperm 99 -permute_within Run -seed 42 \
  -save_predictions "$work/searchlight_parallel.predictions.1D" \
  -save_fold_scores "$work/searchlight_parallel.folds.1D" \
  -save_confusion "$work/searchlight_parallel.confusion.1D" \
  -save_summary_scores "$work/searchlight_parallel.summary.1D" \
  > "$work/searchlight.audit.txt" 2>&1

grep -q "searchlight neighborhood 'SPHERE(1.1)'" "$work/searchlight.audit.txt"
grep -q '2 workers' "$work/searchlight.audit.txt"
test ! -e "$work/searchlight_result.roi.1D"
test "$("$dinfo" -nv "$work/searchlight_result+orig")" = 9
"$maskave" -quiet -ibox 0 0 0 -dindex 0 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != 1) exit 1 }'
"$maskave" -quiet -ibox 3 0 0 -dindex 0 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != .5) exit 1 }'
"$maskave" -quiet -ibox 1 0 0 -dindex 7 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != 3) exit 1 }'
"$maskave" -quiet -ibox 3 0 0 -dindex 7 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != 2) exit 1 }'
"$maskave" -quiet -ibox 2 0 0 -dindex 8 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != 2) exit 1 }'
"$maskave" -quiet -ibox 0 1 0 -dindex 0 \
  "$work/searchlight_result+orig" | awk '{ if ($1 != 0) exit 1 }'

OMP_NUM_THREADS=1 "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -searchlight 'SPHERE(1.1)' -prefix "$work/searchlight_serial" -quiet \
  -nperm 99 -permute_within Run -seed 42 \
  -save_predictions "$work/searchlight_serial.predictions.1D" \
  -save_fold_scores "$work/searchlight_serial.folds.1D" \
  -save_confusion "$work/searchlight_serial.confusion.1D" \
  -save_summary_scores "$work/searchlight_serial.summary.1D"
"$maskdump" -noijk -mask "$work/dset_mask+orig" \
  -o "$work/searchlight_parallel.txt" "$work/searchlight_result+orig" \
  >/dev/null 2>&1
"$maskdump" -noijk -mask "$work/dset_mask+orig" \
  -o "$work/searchlight_serial.txt" "$work/searchlight_serial+orig" \
  >/dev/null 2>&1
cmp "$work/searchlight_parallel.txt" "$work/searchlight_serial.txt"
cmp "$work/searchlight_parallel.predictions.1D" "$work/searchlight_serial.predictions.1D"
cmp "$work/searchlight_parallel.folds.1D" "$work/searchlight_serial.folds.1D"
cmp "$work/searchlight_parallel.confusion.1D" "$work/searchlight_serial.confusion.1D"
cmp "$work/searchlight_parallel.summary.1D" "$work/searchlight_serial.summary.1D"
awk -F '\t' '
  /^#/ || $1 == "UnitType" { next }
  { if ($1 != "Voxel" || $2 != $4 || $5 != 0 || $6 != 0) exit 1; seen[$2]=1; n++ }
  END { if (n != 40 || !seen[0] || !seen[1] || !seen[2] || !seen[3]) exit 1 }
' "$work/searchlight_parallel.confusion.1D"

test "$(awk '!/^#/ && $1 != "UnitType" {n++} END {print n+0}' \
  "$work/searchlight_parallel.predictions.1D")" = 16
test "$(awk '!/^#/ && $1 != "UnitType" {n++} END {print n+0}' \
  "$work/searchlight_parallel.folds.1D")" = 8

if "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -searchlight 1 -roi_sel 1 -prefix "$work/invalid_result" \
  > "$work/invalid.audit.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted -roi_sel with -searchlight" >&2
  exit 1
fi
grep -q -- '-roi_sel cannot be combined with -searchlight' "$work/invalid.audit.txt"

if "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -nperm 10 -prefix "$work/no_block" > "$work/no_block.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted permutation inference without exchangeability blocks" >&2
  exit 1
fi
grep -q -- '-nperm requires -permute_within' "$work/no_block.txt"

if "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -nperm 99 -permute_within Subj -prefix "$work/cross_fold_block" \
  > "$work/cross_fold_block.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted a permutation that removes a training class" >&2
  exit 1
fi
grep -q 'violates CV class coverage' "$work/cross_fold_block.txt"

if "$mvpa" -mode classify -dataTable "@$work/direct.1D" \
  -target_column Condition -cv_column Run -mask "$work/dset_mask+orig" \
  -nperm 10 -permute_within Sample -prefix "$work/identity_only" \
  > "$work/identity_only.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted an identity-only permutation group" >&2
  exit 1
fi
grep -q 'permits only the identity label arrangement' "$work/identity_only.txt"

echo "PASS 3dMVPA ROI/searchlight classification, blocked inference, and deterministic audits"
