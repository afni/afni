#!/usr/bin/env bash
#
# Integration smoke test for 3dInSync repeated-condition inference, the 3dISC
# bridge, and production-hardening options. Run from src after building
# 3dInSync, or pass its path.

set -euo pipefail

prog=${1:-./3dInSync}
tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/insync-phase4.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT

printf '%s\n' '1 -1 1 -1 1 -1 1 -1' '1 2 3 4 5 6 7 8' > "$tmpdir/a1.1D"
printf '%s\n' '1 -1 1 -1 1 -1 1 -1' '1 2 3 4 5 6 7 8' > "$tmpdir/a2.1D"
printf '%s\n' '1 -1 1 -1 1 -1 1 -1' '1 2 3 4 5 6 7 8' > "$tmpdir/a3.1D"
printf '%s\n' '1 2 3 4 5 6 7 8'       '8 7 6 5 4 3 2 1' > "$tmpdir/b1.1D"
printf '%s\n' '8 7 6 5 4 3 2 1'       '1 -1 1 -1 1 -1 1 -1' > "$tmpdir/b2.1D"
printf '%s\n' '1 1 -1 -1 1 1 -1 -1'  '1 3 2 4 3 5 4 6' > "$tmpdir/b3.1D"

{
  echo 'Subj Group Condition Age InputFile'
  for subj in a1 a2 a3; do
    age=${subj#a}
    echo "$subj A C1 2$age $tmpdir/$subj.1D"
    echo "$subj A C2 2$age $tmpdir/$subj.1D"
  done
  for subj in b1 b2 b3; do
    age=${subj#b}
    echo "$subj B C1 3$age $tmpdir/$subj.1D"
    echo "$subj B C2 3$age $tmpdir/$subj.1D"
  done
} > "$tmpdir/long.txt"

AFNI_NOMMAP=YES OMP_NUM_THREADS=2 "$prog" \
  -prefix "$tmpdir/result" \
  -nperm 20 -exact \
  -temporal_null timeshift -nnull 20 \
  -condition_contrast C1 C2 -condition_exact \
  -save_pairwise "$tmpdir/pairs" \
  -dataTableFile "$tmpdir/long.txt" >/dev/null

test "$(3dinfo -nv "$tmpdir/result.1D")" -eq 58
test "$(3dinfo -nv "$tmpdir/pairs.1D")" -eq 60
test "$(wc -l < "$tmpdir/pairs.3dISC.all.txt")" -eq 61
test "$(wc -l < "$tmpdir/pairs.C1__C2.3dISC.txt")" -eq 16

selector=$(awk 'NR==2 {print $NF}' "$tmpdir/pairs.C1__C2.3dISC.txt")
3dinfo "$selector" >/dev/null

# The two conditions are identical, so every planned condition effect is zero.
1dcat "$tmpdir/result.1D[40..42]" |
  awk '{for(ii=1;ii<=NF;ii++) if($ii != 0) exit 1}'

# The Cartesian table validator must reject a missing subject-condition cell.
sed '$d' "$tmpdir/long.txt" > "$tmpdir/incomplete.txt"
if "$prog" -prefix "$tmpdir/bad" -dataTableFile "$tmpdir/incomplete.txt" >/dev/null 2>&1; then
  echo 'FAIL: incomplete Subj x Condition table was accepted' >&2
  exit 1
fi

echo 'PASS: 3dInSync Phase 4 integration tests'

# Phase 5: common censoring, Spearman, atlas summaries/matrices, missing policy,
# and memory-limit enforcement.
printf '%s\n' 1 1 0 1 0 1 1 0 > "$tmpdir/censor.1D"
printf '%s\n' 1 2 > "$tmpdir/atlas.1D"

AFNI_NOMMAP=YES OMP_NUM_THREADS=2 "$prog" -quiet \
  -prefix "$tmpdir/phase5" -correlation spearman \
  -censor "$tmpdir/censor.1D" -atlas "$tmpdir/atlas.1D" \
  -save_matrix "$tmpdir/phase5.matrix.1D" \
  -dataTableFile "$tmpdir/long.txt"

test "$(3dinfo -nv "$tmpdir/phase5.1D")" -eq 11
test "$(wc -l < "$tmpdir/phase5.roi.1D")" -eq 9
test "$(wc -l < "$tmpdir/phase5.matrix.1D")" -eq 61
awk 'NR>1 && $7 != 5 {exit 1}' "$tmpdir/phase5.roi.1D"

cat > "$tmpdir/nonfinite.1D" <<EOF
# <AFNI_3D_dataset
#  self_idcode = "AFN_insync_nonfinite"
#  ni_type     = "8*float"
#  ni_dimen    = "2,1,1"
#  ni_delta    = "1,1,1"
#  ni_origin   = "0,0,0"
#  ni_axes     = "R-L,A-P,I-S"
# >
 1 -1 nan -1 1 -1 1 -1
 1 2 3 4 5 6 7 8
# </AFNI_3D_dataset>
EOF
{
  echo 'Subj InputFile'
  echo "s1 $tmpdir/a1.1D"
  echo "s2 $tmpdir/a2.1D"
  echo "s3 $tmpdir/nonfinite.1D"
} > "$tmpdir/missing.txt"

if "$prog" -quiet -prefix "$tmpdir/missing-error" \
     -dataTableFile "$tmpdir/missing.txt" >"$tmpdir/missing-error.log" 2>&1; then
  echo 'FAIL: default missing-data policy accepted a nonfinite value' >&2
  exit 1
fi
grep -q 'nonfinite retained input at voxel 0, time 3' "$tmpdir/missing-error.log"

AFNI_NOMMAP=YES "$prog" -quiet -prefix "$tmpdir/common" \
  -missing common -dataTableFile "$tmpdir/missing.txt"
test "$(3dinfo -nv "$tmpdir/common.1D")" -eq 3
1dcat "$tmpdir/common.1D[2]" |
  awk 'NR==1 && $1!=7 {exit 1} NR==2 && $1!=8 {exit 1}'

if "$prog" -quiet -memory_limit 0.000001 -prefix "$tmpdir/memfail" \
     -dataTableFile "$tmpdir/long.txt" >/dev/null 2>&1; then
  echo 'FAIL: undersized memory limit was not enforced' >&2
  exit 1
fi

# AFNI sidedness spellings and their quick flags must be exact aliases.
for sided in 1sided 2sided bisided; do
  AFNI_NOMMAP=YES "$prog" -quiet -prefix "$tmpdir/tail-$sided-long" \
    -nperm 20 -exact -tail "$sided" -dataTableFile "$tmpdir/long.txt"
  AFNI_NOMMAP=YES "$prog" -quiet -prefix "$tmpdir/tail-$sided-quick" \
    -nperm 20 -exact "-$sided" -dataTableFile "$tmpdir/long.txt"
  1dcat "$tmpdir/tail-$sided-long.1D" > "$tmpdir/tail-$sided-long.values"
  1dcat "$tmpdir/tail-$sided-quick.1D" > "$tmpdir/tail-$sided-quick.values"
  diff -u "$tmpdir/tail-$sided-long.values" "$tmpdir/tail-$sided-quick.values"
done

# Reversing first-appearance group order reverses the tested effect; the
# bisided z maps must retain that negative direction.
awk 'NR==1 {print; next} $2=="B" {b=b $0 ORS; next} {a=a $0 ORS}
     END {printf "%s%s",b,a}' "$tmpdir/long.txt" > "$tmpdir/long-reversed.txt"
AFNI_NOMMAP=YES "$prog" -quiet -prefix "$tmpdir/tail-bisided-negative" \
  -nperm 20 -exact -bisided -dataTableFile "$tmpdir/long-reversed.txt"
paste <(1dcat "$tmpdir/tail-bisided-negative.1D[8]") \
      <(1dcat "$tmpdir/tail-bisided-negative.1D[11]") |
  awk '$1 < 0 && $2 < 0 {seen=1} $1 < 0 && $2 > 0 {exit 1} END {exit !seen}'

echo 'PASS: 3dInSync Phase 5 integration tests'
