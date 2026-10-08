#!/usr/bin/env bash
#
# Memory-reduction tests for 3dInSync: the compact in-mask store, -roi_only,
# -float16, and -zcensor through the compact path.  Uses small synthetic 4D
# data with a signal shared across subjects.  Run after building 3dInSync, or
# pass its path.  Needs 3dUndump, 3dTcat, 3dcalc, 3dBrickStat, 3dROIstats and
# 1dtranspose from the same directory as 3dInSync.

set -euo pipefail

prog=${1:-./3dInSync}
PATH="$(cd "$(dirname "$prog")" && pwd):$PATH"
prog=$(cd "$(dirname "$prog")" && pwd)/$(basename "$prog")
tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/insync-memory.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT
cd "$tmpdir"
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}

fail() { echo "FAIL: $*" >&2; exit 1; }

# max |A - B| over a mask, for one sub-brick index of each bucket
maxdiff() {   # A B brick mask
  3dcalc -a "$1[$3]" -b "$2[$3]" -expr 'abs(a-b)' -prefix _d -overwrite >/dev/null 2>&1
  3dBrickStat -mask "$4" -max _d+orig 2>/dev/null | awk 'NF{print $1}' | tail -1
}
lt() { awk -v a="$1" -v b="$2" 'BEGIN{exit !(a<b)}'; }

# --- synthetic data: 8 subjects, 8x8x8 grid, 40 time points ----------------
echo "0 0 0 1" > pt.txt
3dUndump -dimen 8 8 8 -datum float -ijk -prefix g.nii.gz pt.txt >/dev/null
args=(); for _ in $(seq 40); do args+=(g.nii.gz); done
3dTcat -prefix rep.nii.gz "${args[@]}" >/dev/null 2>&1
for s in 1 2 3 4 5 6 7 8; do
  3dcalc -a rep.nii.gz -expr "gran(0,1)+2*sin(t/3)+0.3*i" -prefix s$s.nii.gz >/dev/null 2>&1
done
3dcalc -a g.nii.gz -expr 'step(4.5-i)' -prefix mask.nii.gz >/dev/null 2>&1
3dcalc -a g.nii.gz -expr '1+step(i-2.5)+step(i-5.5)+2*step(j-3.5)' -prefix atlas.nii.gz >/dev/null 2>&1
{
  echo 'Subj Group InputFile'
  for s in 1 2 3 4;     do echo "s$s A $tmpdir/s$s.nii.gz"; done
  for s in 5 6 7 8;     do echo "s$s B $tmpdir/s$s.nii.gz"; done
} > table.txt

common=(-isc_method pairwise -nboot 100 -nperm 50 -temporal_null timeshift -nnull 30 -min_shift 3 -seed 11)

# --- 1. in-mask compact store gives the same ISC as the full-grid datasets ---
"$prog" -prefix dense  "${common[@]}" -dataTableFile table.txt -quiet >/dev/null 2>&1
"$prog" -prefix compact -mask mask.nii.gz "${common[@]}" -dataTableFile table.txt \
  >compact.out 2>&1
grep -q 'held as 32-bit floats: .* in-mask voxels' compact.out || fail "compact store not chosen/reported"
for b in 0 2 4; do
  d=$(maxdiff dense+orig compact+orig $b mask.nii.gz)
  lt "$d" 1e-6 || fail "compact vs dense ISC brick $b differs by $d"
done
# outside the mask the p-value bricks keep the whole-grid convention (1)
v=$(3dBrickStat -mask mask.nii.gz -max "compact+orig[9]" 2>/dev/null | awk 'NF{print $1}' | tail -1)
3dcalc -a "compact+orig[9]" -b mask.nii.gz -expr 'a*not(b)' -prefix outp -overwrite >/dev/null 2>&1
o=$(3dBrickStat -max outp+orig 2>/dev/null | awk 'NF{print $1}' | tail -1)
[ "$o" = "1" ] || fail "P_A-B outside the mask should be 1, got $o"
echo "PASS: compact in-mask store matches full-grid analysis"

# --- 2. -float16 stays close to 32-bit storage -------------------------------
"$prog" -prefix half -mask mask.nii.gz -float16 "${common[@]}" -dataTableFile table.txt \
  >half.out 2>&1
grep -q 'held as 16-bit floats' half.out || fail "-float16 not reported"
for b in 0 2 4; do
  d=$(maxdiff compact+orig half+orig $b mask.nii.gz)
  lt "$d" 5e-4 || fail "-float16 ISC brick $b differs by $d"
done
echo "PASS: -float16 agrees with 32-bit storage"

# --- 3. -roi_only equals averaging the ROIs first ---------------------------
mkdir roi
for s in 1 2 3 4 5 6 7 8; do
  3dROIstats -quiet -mask atlas.nii.gz s$s.nii.gz | 1dtranspose stdin: > roi/s$s.1D
done
{
  echo 'Subj Group InputFile'
  for s in 1 2 3 4; do echo "s$s A $tmpdir/roi/s$s.1D"; done
  for s in 5 6 7 8; do echo "s$s B $tmpdir/roi/s$s.1D"; done
} > table_roi.txt
"$prog" -prefix manual "${common[@]}" -dataTableFile table_roi.txt -quiet >/dev/null 2>&1
"$prog" -prefix ro -atlas atlas.nii.gz -roi_only "${common[@]}" -dataTableFile table.txt \
  >ro.out 2>&1
grep -q 'ROIs x 40 time points' ro.out || fail "-roi_only store not reported"
[ -f ro.roi.1D ] || fail "-roi_only did not write PREFIX.roi.1D"
nroi=$(awk 'NR>1' ro.roi.1D | wc -l)
[ "$nroi" -eq "$(awk 'NF' manual.1D | grep -vc '^#')" ] || fail "ROI count differs from the manual route"
# ISC_A is the 4th column of the table and the 1st of the manual bucket
paste -d' ' <(awk 'NR>1{print $4}' ro.roi.1D) <(grep -v '^#' manual.1D | awk 'NF{print $1}') |
  awk '{d=$1-$2; if(d<0)d=-d; if(d>m)m=d} END{exit !(m<1e-5)}' || fail "-roi_only ISC differs from the manual ROI route"
# bucket is painted onto the grid: every voxel of ROI 1 carries ROI 1's value
r1=$(awk 'NR==2{print $4}' ro.roi.1D)
3dcalc -a "ro+orig[0]" -b atlas.nii.gz -expr 'a*equals(b,1)' -prefix paint -overwrite >/dev/null 2>&1
pmax=$(3dBrickStat -non-zero -max paint+orig 2>/dev/null | awk 'NF{print $1}' | tail -1)
awk -v a="$r1" -v b="$pmax" 'BEGIN{d=a-b; if(d<0)d=-d; exit !(d<1e-6)}' || fail "-roi_only painted value $pmax != table value $r1"
echo "PASS: -roi_only matches averaging ROIs first and paints the grid"

# --- 4. invalid combinations are rejected ------------------------------------
if "$prog" -prefix bad1 -roi_only -dataTableFile table.txt -quiet >/dev/null 2>&1; then
  fail "-roi_only without -atlas should fail"; fi
if "$prog" -prefix bad2 -atlas atlas.nii.gz -roi_only -edges -dataTableFile table.txt -quiet >/dev/null 2>&1; then
  fail "-roi_only with -edges should fail"; fi
echo "PASS: invalid -roi_only combinations are rejected"

# --- 5. -zcensor through the compact store and through -roi_only -------------
3dcalc -a s1.nii.gz -expr 'a*not(equals(t,5)+equals(t,17))' -prefix z1.nii.gz >/dev/null 2>&1
sed "s#$tmpdir/s1.nii.gz#$tmpdir/z1.nii.gz#" table.txt > table_z.txt
"$prog" -prefix zc -mask mask.nii.gz -zcensor -dataTableFile table_z.txt -quiet >/dev/null 2>&1
v=$(3dBrickStat -max "zc+orig[ValidTR]" 2>/dev/null | awk 'NF{print $1}' | tail -1)
[ "$v" = "38" ] || fail "-zcensor (compact) should retain 38 time points, got $v"
"$prog" -prefix zr -atlas atlas.nii.gz -roi_only -zcensor -dataTableFile table_z.txt -quiet >/dev/null 2>&1
v=$(3dBrickStat -max "zr+orig[ValidTR]" 2>/dev/null | awk 'NF{print $1}' | tail -1)
[ "$v" = "38" ] || fail "-zcensor (-roi_only) should retain 38 time points, got $v"
echo "PASS: -zcensor works with the compact store and -roi_only"

echo "PASS: 3dInSync memory-reduction tests"
