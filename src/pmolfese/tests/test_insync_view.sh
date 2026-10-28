#!/usr/bin/env bash
#
# 3dInSync output view: the output follows the first input's view (+orig,
# +tlrc) and template space, unless the -mask (or, with no -mask, the -atlas)
# is in a different view, in which case it takes the mask's, as 3dRSA does.
# Run after building 3dInSync, or pass its path.  Needs 3dUndump, 3dTcat,
# 3dcalc, 3dcopy, 3drefit and 3dinfo from the same directory as 3dInSync.

set -euo pipefail

prog=${1:-./3dInSync}
PATH="$(cd "$(dirname "$prog")" && pwd):$PATH"
prog=$(cd "$(dirname "$prog")" && pwd)/$(basename "$prog")
tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/insync-view.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT
cd "$tmpdir"
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}

fail() { echo "FAIL: $*" >&2; exit 1; }
view() { 3dinfo -av_space "$1" 2>/dev/null | head -1; }

# 6 subjects, 6x6x6, 30 time points, built as +orig (a NIfTI with no space code)
echo "0 0 0 1" > pt.txt
3dUndump -dimen 6 6 6 -datum float -ijk -prefix g.nii.gz pt.txt >/dev/null
args=(); for _ in $(seq 30); do args+=(g.nii.gz); done
3dTcat -prefix rep.nii.gz "${args[@]}" >/dev/null 2>&1
for s in 1 2 3 4 5 6; do
  3dcalc -a rep.nii.gz -expr "gran(0,1)+2*sin(t/3)" -prefix s$s.nii.gz >/dev/null 2>&1
done
3dcalc -a g.nii.gz -expr 'step(3.5-i)' -prefix mask_orig.nii.gz >/dev/null 2>&1   # +orig mask
3dcalc -a g.nii.gz -expr '1+step(i-2.5)'  -prefix atlas_orig.nii.gz >/dev/null 2>&1
# the same masks, labeled as template-space (+tlrc / MNI) datasets
3dcopy mask_orig.nii.gz mask_tlrc >/dev/null 2>&1;   3drefit -view tlrc -space MNI mask_tlrc+orig   >/dev/null 2>&1
3dcopy atlas_orig.nii.gz atlas_tlrc >/dev/null 2>&1; 3drefit -view tlrc -space MNI atlas_tlrc+orig >/dev/null 2>&1
# the same subjects, labeled as +tlrc / MNI
for s in 1 2 3 4 5 6; do
  3dcopy s$s.nii.gz t$s >/dev/null 2>&1; 3drefit -view tlrc -space MNI t$s+orig >/dev/null 2>&1
done
{ echo 'Subj Group InputFile'; for s in 1 2 3; do echo "s$s A $tmpdir/s$s.nii.gz"; done
                              for s in 4 5 6; do echo "s$s B $tmpdir/s$s.nii.gz"; done; } > orig.txt
{ echo 'Subj Group InputFile'; for s in 1 2 3; do echo "s$s A $tmpdir/t$s+tlrc"; done
                              for s in 4 5 6; do echo "s$s B $tmpdir/t$s+tlrc"; done; } > tlrc.txt

run() { "$prog" "$@" -quiet >/dev/null 2>&1; }

# subject space: +orig inputs with a +orig mask stay +orig
run -prefix a -mask mask_orig.nii.gz -dataTableFile orig.txt
[ -f a+orig.HEAD ] || fail "+orig inputs with a +orig mask should give +orig"
[ "$(view a+orig.HEAD)" = "+orig" ] || fail "output a is not +orig"

# +orig inputs, no mask: +orig
run -prefix b -dataTableFile orig.txt
[ -f b+orig.HEAD ] || fail "+orig inputs with no mask should give +orig"

# +orig inputs labeled without space, but a template-space mask: +tlrc
run -prefix c -mask mask_tlrc+tlrc -dataTableFile orig.txt
[ -f c+tlrc.HEAD ] || fail "+orig inputs with a +tlrc mask should give +tlrc"
[ ! -f c+orig.HEAD ] || fail "unexpected c+orig"
[ "$(view c+tlrc.HEAD)" = "+tlrc" ] || fail "output c is not +tlrc"

# ... and likewise from the atlas when there is no mask, including -roi_only
run -prefix d -atlas atlas_tlrc+tlrc -roi_only -dataTableFile orig.txt
[ -f d+tlrc.HEAD ] || fail "+orig inputs with a +tlrc atlas (-roi_only) should give +tlrc"

# +tlrc inputs follow themselves, with or without a +tlrc mask, and keep their MNI space
run -prefix e -dataTableFile tlrc.txt
run -prefix f -mask mask_tlrc+tlrc -dataTableFile tlrc.txt
[ -f e+tlrc.HEAD ] && [ -f f+tlrc.HEAD ] || fail "+tlrc inputs should give +tlrc"
[ "$(3dinfo -space e+tlrc.HEAD)" = "MNI" ] || fail "MNI space of the inputs was not kept (e)"
[ "$(3dinfo -space f+tlrc.HEAD)" = "MNI" ] || fail "MNI space of the inputs was not kept (f)"

# pair maps follow the same rule, and the 3dISC table names the matching file
run -prefix g -mask mask_tlrc+tlrc -save_pairwise gp -dataTableFile orig.txt
[ -f gp+tlrc.HEAD ] || fail "pair maps should be +tlrc"
grep -q 'gp+tlrc.BRIK' gp.3dISC.txt || fail "3dISC table should point at gp+tlrc.BRIK"

echo "PASS: 3dInSync output view"
