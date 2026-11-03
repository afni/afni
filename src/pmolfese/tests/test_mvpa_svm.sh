#!/usr/bin/env bash
set -euo pipefail
mvpa=$1
undump=$2
dump=$3
work=$(mktemp -d /tmp/thd_mvpa_svm.XXXXXX)
trap 'rm -rf "$work"' EXIT
make_data() {
  local name=$1 i=0
  shift
  for value in "$@"; do
    printf '%d 0 0 %s\n' "$i" "$value"
    i=$((i+1))
  done > "$work/$name.txt"
  "$undump" -dimen 4 2 1 -ijk -datum float -prefix "$work/$name" "$work/$name.txt"
}
make_data mask 1 1 2 2
make_data low -1 -2 5 5
make_data high 1 2 5 5
cat > "$work/samples.1D" <<TABLE
Subj Run Sample Condition InputFile
s1 r1 a A $work/low+orig
s1 r1 b B $work/high+orig
s1 r2 c A $work/low+orig
s1 r2 d B $work/high+orig
s1 r3 e A $work/low+orig
s1 r3 f B $work/high+orig
TABLE
common=(-mode classify -method svm -dataTableFile "$work/samples.1D" -cv_column Run \
  -target_column Condition -mask "$work/mask+orig" -scale none)
"$mvpa" "${common[@]}" -prefix "$work/fixed" -save_predictions "$work/fixed.pred.1D" \
  -save_fold_scores "$work/fixed.fold.1D"
awk -F '\t' '/^#/ || $1=="ROI"{next}
  $1==1{if($3!=1 || $4!=1 || $5!=1)exit 1;signal=1}
  $1==2{if($3!=.5 || $5!=.5)exit 1;flat=1}
  END{if(!signal || !flat)exit 1}' "$work/fixed.roi.1D"
awk -F '\t' '/^#/{next} $1=="UnitType"{if($14!="C" || $17!="SolverIterations" || $18!="SolverResidual")exit 1;next}
  {n++;if($14!=1 || $15!=0 || $17<1 || $18>1e-6)exit 1} END{if(n!=6)exit 1}' "$work/fixed.fold.1D"
# C=1, four training rows with x=(+/-1,+/-2): margin = 40/41.
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="UnitType"{next}
  $2==1{n++;if($18!=1 || abs(abs($19)-40/41)>1e-5 || abs($19+$20)>1e-5)exit 1}
  END{if(n!=6)exit 1}' "$work/fixed.pred.1D"
for variant in observed permuted; do
  extra=(-quiet)
  if [[ $variant == permuted ]]; then extra=(-quiet -nperm 99 -permute_within Run -seed 42); fi
  "$mvpa" "${common[@]}" -svm_c_grid 10,.01 -prefix "$work/$variant" "${extra[@]}" \
    -save_predictions "$work/$variant.pred.1D" -save_fold_scores "$work/$variant.fold.1D"
done
cmp "$work/observed.pred.1D" "$work/permuted.pred.1D"
cmp "$work/observed.fold.1D" "$work/permuted.fold.1D"
cmp "$work/observed.tuning.1D" "$work/permuted.tuning.1D"
awk -F '\t' '/^#/ || $1=="Unit"{next}
  $7==1{n++;if($5!=.01)exit 1} END{if(n!=6)exit 1}' "$work/observed.tuning.1D"
"$dump" -noijk -mask "$work/mask+orig" "$work/permuted+orig" > "$work/permuted.map"
awk 'NR<=2{if($1!=1 || $3!=.25 || $6!=.25)exit 1} END{if(NR!=4)exit 1}' "$work/permuted.map"
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" "${common[@]}" -svm_c auto \
    -searchlight 'SPHERE(0.1)' -nperm 99 -permute_within Run -seed 42 \
    -prefix "$work/volume_$workers" -save_predictions "$work/volume_$workers.pred.1D" \
    -save_fold_scores "$work/volume_$workers.fold.1D"
  "$dump" -noijk -mask "$work/mask+orig" "$work/volume_$workers+orig" > "$work/volume_$workers.map"
done
cmp "$work/volume_1.map" "$work/volume_2.map"
cmp "$work/volume_1.pred.1D" "$work/volume_2.pred.1D"
cmp "$work/volume_1.fold.1D" "$work/volume_2.fold.1D"
cmp "$work/volume_1.tuning.1D" "$work/volume_2.tuning.1D"
# Multiclass decisions use one margin per encoded class.
make_data ma 2 0 5 5
make_data mb -2 0 5 5
make_data mc 0 2 5 5
printf 'Subj Run Condition InputFile\n' > "$work/multi.1D"
for run in 1 2 3; do
  for letter in a b c; do
    printf 's1 r%d %s %s/m%s+orig\n' "$run" "$letter" "$work" "$letter" >> "$work/multi.1D"
  done
done
"$mvpa" -mode classify -method svm -dataTableFile "$work/multi.1D" -cv_column Run \
  -target_column Condition -mask "$work/mask+orig" -scale feature_zscore -svm_c .5 \
  -prefix "$work/multiclass" -save_predictions "$work/multiclass.pred.1D"
awk -F '\t' '/^#/ || $1=="ROI"{next} $1==1{if($3!=1)exit 1;n++} END{if(n!=1)exit 1}' "$work/multiclass.roi.1D"
awk -F '\t' '/^#/{next} $1=="UnitType"{if($21!="DecisionClass2")exit 1;next} {if(NF!=21)exit 1;n++} END{if(n!=18)exit 1}' "$work/multiclass.pred.1D"
expect_failure() {
  if "$mvpa" "${common[@]}" "$@" > "$work/error" 2>&1; then
    echo 'Unexpected SVM option success' >&2;exit 1
  fi
}
expect_failure -svm_c 0 -prefix "$work/bad"
grep -q 'finite and positive' "$work/error"
expect_failure -method ridge -svm_c 1 -prefix "$work/bad"
grep -q 'SVM options require' "$work/error"
expect_failure -lambda 1 -prefix "$work/bad"
grep -q 'require -method ridge' "$work/error"
expect_failure -distance cosine -prefix "$work/bad"
grep -q 'only used by centroid' "$work/error"
expect_failure -svm_c 1 -svm_c_grid 1,10 -prefix "$work/bad"
grep -q 'cannot accompany numeric' "$work/error"
expect_failure -svm_c_grid 1,1 -prefix "$work/bad"
grep -q 'duplicate penalties' "$work/error"
expect_failure -svm_tol nan -prefix "$work/bad"
grep -q 'must be in' "$work/error"
expect_failure -svm_max_iter 1 -svm_tol 1e-12 -prefix "$work/bad"
grep -q 'did not converge' "$work/error"
expect_failure -svm_c auto -inner_cv_column Condition -prefix "$work/bad"
grep -q 'inner fold.*no class' "$work/error"
expect_failure -save_coefficients "$work/badcoef" -prefix "$work/bad"
grep -q 'requires -method ridge' "$work/error"
echo 'MVPA native linear SVM checks passed'
