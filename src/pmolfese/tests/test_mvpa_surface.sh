#!/usr/bin/env bash
set -euo pipefail

mvpa=$1
createico=$2
convertdset=$3
dinfo=$4
work=$(mktemp -d "/tmp/thd_mvpa_surface.XXXXXX")
trap 'rm -rf "$work"' EXIT

"$createico" -ld 1 -rad 10 -prefix "$work/mesh.gii" >/dev/null 2>&1

cat > "$work/nodes.1D" <<'EOF'
0
2
5
9
EOF
cat > "$work/values.1D" <<'EOF'
0 10 1 9
0 10 1 9
5 5 5 5
5 5 5 5
EOF
cat > "$work/mask.1D" <<'EOF'
1
1
1
1
EOF

"$convertdset" -o_niml -input "$work/values.1D" \
  -node_index_1D "$work/nodes.1D" -dset_labels 'a1 b1 a2 b2' \
  -prefix "$work/samples.niml.dset"
"$convertdset" -o_niml -input "$work/mask.1D" \
  -node_index_1D "$work/nodes.1D" -dset_labels mask \
  -prefix "$work/mask.niml.dset"

cat > "$work/samples.1D" <<EOF
Subj Run Sample Condition InputFile
s01 r1 a1 A $work/samples.niml.dset[0]
s01 r1 b1 B $work/samples.niml.dset[1]
s01 r2 a2 A $work/samples.niml.dset[2]
s01 r2 b2 B $work/samples.niml.dset[3]
EOF

OMP_NUM_THREADS=2 "$mvpa" -mode classify -dataTableFile "$work/samples.1D" \
  -target_column Condition -cv_column Run -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 \
  -nperm 99 -permute_within Run -seed 42 \
  -prefix "$work/masked.niml.dset" \
  -save_predictions "$work/masked.predictions.1D" \
  -save_fold_scores "$work/masked.folds.1D" \
  -save_confusion "$work/masked.confusion.1D" \
  -save_summary_scores "$work/masked.summary.1D" \
  > "$work/masked.audit.txt" 2>&1

grep -q 'geodesic surface searchlight, radius 100 mm' "$work/masked.audit.txt"
grep -q '4 centers, at most 4 features, 2 workers' "$work/masked.audit.txt"
grep -q '4 synchronized label relabelings (exact; 2 blocks from Run)' \
  "$work/masked.audit.txt"
test "$("$dinfo" -nv "$work/masked.niml.dset")" = 9

"$convertdset" -o_1Dp_stdout -input "$work/masked.niml.dset" \
  -prepend_node_index_1D > "$work/masked.1D"
awk '
  NF {
    n++
    if ($1 != id[n] || $2 != 1 || $3 != .5 || $4 != .5 ||
        $6 != .5 || $7 != .5 || $9 != 4 || $10 != 2) exit 1
  }
  BEGIN { id[1]=0; id[2]=2; id[3]=5; id[4]=9 }
  END { if (n != 4) exit 1 }
' "$work/masked.1D"
awk -F '\t' '
  /^#/ { next }
  $1 == "UnitType" { next }
  {
    n++; seen[$2]=1
    if ($1 != "Node" || $4 != "NA" || $5 != "NA" || $6 != "NA" ||
        $7 != 4 || $16 != $17 || $18 != 1) exit 1
  }
  END { if (n != 16 || !seen[0] || !seen[2] || !seen[5] || !seen[9]) exit 1 }
' "$work/masked.predictions.1D"
awk -F '\t' '
  /^#/ { next }
  $1 == "UnitType" { next }
  { n++; if ($1 != "Node" || $7 != 4 || $10 != 2 || $11 != 2 || $12 != 1 || $13 != 1) exit 1 }
  END { if (n != 8) exit 1 }
' "$work/masked.folds.1D"

OMP_NUM_THREADS=1 "$mvpa" -mode classify -dataTableFile "$work/samples.1D" \
  -target_column Condition -cv_column Run -surf "$work/mesh.gii" \
  -searchlight 100 -prefix "$work/nomask.niml.dset" \
  -nperm 99 -permute_within Run -seed 42 \
  -save_predictions "$work/nomask.predictions.1D" \
  -save_fold_scores "$work/nomask.folds.1D" \
  -save_confusion "$work/nomask.confusion.1D" \
  -save_summary_scores "$work/nomask.summary.1D" -quiet
"$convertdset" -o_1Dp_stdout -input "$work/nomask.niml.dset" \
  -prepend_node_index_1D > "$work/nomask.1D"
cmp "$work/masked.1D" "$work/nomask.1D"
cmp "$work/masked.predictions.1D" "$work/nomask.predictions.1D"
cmp "$work/masked.folds.1D" "$work/nomask.folds.1D"
cmp "$work/masked.confusion.1D" "$work/nomask.confusion.1D"
cmp "$work/masked.summary.1D" "$work/nomask.summary.1D"
awk -F '\t' '
  /^#/ || $1 == "UnitType" { next }
  {
    n++; seen[$2]=1
    if ($1 != "Node" || $4 != "NA" || $5 != "NA" || $6 != "NA" || $7 != 4 || $13 != 1) exit 1
    if ($8 == "Pooled" && $12 != 2) exit 1
    if ($10 == 0 && ($14 != $12 || $15 != 0)) exit 1
    if ($10 == 1 && ($14 != 0 || $15 != $12)) exit 1
  }
  END { if (n != 40 || !seen[0] || !seen[2] || !seen[5] || !seen[9]) exit 1 }
' "$work/masked.confusion.1D"


cat > "$work/ridge_class.1D" <<EOF
Subj Run Sample Condition InputFile
s01 r1 a1 A $work/samples.niml.dset[0]
s01 r1 b1 B $work/samples.niml.dset[1]
s01 r2 a2 A $work/samples.niml.dset[2]
s01 r2 b2 B $work/samples.niml.dset[3]
s01 r3 a3 A $work/samples.niml.dset[0]
s01 r3 b3 B $work/samples.niml.dset[1]
EOF
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method ridge \
    -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -lambda_grid 10,1 -nperm 99 -permute_within Run -seed 42 \
    -prefix "$work/ridge_class_$workers.niml.dset" \
    -save_predictions "$work/ridge_class_$workers.pred.1D" \
    -save_fold_scores "$work/ridge_class_$workers.fold.1D"
  "$convertdset" -o_1Dp_stdout -input "$work/ridge_class_$workers.niml.dset" \
    -prepend_node_index_1D > "$work/ridge_class_$workers.1D"
done
cmp "$work/ridge_class_1.1D" "$work/ridge_class_2.1D"
cmp "$work/ridge_class_1.pred.1D" "$work/ridge_class_2.pred.1D"
cmp "$work/ridge_class_1.fold.1D" "$work/ridge_class_2.fold.1D"
cmp "$work/ridge_class_1.niml.dset.tuning.1D" "$work/ridge_class_2.niml.dset.tuning.1D"
awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
  NF{n++;if($1!=id[n] || $2!=1 || $4!=.25 || $7!=.25 || $10!=3)exit 1}
  END{if(n!=4)exit 1}' "$work/ridge_class_1.1D"
"$mvpa" -mode classify -method ridge -dataTableFile "$work/ridge_class.1D" \
  -target_column Condition -cv_column Run -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -lambda_grid 10,1 \
  -prefix "$work/ridge_observed.niml.dset" -save_predictions "$work/ridge_observed.pred.1D"
cmp "$work/ridge_observed.pred.1D" "$work/ridge_class_1.pred.1D"
cmp "$work/ridge_observed.niml.dset.tuning.1D" "$work/ridge_class_1.niml.dset.tuning.1D"

# Native SVM uses the same sparse mesh identities and synchronized label null.
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method svm \
    -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -svm_c_grid 10,.01 -nperm 99 -permute_within Run -seed 42 \
    -prefix "$work/svm_class_$workers.niml.dset" \
    -save_predictions "$work/svm_class_$workers.pred.1D" \
    -save_fold_scores "$work/svm_class_$workers.fold.1D"
  "$convertdset" -o_1Dp_stdout -input "$work/svm_class_$workers.niml.dset" \
    -prepend_node_index_1D > "$work/svm_class_$workers.1D"
done
cmp "$work/svm_class_1.1D" "$work/svm_class_2.1D"
cmp "$work/svm_class_1.pred.1D" "$work/svm_class_2.pred.1D"
cmp "$work/svm_class_1.fold.1D" "$work/svm_class_2.fold.1D"
cmp "$work/svm_class_1.niml.dset.tuning.1D" "$work/svm_class_2.niml.dset.tuning.1D"
awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
  NF{n++;if($1!=id[n] || $2!=1 || $4!=.25 || $7!=.25 || $10!=3)exit 1}
  END{if(n!=4)exit 1}' "$work/svm_class_1.1D"
"$mvpa" -mode classify -method svm -dataTableFile "$work/ridge_class.1D" \
  -target_column Condition -cv_column Run -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -svm_c_grid 10,.01 \
  -prefix "$work/svm_observed.niml.dset" -save_predictions "$work/svm_observed.pred.1D" \
  -save_fold_scores "$work/svm_observed.fold.1D"
cmp "$work/svm_observed.pred.1D" "$work/svm_class_1.pred.1D"
cmp "$work/svm_observed.fold.1D" "$work/svm_class_1.fold.1D"
cmp "$work/svm_observed.niml.dset.tuning.1D" "$work/svm_class_1.niml.dset.tuning.1D"

for method in lda_diag lda_shrinkage; do
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method "$method" \
      -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -nperm 99 -permute_within Run -seed 42 -prefix "$work/${method}_$workers.niml.dset" \
      -save_predictions "$work/${method}_$workers.pred.1D" -save_fold_scores "$work/${method}_$workers.fold.1D"
    "$convertdset" -o_1Dp_stdout -input "$work/${method}_$workers.niml.dset" \
      -prepend_node_index_1D > "$work/${method}_$workers.1D"
  done
  cmp "$work/${method}_1.1D" "$work/${method}_2.1D"
  cmp "$work/${method}_1.pred.1D" "$work/${method}_2.pred.1D"
  cmp "$work/${method}_1.fold.1D" "$work/${method}_2.fold.1D"
  awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
    NF{n++;if($1!=id[n] || $2!=1 || $4!=.25 || $7!=.25 || $10!=3)exit 1}
    END{if(n!=4)exit 1}' "$work/${method}_1.1D"
  "$mvpa" -mode classify -method "$method" -dataTableFile "$work/ridge_class.1D" \
    -target_column Condition -cv_column Run -mask "$work/mask.niml.dset" \
    -surf "$work/mesh.gii" -searchlight 100 -prefix "$work/${method}_observed.niml.dset" \
    -save_predictions "$work/${method}_observed.pred.1D" -save_fold_scores "$work/${method}_observed.fold.1D"
  cmp "$work/${method}_observed.pred.1D" "$work/${method}_1.pred.1D"
  cmp "$work/${method}_observed.fold.1D" "$work/${method}_1.fold.1D"
done
if "$mvpa" -mode classify -method lda_diag -dataTableFile "$work/samples.1D" \
  -target_column Condition -cv_column Run -mask "$work/mask.niml.dset" \
  -prefix "$work/lda_insufficient.niml.dset" > "$work/lda_insufficient.txt" 2>&1; then
  echo "FAIL LDA accepted zero residual covariance degrees of freedom" >&2
  exit 1
fi
grep -q 'more training rows than classes' "$work/lda_insufficient.txt"

cat > "$work/continuous.1D" <<EOF
Subj Run Sample Rating InputFile
s01 r1 a1 0 $work/samples.niml.dset[0]
s01 r1 b1 10 $work/samples.niml.dset[1]
s01 r2 a2 1 $work/samples.niml.dset[2]
s01 r2 b2 9 $work/samples.niml.dset[3]
EOF
OMP_NUM_THREADS=2 "$mvpa" -mode decode -dataTableFile "$work/continuous.1D" \
  -target_column Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -prefix "$work/decode.niml.dset" \
  -save_predictions "$work/decode.pred.1D" -save_fold_scores "$work/decode.fold.1D" \
  -save_coefficients "$work/dcoef"
OMP_NUM_THREADS=1 "$mvpa" -mode decode -dataTableFile "$work/continuous.1D" \
  -target_column Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -prefix "$work/decode_one.niml.dset" \
  -save_predictions "$work/decode_one.pred.1D" -save_fold_scores "$work/decode_one.fold.1D" \
  -save_coefficients "$work/dcoef_one.niml.dset"
cmp "$work/decode.pred.1D" "$work/decode_one.pred.1D"
cmp "$work/decode.fold.1D" "$work/decode_one.fold.1D"
# Every unit keeps mesh IDs, including sparse holes, and maps neural weights.
for unit in 0 2 5 9; do
  "$convertdset" -o_1Dp_stdout -input "$work/dcoef.unit_$unit.niml.dset" \
    -prepend_node_index_1D > "$work/dcoef.1D"
  "$convertdset" -o_1Dp_stdout -input "$work/dcoef_one.unit_$unit.niml.dset" \
    -prepend_node_index_1D > "$work/dcoef_one.1D"
  cmp "$work/dcoef.1D" "$work/dcoef_one.1D"
  awk 'function abs(x){return x<0?-x:x}
    BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
    NF{n++;if($1!=id[n] || abs($2-(n<=2?82/165:0))>1e-6)exit 1}
    END{if(n!=4)exit 1}' "$work/dcoef.1D"
done
awk -F '\t' '/^#/ || $1=="Unit"{next} {n++;seen[$2]=1}
  END{if(n!=16 || !seen["node_0"] || !seen["node_2"] || !seen["node_5"] || !seen["node_9"])exit 1}' "$work/dcoef.coefficients.1D"

cmp "$work/decode.niml.dset.continuous.1D" "$work/decode_one.niml.dset.continuous.1D"
"$convertdset" -o_1Dp_stdout -input "$work/decode.niml.dset" \
  -prepend_node_index_1D > "$work/decode.1D"
awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9} NF{n++;if($1!=id[n] || $2<.9 || $8!=1)exit 1} END{if(n!=4)exit 1}' "$work/decode.1D"

"$mvpa" -mode encode -dataTableFile "$work/continuous.1D" \
  -design_columns Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -score voxel -prefix "$work/encode.niml.dset" -save_predictions "$work/encode.pred.1D" \
  -save_coefficients "$work/ecoef"
"$convertdset" -o_1Dp_stdout -input "$work/encode.niml.dset" \
  -prepend_node_index_1D > "$work/encode.1D"
awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9} NF{n++;if($1!=id[n])exit 1; if(n<=2){if($2<.9 || $8!=1)exit 1}else if($2!=0 || $8!=0)exit 1} END{if(n!=4)exit 1}' "$work/encode.1D"
awk -F '\t' 'NR>1 {seen[$2]=1} END{if(!seen["node_0"] || !seen["node_2"] || !seen["node_5"] || !seen["node_9"])exit 1}' "$work/encode.pred.1D"
"$convertdset" -o_1Dp_stdout -input "$work/ecoef.niml.dset" \
  -prepend_node_index_1D > "$work/ecoef.1D"
awk 'function abs(x){return x<0?-x:x}
  BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
  NF{n++;if($1!=id[n] || abs($2-(n<=2?82/83:0))>1e-6 || abs($3-(n<=2?5/83:5))>1e-6)exit 1}
  END{if(n!=4)exit 1}' "$work/ecoef.1D"
OMP_NUM_THREADS=2 "$mvpa" -mode encode -dataTableFile "$work/continuous.1D" \
  -design_columns Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -score pattern -prefix "$work/encode_pattern.niml.dset"
awk -F '\t' '/^#/ || $1=="Unit"{next} {n++;if($3!=4 || $4!=2 || $5<.9)exit 1} END{if(n!=4)exit 1}' "$work/encode_pattern.niml.dset.continuous.1D"

for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode decode -dataTableFile "$work/continuous.1D" \
    -target_column Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
    -surf "$work/mesh.gii" -searchlight 100 -lambda_grid 10,1 -inner_cv_column Rating \
    -prefix "$work/tuned_$workers.niml.dset" -save_fold_scores "$work/tuned_$workers.folds.1D"
done
cmp "$work/tuned_1.niml.dset.tuning.1D" "$work/tuned_2.niml.dset.tuning.1D"
cmp "$work/tuned_1.folds.1D" "$work/tuned_2.folds.1D"
awk -F '\t' '/^#/ || $1=="Unit"{next} {seen[$1]=1;n++} END{if(n!=16 || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}' "$work/tuned_1.niml.dset.tuning.1D"
OMP_NUM_THREADS=2 "$mvpa" -mode encode -dataTableFile "$work/continuous.1D" \
  -design_columns Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -score pattern -lambda auto -inner_cv_column Rating \
  -prefix "$work/tuned_encode.niml.dset"
test -s "$work/tuned_encode.niml.dset.tuning.1D"

for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode decode -dataTableFile "$work/continuous.1D" \
    -target_column Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
    -surf "$work/mesh.gii" -searchlight 100 -lambda_grid 10,1 -inner_cv_column Rating \
    -nperm 99 -permute_within Run -seed 42 -prefix "$work/perm_$workers.niml.dset" \
    -save_fold_scores "$work/perm_$workers.folds.1D"
done
cmp "$work/perm_1.niml.dset.inference.1D" "$work/perm_2.niml.dset.inference.1D"
cmp "$work/perm_1.niml.dset.tuning.1D" "$work/tuned_1.niml.dset.tuning.1D"
cmp "$work/perm_1.folds.1D" "$work/tuned_1.folds.1D"
test "$("$dinfo" -nv "$work/perm_1.niml.dset")" = 17
"$convertdset" -o_1Dp_stdout -input "$work/perm_1.niml.dset" \
  -prepend_node_index_1D > "$work/perm.1D"
awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9} NF{n++;if($1!=id[n])exit 1} END{if(n!=4)exit 1}' "$work/perm.1D"
awk -F '\t' '/^#/ || $1=="Unit"{next} {n++;if($6<.25 || $6>1 || $9<$6 || $12!=4)exit 1} END{if(n!=4)exit 1}' \
  "$work/perm_1.niml.dset.inference.1D"
OMP_NUM_THREADS=2 "$mvpa" -mode encode -dataTableFile "$work/continuous.1D" \
  -design_columns Rating -cv_column Run -scale none -mask "$work/mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -score pattern -metric rmse \
  -nperm 99 -permute_within Run -prefix "$work/perm_encode.niml.dset"
awk -F '\t' '/^#/ || $1=="Unit"{next} {n++;if($5<0 || $6<.25 || $9<$6 || $12!=4)exit 1} END{if(n!=4)exit 1}' \
  "$work/perm_encode.niml.dset.inference.1D"

cat > "$work/wrong_nodes.1D" <<'EOF'
0
2
5
8
EOF
"$convertdset" -o_niml -input "$work/mask.1D" \
  -node_index_1D "$work/wrong_nodes.1D" -dset_labels mask \
  -prefix "$work/wrong_mask.niml.dset"
if "$mvpa" -mode classify -dataTableFile "$work/samples.1D" \
  -target_column Condition -cv_column Run -mask "$work/wrong_mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -prefix "$work/wrong_domain.niml.dset" \
  > "$work/wrong_domain.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted mismatched surface node-row domains" >&2
  exit 1
fi
grep -q 'surface node-row domains differ at row 4' "$work/wrong_domain.txt"

cat > "$work/outside_nodes.1D" <<'EOF'
0
2
5
99
EOF
"$convertdset" -o_niml -input "$work/values.1D" \
  -node_index_1D "$work/outside_nodes.1D" -dset_labels 'a1 b1 a2 b2' \
  -prefix "$work/outside_samples.niml.dset"
"$convertdset" -o_niml -input "$work/mask.1D" \
  -node_index_1D "$work/outside_nodes.1D" -dset_labels mask \
  -prefix "$work/outside_mask.niml.dset"
sed "s#samples.niml.dset#outside_samples.niml.dset#" \
  "$work/samples.1D" > "$work/outside.1D"
if "$mvpa" -mode classify -dataTableFile "$work/outside.1D" \
  -target_column Condition -cv_column Run -mask "$work/outside_mask.niml.dset" \
  -surf "$work/mesh.gii" -searchlight 100 -prefix "$work/outside.niml.dset" \
  > "$work/outside.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted a node outside the mesh domain" >&2
  exit 1
fi
grep -q 'node 99 outside mesh range 0..11' "$work/outside.txt"

# All spatial paths must reject duplicate node IDs, even without a mesh.
printf '0\n2\n2\n9\n' > "$work/duplicate_nodes.1D"
"$convertdset" -o_niml -input "$work/values.1D" \
  -node_index_1D "$work/duplicate_nodes.1D" -dset_labels 'a1 b1 a2 b2' \
  -prefix "$work/duplicate_samples.niml.dset"
"$convertdset" -o_niml -input "$work/mask.1D" \
  -node_index_1D "$work/duplicate_nodes.1D" -dset_labels mask \
  -prefix "$work/duplicate_mask.niml.dset"
sed 's#samples.niml.dset#duplicate_samples.niml.dset#' \
  "$work/continuous.1D" > "$work/duplicate.1D"
for mode in classify decode encode; do
  for spatial in roi searchlight; do
    extra=(-mask "$work/duplicate_mask.niml.dset")
    if test "$spatial" = searchlight; then
      extra+=(-surf "$work/mesh.gii" -searchlight 100)
    fi
    case "$mode" in
      classify) role=(-target_column Run) ;;
      decode) role=(-target_column Rating) ;;
      encode) role=(-design_columns Rating -score pattern) ;;
    esac
    # Run cannot be both target and held-out group: use Rating as CV groups
    # for the classification rejection so every training fold has both runs.
    cv=Run
    if test "$mode" = classify; then cv=Rating; fi
    if "$mvpa" -mode "$mode" -dataTableFile "$work/duplicate.1D" \
      "${role[@]}" -cv_column "$cv" \
      "${extra[@]}" -prefix "$work/duplicate_${mode}_${spatial}.niml.dset" \
      > "$work/duplicate_${mode}_${spatial}.txt" 2>&1; then
      echo "FAIL $mode $spatial accepted repeated surface node IDs" >&2
      exit 1
    fi
    grep -q 'repeats node 2' "$work/duplicate_${mode}_${spatial}.txt"
  done
done
if "$mvpa" -mode encode -dataTableFile "$work/duplicate.1D" \
  -design_columns Rating -cv_column Run -score voxel \
  -mask "$work/duplicate_mask.niml.dset" -prefix "$work/duplicate_voxel.niml.dset" \
  > "$work/duplicate_voxel.txt" 2>&1; then
  echo "FAIL voxel encoding accepted repeated surface node IDs" >&2
  exit 1
fi
grep -q 'repeats node 2' "$work/duplicate_voxel.txt"

if "$mvpa" -mode classify -dataTableFile "$work/samples.1D" \
  -target_column Condition -cv_column Run -surf "$work/mesh.gii" \
  -searchlight 'SPHERE(5)' -prefix "$work/bad_radius.niml.dset" \
  > "$work/bad_radius.txt" 2>&1; then
  echo "FAIL 3dMVPA accepted a shaped surface neighborhood" >&2
  exit 1
fi
grep -q 'surface -searchlight radius needs one finite number > 0' "$work/bad_radius.txt"

# Fixed splits use the same sparse node domain for all three prediction modes.
# Two training runs permit nested tuning; only r3 is evaluated.
awk 'NR==1 {print "Split Rating " $0; next}
     {print ($2=="r3" ? "test" : "train"), ($4=="A" ? 0 : 10), $0}' \
  "$work/ridge_class.1D" > "$work/fixed.1D"
"$convertdset" -o_niml -input "$work/samples.niml.dset" -prefix "$work/test_samples.niml.dset"
awk 'NR==1 || $1=="train"' "$work/fixed.1D" > "$work/surf_train.1D"
awk 'NR==1 || $1=="test"' "$work/fixed.1D" | \
  sed 's#samples.niml.dset#test_samples.niml.dset#' > "$work/surf_test.1D"
for mode in classify decode encode; do
  case "$mode" in
    classify) role=(-target_column Condition) ;;
    decode) role=(-target_column Rating) ;;
    encode) role=(-design_columns Rating -score pattern) ;;
  esac
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode "$mode" -method ridge \
      -dataTableFile "$work/fixed.1D" "${role[@]}" -partition_column Split \
      -lambda_grid 1,10 -inner_cv_column Run -nperm 99 -permute_within Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -prefix "$work/fixed_${mode}_${workers}.niml.dset" \
      -save_predictions "$work/fixed_${mode}_${workers}.pred.1D" \
      -save_fold_scores "$work/fixed_${mode}_${workers}.fold.1D"
    "$convertdset" -o_1Dp_stdout -input "$work/fixed_${mode}_${workers}.niml.dset" \
      -prepend_node_index_1D > "$work/fixed_${mode}_${workers}.1D"
    awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
         NF{n++;if($1!=id[n] || $NF!=1)exit 1} END{if(n!=4)exit 1}' \
      "$work/fixed_${mode}_${workers}.1D"
    if test "$mode" = classify; then
      awk -F '\t' '/^#/ {next}
        $11!="r3" {n++;if($14!="NA" || $17!="NA" || $19!="NA" || $20!="NA")exit 1}
        $11=="r3" {t++;if($14!="test" || $17=="NA")exit 1}
        END{if(n!=16 || t!=8)exit 1}' "$work/fixed_${mode}_${workers}.pred.1D"
    else
      awk -F '\t' '/^#/ {next}
        $6!="r3" {n++;if($7!="NA" || $9!="NA" || $10!="NA")exit 1}
        $6=="r3" {t++;if($7!="test" || $9=="NA")exit 1}
        END{if(n<16 || t!=n/2)exit 1}' "$work/fixed_${mode}_${workers}.pred.1D"
    fi
  done
  for suffix in .1D .pred.1D .fold.1D .niml.dset.tuning.1D; do
    cmp "$work/fixed_${mode}_1$suffix" "$work/fixed_${mode}_2$suffix"
  done
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode "$mode" -method ridge \
      -dataTableFile "$work/surf_train.1D" -testTableFile "$work/surf_test.1D" "${role[@]}" \
      -lambda_grid 1,10 -inner_cv_column Run -nperm 99 -permute_within Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -prefix "$work/dual_${mode}_${workers}.niml.dset" \
      -save_predictions "$work/dual_${mode}_${workers}.pred.1D"
    "$convertdset" -o_1Dp_stdout -input "$work/dual_${mode}_${workers}.niml.dset" \
      -prepend_node_index_1D > "$work/dual_${mode}_${workers}.1D"
    cmp "$work/fixed_${mode}_1.1D" "$work/dual_${mode}_${workers}.1D"
    cmp "$work/fixed_${mode}_1.niml.dset.tuning.1D" "$work/dual_${mode}_${workers}.niml.dset.tuning.1D"
  done
  cmp "$work/dual_${mode}_1.pred.1D" "$work/dual_${mode}_2.pred.1D"
done

echo "PASS 3dMVPA sparse geodesic surface searchlights, fixed partitions and domain validation"

# Nuisance adjustment on sparse surfaces: outer/inner fits and synchronized
# permutations must agree across worker counts and retain original node IDs.
awk 'BEGIN {for(v=0;v<4;v++){for(i=0;i<16;i++){
  z=(i%4<2?-1:1);s=(i%2?-1:1);
  printf "%s%.6f",(i?" ":""),(v<2?10+4*z+(v+1)*s+int(i/4)*.1:5+2*z);
}printf "\n"}}' > "$work/nuis_values.1D"
"$convertdset" -o_niml -input "$work/nuis_values.1D" \
  -node_index_1D "$work/nodes.1D" -prefix "$work/nuis.niml.dset"
awk -v file="$work/nuis.niml.dset" 'BEGIN {
 print "Run Split Block Condition Y X Z InputFile";
 for(i=0;i<16;i++){z=(i%4<2?-1:1);s=(i%2?-1:1);
 printf "r%d %s b%d %s %.6f %.6f %d %s[%d]\n",int(i/4),(i<12?"train":"test"),int(i/2),(s<0?"A":"B"),3+2*s+.2*z,s+2*z,z,file,i;
 }}' > "$work/nuis_table.1D"
for mode in classify decode encode; do
  role=(-target_column Condition)
  if test "$mode" = decode; then role=(-target_column Y); fi
  if test "$mode" = encode; then role=(-design_columns X -score pattern); fi
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode "$mode" -method ridge \
      -dataTableFile "$work/nuis_table.1D" -partition_column Split "${role[@]}" \
      -nuisance_columns Z -lambda_grid 1,10 -inner_cv_column Run \
      -nperm 9 -permute_within Block -seed 11 \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -prefix "$work/nuis_${mode}_${workers}.niml.dset" \
      -save_predictions "$work/nuis_${mode}_${workers}.pred.1D"
    "$convertdset" -o_1Dp_stdout -input "$work/nuis_${mode}_${workers}.niml.dset" \
      -prepend_node_index_1D > "$work/nuis_${mode}_${workers}.1D"
    awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
         NF{n++;if($1!=id[n])exit 1} END{if(n!=4)exit 1}' "$work/nuis_${mode}_${workers}.1D"
  done
  for suffix in .1D .pred.1D .niml.dset.tuning.1D; do
    cmp "$work/nuis_${mode}_1$suffix" "$work/nuis_${mode}_2$suffix"
  done
done
echo "PASS 3dMVPA nuisance-adjusted sparse surface tuning and permutations"

# Fold-local feature selection must retain sparse mesh node IDs, not row IDs.
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method ridge \
    -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -select_features_grid 2,1 -lambda_grid 10,1 \
    -nperm 99 -permute_within Run -seed 42 \
    -prefix "$work/selection_$workers.niml.dset" \
    -save_predictions "$work/selection_$workers.pred.1D" \
    -save_fold_scores "$work/selection_$workers.fold.1D"
done
for suffix in features.1D feature_tuning.1D tuning.1D; do
  cmp "$work/selection_1.niml.dset.$suffix" "$work/selection_2.niml.dset.$suffix"
done
cmp "$work/selection_1.pred.1D" "$work/selection_2.pred.1D"
cmp "$work/selection_1.fold.1D" "$work/selection_2.fold.1D"
awk -F '\t' '
  /^#/ { next }
  {
    n++
    if ($2 == "" || $4 != 4 || $5 != 4 || $7 < 1 || $7 > 2) exit 1
    if ($10 != "node_0" && $10 != "node_2") exit 1
    if ($9 < 0 || $9 > 3 || $11 < .98 || $11 > 1) exit 1
    seen[$1 SUBSEP $2 SUBSEP $10]++
    if (seen[$1 SUBSEP $2 SUBSEP $10] != 1) exit 1
  }
  END { if (n != 24) exit 1 }
' "$work/selection_1.niml.dset.features.1D"
# Continuous surface selection uses the same audited node mapping.
OMP_NUM_THREADS=2 "$mvpa" -mode decode -method ridge \
  -dataTableFile "$work/continuous.1D" -target_column Rating -cv_column Run \
  -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
  -select_features 1 -prefix "$work/selection_decode.niml.dset"
awk -F '\t' '
  /^#/ { next }
  { n++; if (($10 != "node_0" && $10 != "node_2") || $7 != 1 || $11 < .99999) exit 1 }
  END { if(n != 8) exit 1 }
' "$work/selection_decode.niml.dset.features.1D"
OMP_NUM_THREADS=2 "$mvpa" -mode encode -method ridge \
  -dataTableFile "$work/continuous.1D" -design_columns Rating -cv_column Run \
  -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 -score pattern \
  -select_features 2 -prefix "$work/selection_encode.niml.dset"
awk -F '\t' '
  /^#/ { next }
  { n++; if ($6 != 2 || $7 != 1 || $9 != 0 || $10 != "Rating" || $11 < .99999) exit 1 }
  END { if(n != 8) exit 1 }
' "$work/selection_encode.niml.dset.features.1D"

# Explicit inner objectives and common candidates retain sparse unit identities.
for method in ridge svm; do
  for workers in 1 2; do
    penalty=(-lambda_grid .1,10)
    if test "$method" = svm; then penalty=(-svm_c_grid .1,10); fi
    OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method "$method" \
      -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -select_features_grid 2,1 "${penalty[@]}" -metric accuracy -tune_metric BACC \
      -nperm 99 -permute_within Run -prefix "$work/objective_${method}_$workers.niml.dset" \
      -save_tuning "$work/objective_${method}_$workers.candidates.1D"
  done
  cmp "$work/objective_${method}_1.candidates.1D" "$work/objective_${method}_2.candidates.1D"
  awk -F '\t' '
    /^#/ {next}
    { n++;seen[$1]=1;if($5!="balanced_accuracy" || $12!=4 || $13!=2 || $14<0 || $14>1)exit 1;chosen+=$15 }
    END{if(n!=48 || chosen!=12 || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
  ' "$work/objective_${method}_1.candidates.1D"
done
# The 3-run table permits grouped inner fits with at least two rows; Run is a
# numeric response here so both continuous modes can reuse the sparse fixture.
awk 'BEGIN{OFS="\t"} NR==1{print $0,"Rating";next} {print $0,substr($2,2)+0}' \
  "$work/ridge_class.1D" > "$work/objective_continuous.1D"
for mode in decode encode; do
  role=(-target_column Rating)
  if test "$mode" = encode; then role=(-design_columns Rating -score pattern); fi
  OMP_NUM_THREADS=2 "$mvpa" -mode "$mode" -method ridge \
    -dataTableFile "$work/objective_continuous.1D" "${role[@]}" -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -lambda_grid .1,10 -tune_metric MAE \
    -prefix "$work/objective_$mode.niml.dset" -save_tuning "$work/objective_$mode.candidates.1D"
  awk -F '\t' '
    /^#/ {next}
    {n++;seen[$1]=1;if($5!="MAE" || $12!=4 || $14<0)exit 1;chosen+=$15}
    END{if(n!=24 || chosen!=12 || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
  ' "$work/objective_$mode.candidates.1D"
done

# Fold-local PCA: sparse IDs, joint grids, permutations and deterministic workers.
for method in ridge svm centroid; do
  for workers in 1 2; do
    penalty=()
    if test "$method" = ridge; then penalty=(-lambda_grid .1,10); fi
    if test "$method" = svm; then penalty=(-svm_c_grid .1,10); fi
    OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method "$method" \
      -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -select_features_grid 2,4 -pca_components_grid 1,9 "${penalty[@]}" \
      -nperm 99 -permute_within Run -prefix "$work/pca_${method}_$workers.niml.dset" \
      -save_predictions "$work/pca_${method}_$workers.pred.1D" \
      -save_pca_loadings "$work/pca_${method}_$workers.load.1D"
    "$convertdset" -o_1Dp_stdout -input "$work/pca_${method}_$workers.niml.dset" \
      -prepend_node_index_1D > "$work/pca_${method}_$workers.1D"
  done
  for suffix in pca.1D pca_tuning.1D; do
    cmp "$work/pca_${method}_1.niml.dset.$suffix" "$work/pca_${method}_2.niml.dset.$suffix"
  done
  for suffix in pred.1D load.1D 1D; do
    cmp "$work/pca_${method}_1.$suffix" "$work/pca_${method}_2.$suffix"
  done
  awk -F '\t' '
    /^#/ {next}
    {n++;seen[$1]=1;if($6!="node_0" && $6!="node_2" && $6!="node_5" && $6!="node_9")exit 1}
    END{if(n<24 || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
  ' "$work/pca_${method}_1.load.1D"
done
for mode in decode encode; do
  role=(-target_column Rating)
  if test "$mode" = encode; then role=(-design_columns Rating -score pattern); fi
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode "$mode" -method ridge \
      -dataTableFile "$work/objective_continuous.1D" "${role[@]}" -cv_column Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -lambda_grid .1,10 -pca_components_grid 1,3 -tune_metric MAE \
      -nperm 99 -permute_within Run -prefix "$work/pca_${mode}_$workers.niml.dset" \
      -save_pca_loadings "$work/pca_${mode}_$workers.load.1D"
  done
  cmp "$work/pca_${mode}_1.niml.dset.pca.1D" "$work/pca_${mode}_2.niml.dset.pca.1D"
  cmp "$work/pca_${mode}_1.niml.dset.pca_tuning.1D" "$work/pca_${mode}_2.niml.dset.pca_tuning.1D"
  cmp "$work/pca_${mode}_1.load.1D" "$work/pca_${mode}_2.load.1D"
done

# Descriptive outer-training activation patterns retain neural sparse IDs.
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method ridge \
    -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -select_features_grid 2,4 -pca_components_grid 1,2 -lambda_grid .1,10 \
    -nperm 99 -permute_within Run -prefix "$work/pattern_score_$workers.niml.dset" \
    -save_patterns "$work/pattern_$workers.niml.dset" \
    -save_predictions "$work/pattern_$workers.pred.1D"
  sed "s/pattern_$workers/pattern/g" "$work/pattern_$workers.niml.dset.patterns.1D" > "$work/pattern_$workers.table"
done
cmp "$work/pattern_1.table" "$work/pattern_2.table"
cmp "$work/pattern_1.niml.dset.pattern_covariance.1D" "$work/pattern_2.niml.dset.pattern_covariance.1D"
cmp "$work/pattern_1.pred.1D" "$work/pattern_2.pred.1D"
awk -F '\t' '
  /^#/ {next}
  {n++;seen[$6]=1;if($11!=1 || $4!=4 || $5<0 || $5>3 || $16<0 || $16>1)exit 1}
  END{if(n!=96 || !seen["node_0"] || !seen["node_2"] || !seen["node_5"] || !seen["node_9"])exit 1}
' "$work/pattern_1.niml.dset.patterns.1D"
for unit in 0 2 5 9; do
  for fold in 1 2 3; do
    for workers in 1 2; do
      "$convertdset" -o_1Dp_stdout -input "$work/pattern_$workers.unit_$unit.fold_$fold.niml.dset" \
        -prepend_node_index_1D > "$work/pattern_map_$workers.1D"
    done
    cmp "$work/pattern_map_1.1D" "$work/pattern_map_2.1D"
    awk 'BEGIN{id[1]=0;id[2]=2;id[3]=5;id[4]=9}
      NF{n++;if($1!=id[n] || ($2+$3)>1e-5 || ($2+$3)<-1e-5)exit 1}
      END{if(n!=4)exit 1}' "$work/pattern_map_1.1D"
  done
done

# Paired comparisons consume sparse classification/continuous audits directly.
for kind in ridge decode encode; do
  "$mvpa" -compare_predictions "$work/pca_${kind}_1.pred.1D" "$work/pca_${kind}_2.pred.1D" \
    -prefix "$work/paired_$kind"
  awk -F '\t' '
    /^#/ {next}
    {n++;seen[$2]=1;if($11!="NA" && $11!=0)exit 1;if($12!="NA" && $12!=0)exit 1}
    END{if(!n || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
  ' "$work/paired_$kind.comparison.1D"
done

# Response-specific lambdas in shared sparse-surface decode/encode fits.
for mode in decode encode; do
  role=(-target_column Rating)
  if test "$mode" = encode; then role=(-design_columns Rating -score pattern); fi
  for workers in 1 2; do
    OMP_NUM_THREADS=$workers "$mvpa" -mode "$mode" -method ridge \
      -dataTableFile "$work/objective_continuous.1D" "${role[@]}" -cv_column Run \
      -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
      -lambda_per_response -lambda_grid .1,10 -pca_components_grid 1,3 \
      -nperm 99 -permute_within Run -prefix "$work/response_${mode}_$workers.niml.dset" \
      -save_predictions "$work/response_${mode}_$workers.pred.1D"
  done
  cmp "$work/response_${mode}_1.pred.1D" "$work/response_${mode}_2.pred.1D"
  cmp "$work/response_${mode}_1.niml.dset.tuning.1D" "$work/response_${mode}_2.niml.dset.tuning.1D"
  awk -F '\t' -v mode="$mode" '
    /^#/ {next}
    {n++;seen[$1]=1;chosen+=$15;if($13<0 || $13>1)exit 1;
     if(mode=="encode" && $19!="node_0" && $19!="node_2" && $19!="node_5" && $19!="node_9")exit 1}
    END{if(!n || chosen!=(mode=="encode"?48:12) || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
  ' "$work/response_${mode}_1.niml.dset.tuning.1D"
done

# #38: exact sparse domains and the existing runners also support transfer.
cat > "$work/transfer.1D" <<EOF2
Subj Run Sample Item Session Task Condition Y X InputFile
s01 r1 a1 i1 pre visual A 0 0 $work/samples.niml.dset[0]
s01 r1 b1 i2 pre visual B 10 10 $work/samples.niml.dset[1]
s01 r2 a2 i3 post auditory A 1 1 $work/samples.niml.dset[2]
s01 r2 b2 i4 post auditory B 9 9 $work/samples.niml.dset[3]
EOF2
for mode in classify decode encode; do
    target=( -target_column Condition )
    if [[ "$mode" == decode ]]; then target=( -target_column Y ); fi
    if [[ "$mode" == encode ]]; then target=( -design_columns X -score pattern ); fi
    for kind in transfer fixed; do
        partition=( -transfer_column Task -train_domain visual -test_domain auditory -transfer_subjects within )
        if [[ "$kind" == fixed ]]; then partition=( -partition_column Task -train_value visual -test_value auditory ); fi
        OMP_NUM_THREADS=2 "$mvpa" -mode "$mode" -dataTableFile "$work/transfer.1D" \
            "${target[@]}" "${partition[@]}" -method ridge -lambda 1 \
            -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
            -prefix "$work/$mode.$kind.niml.dset" > "$work/$mode.$kind.log" 2>&1
        "$convertdset" -o_1Dp_stdout -input "$work/$mode.$kind.niml.dset" \
            -prepend_node_index_1D > "$work/$mode.$kind.values.1D"
    done
    cmp "$work/$mode.transfer.values.1D" "$work/$mode.fixed.values.1D"
done
printf 'PASS sparse surface transfer matches fixed-split classification, decoding and encoding\n'

# #29: logistic probability audits preserve sparse IDs and worker determinism.
for workers in 1 2; do
  OMP_NUM_THREADS=$workers "$mvpa" -mode classify -method logistic \
    -dataTableFile "$work/ridge_class.1D" -target_column Condition -cv_column Run \
    -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
    -logistic_lambda_grid .1,10 -select_features_grid 1,2 -pca_components_grid 1,2 \
    -nperm 99 -permute_within Run -seed 29 -prefix "$work/logistic_$workers.niml.dset" \
    -save_predictions "$work/logistic_$workers.pred.1D" \
    -save_probability_scores "$work/logistic_$workers.scores.1D" \
    -save_calibration "$work/logistic_$workers.cal.1D"
done
for suffix in pred scores cal; do
  cmp "$work/logistic_1.$suffix.1D" "$work/logistic_2.$suffix.1D"
done
awk -F '\t' '
  /^#/ {next}
  {n++;seen[$2]=1;if($22<0 || $23<0 || ($22+$23)<.999999 || ($22+$23)>1.000001)exit 1}
  END{if(n!=24 || !seen[0] || !seen[2] || !seen[5] || !seen[9])exit 1}
' "$work/logistic_1.pred.1D"
# Apply the same logistic estimator through the directional transfer runner.
"$mvpa" -mode classify -method logistic -dataTableFile "$work/transfer.1D" \
  -target_column Condition -transfer_column Task -train_domain visual -test_domain auditory \
  -transfer_subjects within -mask "$work/mask.niml.dset" -surf "$work/mesh.gii" -searchlight 100 \
  -prefix "$work/logistic_transfer.niml.dset" -save_predictions "$work/logistic_transfer.pred.1D"
printf 'PASS logistic sparse surface probabilities, nested selection/PCA, permutations and transfer\n'
