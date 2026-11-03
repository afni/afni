#!/usr/bin/env bash
set -euo pipefail
# Exercise collision preflight regardless of the user's AFNI configuration.
export AFNI_DECONFLICT=NO
mvpa=$1
undump=$2
dump=$3
work=$(mktemp -d /tmp/thd_mvpa_coefficients.XXXXXX)
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
make_data mid 0 0 5 5
make_data high 1 2 5 5
cat > "$work/samples.1D" <<TABLE
Subj Run Sample X Y Z Flat Condition InputFile
s1 r1 a -1 8 23 5 A $work/low+orig
s1 r1 b 0 10 20 5 A $work/mid+orig
s1 r1 c 1 12 17 5 B $work/high+orig
s1 r2 d -1 8 23 5 A $work/low+orig
s1 r2 e 0 10 20 5 A $work/mid+orig
s1 r2 f 1 12 17 5 B $work/high+orig
TABLE
common=(-dataTableFile "$work/samples.1D" -cv_column Run -mask "$work/mask+orig" -scale none -lambda 1)
# Full-data ridge on x,2x has slopes (8,16)/21 for Y, (-12,-24)/21 for Z.
"$mvpa" -mode decode "${common[@]}" -target_columns Y,Z -prefix "$work/baseline" \
  -save_predictions "$work/baseline.pred.1D" -nperm 4 -permute_within Run
"$mvpa" -mode decode "${common[@]}" -target_columns Y,Z -prefix "$work/decode" \
  -save_predictions "$work/decode.pred.1D" -nperm 4 -permute_within Run \
  -save_coefficients "$work/dcoef"
cmp "$work/baseline.pred.1D" "$work/decode.pred.1D"
cmp "$work/baseline.continuous.1D" "$work/decode.continuous.1D"
cmp "$work/baseline.inference.1D" "$work/decode.inference.1D"
"$dump" -noijk "$work/baseline+orig" > "$work/baseline.map"
"$dump" -noijk "$work/decode+orig" > "$work/decode.map"
cmp "$work/baseline.map" "$work/decode.map"
awk -F '\t' '
function abs(x){return x<0?-x:x}
/^#/ || $1=="Unit" {next}
{if($7!=6 || $6!=1)exit 1; n++}
$1==1 {expected=($3=="Y"?8:-12)/21*($2=="voxel_1"?2:1);
       if(abs($4-expected)>1e-8 || abs($5-($3=="Y"?10:20))>1e-8)exit 1}
$1==2 {if($4!=0)exit 1}
END {if(n!=8)exit 1}
' "$work/dcoef.coefficients.1D"
"$dump" -noijk -mask "$work/mask+orig" "$work/dcoef.unit_1+orig" > "$work/dcoef.map"
awk 'function abs(x){return x<0?-x:x} NR==1 {if(abs($1-8/21)>1e-6 || abs($2+12/21)>1e-6)exit 1}
NR==2 {if(abs($1-16/21)>1e-6)exit 1} NR>2 {if($1!=0 || $2!=0)exit 1}
END{if(NR!=4)exit 1}' "$work/dcoef.map"
# Encoding combines nonoverlapping response domains into one map.
"$mvpa" -mode encode "${common[@]}" -design_columns X -prefix "$work/encode" -save_coefficients "$work/ecoef"
"$dump" -noijk -mask "$work/mask+orig" "$work/ecoef+orig" > "$work/ecoef.map"
awk 'function abs(x){return x<0?-x:x} NR==1 {if(abs($1-.8)>1e-6 || $2!=0)exit 1}
NR==2 {if(abs($1-1.6)>1e-6 || $2!=0)exit 1} NR>2 {if($1!=0 || $2!=5)exit 1}
END{if(NR!=4)exit 1}' "$work/ecoef.map"
# Population-SD scaling and constant-design handling have explicit audit values.
"$mvpa" -mode encode "${common[@]}" -scale feature_zscore -design_columns X,Flat \
  -score pattern -prefix "$work/zscore" -save_coefficients "$work/zcoef"
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="Unit"{next}
$2=="X"{if(abs($5-sqrt(2/3))>1e-8 || $4!=0)exit 1;n++}
$2=="Flat"{if($4!=5 || $5!=1 || $6!=1)exit 1;c++}
END{if(n!=2 || c!=2)exit 1}' "$work/zcoef.preprocessing.1D"
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="Unit"{next}
$2=="Flat"{if($4!=0)exit 1}
$2=="X" && $3=="voxel_0"{if(abs($4-4/(7*sqrt(2/3)))>1e-6)exit 1;n++}
END{if(n!=1)exit 1}' "$work/zcoef.coefficients.1D"
# Sample centering is a per-row transform; run centering saves each group's means.
"$mvpa" -mode decode "${common[@]}" -scale sample_center -target_columns Y \
  -prefix "$work/sc" -save_coefficients "$work/scoef"
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="Unit"{next}
$1==1{if(abs($4-($2=="voxel_0"?-4/3:4/3))>1e-8)exit 1;n++} END{if(n!=2)exit 1}' "$work/scoef.coefficients.1D"
# Hold out samples within each run so every fold has both run groups.
"$mvpa" -mode decode -dataTableFile "$work/samples.1D" -cv_column Condition \
  -mask "$work/mask+orig" -scale run_center -target_column Y -lambda 1 \
  -prefix "$work/rc" -save_coefficients "$work/rcoef"
awk -F '\t' '/^#/ || $1=="Unit"{next} {if($3!="0" && $3!="1")exit 1;n++} END{if(n!=8)exit 1}' "$work/rcoef.preprocessing.1D"
# Ridge classification exposes one response per 0/1 class with class labels.
"$mvpa" -mode classify -method ridge "${common[@]}" -target_column Condition \
  -prefix "$work/class" -save_coefficients "$work/ccoef"
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="Unit"{next}
$1==1 && $3=="B"{if(abs($4-($2=="voxel_0"?2/21:4/21))>1e-8 || abs($5-1/3)>1e-8)exit 1;n++}
END{if(n!=2)exit 1}' "$work/ccoef.coefficients.1D"
# Overlapping units retain distinct weights and do not center-paint them.
for threads in 1 2; do
  OMP_NUM_THREADS=$threads "$mvpa" -mode decode "${common[@]}" -target_column Y \
    -searchlight 'SPHERE(2)' -prefix "$work/sl$threads" -save_coefficients "$work/slcoef$threads"
done
sed "s/slcoef1/slcoef/g" "$work/slcoef1.coefficients.1D" > "$work/sl1.1D"
sed "s/slcoef2/slcoef/g" "$work/slcoef2.coefficients.1D" > "$work/sl2.1D"
cmp "$work/sl1.1D" "$work/sl2.1D"
for unit in 0 1 2 3; do
  "$dump" -noijk "$work/slcoef1.unit_$unit+orig" > "$work/m1"
  "$dump" -noijk "$work/slcoef2.unit_$unit+orig" > "$work/m2"
  cmp "$work/m1" "$work/m2"
done
# A separately specified refit penalty accompanies tuned CV without changing it.
for fit in plain coefficients; do
  extra=()
  if [[ $fit == coefficients ]]; then extra=(-save_coefficients "$work/autocoef" -coef_lambda 4); fi
  "$mvpa" -mode decode -dataTableFile "$work/samples.1D" -cv_column Sample -inner_cv_column Run \
    -mask "$work/mask+orig" -scale none -lambda auto -target_column Y \
    -prefix "$work/auto_$fit" -save_predictions "$work/auto_$fit.pred.1D" ${extra[@]+"${extra[@]}"}
done
cmp "$work/auto_plain.pred.1D" "$work/auto_coefficients.pred.1D"
cmp "$work/auto_plain.tuning.1D" "$work/auto_coefficients.tuning.1D"
awk -F '\t' 'function abs(x){return x<0?-x:x} /^#/ || $1=="Unit"{next}
$1==1{if($6!=4 || abs($4-($2=="voxel_0"?1/3:2/3))>1e-8)exit 1;n++}
END{if(n!=2)exit 1}' "$work/autocoef.coefficients.1D"
expect_failure() {
  if "$mvpa" "$@" > "$work/error" 2>&1; then
    echo 'Unexpected coefficient option success' >&2;exit 1
  fi
}
expect_failure -mode decode -dataTableFile "$work/samples.1D" -cv_column Run -mask "$work/mask+orig" -target_column Y -lambda auto -save_coefficients "$work/bad" -prefix "$work/badscore"
grep -q 'explicit -coef_lambda' "$work/error"
expect_failure -mode decode "${common[@]}" -target_column Y -coef_lambda 0 -save_coefficients "$work/bad" -prefix "$work/badscore"
grep -q 'finite and positive' "$work/error"
expect_failure -mode classify -dataTableFile "$work/samples.1D" -cv_column Run \
  -mask "$work/mask+orig" -scale none -target_column Condition -save_coefficients "$work/bad" -prefix "$work/badscore"
grep -q 'requires -method ridge' "$work/error"
expect_failure -mode decode "${common[@]}" -target_column Y -save_coefficients "$work/bad" \
  -save_predictions "$work/bad.coefficients.1D" -prefix "$work/badscore"
grep -q 'filenames must differ' "$work/error"
expect_failure -mode decode "${common[@]}" -target_column Y -save_coefficients "$work/collision" -prefix "$work/collision.unit_1"
grep -q 'held-out score dataset must differ' "$work/error"
# Existing coefficient outputs are protected before CV opens audit files.
expect_failure -mode decode "${common[@]}" -target_column Y -save_coefficients "$work/dcoef" -prefix "$work/badscore"
grep -q 'already exists' "$work/error"
echo 'MVPA descriptive coefficient checks passed'
