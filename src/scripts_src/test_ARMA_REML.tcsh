#!/bin/tcsh

# --- The shell script below is for testing this ARMA/REML implementation. ---
#     It was extracted from the end of 3dDeconvolve.c.  10 Sep 2025 [rickr]

### Script to test REML GLSQ vs OLSQ regression

# B      = signal amplitude for all repetitions
# P      = signal period (TRs)
# nstim  = number of signals (IM regression)
# numvox = number of voxels to simulate
# so there is a total of $nstim * $numvox stimuli being simulated

set B      = 2
set P      = 12
set nstim  = 20
set numvox = 400

# ARMA(1,1) parameters for this test/simulation

set AA  = 0.8
set LAM = 0.5

# D = number of time points (TR=1)

@ D = $P * $nstim

# create stimulus timing

1deval -num $nstim -expr "i*${P}"  > stim.1D

# create the voxel time series = simulated data

1deval -num $D -expr "${B}*sin(PI*t/${P})^2"  > signal.1D
foreach ii ( `count_afni -dig 4 1 $numvox` )
  1dgenARMA11 -num $D -a $AA -lam $LAM               > noise.1D
  1deval      -a noise.1D -b signal.1D -expr 'a+b'   > data${ii}.1D
end

# glue them together into one file

1dcat data0*.1D > data.1D
\rm -f data0*.1D noise.1D signal.1D

# create the regression matrix

3dDeconvolve -num_stimts 1                                            \
             -stim_times_IM 1 stim.1D "EXPR(0,${P}) sin(PI*t/${P})^2" \
             -stim_label    1 'sinsq'                                 \
             -nodata $D 1 -x1D_stop -polort 2 -x1D test.xmat.1D

# analyses

3dREMLfit -matrix test.xmat.1D \
          -input data.1D\'     \
          -Rvar  test.Rvar.1D  \
          -Rbeta test.Rbeta.1D \
          -Obeta test.Obeta.1D \
          -nobout -Grid 5 -MAXa 0.9 -MAXb 0.9 -NEGcor

# extract the betas for each voxel into one long single column 1D file
# instead of the multi-column file output by 3dREMLfit

@ ns1 = $nstim - 1
if( -f test.Rbeta.all.1D ) \rm test.Rbeta.all.1D
if( -f test.Obeta.all.1D ) \rm test.Obeta.all.1D
foreach ii ( `count_afni -dig 1 0 $ns1` )
  1dcat test.Rbeta.1D"[$ii]" >> test.Rbeta.all.1D
  1dcat test.Obeta.1D"[$ii]" >> test.Obeta.all.1D
end

# compute the mean and stdev of the GLSQ and OLSQ betas
# (means should be about B, or something weird happened)

3dTstat -mean -stdev -prefix test.Rbeta.stat.1D test.Rbeta.all.1D\'
3dTstat -mean -stdev -prefix test.Obeta.stat.1D test.Obeta.all.1D\'

# compute the ratio of the stdevs
# srat > 1 means OLSQ stdev was bigger than GLSQ (what we expect)

set Rsig = `1dcat test.Rbeta.stat.1D'[1]'`
set Osig = `1dcat test.Obeta.stat.1D'[1]'`
set srat = `ccalc "$Osig/$Rsig"`

# print out these results

echo "======================="
echo "a = $AA  lam = $LAM"
echo "REML mean stdev = " `1dcat test.Rbeta.stat.1D`
echo "OLSQ mean stdev = " `1dcat test.Obeta.stat.1D`
echo "Osig/Rsig       =  $srat"
echo "======================="

time ; exit 0
