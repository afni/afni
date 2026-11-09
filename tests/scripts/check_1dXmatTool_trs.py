#!/usr/bin/env python3
"""Stdout TR queries: independent threshold/censor/window oracle and shell use."""
import argparse
import os
from pathlib import Path
import subprocess
import tempfile
from check_1dXmatTool import write
import numpy as np

p=argparse.ArgumentParser()
p.add_argument('--tool',required=True)
p.add_argument('--tstat')
a=p.parse_args();tool=str(Path(a.tool).resolve())
with tempfile.TemporaryDirectory(prefix='xmat-trs-') as tmp:
    w=Path(tmp)
    values=np.array([0,1,2,4,2,-1,0,1, 0,2,4,8,4,-2,0,2],dtype=float)
    x=np.column_stack([np.ones(16),values,values/2])
    attrs=dict(ColumnLabels='base ; face#0 ; face#1',ColumnGroups='-1,1,1',
               NRowFull='16',GoodList='0..15',RunStart='0,8',RowTR='2',
               Nstim='1',StimLabels='face',StimBots='1',StimTops='2')
    write(w/'full.1D',x,attrs)
    good=[i for i in range(16) if i not in [3,11]]
    write(w/'censored.1D',x[good],dict(attrs,GoodList=','.join(map(str,good))))
    def run(extra,expect=None,fail=None,source='full.1D'):
        r=subprocess.run([tool,'-infile',source,'-show_trs',*map(str,extra)],cwd=w,text=True,capture_output=True)
        if fail:
            assert r.returncode!=0 and r.stdout=='' and fail in r.stderr,(extra,r.stdout,r.stderr)
        else:
            assert r.returncode==0 and r.stdout==expect+'\n',(extra,r.stdout,r.stderr)
        return r
    base=['-runs','2','-select_labels','face#0']
    run([*base,'-tr_above','2'],expect='2,3,4')
    run([*base,'-tr_above_frac','.5'],expect='3')  # strict > excludes ties
    run([*base,'-tr_above_frac','.25','-tr_window','1..4'],expect='2,3,4')
    run([*base,'-tr_above_frac','.5','-tr_window','0..2'],expect='2')
    run([*base,'-tr_above','1'],source='censored.1D',expect='1,2,4,7')
    run([*base,'-tr_above_frac','.25','-uncensored','full.1D'],source='censored.1D',expect='2,4')
    run([*base,'-tr_above_frac','.5','-uncensored','full.1D'],source='censored.1D',fail='no retained TRs')
    run([*base,'-tr_above_frac','.5'],source='censored.1D',fail='requires -uncensored')
    run(['-runs','1','-select_columns','1','-tr_above','0','-tr_window','1..4'],expect='1,2,3,4')
    single=dict(attrs,StimTops='1',ColumnGroups='-1,1,0')
    write(w/'single.1D',x,single)
    run(['-runs','1','-select_stimuli','face','-tr_above_frac','.5'],source='single.1D',expect='3')
    for extra,msg in [(['-runs','1','-select_stimuli','face','-tr_above','0'],'exactly one design column'),
                      ([*base,'-tr_above','99'],'no retained TRs'),
                      ([*base,'-tr_above_frac','.5','-tr_window','5..6'],'no positive'),
                      ([*base,'-tr_above','0','-tr_window','0..8'],'within the selected run'),
                      ([*base,'-tr_above','0','-tr_window','4..2'],'within the selected run'),
                      ([*base,'-tr_above','0','-describe'],'choose one operation'),
                      ([*base,'-tr_above','0','-tr_above_frac','.5'],'exactly one'),
                      ([*base,'-tr_above_frac','nan'],'invalid TR threshold'),
                      ([*base,'-tr_above_frac','1'],'invalid TR threshold'),
                      (['-select_labels','face#0','-tr_above','0'],'exactly one run'),
                      (['-runs','1,2','-select_labels','face#0','-tr_above','0'],'exactly one run'),
                      (['-runs','1','-select_labels','missing','-tr_above','0'],'unknown column')]:
        run(extra,fail=msg)
    bad=x.copy();bad[1,1]=100
    write(w/'badref.1D',bad,attrs)
    run([*base,'-tr_above_frac','.5','-uncensored','badref.1D'],source='censored.1D',fail='differs at a retained')
    # Shell command substitution has exactly the selector, no banner or report.
    env=dict(os.environ,XMAT_QUERY_TOOL=tool)
    r=subprocess.run(['bash','-c','trs=$("$XMAT_QUERY_TOOL" -infile full.1D -show_trs -runs 2 -select_labels "face#0" -tr_above 2) || exit 1; printf "%s" "epi.nii.gz[$trs]"'],cwd=w,env=env,text=True,capture_output=True)
    assert r.returncode==0 and r.stdout=='epi.nii.gz[2,3,4]',r
    if a.tstat:
        import nibabel as nib
        data=np.broadcast_to(np.arange(8,dtype=np.float32).reshape(1,1,1,8),(2,2,2,8)).copy()
        nib.save(nib.Nifti1Image(data,np.eye(4)),w/'epi.nii.gz')
        env['TSTAT_TOOL']=str(Path(a.tstat).resolve())
        r=subprocess.run(['bash','-c','trs=$("$XMAT_QUERY_TOOL" -infile full.1D -show_trs -runs 2 -select_labels "face#0" -tr_above 2) || exit 1; "$TSTAT_TOOL" -mean -prefix mean.nii "epi.nii.gz[$trs]"'],cwd=w,env=env,text=True,capture_output=True)
        assert r.returncode==0,r.stderr
        assert np.allclose(nib.load(w/'mean.nii').get_fdata(),3)
print('PASS TR selectors: thresholds, pre-censor peaks, windows, original run-local indices, ambiguity/errors and Bash substitution')
