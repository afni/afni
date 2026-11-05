#!/usr/bin/env python3
"""Explicitly fit generated M6 default/custom models, never during generation.

Exit 77 records missing dependencies; no package installation is attempted.
"""
import argparse
import json
import math
import os
from pathlib import Path
import random
import shutil
import subprocess
import sys
from insync_window_fixtures import nifti
from run_insync_estimation import image
from run_insync_block_summary import read_table


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True)
    p.add_argument('--work',required=True); p.add_argument('--r-bin')
    args=p.parse_args(); env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO')
    if args.r_bin: env['PATH']=str(Path(args.r_bin).resolve())+os.pathsep+env['PATH']
    rscript=shutil.which('Rscript',path=env['PATH']); isc=shutil.which('3dISC',path=env['PATH'])
    if not rscript or not isc:
        print('SKIP: Rscript and 3dISC are required'); return 77
    dependencies=subprocess.run([rscript,'-e','quit(status=if(requireNamespace("lme4",quietly=TRUE)) 0 else 77)'],env=env,capture_output=True,text=True)
    if dependencies.returncode:
        print(f'SKIP: lme4 unavailable in {rscript}'); return 77
    root=Path(args.work).resolve(); root.mkdir(parents=True,exist_ok=False)
    rng=random.Random(20261105); ns=12; nt=80
    rows=['Subj Group Age InputFile']
    for si in range(ns):
        values=[]
        for t in range(nt):
            for v in range(24):
                values.append(.7*math.sin(t*.17+v*.07)+.35*math.cos(t*.31+si*.2)+rng.gauss(0,.7)+.015*si*math.sin(t*.17+v*.07))
        nifti(root/f's{si:02d}.nii',values,nt)
        rows.append(f's{si:02d} {"A" if si<6 else "B"} {20+si} s{si:02d}.nii')
    (root/'subjects.txt').write_text('\n'.join(rows)+'\n')
    program=str(Path(args.program).resolve()); cases=[]
    for name,options in [('default',[]),('custom',[ '-isc_model','I(Age1+Age2)+(1|Subj1)+(1|Subj2)', '-isc_qVars','Age1,Age2','-isc_qVarCenters','25,25','-isc_gltCode','intercept','1 0','-isc_gltCode','age_sum','0 1'])]:
        parent=root/("folder's space" if name=='custom' else 'ordinary'); parent.mkdir()
        pairprefix=str(parent/name)
        generation=subprocess.run([program,'-quiet','-window',str(nt),'-prefix',f'desc_{name}','-save_pairwise',pairprefix]+options+['-dataTableFile','subjects.txt'],cwd=root,env=env,text=True,capture_output=True)
        (root/f'{name}.generation.log').write_text(generation.stdout+generation.stderr)
        assert generation.returncode==0,generation.stderr
        bundle=Path(pairprefix+'.isc'); job=read_table(bundle/'jobs.tsv')[0]
        # The test explicitly invokes the generated job from a different cwd.
        fit=subprocess.run(['sh',str(bundle/job['Script'])],cwd=root.parent,env=env,text=True,capture_output=True,timeout=300)
        (root/f'{name}.fit.log').write_text(fit.stdout+fit.stderr)
        assert fit.returncode==0,(name,fit.stdout[-4000:],fit.stderr[-4000:])
        path=bundle/job['FitPrefix']; assert path.exists(),(name,'missing fitted output')
        shape,values=image(path); assert len(values)==24*(2 if name=='default' else 4),(name,shape)
        assert all(math.isfinite(v) for v in values) and all(any(abs(values[b*24+v])>1e-5 for b in range(len(values)//24)) for v in range(24)),(name,values)
        cases.append(dict(model=name,script=str(bundle/job['Script']),output=str(path),shape=shape,nonzero=sum(abs(v)>1e-5 for v in values)))
    record=dict(Rscript=rscript,ISC=isc,cases=cases)
    (root/'fits.json').write_text(json.dumps(record,indent=2)+'\n')
    print('PASS: generated default and custom 3dISC models fitted from another working directory')
    print(root/'fits.json'); return 0

if __name__=='__main__': sys.exit(main())
