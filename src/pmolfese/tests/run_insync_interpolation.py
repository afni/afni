#!/usr/bin/env python3
"""Interpolation CLI contracts and temporal estimand separation (stdlib only).

The larger source-local suites accept --interp K and compare correlations,
counts, resampling and exports against independent augmented-data oracles.
This suite checks cross-policy invariants and option failures explicitly.
"""
import argparse
import os
from pathlib import Path
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image
from run_insync_pairwise_missing import table


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--program',required=True)
    program=str(Path(ap.parse_args().program).resolve())
    runs=0
    with tempfile.TemporaryDirectory(prefix='insync-interpolation-') as tmp:
        root=write_fixture(Path(tmp)/'fixture')
        nifti(root/'small.nii',[int(v==2) for v in range(24)])
        def run(options, error=None, filename='censor_subjects.txt'):
            nonlocal runs
            runs+=1; prefix=f'r{runs}'
            r=subprocess.run([program,'-quiet','-mask','small.nii','-prefix',prefix,
                *options,'-dataTableFile',filename],cwd=root,text=True,capture_output=True,
                env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_DECONFLICT='NO'))
            if error:
                assert r.returncode and error in r.stderr,(r.args,r.stderr)
                assert not list(root.glob(prefix+'.sel*'))
            else:assert not r.returncode,(r.args,r.stderr)
            return prefix
        for options,error in [
            (['-missing','interp'],'requires -interp_maxgap'),
            (['-interp_maxgap','2'],'requires -missing interp'),
            (['-missing','interp','-interp_maxgap','0'],'invalid -interp_maxgap'),
            (['-missing','interp','-interp_maxgap','2.5'],'invalid -interp_maxgap'),
            (['-missing','interp','-interp_maxgap','999999999999999999999'],'invalid -interp_maxgap'),
            (['-missing','interp','-interp_maxgap','2','-float16'],'requires float32'),
            (['-missing','interp','-interp_maxgap','2','-temporal_null','phase','-nnull','20'],'unsupported'),
        ]:run(options,error)
        base=['-window','12','-step','12','-run_starts','0,24',
              '-temporal_null','timeshift','-temporal_censor','joint_shift',
              '-min_shift','1','-nnull','20','-verb']
        def maps(prefix,kind):
            rows=table(root/(prefix+('.inference.tsv' if kind=='infer' else '.bricks.tsv')))
            out={}
            for row in rows:
                if kind!='infer' and row['Kind']!='result':continue
                label=row['Label']
                if label.startswith('FilledTR_'):continue
                out[row['SelectionID'],label]=image(root/row['File'])[1][int(row['Brick'])*24+2]
            return out
        for method in ['pairwise','loo']:
            a=run(['-missing','pairwise',*base,'-isc_method',method,'-save_missing',method+'_original.tsv'])
            b=run(['-missing','interp','-interp_maxgap','2',*base,'-isc_method',method,'-save_missing',method+'_interp.tsv'])
            assert table(root/(method+'_original.tsv'))==table(root/(method+'_interp.tsv'))
            assert maps(a,'infer')==maps(b,'infer'),'temporal identity and nulls must be observed-only'
            aa,bb=maps(a,'desc'),maps(b,'desc')
            assert any(aa[k]!=bb[k] for k in aa if k[1].startswith('ISC_')),'fixture must distinguish the two estimands'
            for k in aa:
                if k[1].startswith('ValidTR_'):assert aa[k]==bb[k]
        # Complete data produce the same maps and subject-resampling results.
        options=['-window','12','-step','12','-exact','-nboot','100']
        a=run(['-missing','pairwise',*options],filename='subjects.txt')
        b=run(['-missing','interp','-interp_maxgap','2',*options],filename='subjects.txt')
        assert maps(a,'desc')==maps(b,'desc')
        assert maps(a,'infer')==maps(b,'infer')
    print(f'PASS: {runs} interpolation contract executions')


if __name__=='__main__':main()
