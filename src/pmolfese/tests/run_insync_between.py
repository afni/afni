#!/usr/bin/env python3
"""Cross-group output isolation, ROI labeling and scientific distinction.

Use run_insync_pairwise_missing.py --between-groups for independent numerical
oracles across blocks, missingness, weighting and more than two groups.
"""
import argparse, os, subprocess, tempfile
from pathlib import Path
from insync_window_fixtures import write_fixture,nifti
from run_insync_estimation import image
from run_insync_pairwise_missing import table


def main():
    ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--program',required=True)
    program=str(Path(ap.parse_args().program).resolve());runs=0
    with tempfile.TemporaryDirectory(prefix='insync-between-') as tmp:
        root=write_fixture(Path(tmp)/'fixture')
        def run(opts,filename='conditions.txt',error=None):
            nonlocal runs
            runs+=1;p=f'r{runs}'
            r=subprocess.run([program,'-quiet','-prefix',p,*opts,'-dataTableFile',filename],cwd=root,
                text=True,capture_output=True,env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_DECONFLICT='NO'))
            if error:assert r.returncode and error in r.stderr,(r.args,r.stderr)
            else:assert not r.returncode,(r.args,r.stderr)
            return p
        def maps(p,infer=False):
            out={};cache={}
            for r in table(root/(p+('.inference.tsv' if infer else '.bricks.tsv'))):
                if not infer and r['Kind']!='result':continue
                cache.setdefault(r['File'],image(root/r['File'])[1]);b=int(r['Brick'])
                out[r['SelectionID'],r['Label']]=cache[r['File']][b*24:(b+1)*24]
            return out
        for missing in (['-missing','pairwise'],['-missing','interp','-interp_maxgap','2']):
            for method in ('pairwise','loo'):
                opts=missing+['-isc_method',method,'-atlas','atlas.nii','-window','24','-step','24','-exact','-nboot','100']
                a=run(opts);b=run(opts+['-between_groups']);aa,bb=maps(a),maps(b)
                assert aa=={k:v for k,v in bb.items() if not k[1].startswith('Cross')}
                assert maps(a,True)==maps(b,True),'descriptive option must not alter inference'
                cross=[r for r in table(root/(b+'.roi.tsv')) if r['Label'].startswith('Cross')]
                assert cross
                for r in cross:assert '_'+r['Condition']+'_' in r['Label'],r
        # Internally identical groups with opposite responses: within-group
        # difference is zero while every cross-group pair is -1.
        for sub in range(6):
            sign=1 if sub<3 else -1
            nifti(root/f's{sub+1:02d}.nii',[sign*((t%7)-3) for t in range(48) for v in range(24)],48)
        p=run(['-missing','pairwise','-between_groups'],filename='subjects.txt')
        labels={label:vals[0] for (sid,label),vals in maps(p).items()}
        assert labels['ISC_A']==labels['ISC_B']==1
        assert labels['ISC_A-B']==0
        assert labels['CrossISC_All_A_B']==-1 and labels['CrossN_All_A_B']==9
        assert labels['CrossComplete_All_A_B']==1 and labels['CrossMinTR_All_A_B']==48
        (root/'one_group.txt').write_text((root/'subjects.txt').read_text().replace(' B ',' A '))
        run(['-missing','pairwise','-between_groups','-check_table'],'one_group.txt',error='at least two groups')
    print(f'PASS: {runs} between-group output isolation/distinction executions')


if __name__=='__main__':main()
