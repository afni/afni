#!/usr/bin/env python3
"""Whole-run source censor masks must reproduce a fixed common censor run."""
import argparse
import itertools
import os
from pathlib import Path
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True)
    args=p.parse_args(); program=str(Path(args.program).resolve()); count=0
    env=dict(os.environ,AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO',OMP_NUM_THREADS='2')
    with tempfile.TemporaryDirectory(prefix='insync-censor-') as temp:
        root=write_fixture(Path(temp)/'fixture')
        def run(opts=(),table='censor_subjects.txt',error=None):
            nonlocal count
            count+=1; prefix=f'c{count:03d}.nii'
            opts=[x.replace('@',prefix) for x in opts]
            result=subprocess.run([program,'-quiet','-seed','123','-prefix',prefix]+opts+['-dataTableFile',table],cwd=root,env=env,text=True,capture_output=True)
            if error:
                assert result.returncode and error.lower() in result.stderr.lower(),(error,result.stderr)
                assert not (root/prefix).exists()
                return
            assert result.returncode==0,(opts,table,result.stderr)
            assert not list(root.glob(prefix+'.sel*'))
            return prefix
        def equal(a,b):
            sa,va=image(root/a); sb,vb=image(root/b); assert sa==sb
            assert max(abs(x-y) for x,y in zip(va,vb))<=1e-6
        def table(name,lines): (root/name).write_text('\n'.join(lines)+'\n'); return name
        cases=[]
        for method,metric,summary in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean')):
            cases.append(['-isc_method',method,'-correlation',metric,'-summary',summary])
        cases += [['-nboot','100','-nperm','20','-exact'],['-save_pairwise','@.pairs.nii'],
                  ['-atlas','atlas.nii'],['-atlas','atlas.nii','-roi_only'],
                  ['-mask','mask.nii'],['-mask','mask.nii','-float16'],
                  ['-atlas','atlas.nii','-edges','-edges_rss']]
        for options in cases:
            a=run(options); b=run(options+['-censor','common.censor.1D'],'subjects.txt'); equal(a,b)
            assert (root/(a+'.common_censor.1D')).read_text()==(root/'common.censor.1D').read_text()
            assert '46\t48\t' in (root/(a+'.censor.tsv')).read_text()
            if '-save_pairwise' in options:
                equal(a+'.pairs.nii',b+'.pairs.nii')
                ta=(root/(a+'.pairs.nii.3dISC.txt')).read_text(); tb=(root/(b+'.pairs.nii.3dISC.txt')).read_text()
                assert 'Censor' not in ta.splitlines()[0] and 'Age1 Age2' in ta.splitlines()[0]
                assert ta.replace(a,'OUTPUT')==tb.replace(b,'OUTPUT')
            for suffix in ('.roi.1D','.edge.1D','.frames.1D'):
                if (root/(a+suffix)).exists(): assert (root/(a+suffix)).read_text()==(root/(b+suffix)).read_text()
        # Repeated conditions may have different censor files; the common
        # mask spans every source and remains fixed for condition resampling.
        base=(root/'conditions.txt').read_text().splitlines(); lines=[base[0].replace('InputFile','Censor InputFile')]
        for line in base[1:]:
            parts=line.split(); lines.append(' '.join(parts[:-1]+[parts[0]+'.censor.1D',parts[-1]]))
        table('conditions_censor.txt',lines)
        opts=['-condition_contrast','C1','C2','-condition_exact','-save_pairwise','@.pairs.nii']
        a=run(opts,'conditions_censor.txt'); b=run(opts+['-censor','common.censor.1D'],'conditions.txt'); equal(a,b); equal(a+'.pairs.nii',b+'.pairs.nii')
        extra=[1]*48; extra[0]=0; table('extra.1D',list(map(str,extra)))
        common=[int(v) for v in (root/'common.censor.1D').read_text().split()]; common[0]=0
        table('combined.1D',list(map(str,common)))
        changed=lines[:]; changed[2]=changed[2].replace('s01.censor.1D','extra.1D'); table('conditions_diff.txt',changed)
        # The other C1 source still excludes s01's original two samples.
        a=run([], 'conditions_diff.txt'); b=run(['-censor','combined.1D'],'conditions.txt'); equal(a,b)
        a=run(['-censor','extra.1D']); b=run(['-censor','combined.1D'],'subjects.txt'); equal(a,b)
        # Remapped Censor columns are source metadata, not model predictors.
        text=(root/'censor_subjects.txt').read_text().replace('Censor','KeepFile'); (root/'custom.txt').write_text(text)
        a=run(['-censor_column','KeepFile','-save_pairwise','@.pairs.nii'],'custom.txt')
        assert 'KeepFile' not in (root/(a+'.pairs.nii.3dISC.txt')).read_text().splitlines()[0]
        # zcensor is applied on original volumes before the final saved mask.
        for i in range(1,7):
            _,values=image(root/f's{i:02d}.nii')
            if i==1: values[:24]=[0.]*24
            nifti(root/f'z{i}.nii',values,48)
        raw=(root/'subjects.txt').read_text(); cens=(root/'censor_subjects.txt').read_text()
        for i in range(1,7): raw=raw.replace(f's{i:02d}.nii',f'z{i}.nii'); cens=cens.replace(f's{i:02d}.nii',f'z{i}.nii')
        (root/'z.txt').write_text(raw); (root/'zc.txt').write_text(cens)
        a=run(['-zcensor'],'zc.txt'); b=run(['-zcensor','-censor','common.censor.1D'],'z.txt'); equal(a,b)
        assert (root/(a+'.common_censor.1D')).read_text().split()[0]=='0'
        # All-one and identical masks.
        table('one.1D',['1']*48)
        raw=(root/'censor_subjects.txt').read_text()
        for mask in ('one.1D','common.censor.1D'):
            text=raw
            for i in range(1,7): text=text.replace(f's{i:02d}.censor.1D',mask)
            (root/'same.txt').write_text(text)
            a=run([], 'same.txt'); b=run(['-censor',mask],'subjects.txt'); equal(a,b)
        for bad,values,error in [('short',['1']*47,'48 values'),('fraction',['1']*47+['.5'],'invalid Censor'),('empty',['0']*48,'at least 3')]:
            table(bad+'.1D',values)
            (root/'bad.txt').write_text(raw.replace('s01.censor.1D',bad+'.1D'))
            run(table='bad.txt',error=error)
        run(['-temporal_null','timeshift','-nnull','20'],error='cannot be combined')
        run(['-temporal_null','phase','-nnull','20'],error='cannot be combined')
        run(['-atlas','atlas.nii','-edges','-edges_nnull','20'],error='cannot be combined')
        run(['-censor_column','Absent'],error='absent')
        run(['-censor_column','Subj'],error='separate')
        print(f'PASS: {count} whole-run subject censor cases')


if __name__=='__main__': main()
