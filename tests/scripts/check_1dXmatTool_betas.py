#!/usr/bin/env python3
# Developed at the NIH. Part of AFNI; see LICENSE.txt for licensing terms.
"""Exercise real fixed-design Picard betas, explicit annotations and both readers.

Candidate selection is also exercised by 1dXmatMaker's workflow gate. These
checks concentrate on mapping errors that could silently attach the wrong
condition/run to a fitted volume. All data and outputs are temporary.
"""
import argparse
import csv
import os
from pathlib import Path
import subprocess
import tempfile

import nibabel as nib
import numpy as np


def read_table(path):
    """Read the AFNI token convention, including doubled quotes, for assertions."""
    lines=path.read_text().splitlines()
    header=next(line[11:].split() for line in lines if line.startswith('# Columns: '))
    rows=csv.reader([line for line in lines if line and not line.startswith('#')],
                    delimiter=' ',skipinitialspace=True)
    return [dict(zip(header,row,strict=True)) for row in rows]


def write_tsv(path,rows):
    """Keep Picard's quoted TSV convention when constructing negative cases."""
    with path.open('w',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=list(rows[0]),delimiter='\t',lineterminator='\n')
        writer.writeheader();writer.writerows(rows)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ['tool','maker','picard','mvpa','rsa']:
        parser.add_argument('--'+name,required=True,type=Path)
    args=parser.parse_args()
    exe={name:str(getattr(args,name).resolve()) for name in ['tool','maker','picard','mvpa','rsa']}
    env={k:v for k,v in os.environ.items() if not k.startswith('AFNI_')}
    env.update(OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',AFNI_VERSION_CHECK='NO',AFNI_PATH_SPACES_OK='NO')
    with tempfile.TemporaryDirectory(prefix='xmat-beta-export-') as temp:
        work=Path(temp)
        def run(program,options,success=True,contains=None):
            result=subprocess.run([exe[program],*map(str,options)],cwd=work,env=env,capture_output=True,text=True)
            assert (result.returncode==0)==success,(program,options,result.stdout,result.stderr)
            if contains:assert contains in result.stdout+result.stderr,(contains,result.stdout,result.stderr)
            return result
        def export(prefix,kind='manifest',extra=(),manifest='fits/fixed.betas.tsv',success=True,contains=None):
            return run('tool',['-beta_manifest',manifest,'-export_manifest','-subject','subject "one"',
                       '-input_base',work,'-manifest_format',kind,'-prefix',prefix,*extra],success,contains)
        (work/'times.1D').write_text(('10 40 70 100 130 160\n')*3)
        run('maker',['-run_lengths','120,120,120','-TR','2','-polort','1','-num_stimts','1','-local_times',
                     '-stim_times_IM','1','times.1D','GAM','-stim_label','1','trial','-x1D','X.xmat.1D'])
        x=np.loadtxt(work/'X.xmat.1D');rng=np.random.default_rng(731)
        y=x@rng.normal(size=(x.shape[1],8))+rng.normal(scale=.1,size=(360,8))
        for r in range(3):
            data=y[r*120:(r+1)*120].T.reshape((2,2,2,120))
            img=nib.Nifti1Image(data.astype('float32'),np.eye(4));img.header.set_zooms((1,1,1,2))
            nib.save(img,work/f'run{r+1}.nii.gz')
        nib.save(nib.Nifti1Image(np.ones((2,2,2),dtype='uint8'),np.eye(4)),work/'mask.nii.gz')
        (work/'fits').mkdir()
        run('picard',['-input','run1.nii.gz run2.nii.gz run3.nii.gz','-matrix','X.xmat.1D',
                      '-fraction','.6','-save_nuisance','-write_manifest','-prefix','fits/fixed.nii.gz'])
        with (work/'fits/fixed.betas.tsv').open() as f:original=list(csv.DictReader(f,delimiter='\t'))
        assert len(original)==18
        assert [int(row['Volume']) for row in original]==list(range(18))
        assert [int(row['XColumn']) for row in original]==list(range(6,24))
        annotations=[dict(Volume=i,Condition=f'C{i%6}',Run=i//6+1,Trial=500+i,Item='item "quoted" with spaces') for i in range(18)]
        write_tsv(work/'events.1D',annotations)
        extra=['-events','events.1D']
        export('missing',success=False,contains='missing Condition')
        export('mvpa',extra=extra)
        rows=read_table(work/'mvpa.trials.1D')
        assert len(rows)==18
        assert [int(r['Volume']) for r in rows]==list(range(18))
        assert [int(r['XColumn']) for r in rows]==list(range(6,24))
        assert [int(r['Run']) for r in rows]==[i//6+1 for i in range(18)]
        assert {r['SourceRun'] for r in rows}=={'-1'}
        assert all(r['Subj']=='subject "one"' and r['Item']=='item "quoted" with spaces' for r in rows)
        assert all(Path(r['InputFile']).is_absolute() and Path(r['Xmat']).is_absolute() for r in rows)
        mvpa=['-mode','classify','-target_column','Condition','-cv_column','Run','-mask','mask.nii.gz']
        run('mvpa',mvpa+['-trial_manifest','mvpa.trials.1D','-validate_only'])
        run('mvpa',mvpa+['-trial_manifest','mvpa.trials.1D','-method','ridge','-prefix','decoded'])
        export('selected','selected',extra)
        selected=read_table(work/'selected.trials.1D')
        assert all('Xmat' not in r and 'XColumn' not in r for r in selected)
        assert [r['SourceColumn'] for r in selected]==[str(i) for i in range(6,24)]
        run('mvpa',mvpa+['-dataTableFile','selected.trials.1D','-validate_only'])
        export('classic_repeats','rsa',extra,success=False,contains='one beta per condition')
        export('classic','rsa',extra+['-runs','1'])
        classic=read_table(work/'classic.rsa.1D')
        assert len(classic)==6 and list(classic[0])==['Subj','Condition','InputFile']
        assert [r['Trial'] for r in read_table(work/'classic.identities.1D')]==[str(i) for i in range(500,506)]
        model=np.abs(np.arange(6)[:,None]-np.arange(6)[None,:]);np.savetxt(work/'model.1D',model)
        rsa=['-mode','RSA','-mask','mask.nii.gz','-model_mat','ordinal','model.1D',
             '-classic_null','conditions','-nperm','24','-seed','731']
        run('rsa',rsa+['-dataTableFile','classic.rsa.1D','-condition_column','Condition',
                       '-condition_order','C0,C1,C2,C3,C4,C5','-prefix','classic_fit'])
        export('cross','rsa_runwise',extra)
        cross=read_table(work/'cross.runwise.1D')
        assert [r['Run'] for r in cross]==['1','2','3']
        for r,row in enumerate(cross):
            assert row['InputFile'].endswith('['+','.join(str(i) for i in range(r*6,(r+1)*6))+']')
            trial=read_table(Path(row['TrialFile']))
            assert [t['Trial'] for t in trial]==[str(i) for i in range(500+r*6,506+r*6)]
        run('rsa',rsa+['-runwiseTable','cross.runwise.1D','-prefix','cross_fit'])
        # Repeated trial conditions are valid for runwise RSA; the exporter keeps
        # every beta and lets RSA perform its documented within-run averaging.
        (work/'repeat_times.1D').write_text((' '.join(str(10*i) for i in range(1,13))+'\n')*3)
        run('maker',['-run_lengths','120,120,120','-TR','2','-polort','1','-num_stimts','1','-local_times',
                     '-stim_times_IM','1','repeat_times.1D','GAM','-stim_label','1','trial','-x1D','XR.xmat.1D'])
        run('picard',['-input','run1.nii.gz run2.nii.gz run3.nii.gz','-matrix','XR.xmat.1D',
                      '-fraction','.6','-write_manifest','-prefix','fits/repeated.nii.gz'])
        repeated=[dict(Volume=i,Condition=f'C{i%6}',Run=i//12+1,Trial=1000+i) for i in range(36)]
        write_tsv(work/'repeated.1D',repeated)
        export('repeated','rsa_runwise',['-events','repeated.1D'],manifest='fits/repeated.betas.tsv')
        assert len(read_table(work/'repeated.r01.trials.1D'))==12
        run('rsa',rsa+['-runwiseTable','repeated.runwise.1D','-prefix','repeated_fit'])
        export('one_run','rsa_runwise',extra+['-runs','1'],success=False,contains='at least two')
        export('absent_run',extra=extra+['-runs','4'],success=False)
        export('repeat_run',extra=extra+['-runs','1,1'],success=False)
        export('bad_mode',extra=extra+['-infile','X.xmat.1D'],success=False)
        # Reimporting an existing 1dXmatTool manifest has an explicit ONE-based
        # run convention. This is also the convention of the older LSS exporter.
        export('reimport',manifest='mvpa.trials.1D',extra=['-manifest_run_base','1'])
        assert [r['Run'] for r in read_table(work/'reimport.trials.1D')]==[r['Run'] for r in rows]
        # Every failed mapping is checked before publication, not just rows kept
        # by a requested run selection. No invalid annotation may silently shift IDs.
        malformed=[]
        bad=[dict(r) for r in original];bad[0]['Volume']='999';malformed.append(bad)
        bad=[dict(r) for r in original];bad[0]['Volume']='0junk';malformed.append(bad)
        bad=[dict(r) for r in original];bad[1]['Volume']='0';malformed.append(bad)
        bad=[dict(r) for r in original];bad[0]['XColumn']='999';malformed.append(bad)
        bad=[dict(r) for r in original];bad[0]['InputFile']='missing.nii';malformed.append(bad)
        bad=[dict(r) for r in original];bad[0]['InputFile']+='[0]';malformed.append(bad)
        bad=[dict(r,Run='0') for r in original];malformed.append(bad) # contradicts events on later runs
        for i,bad in enumerate(malformed):
            write_tsv(work/'bad.tsv',bad)
            export(f'bad{i}',extra=extra+['-runs','1'],manifest='bad.tsv',success=False)
            assert not (work/f'bad{i}.trials.1D').exists()
        for i,bad in enumerate([annotations[:-1],annotations+[annotations[0]],
                                [dict(r,Trial=500) for r in annotations]]):
            write_tsv(work/'bad_events.tsv',bad)
            export(f'event_bad{i}',extra=['-events','bad_events.tsv'],success=False)
        # Preserve pre-existing companions and never leave a primary table after
        # a preflight collision. A symlink is also an existing destination.
        (work/'collision.identities.1D').write_text('preserve')
        export('collision','rsa',extra+['-runs','1'],success=False)
        assert (work/'collision.identities.1D').read_text()=='preserve'
        assert not (work/'collision.rsa.1D').exists()
        (work/'symlink.trials.1D').symlink_to(work/'missing_target')
        export('symlink',extra=extra,success=False)
        assert (work/'symlink.trials.1D').is_symlink()
        assert not list(work.rglob('*.tmp*'))
        print('PASS fixed Picard -> 1dXmatTool -> MVPA/classic RSA/runwise RSA; mappings, quoting, run bases, rejection and no-overwrite checks')

if __name__=='__main__':
    main()
