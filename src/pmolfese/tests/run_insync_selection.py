#!/usr/bin/env python3
"""M1 black-box schedule tests: original coordinates, normalization, and errors."""
import argparse
import csv
import io
import json
import os
import random
import struct
import subprocess
import tempfile
from pathlib import Path
from insync_window_fixtures import write_fixture, nifti


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--program',required=True)
    args=parser.parse_args()
    program=str(Path(args.program).resolve())
    env=dict(os.environ,AFNI_DECONFLICT='NO',AFNI_NOMMAP='YES',OMP_NUM_THREADS='1')
    with tempfile.TemporaryDirectory(prefix='insync-m1-') as directory:
        root=write_fixture(Path(directory)/'fixture')
        count=0
        def run(options=(), table='subjects.txt', error=None, check=True, prefix=None):
            nonlocal count
            count+=1; prefix=prefix or f'check{count:03d}'
            cmd=[program,'-prefix',prefix]
            if check: cmd+=['-check_table']
            cmd+=list(options)+['-dataTableFile',table]
            result=subprocess.run(cmd,cwd=root,env=env,text=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
            if error:
                assert result.returncode!=0,(cmd,result.stdout,result.stderr)
                assert error.lower() in result.stderr.lower(),(error,result.stderr)
                if not prefix.startswith('collision'):
                    assert not (root/(prefix+'.selections.tsv')).exists(),cmd
                    assert not (root/(prefix+'.selection_summary.txt')).exists(),cmd
                return result
            assert result.returncode==0,(cmd,result.stdout,result.stderr)
            text=(root/(prefix+'.selections.tsv')).read_text()
            rows=list(csv.DictReader(io.StringIO('\n'.join(l for l in text.splitlines() if not l.startswith('#'))), delimiter='\t'))
            report=(root/(prefix+'.selection_summary.txt')).read_text()
            return rows,report,result
        def table(name, rows):
            (root/name).write_text('\n'.join(rows)+'\n'); return name
        original=(root/'subjects.txt').read_text().splitlines()
        manual=(root/'intervals.txt').read_text().splitlines()
        expected=json.loads((root/'expected_schedule.json').read_text())
        rows,report,_=run(['-window','8','-step','4','-run_starts','0,24'])
        one=[r for r in rows if r['Subject']=='s01']
        assert [[int(r['OriginalStartTR']),int(r['OriginalStopTR'])] for r in one]==expected['windows_w8_s4']
        assert len(rows)==60 and len({r['SelectionID'] for r in rows})==10
        assert 'Unique subject/condition sources: 6' in report
        assert 'pending' in report
        rows2,_,_=run(['-window','8','-step','4','-run_starts','0,24','-censor','common.censor.1D'])
        censor=[int(x) for x in (root/'common.censor.1D').read_text().split()]
        for a,b in zip(rows,rows2):
            for key in ('SelectionID','StartTR','StopTR','OriginalStartTR','OriginalStopTR'): assert a[key]==b[key]
            assert int(b['CensorKeptN'])==sum(censor[int(b['OriginalStartTR']):int(b['OriginalStopTR'])+1])
        # Explicit windows reproduce the generated coordinates.
        explicit=['Subj Group Selection Segment Run StartTR StopTR InputFile']
        for sub in range(1,7):
            for i,(start,stop) in enumerate(expected['windows_w8_s4']):
                explicit.append(f's{sub:02d} {"A" if sub<=3 else "B"} w{i:02d} b{i:02d} {start//24+1} {start%24} {stop%24} s{sub:02d}.nii')
        er,_,_=run(['-run_starts','0,24'],table('explicit.txt',explicit))
        assert sorted((r['Subject'],r['OriginalStartTR'],r['OriginalStopTR']) for r in er)==sorted((r['Subject'],r['OriginalStartTR'],r['OriginalStopTR']) for r in rows)
        mr,report,_=run(['-run_starts','0,24'], 'intervals.txt')
        assert len(mr)==48 and len({r['SelectionID'] for r in mr})==5
        assert sum(r['SelectionLabel']=='Vis_all' and r['Subject']=='s01' for r in mr)==4
        shuffled=manual[1:]; random.Random(71).shuffle(shuffled)
        sr,_,_=run(['-run_starts','0,24'],table('shuffled.txt',[manual[0]]+shuffled))
        def normalized(records): return [{k:v for k,v in r.items() if k!='SourceRow'} for r in records]
        assert normalized(mr)==normalized(sr)
        # Exact-fit and short blocks retain nominal reporting, including zeros.
        rows,report,_=run(['-window','8','-run_starts','0,24'],'intervals.txt')
        assert len(rows)==48
        run(['-window','9','-run_starts','0,24'],'intervals.txt',error='no complete windows')
        short=manual.copy()
        short=[line.replace(' 2 9 ', ' 2 8 ') for line in short]  # fixture uses unpadded fields
        rows,report,_=run(['-window','8','-run_starts','0,24'],table('short.txt',short))
        assert len(rows)==24 and 'Short source segments producing zero windows: 24' in report
        assert '\t2\t8\t7\t7\t0' in report
        bad=short.copy(); bad[1]=bad[1].replace(' 2 8 ', ' 3 8 ')
        run(['-window','8','-run_starts','0,24'],table('badshort.txt',bad),error='misaligned')
        # Two 100-TR runs: eight complete 30-TR windows per run with step 10.
        for sub in range(1,7): nifti(root/f'long{sub}.nii',[1.0]*(24*200),200)
        longrows=['Subj Group InputFile']+[f's{sub:02d} {"A" if sub<=3 else "B"} long{sub}.nii' for sub in range(1,7)]
        rows,_,_=run(['-window','30','-step','10','-run_starts','0,100'],table('long.txt',longrows))
        one=[r for r in rows if r['Subject']=='s01']
        assert len(one)==16
        assert [(int(r['OriginalStartTR']),int(r['OriginalStopTR'])) for r in one]==[(a,a+29) for a in list(range(0,71,10))+list(range(100,171,10))]
        rows,_,_=run(table='conditions.txt')
        assert len(rows)==12 and {r['Condition'] for r in rows}=={'C1','C2'}
        run(table=table('incomplete.txt',(root/'conditions.txt').read_text().splitlines()[:-1]),error='missing condition')
        # Verify metadata-only operation using an input with no voxel payload.
        header=(root/'s01.nii').read_bytes()[:352]; (root/'header_only.nii').write_bytes(header)
        headerrows=['Subj Group InputFile']+[f's{sub:02d} A header_only.nii' for sub in range(1,4)]
        run(['-window','8','-zcensor','-missing','common'],table('headeronly.txt',headerrows))
        # Header mismatch checks.
        changed=bytearray((root/'s01.nii').read_bytes()); struct.pack_into('<f',changed,92,3.0)
        (root/'tr3.nii').write_bytes(changed)
        bad=original.copy(); bad[1]=bad[1].replace('s01.nii','tr3.nii')
        run(table=table('trbad.txt',bad),error='TR mismatch')
        run(['-tr','3'],error='conflicts')
        run(['-tr','2'])
        changed=bytearray((root/'s01.nii').read_bytes()); struct.pack_into('<h',changed,48,47)
        (root/'nt47.nii').write_bytes(changed)
        bad=original.copy(); bad[1]=bad[1].replace('s01.nii','nt47.nii')
        run(table=table('ntbad.txt',bad),error='TRs; expected')
        changed=bytearray((root/'s01.nii').read_bytes()); struct.pack_into('<f',changed,280,3.0)
        struct.pack_into('<f',changed,80,3.0)
        (root/'offgrid.nii').write_bytes(changed)
        bad=original.copy(); bad[1]=bad[1].replace('s01.nii','offgrid.nii')
        run(table=table('gridbad.txt',bad),error='off-grid')
        run(['-atlas','offgrid.nii'],error='off-grid')
        run(['-roi_only'],error='require -atlas')
        # Distinct human labels that sanitize to the same token still get distinct IDs.
        labels=['Subj Group Selection Segment Run StartTR StopTR InputFile']
        for sub in range(1,7):
            for label in ('a/b','a?b'):
                labels.append(f's{sub:02d} {"A" if sub<=3 else "B"} {label} seg 1 2 9 s{sub:02d}.nii')
        lr,_,_=run(table=table('labels.txt',labels))
        assert len({r['SelectionID'] for r in lr})==2
        # Quoted labels retain their table meaning in TSV. The AFNI dataset
        # opener itself does not accept literal whitespace in InputFile paths.
        (root/'space file.nii').write_bytes((root/'s01.nii').read_bytes())
        names=['Subj Group Selection Segment Run StartTR StopTR InputFile']+[f's{sub} A "visual block" "first segment" 1 0 7 s01.nii' for sub in range(1,4)]
        nr,_,_=run(table=table('spaces.txt',names))
        assert all(r['SelectionLabel']=='visual block' and r['InputFile']=='s01.nii' for r in nr)
        run(table=table('pathspaces.txt',[line.replace('s01.nii', '"space file.nii"') for line in names]),error='cannot open')
        _,_,result=run(['-run_starts','0,24','-show_table'],table='intervals.txt')
        assert 'Unique subject/condition sources: 6' in result.stdout
        # Missing TR is represented as NA; an explicit TR supplies it.
        changed=bytearray((root/'s01.nii').read_bytes()); struct.pack_into('<f',changed,92,0.0)
        (root/'notr.nii').write_bytes(changed)
        no_tr=['Subj Group InputFile']+[f's{sub} A notr.nii' for sub in range(1,4)]
        nr,_,_=run(table=table('notr.txt',no_tr))
        assert {r['TRSeconds'] for r in nr}=={'NA'}
        nr,_,_=run(['-tr','2'],table='notr.txt')
        assert {r['TRSeconds'] for r in nr}=={'2'}
        # Strict syntax, duplicate options, unsupported paths.
        for opts,message in [(['-step','2'],'requires'),(['-window','2'],'integer'),
                (['-window','8junk'],'integer'),(['-window','2147483648'],'integer'),
                (['-window','8','-window','8'],'twice'),(['-check_table'],'twice'),
                (['-tr','nan'],'positive finite'),(['-run_starts','1,24'],'start at 0'),
                (['-run_starts','0,24,24'],'increase'),(['-run_starts','0,48'],'within'),
                (['-run_starts','0,'],'integer'),(['-edges'],'deferred')]:
            run(opts,error=message)
        run(['-window','8','-edges'],check=False,error='deferred')
        run(['-run_starts','0,24','-block_weight','retained_n'],table='intervals.txt',check=False,error='M4a')
        run(table='xmat_subjects.txt')
        run(table='censor_subjects.txt')
        run(table=table('duplicate.txt',original+[original[1]]),error='duplicate Subj')
        run(['-run_starts','0,24'],table('duplicate_segment.txt',manual+[manual[1]]),error='duplicate segment')
        # It overlaps another constituent of Vis_all after adding a new row.
        overlap=manual[2].replace('b1_1','extra')
        run(['-run_starts','0,24'],table('overlap.txt',manual+[overlap]),error='overlapping')
        bad=manual.copy(); bad[1]=bad[1].replace('s01.nii','s02.nii')
        run(['-run_starts','0,24'],table('sourcebad.txt',bad),error='changes InputFile')
        bad=manual.copy(); bad[1]=bad[1].replace(' A ',' B ')
        run(['-run_starts','0,24'],table('groupbad.txt',bad),error='covariate')
        run(['-run_starts','0,24'],table('missing_segment.txt',manual[:-1]),error='missing/misaligned')
        bad=manual.copy(); bad[1]=bad[1].replace(' 2 9 ',' 9 2 ')
        run(['-run_starts','0,24'],table('reversed.txt',bad),error='invalid interval')
        bad=manual.copy(); bad[1]=bad[1].replace(' 2 9 ',' 2 24 ')
        run(['-run_starts','0,24'],table('outofrun.txt',bad),error='invalid interval')
        run(table=table('partial.txt',['Subj Selection InputFile','a x s01.nii']),error='require Selection Segment')
        run(prefix='collision')
        old=(root/'collision.selections.tsv').read_bytes()
        run(prefix='collision',error='already exists')
        assert old==(root/'collision.selections.tsv').read_bytes()
        _,_,result=run(['-quiet'])
        assert not result.stdout
        (root/'badcensor.1D').write_text('1\n0\n')
        run(['-censor','badcensor.1D'],error='48 values')
        (root/'badcensor.1D').write_text('1\n'*47+'2\n')
        run(['-censor','badcensor.1D'],error='invalid Censor')
        # All new options are terminators for an inline data table.
        inline=['Subj','Group','InputFile']
        for sub in range(1,4): inline += [f's{sub:02d}','A',f's{sub:02d}.nii']
        result=subprocess.run([program,'-prefix','inline','-dataTable',*inline,'-window','8','-check_table'],cwd=root,env=env,capture_output=True,text=True)
        assert result.returncode==0,result.stderr
        assert not list(root.glob('check*.nii*')), 'preflight wrote result images'
        print(f'PASS: {count+1} M1 schedule/preflight cases')


if __name__=='__main__': main()
