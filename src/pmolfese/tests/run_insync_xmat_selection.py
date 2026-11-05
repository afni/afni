#!/usr/bin/env python3
"""M2 metadata-only integration tests; hand-computed block/mask expectations."""
import argparse
import csv
import io
import json
import os
from pathlib import Path
import re
import struct
import subprocess
import tempfile
from insync_window_fixtures import write_fixture


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--program', required=True)
    args = parser.parse_args()
    program = str(Path(args.program).resolve())
    env = dict(os.environ, AFNI_DECONFLICT='NO', AFNI_NOMMAP='YES', OMP_NUM_THREADS='1')
    with tempfile.TemporaryDirectory(prefix='insync-m2-') as directory:
        root = write_fixture(Path(directory)/'fixture')
        expected = json.loads((root/'expected_schedule.json').read_text())
        count = 0
        blocks = ['-xmat_blocks', 'each', '-xmat_threshold', '0.5']

        def run(options=blocks, table='full.txt', error=None, check=True):
            nonlocal count
            count += 1
            prefix = f'check{count:03d}'
            cmd = [program, '-prefix', prefix] + (['-check_table'] if check else [])
            result = subprocess.run(cmd+list(options)+['-dataTableFile', table], cwd=root,
                                    env=env, text=True, capture_output=True)
            if error:
                assert result.returncode, (cmd, result.stdout, result.stderr)
                assert error.lower() in result.stderr.lower(), (error, result.stderr)
                assert not (root/(prefix+'.selections.tsv')).exists()
                return result
            assert result.returncode == 0, (cmd, result.stdout, result.stderr)
            text = (root/(prefix+'.selections.tsv')).read_text()
            rows = list(csv.DictReader(io.StringIO('\n'.join(x for x in text.splitlines()
                                      if not x.startswith('#'))), delimiter='\t'))
            assert not list(root.glob(prefix+'*.nii*'))
            return rows, (root/(prefix+'.selection_summary.txt')).read_text(), result

        def table(name, paths=None, columns='Xmat', extra=None):
            paths = paths or ['X.full.xmat.1D']*6
            lines = ['Subj Group '+columns+' InputFile']
            for i in range(6):
                lines.append(f's{i+1:02d} {"A" if i<3 else "B"} {paths[i]} '+
                             (extra[i] if extra else f's{i+1:02d}.nii'))
            (root/name).write_text('\n'.join(lines)+'\n')
            return name

        original = (root/'X.full.xmat.1D').read_text()

        def matrix(name, attrs=None, transform=None):
            text = original
            for key, value in (attrs or {}).items():
                text = re.sub(r'^# '+key+r' = ".*"\n', '' if value is None else
                              f'# {key} = "{value}"\n', text, flags=re.M)
            if transform:
                lines = text.splitlines()
                indices = [i for i, line in enumerate(lines) if not line.startswith('#')]
                values = [list(map(float, lines[i].split())) for i in indices]
                transform(values)
                for i, row in zip(indices, values):
                    lines[i] = ' '.join(map(str, row))
                text = '\n'.join(lines)+'\n'
            (root/name).write_text(text)
            return name

        def one(records, stimulus=None, role=None):
            return [r for r in records if r['Subject']=='s01' and
                    (stimulus is None or r['Stimulus']==stimulus) and
                    (role is None or r['Role']==role)]

        table('full.txt')
        rows, report, _ = run()
        assert len(rows)==48 and len({r['SelectionID'] for r in rows})==8
        for stimulus in ('Vis','Aud'):
            assert [[int(r['OriginalStartTR']),int(r['OriginalStopTR'])]
                    for r in one(rows,stimulus)]==expected['blocks'][stimulus]
        assert '1 unique designs read' in report
        assert report.count('s01\tA\t2\t2\t4\t7')==2
        assert report.count('Total\t-\t12\t12\t24\t-')==2
        assert all(r['SourceKeptN']==r['NominalN']==r['CensorKeptN'] for r in rows)
        censored, report, _ = run(table='xmat_subjects.txt')
        assert len(censored)==len(rows)
        for a,b in zip(rows,censored):
            for key in ('SelectionLabel','SegmentID','Block','Run','StartTR','StopTR'):
                assert a[key]==b[key]
            mask = [int(v) for v in (root/(b['Subject']+'.censor.1D')).read_text().split()]
            assert int(b['SourceKeptN'])==sum(mask[int(b['OriginalStartTR']):int(b['OriginalStopTR'])+1])
        assert [int(r['CensorKeptN']) for r in one(censored,'Vis')]==expected['common_vis_block_kept']
        assert '7 unique designs read' in report
        assert 's01\tAll\t48\t46\t46\t0' in report
        assert report.count('s01\tA\t2\t2\t4\t7')==2
        paths=[f's{i:02d}.X.xmat.1D' for i in range(1,7)]
        table('censored.txt',paths)
        run(table='censored.txt',error='XmatFull')
        metadata, report, _=run([],table='censored.txt')
        assert len(metadata)==12 and all(r['CensorKeptN']=='18' for r in metadata)
        windows,_,_=run(['-window','8','-step','4'],table='censored.txt')
        assert [[int(r['OriginalStartTR']),int(r['OriginalStopTR'])] for r in one(windows)]==expected['windows_w8_s4']
        # Stimulus filtering preserves its identity and support, even in reverse list order.
        reverse,_,_=run(blocks+['-xmat_stims','Aud,Vis'])
        assert reverse==rows
        aud,_,_=run(blocks+['-xmat_stims','Aud'])
        for a,b in zip([r for r in rows if r['Stimulus']=='Aud'],aud):
            assert {k:v for k,v in a.items() if k!='SelectionID'}=={k:v for k,v in b.items() if k!='SelectionID'}
        run2,report,_=run(blocks+['-xmat_runs','2'])
        assert len(run2)==24 and all(r['Run']=='2' for r in run2)
        assert 'Condition=All Run=1' not in report
        by=['-xmat_blocks','by_stim','-xmat_threshold','.5']
        aggregate,_,_=run(by)
        assert len(aggregate)==96 and len({r['SelectionID'] for r in aggregate})==12
        assert len(one(aggregate,role='block'))==8
        aggregate,_,_=run(by+['-xmat_across_runs'])
        assert len(aggregate)==96 and len({r['SelectionID'] for r in aggregate})==10
        assert len({r['SelectionID'] for r in one(aggregate,'Vis','block_aggregate_component')})==1
        windows,report,_=run(blocks+['-window','8'])
        assert len(windows)==24 and 'Short source segments producing zero windows: 24' in report
        assert all(r['Stimulus']=='Vis' for r in windows)
        run(blocks+['-window','9'],error='no complete windows')
        # Threshold is per full-design stimulus peak and inclusive at equality.
        weighted=matrix('weighted.xmat',transform=lambda v:[row.__setitem__(2,row[2]*(2 if i>=24 else 1)) for i,row in enumerate(v)])
        table('weighted.txt',[weighted]*6)
        w,_,_=run(blocks,table='weighted.txt')
        assert len(w)==48
        w,report,_=run(['-xmat_blocks','each','-xmat_threshold','.51'],table='weighted.txt')
        assert len(w)==36 and 's01\tA\t0\t2\t2\t0' in report
        # Permute columns and StimLabels for a subsequent subject; map by label.
        permuted=matrix('permuted.xmat',{'ColumnLabels':'Run#1Pol#0 ; Run#2Pol#0 ; Aud#0 ; Vis#0',
                                       'StimLabels':'Aud ; Vis'},
                        lambda v:[row.__setitem__(slice(2,4),row[2:4][::-1]) for row in v])
        table('permuted.txt',['X.full.xmat.1D',permuted]+['X.full.xmat.1D']*4)
        reordered,_,_=run(table='permuted.txt')
        assert [(r['Subject'],r['Stimulus'],r['StartTR'],r['StopTR']) for r in reordered]==[(r['Subject'],r['Stimulus'],r['StartTR'],r['StopTR']) for r in rows]
        table('first_permuted.txt',[permuted]+['X.full.xmat.1D']*5)
        reordered,report,_=run(table='first_permuted.txt')
        assert one(reordered)[0]['Stimulus']=='Aud' and 'Group\tAud\tVis' in report
        # Censor intersections never change nominal support; report extra removals.
        additional=[1]*48; additional[0]=additional[2]=0
        (root/'extra.1D').write_text('\n'.join(map(str,additional))+'\n')
        table('extra.txt',[f'{p} X.full.xmat.1D extra.1D' for p in paths],columns='Xmat XmatFull Censor')
        extra,report,_=run(table='extra.txt')
        assert one(extra,'Vis')[0]['CensorKeptN']=='1'
        assert 's01\tAll\t48\t46\t44\t2' in report
        extra,_,_=run(blocks+['-censor','extra.1D'],table='xmat_subjects.txt')
        assert one(extra,'Vis')[0]['CensorKeptN']=='1'
        quiet,report,result=run(blocks+['-quiet'])
        assert quiet==rows and 'Block counts' in report and 'Block counts' not in result.stdout
        # Conditions remain distinct, and source Xmat/Censor paths can differ.
        lines=['Subj Group Condition Xmat Censor InputFile']
        for i in range(1,7):
            for condition in ('C1','C2'):
                lines.append(f's{i:02d} {"A" if i<=3 else "B"} {condition} X.full.xmat.1D '+
                             (f's{i:02d}.censor.1D' if condition=='C1' else 'extra.1D')+f' s{i:02d}.nii')
        (root/'conditions_x.txt').write_text('\n'.join(lines)+'\n')
        c,report,_=run(table='conditions_x.txt')
        assert len(c)==96 and 'Condition=C1 Run=1' in report and 'Condition=C2 Run=2' in report
        # Missing header TR is resolved from Xmat without image sample reads.
        raw=bytearray((root/'s01.nii').read_bytes()[:352]); struct.pack_into('<f',raw,92,0.)
        (root/'header_only.nii').write_bytes(raw)
        table('header_only.txt',extra=['header_only.nii']+[f's{i:02d}.nii' for i in range(2,7)])
        h,_,_=run(table='header_only.txt')
        assert all(r['TRSeconds']=='2' for r in h)
        # Shared support across a run boundary is split at that boundary.
        boundary=matrix('boundary.xmat',transform=lambda v:[row.__setitem__(2,1.) for row in v])
        b,_,_=run(blocks+['-xmat_stims','Vis'],table=table('boundary.txt',[boundary]*6))
        assert [[int(r['OriginalStartTR']),int(r['OriginalStopTR'])] for r in one(b)]==[[0,23],[24,47]]
        b,_,_=run(blocks+['-xmat_stims','Vis','-window','8','-step','4'],table='boundary.txt')
        assert [[int(r['OriginalStartTR']),int(r['OriginalStopTR'])] for r in one(b)]==expected['windows_w8_s4']
        # Native compressed GoodList ranges are supported, without defaults.
        compressed=matrix('compressed.xmat',{'GoodList':'0..47'})
        b,_,_=run(table=table('compressed.txt',[compressed]*6))
        assert [(r['StartTR'],r['StopTR']) for r in b]==[(r['StartTR'],r['StopTR']) for r in rows]
        # Supported command combinations and parser failures.
        for options,error in [(['-xmat_blocks','each'],'threshold'),
                              (['-xmat_stims','Vis'],'require -xmat_blocks'),
                              (['-xmat_threshold','.5'],'require -xmat_blocks'),
                              (blocks+['-xmat_across_runs'],'by_stim'),
                              (by+['-window','3'],'each'),
                              (blocks+['-xmat_runs','3'],'unknown'),
                              (blocks+['-xmat_runs','1,1'],'duplicate'),
                              (blocks+['-xmat_runs','1,'],'integer'),
                              (blocks+['-xmat_stims','Vis,'],'empty'),
                              (blocks+['-xmat_stims','Vis,Vis'],'duplicate'),
                              (blocks+['-xmat_stims','visual'],'missing stimulus'),
                              (blocks+['-run_starts','0'],'conflicts'),
                              (blocks+['-tr','3'],'conflicts'),
                              (['-xmat_blocks','each','-xmat_threshold','nan'],'threshold'),
                              (['-xmat_blocks','each','-xmat_threshold','0'],'threshold'),
                              (['-xmat_blocks','each','-xmat_threshold','1.01'],'threshold'),
                              (blocks+['-xmat_threshold','.6'],'twice')]:
            run(options,error=error)
        run(blocks,table='subjects.txt',error='Xmat column')
        run(blocks,table='intervals.txt',error='Xmat column')
        run(blocks+['-edges'],check=False,error='deferred')
        table('selector.txt',extra=['s01.nii[0..47]']+[f's{i:02d}.nii' for i in range(2,7)])
        run(table='selector.txt',error='unselected')
        # Malformed metadata / unsupported response bases.
        for name,attrs,transform,error in [
            ('nfull',{'NRowFull':'49'},None,'NRowFull'),
            ('nfulltext',{'NRowFull':'48junk'},None,'NRowFull'),
            ('runs',{'RunStart':'0,23'},None,'RunStart mismatch'),
            ('tr',{'RowTR':'3'},None,'RowTR'),
            ('badtr',{'RowTR':'2junk'},None,'RowTR'),
            ('nogood',{'GoodList':None},None,'GoodList'),
            ('nolabel',{'ColumnLabels':None},None,'ColumnLabels'),
            ('shortlabels',{'ColumnLabels':'A ; B'},None,'ColumnLabels'),
            ('badgood',{'GoodList':'0junk,1..47'},None,'GoodList'),
            ('badrun',{'RunStart':'0,24junk'},None,'RunStart'),
            ('badbot',{'StimBots':'2junk,3'},None,'StimBots'),
            ('badnstim',{'Nstim':'3'},None,'Nstim'),
            ('missing',{'StimLabels':'Vis ; Other'},None,'missing stimulus'),
            ('duplicate',{'StimLabels':'Vis ; Vis'},None,'duplicate'),
            ('zero',{},lambda v:[row.__setitem__(2,0.) for row in v],'zero peak'),
            ('signed',{},lambda v:v[2].__setitem__(2,-1.),'nonnegative'),
            ('nan',{},lambda v:v[2].__setitem__(2,float('nan')),'finite'),
            ('basis',{'StimBots':'1,3'},None,'multiple columns'),
        ]:
            path=matrix(name+'.xmat',attrs,transform)
            run(table=table(name+'.txt',['X.full.xmat.1D',path]+['X.full.xmat.1D']*4),error=error)
        # Same counts, shifted support: print counts before rejecting alignment.
        shifted=matrix('shifted.xmat',transform=lambda v:(v[2].__setitem__(2,0),v[10].__setitem__(2,1)))
        result=run(table=table('shifted.txt',['X.full.xmat.1D',shifted]+['X.full.xmat.1D']*4),error='misaligned')
        assert 'Block counts' in result.stderr and 's02\tA\t2\t2' in result.stderr
        merged=matrix('merged.xmat',transform=lambda v:[v[i].__setitem__(2,1) for i in range(10,14)])
        result=run(table=table('merged.txt',['X.full.xmat.1D',merged]+['X.full.xmat.1D']*4),error='misaligned')
        assert 's02\tA\t1\t2' in result.stderr
        # A mismatching full reference is rejected even at retained rows.
        changed=matrix('changed.xmat',transform=lambda v:v[2].__setitem__(2,.9))
        run(table=table('badfull.txt',[f'{p} {changed}' for p in paths],columns='Xmat XmatFull'),error='retained value mismatch')
        run(table=table('notfull.txt',[f'{p} {p}' for p in paths],columns='Xmat XmatFull'),error='full timeline')
        (root/'invalid.1D').write_text('1\n'*47+'0.5\n')
        run(table=table('badmask.txt',['X.full.xmat.1D invalid.1D']*6,columns='Xmat Censor'),error='invalid Censor')
        print(f'M2 Xmat selection checks passed: {count} cases')


if __name__=='__main__':
    main()
