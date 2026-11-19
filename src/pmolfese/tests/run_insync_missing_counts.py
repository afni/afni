#!/usr/bin/env python3
"""Check source-local coverage against raw samples, independent of ISC estimates.

Uses only standard-library fixtures. Exercises original-time counts, overlapping
reasons, repeated conditions, ROI means, compact storage, and threaded output.
"""
import argparse
import csv
import io
import math
import os
from pathlib import Path
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--program', required=True)
    program = str(Path(parser.parse_args().program).resolve())
    count = 0
    with tempfile.TemporaryDirectory(prefix='insync-missing-') as tmp:
        root = write_fixture(Path(tmp)/'fixture')
        # Zero-volume and explicit-censor reasons overlap for one subject.
        _, data = image(root/'s01.nii')
        data[:24] = [0.0]*24
        nifti(root/'s01.nii', data, 48)
        keep = [int(x) for x in (root/'s01.censor.1D').read_text().split()]
        keep[0] = keep[2] = 0
        (root/'s01.censor.1D').write_text('\n'.join(map(str, keep))+'\n')
        # Different conditions are different sources, not extra people.
        lines = (root/'conditions.txt').read_text().splitlines()
        lines[0] = lines[0].replace('InputFile', 'Censor InputFile')
        for i in range(1, len(lines)):
            row = lines[i].split()
            row.insert(-1, row[0]+'.censor.1D')
            lines[i] = ' '.join(row)
        (root/'conditions_censor.txt').write_text('\n'.join(lines)+'\n')

        def run(extra=(), table='censor_subjects.txt', threads=1):
            nonlocal count
            count += 1
            report = f'counts{count}.tsv'
            result = subprocess.run([program, '-quiet', '-missing', 'common',
                '-zcensor', '-verb', '-save_missing', report, '-prefix', f'out{count}.nii',
                *extra, '-dataTableFile', table], cwd=root, text=True, capture_output=True,
                env=dict(os.environ, AFNI_DECONFLICT='NO', AFNI_NOMMAP='YES', AFNI_FLOATSCAN='NO', OMP_NUM_THREADS=str(threads)))
            assert result.returncode == 0, result.stderr
            text = (root/report).read_text()
            assert text in result.stderr, 'saved and printed reports differ'
            rows = list(csv.DictReader(io.StringIO('\n'.join(x for x in text.splitlines() if not x.startswith('#'))), delimiter='\t'))
            # Fixture tables use single spaces, but parse arbitrary whitespace.
            fields = (root/table).read_text().splitlines()
            sources = [dict(zip(fields[0].split(), line.split())) for line in fields[1:]]
            _, mask = image(root/'mask.nii')
            _, atlas = image(root/'atlas.nii')
            raw = {s['InputFile']: image(root/s['InputFile'])[1] for s in sources}
            censors = {s['Censor']: [int(x) for x in (root/s['Censor']).read_text().split()] for s in sources}
            seen = set()
            for row in rows:
                t, unit = int(row['OrigTR']), int(row['Unit'])
                key = tuple(row[k] for k in ('Condition','Group','OrigTR','UnitKind','Unit'))
                assert key not in seen
                seen.add(key)
                members = [s for s in sources if s.get('Condition', 'All') == row['Condition'] and s['Group'] == row['Group']]
                assert len(members) == 3
                voxels = [unit] if row['UnitKind']=='voxel' else [v for v in range(24) if atlas[v]==unit and ('-mask' not in extra or mask[v])]
                if '-mask' in extra and row['UnitKind']=='voxel': assert mask[unit]
                missing = censored = zero = nonfinite = 0
                for subject in members:
                    values = raw[subject['InputFile']]
                    c = not censors[subject['Censor']][t]
                    if '-censor' in extra: c = c or not int((root/extra[extra.index('-censor')+1]).read_text().split()[t])
                    z = all(values[t*24+v]==0 for v in range(24) if '-mask' not in extra or mask[v])
                    b = any(not math.isfinite(values[t*24+v]) for v in voxels)
                    censored += c; zero += z; nonfinite += b; missing += c or z or b
                expected = dict(NSubj=3, NValid=3-missing, NMissing=missing, NCensor=censored, NZero=zero, NNonfinite=nonfinite)
                assert all(int(row[k])==v for k,v in expected.items()), (row, expected)
            expected_units = sum(bool(x) for x in mask) if '-mask' in extra else 24
            if '-roi_only' in extra: expected_units = len(set(atlas)-{0})
            elif '-atlas' in extra and '-window' in extra: expected_units += len(set(atlas)-{0})
            assert len(rows)==48*2*(2 if table.startswith('conditions') else 1)*expected_units
            return text

        original = run()
        assert original == run(threads=2)
        run(['-mask','mask.nii'])
        run(['-mask','mask.nii','-float16'])
        run(['-atlas','atlas.nii','-roi_only'])
        run(['-window','12','-run_starts','0,24'])
        run(['-window','12','-atlas','atlas.nii'])
        run(['-window','12','-atlas','atlas.nii','-roi_only'])
        run(['-window','12','-mask','mask.nii','-float16'])
        run(['-censor','common.censor.1D'])
        run(table='conditions_censor.txt')
        # AFNI's NIfTI reader repairs nonfinite values before the driver sees
        # them. Embedded .1D datasets preserve NaN/Inf for this separate oracle.
        rawtable = ['Subj Group Censor InputFile']
        for subject in range(1, 7):
            rows = [[float(t+v+subject) for t in range(48)] for v in range(2)]
            if subject==1:
                rows[0][1] = float('nan')
                rows[1][2] = float('inf')  # overlaps source censor at TR 2
            name = f'raw{subject}.1D'
            (root/name).write_text('# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "2,1,1"\n# >\n'+
                '\n'.join(' '.join(map(str, row)) for row in rows)+'\n# </AFNI_3D_dataset>\n')
            rawtable.append(f's{subject:02d} {"A" if subject<=3 else "B"} s{subject:02d}.censor.1D {name}')
        (root/'raw.txt').write_text('\n'.join(rawtable)+'\n')
        for selection in ([], ['-window','12']):
            name = 'rawcounts'+str(len(selection))+'.tsv'
            result = subprocess.run([program,'-quiet','-missing','common','-save_missing',name,
                '-prefix',name+'.nii',*selection,'-dataTableFile','raw.txt'],cwd=root,text=True,capture_output=True,
                env=dict(os.environ,AFNI_DECONFLICT='NO',OMP_NUM_THREADS='2'))
            assert result.returncode==0, result.stderr
            rows = list(csv.DictReader((line for line in (root/name).read_text().splitlines() if not line.startswith('#')), delimiter='\t'))
            for row in rows:
                bad = row['Group']=='A' and (int(row['Unit']),int(row['OrigTR'])) in ((0,1),(1,2))
                assert int(row['NNonfinite'])==int(bad), row
                if bad: assert int(row['NMissing'])>=1
            overlap = next(r for r in rows if r['Group']=='A' and r['OrigTR']=='2' and r['Unit']=='1')
            assert int(overlap['NMissing'])==int(overlap['NCensor']), overlap
        # Exclusive output creation preserves an existing report, even when
        # the surrounding AFNI environment allows overwriting datasets.
        before = (root/'counts1.tsv').read_bytes()
        for extra in (['-save_missing','counts1.tsv'], ['-check_table','-verb'], ['-save_missing'], ['-save_missing','./rejected.nii'], ['-window','12','-save_missing','rejected.nii.sel000001.nii.gz']):
            args = [program, '-quiet', '-prefix', 'rejected.nii', '-dataTableFile', 'subjects.txt', *extra]
            result = subprocess.run(args, cwd=root, capture_output=True, text=True, env=dict(os.environ, AFNI_DECONFLICT='OVERWRITE'))
            assert result.returncode and not (root/'rejected.nii').exists()
        assert (root/'counts1.tsv').read_bytes()==before
    print(f'PASS: {count} voxel/ROI count oracles, 2 nonfinite oracles, and 5 rejection cases')


if __name__ == '__main__':
    main()
