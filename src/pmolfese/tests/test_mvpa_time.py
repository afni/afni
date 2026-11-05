#!/usr/bin/env python3
"""Temporal classification vs independent latency fits, real ASCII GIFTI I/O.
Stdlib only; optional CreateIcosahedron argument enables surface searchlights.
"""
import csv
import os
from pathlib import Path
import random
import struct
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
mesh_tool = str(Path(sys.argv[2]).resolve()) if len(sys.argv) > 2 else None
converter = str(Path(sys.argv[3]).resolve()) if len(sys.argv) > 3 else None
env = dict(os.environ, AFNI_DECONFLICT='NO', AFNI_NIFTI_TYPE_WARN='NO', OMP_NUM_THREADS='2')


def run(args, error=None):
    p = subprocess.run([binary, *map(str, args)], env=env, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if error:
        assert p.returncode and error in p.stdout, p.stdout
    else:
        assert p.returncode == 0, p.stdout
    return p.stdout


def gifti(path, columns, nodes=(0, 2, 5, 9)):
    arrays = []
    for values, intent, dtype in ([(nodes, 'NIFTI_INTENT_NODE_INDEX', 'NIFTI_TYPE_INT32')] if nodes is not None else []) + [
            (v, 'NIFTI_INTENT_TIME_SERIES', 'NIFTI_TYPE_FLOAT32') for v in columns]:
        arrays.append(f'<DataArray Intent="{intent}" DataType="{dtype}" '
                      f'ArrayIndexingOrder="RowMajorOrder" Dimensionality="1" Dim0="{len(values)}" '
                      'Encoding="ASCII" Endian="LittleEndian" ExternalFileName="" ExternalFileOffset="0">'
                      '<MetaData/><Data>' + ' '.join(map(str, values)) + '</Data></DataArray>')
    path.write_text('<?xml version="1.0"?><GIFTI Version="1.0" NumberOfDataArrays="' +
                    str(len(arrays)) + '"><MetaData/>' + ''.join(arrays) + '</GIFTI>')


def nifti(path, columns):
    h = bytearray(352)
    struct.pack_into('<i', h, 0, 348)
    struct.pack_into('<8h', h, 40, 4, len(columns[0]), 1, 1, len(columns), 1, 1, 1)
    struct.pack_into('<2h', h, 70, 16, 32)
    struct.pack_into('<8f', h, 76, *([1.] * 8))
    struct.pack_into('<f', h, 108, 352)
    struct.pack_into('<h', h, 254, 1)
    for i in range(3):
        row = [0.] * 4; row[i] = 1
        struct.pack_into('<4f', h, 280 + 16*i, *row)
    h[344:348] = b'n+1\0'
    vals = [x for col in columns for x in col]
    path.write_bytes(h + struct.pack('<%df' % len(vals), *vals))


def read(path):
    lines = path.read_text().splitlines()
    header = next(s[11:] for s in lines if s.startswith('# Columns: '))
    rows = list(csv.DictReader([header] + [s for s in lines if s and not s.startswith('#')], delimiter='\t'))
    assert all(None not in row and all(v is not None for v in row.values()) for row in rows), path
    return rows


def compare(temporal, single, t):
    selected = [{k:v for k,v in row.items() if k not in ('TimeIndex', 'TimeSeconds')}
                for row in temporal if int(row['TimeIndex']) == t]
    assert selected == single, (t, selected[:2], single[:2])


help_text=run(['-help'])
assert "'1D: 601%-0.100%0.001'" in help_text
assert "'1D: 101%-0.100%0.001'" in help_text

with tempfile.TemporaryDirectory(prefix='mvpa_time_') as directory:
    w = Path(directory)
    times = w/'times.1D'; times.write_text('# Seconds relative to event\n-0.1\n0\n0.075\n')
    rng = random.Random(83)
    # Three runs, both classes, repeated trials. Flat baseline, strong signal,
    # then unrelated noise: tests that time slices actually change predictions.
    values = []
    for i in range(18):
        y = i % 2
        values.append([[1., 1., 1., 1.],
                       [5*y + rng.random(), 4*y + rng.random(), rng.random(), rng.random()],
                       [rng.random() for _ in range(4)]])
    mesh = w/'mesh.gii'
    if mesh_tool:
        subprocess.run([mesh_tool, '-ld', '1', '-rad', '10', '-prefix', str(mesh)],
                       check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for fmt in ('volume', 'surface'):
        writer = nifti if fmt == 'volume' else gifti
        ext = '.nii' if fmt == 'volume' else '.func.gii'
        mask = w/('mask'+ext); writer(mask, [[1,1,1,1]])
        files = [w/(f'{fmt}_trial{i}'+ext) for i in range(18)]
        for file, data in zip(files, values): writer(file, data)
        def table(path, t=None):
            path.write_text('Subj Run Sample Condition InputFile\n' + ''.join(
                f's01 r{i//6} trial{i} {"AB"[i%2]} {file}' + (f'[{t}]' if t is not None else '') + '\n'
                for i,file in enumerate(files)))
        tab = w/(fmt+'.1D'); table(tab)
        modes = ['roi', 'search'] if fmt == 'volume' or mesh_tool else ['roi']
        for spatial in modes:
            base = ['-mode','classify','-target_column','Condition','-cv_column','Run', '-mask',mask,'-quiet']
            if spatial == 'search':
                base += ['-searchlight','100']
                if fmt == 'surface': base += ['-surf',mesh]
            methods = ['centroid','ridge','svm','logistic','lda_diag'] if spatial == 'roi' else ['ridge']
            for method in methods:
                args = base + ['-method',method]
                # Nested preprocessing tests reset state at each latency.
                if method == 'ridge': args += ['-lambda_grid','.1,1','-select_features_grid','2,4','-pca_components_grid','1,2']
                root = w/f'{fmt}_{spatial}_{method}'
                out = Path(str(root)+('.nii' if fmt == 'volume' else '.niml.dset'))
                audits = ['-save_predictions',str(root)+'.pred', '-save_fold_scores',str(root)+'.fold',
                          '-save_confusion',str(root)+'.conf', '-save_summary_scores',str(root)+'.summary']
                if method == 'logistic': audits += ['-save_probability_scores',str(root)+'.prob','-save_calibration',str(root)+'.cal']
                if method == 'ridge': audits += ['-save_pca_loadings',str(root)+'.load','-save_tuning',str(root)+'.candidates']
                run(args+['-dataTableFile',tab,'-time_resolved','-time_axis',times,'-prefix',out]+audits)
                scores = read(Path(str(out)+'.temporal.1D'))
                # Bare temporal mode processes exactly the same columns, but
                # no audit may invent physical seconds from image metadata.
                index_out=str(root)+'_index'+('.nii' if fmt=='volume' else '.niml.dset')
                index_audits=sum(([audits[j],str(root)+f'.index{j}'] for j in range(0,len(audits),2)),[])
                run(args+['-dataTableFile',tab,'-time_resolved','-prefix',index_out]+index_audits)
                def without_seconds(rows):
                    return [{k:v for k,v in row.items() if k!='TimeSeconds'} for row in rows]
                index_scores=read(Path(index_out+'.temporal.1D'))
                assert all(row['TimeSeconds']=='NA' for row in index_scores)
                assert without_seconds(index_scores)==without_seconds(scores)
                for j in range(0,len(audits),2):
                    a=read(Path(index_audits[j+1]));b=read(Path(audits[j+1]))
                    assert all(row['TimeSeconds']=='NA' for row in a)
                    assert without_seconds(a)==without_seconds(b)
                if method=='ridge':
                    for suffix in ('.tuning.1D','.features.1D','.feature_tuning.1D','.pca.1D','.pca_tuning.1D'):
                        a=read(Path(index_out+suffix));b=read(Path(str(out)+suffix))
                        assert all(row['TimeSeconds']=='NA' for row in a)
                        assert without_seconds(a)==without_seconds(b)

                if spatial == 'search':
                    serial = str(root)+'_serial'+('.nii' if fmt=='volume' else '.niml.dset')
                    serial_audits = sum(([audits[j],str(root)+f'.serial{j}'] for j in range(0,len(audits),2)),[])
                    env['OMP_NUM_THREADS']='1'
                    run(args+['-dataTableFile',tab,'-time_resolved','-time_axis',times,'-prefix',serial]+serial_audits)
                    env['OMP_NUM_THREADS']='2'
                    assert scores == read(Path(serial+'.temporal.1D'))
                    for j in range(0,len(audits),2):
                        assert read(Path(audits[j+1])) == read(Path(serial_audits[j+1]))

                if fmt == 'volume':
                    raw = out.read_bytes()
                    if raw.startswith(b'# <AFNI_3D_dataset'):
                        rows = [[float(v) for v in line.split()] for line in raw.decode().splitlines()
                                if line and not line.startswith('#')]
                        assert len(rows)==4 and all(len(r)==3 for r in rows)
                        maps = [row[t] for t in range(3) for row in rows]
                    else:
                        dims = struct.unpack_from('<8h',raw,40)
                        nv = 1
                        for d in dims[4:dims[0]+1]: nv *= d
                        offset = int(struct.unpack_from('<f',raw,108)[0])
                        assert nv == 3
                        maps = struct.unpack_from('<12f',raw,offset)
                elif converter:
                    text = subprocess.check_output([converter,'-o_1Dp_stdout','-input',str(out),'-prepend_node_index_1D'],text=True)
                    rows = [[float(v) for v in line.split()] for line in text.splitlines() if line and not line.startswith('#')]
                    assert [int(r[0]) for r in rows] == [0,2,5,9]
                    assert all(len(r)==4 for r in rows)
                    maps = [row[t+1] for t in range(3) for row in rows]
                else:
                    maps = None
                if maps is not None:
                    for row in scores:
                        t=int(row['TimeIndex'])
                        vals=maps[t*4:t*4+4]
                        assert all(abs(v-float(row['SCORE']))<1e-6 for v in vals), (vals,row)
                assert {int(r['TimeIndex']) for r in scores} == {0,1,2}
                assert all(float(r['SCORE']) == .5 for r in scores if r['TimeIndex'] == '0')
                assert all(float(r['SCORE']) == 1 for r in scores if r['TimeIndex'] == '1')
                assert {float(r['TimeSeconds']) for r in scores} == {-.1,0,.075}
                for t in range(3):
                    one = w/'one.1D'; table(one,t)
                    single = str(root)+f'_single{t}'
                    single_out = single+('.nii' if fmt == 'volume' else '.niml.dset')
                    single_audits = sum(([audits[j],single+f'.audit{j}'] for j in range(0,len(audits),2)),[])
                    run(args+['-dataTableFile',one,'-prefix',single_out]+single_audits)
                    for j in range(0,len(audits),2):
                        compare(read(Path(audits[j+1])),read(Path(single_audits[j+1])),t)
                    if method == 'ridge':
                        for suffix in ('.tuning.1D','.features.1D','.feature_tuning.1D','.pca.1D','.pca_tuning.1D'):
                            compare(read(Path(str(out)+suffix)),read(Path(single_out+suffix)),t)
                print('PASS',fmt,spatial,method,flush=True)
        # Fixed partitions and independent tables keep trial identity/folds
        # across time, and must match their ordinary single-latency analyses.
        for split in ('partition','two_tables'):
            def split_tables(t=None):
                table(tab,t)
                lines=tab.read_text().splitlines()
                train=w/(fmt+'_train.1D');test=w/(fmt+'_test.1D')
                train.write_text('\n'.join(lines[:13])+'\n')
                test.write_text('\n'.join([lines[0]]+lines[13:])+'\n')
                if split=='two_tables': return ['-dataTableFile',train,'-testTableFile',test]
                part=w/(fmt+'_part.1D')
                part.write_text(lines[0]+' Split\n'+'\n'.join(line+(' train' if i<12 else ' test')
                                for i,line in enumerate(lines[1:]))+'\n')
                return ['-dataTableFile',part,'-partition_column','Split']
            root=w/(fmt+'_'+split)
            out=str(root)+('.nii' if fmt=='volume' else '.niml.dset')
            fixed=['-mode','classify','-method','ridge','-target_column','Condition','-mask',mask,'-quiet']
            run(fixed+split_tables()+['-time_resolved','-time_axis',times,'-prefix',out,'-save_predictions',str(root)+'.pred'])
            for t in range(3):
                one=str(root)+f'_single{t}'
                run(fixed+split_tables(t)+['-prefix',one,'-save_predictions',one+'.pred'])
                compare(read(Path(str(root)+'.pred')),read(Path(one+'.pred')),t)
            print('PASS',fmt,split,flush=True)
        table(tab)
        if fmt == 'surface':
            dense_mask=w/'dense_mask.func.gii';gifti(dense_mask,[[1,1,1,1]],nodes=None)
            dense_files=[w/f'dense_trial{i}.func.gii' for i in range(18)]
            for file,data in zip(dense_files,values):gifti(file,data,nodes=None)
            dense_tab=w/'dense.1D'
            dense_tab.write_text('Subj Run Sample Condition InputFile\n'+''.join(
                f's01 r{i//6} trial{i} {"AB"[i%2]} {file}\n' for i,file in enumerate(dense_files)))
            dense_out=w/'dense.niml.dset'
            run(['-mode','classify','-method','centroid','-target_column','Condition','-cv_column','Run',
                 '-mask',dense_mask,'-dataTableFile',dense_tab,'-time_resolved','-time_axis',times,'-prefix',dense_out])
            assert read(Path(str(dense_out)+'.temporal.1D')) == read(w/'surface_roi_centroid.niml.dset.temporal.1D')
            print('PASS dense functional GIFTI',flush=True)
        base = ['-mode','classify','-method','ridge','-target_column','Condition','-cv_column','Run','-mask',mask,
                '-dataTableFile',tab,'-prefix',w/(fmt+'_validate'+('.nii' if fmt=='volume' else '.niml.dset'))]
        bare=run(base+['-time_resolved','-validate_only'])
        assert '# TimeAxisSource\tindex_only' in bare and '# TimeAxis\t2\tNA' in bare
        run(base+['-time_axis',times], 'require -time_resolved')
        temporal = base+['-time_resolved','-time_axis',times]
        text = run(temporal+['-validate_only']); assert '# TimeAxis\t2\t' in text
        assert '# FiniteSelectedValues\t216' in text
        # Inline AFNI vectors and sequences must select identical trial data.
        axisfile=w/'regular_times.1D';axisfile.write_text('-.1\n-.05\n0\n')
        reference=w/(fmt+'_inline_reference.niml.dset' if fmt=='surface' else fmt+'_inline_reference.nii')
        run(base[:-2]+['-prefix',reference,'-time_resolved','-time_axis',axisfile])
        reference_rows=read(Path(str(reference)+'.temporal.1D'))
        generated=w/(fmt+'_generated'+('.niml.dset' if fmt=='surface' else '.nii'))
        run(base[:-2]+['-prefix',generated,'-time_resolved','-time_start','-.1','-time_step','.05'])
        assert read(Path(str(generated)+'.temporal.1D'))==reference_rows
        for opts,error in [
            (['-time_start','-.1'],'supplied together'),
            (['-time_step','.001'],'supplied together'),
            (['-time_start','-.1','-time_step','0'],'finite and positive'),
            (['-time_start','-.1','-time_step','-.001'],'finite and positive'),
            (['-time_start','nan','-time_step','.001'],'must be finite'),
            (['-time_start','-.1','-time_step','inf'],'finite and positive'),
            (['-time_start','-.1','-time_step','.001','-time_axis',times],'cannot accompany'),
            (['-time_start','1e308','-time_step','1e308'],'finite and strictly increasing'),
            (['-time_start','1e16','-time_step','.001'],'finite and strictly increasing'),
            (['-time_start','0','-time_start','1','-time_step','.1'],'duplicate -time_start')]:
            run(base+['-time_resolved','-validate_only']+opts,error)
        run(base+['-time_start','0','-time_step','.001','-validate_only'],'require -time_resolved')
        for count in (1,5):
            for file in files:writer(file,[[1,2,3,4]]*count)
            for timing in ([],['-time_start','-.1','-time_step','.001']):
                text=run(base+['-time_resolved','-validate_only']+timing)
                axis=[line.split() for line in text.splitlines() if line.startswith('# TimeAxis\t')]
                assert len(axis)==count
                if timing:assert abs(float(axis[-1][-1])-(-.1+(count-1)*.001))<1e-12
                else:assert all(row[-1]=='NA' for row in axis)
        for file,data in zip(files,values):writer(file,data)
        print('PASS',fmt,'inferred count, index-only and start/step shortcuts',flush=True)

        for k,spec in enumerate(('1D: 3%-0.100%0.050','1D: -0.1 -0.05 0')):
            out=w/(fmt+f'_inline{k}'+('.niml.dset' if fmt=='surface' else '.nii'))
            run(base[:-2]+['-prefix',out,'-time_resolved','-time_axis',spec])
            rows=read(Path(str(out)+'.temporal.1D'))
            assert len(rows)==len(reference_rows)
            for a,b in zip(rows,reference_rows):
                assert abs(float(a['TimeSeconds'])-float(b['TimeSeconds']))<1e-8
                assert {k:v for k,v in a.items() if k!='TimeSeconds'}=={k:v for k,v in b.items() if k!='TimeSeconds'}
        for spec in ('1D: -0.100..0','1D: 3%-0.100,0.050','1D: 0 0 1','1D: nan',
                     '1D: 3%0%-0.1','1D: 3%0%0','1D: 3%0%0.1garbage','1D: 2147483647%0%1'):
            run(base+['-time_resolved','-time_axis',spec,'-validate_only'],'time axis')
        for count in (101,601):
            for file in files:writer(file,[[1,2,3,4]]*count)
            text=run(base+['-time_resolved','-time_axis',f'1D: {count}%-0.100%0.001','-validate_only'])
            axis=[line.split() for line in text.splitlines() if line.startswith('# TimeAxis\t')]
            assert len(axis)==count and abs(float(axis[0][-1])+.1)<1e-7
            assert abs(float(axis[-1][-1])-(-.1+(count-1)*.001))<1e-7
        for file,data in zip(files,values):writer(file,data)
        print('PASS',fmt,'inline axes and documented 101/601-sample examples',flush=True)
        env['AFNI_DECONFLICT']='OVERWRITE'
        run(temporal+['-save_predictions',times,'-validate_only'], 'cannot replace input')
        env['AFNI_DECONFLICT']='NO'
        assert not list(w.glob(fmt+'_validate*'))
        run(temporal+['-nperm','10','-permute_within','Run'], 'temporal permutation inference')
        original=tab.read_text();tab.write_text(original.replace(str(files[1]),str(files[0])))
        run(temporal+['-validate_only'],'same trial dataset');tab.write_text(original)
        table(tab,0); run(temporal+['-validate_only'], 'must be a whole dataset'); table(tab)
        writer(files[0],values[0][:2]);run(temporal+['-validate_only'],'exactly 3 time points')
        run(base+['-time_resolved','-validate_only'],'time points')
        run(base+['-time_resolved','-time_start','0','-time_step','.001','-validate_only'],'time points')
        writer(files[0],values[0])
        if fmt == 'surface':
            gifti(files[0],values[0],nodes=(0,2,5,8));run(temporal+['-validate_only'],'domain');gifti(files[0],values[0])
        for bad in ('0\n0\n1\n','0\nnan\n1\n','0 1 2\n',''):
            times.write_text(bad);run(temporal+['-validate_only'],'time axis')
        times.write_text('-.1\n0\n.075\n')
print('PASS temporal validation and independent-latency oracles')
