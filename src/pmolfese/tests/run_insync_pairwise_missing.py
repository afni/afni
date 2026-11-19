#!/usr/bin/env python3
"""Independent overlap/LOO/block oracles for -missing pairwise (stdlib only).

Check every exported component and raw/used count, then group maps, individual
availability maps, and ROI painting. Reference calculations operate on original
input coordinates, never production intermediate buffers or reported counts.
"""
import argparse
import csv
import itertools
import math
import os
from pathlib import Path
import statistics
import subprocess
import tempfile
from insync_window_fixtures import write_fixture, nifti
from run_insync_estimation import image, f32
from run_insync_window_baseline import ranks, summary


def table(path):
    return list(csv.DictReader((s for s in path.read_text().splitlines() if s and not s.startswith('#')), delimiter='\t'))


def pearson(x, y, metric):
    if metric == 'spearman':
        x, y = ranks(x), ranks(y)
    mx, my = statistics.mean(x), statistics.mean(y)
    xx = sum((v-mx)**2 for v in x)
    yy = sum((v-my)**2 for v in y)
    if not xx or not yy:
        return math.nan
    return max(-1., min(1., sum((a-mx)*(b-my) for a,b in zip(x,y))/math.sqrt(xx*yy)))


def weighted(values, weights, kind):
    pairs=sorted((v,w) for v,w in zip(values,weights) if math.isfinite(v) and w>0)
    if not pairs: return math.nan
    total=sum(w for _,w in pairs)
    if kind=='fisher_mean':
        return math.tanh(sum(w*math.atanh(max(-.999999,min(.999999,v))) for v,w in pairs)/total)
    cumulative=0
    for i,(v,w) in enumerate(pairs):
        cumulative+=w
        if cumulative*2>total:return v
        if cumulative*2==total:return (v+pairs[i+1][0])/2


def interpolate(values, starts, maximum):
    """Independent endpoint-pair oracle; only consecutive clean anchors fill."""
    result=list(values)
    for begin,end in zip(starts,starts[1:]+[len(values)]):
        anchors=[t for t in range(begin,end) if math.isfinite(values[t])]
        for a,b in zip(anchors,anchors[1:]):
            if b-a-1>maximum:continue
            for t in range(a+1,b):result[t]=f32(((b-t)*values[a]+(t-a)*values[b])/(b-a))
    return result


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--program', required=True)
    ap.add_argument('--between-groups',action='store_true')
    ap.add_argument('--interp',type=int,default=0,help='also test bounded interpolation with this maximum gap')
    args = ap.parse_args()
    program = str(Path(args.program).resolve())
    missing=['-missing','interp','-interp_maxgap',str(args.interp)] if args.interp else ['-missing','pairwise']
    if args.between_groups:missing+=['-between_groups']
    executions = comparisons = 0
    maximum_error = {"float32":0.0, "float16":0.0}
    with tempfile.TemporaryDirectory(prefix='insync-pairwise-') as temp:
        root = write_fixture(Path(temp)/'fixture')
        # Independent zero-volume and individual censor exclusions. Condition
        # C2 is a different file and must not inherit C1's source-local loss.
        _, flat = image(root/'s01.nii')
        flat[:24] = [0.]*24
        nifti(root/'s01.nii', flat, 48)
        (root/'conditions_censor.txt').write_text('\n'.join(
            line.replace('InputFile','Censor InputFile') if i==0 else ' '.join(line.split()[:-1]+[line.split()[0]+'.censor.1D',line.split()[-1]])
            for i,line in enumerate((root/'conditions.txt').read_text().splitlines()))+'\n')
        raw = (root/'censor_subjects.txt').read_text().splitlines()
        lines = [raw[0]+' Selection Segment Run StartTR StopTR']
        for line in raw[1:]:
            for name,start,stop in [('early',0,15),('late',24,47)]:
                lines.append(f'{line} joined {name} 1 {start} {stop}')
        (root/'blocks.txt').write_text('\n'.join(lines)+'\n')
        # NIfTI readers repair NaN; embedded .1D datasets preserve NaN/Inf.
        # Quantized series exercise ties, a constant subject and constant LOO
        # references, including voxels that remain valid for unrelated pairs.
        lines = [raw[0]]
        for sub,line in enumerate(raw[1:]):
            rows = [[float(((t*(sub+2)+v*3)%11)-5) for t in range(48)] for v in range(3)]
            if sub==0:
                rows[0][3] = math.nan
                rows[0][4] = math.inf
                rows[1] = [2.]*48
            if sub==1: rows[2] = [float(t%7) for t in range(48)]
            if sub==2: rows[2] = [-float(t%7) for t in range(48)]
            name=f'raw{sub}.1D'
            header='# <AFNI_3D_dataset\n# ni_type = "48*float"\n# ni_dimen = "3,1,1"\n# >\n'
            (root/name).write_text(header+'\n'.join(' '.join(map(str,row)) for row in rows)+'\n# </AFNI_3D_dataset>\n')
            lines.append(line.replace(f's{sub+1:02d}.nii',name))
        (root/'raw.txt').write_text('\n'.join(lines)+'\n')
        blockraw=[lines[0]+' Selection Segment Run StartTR StopTR']
        for line in lines[1:]:
            for name,start,stop in [('early',0,15),('late',24,47)]: blockraw.append(f'{line} joined {name} 1 {start} {stop}')
        (root/'rawblocks.txt').write_text('\n'.join(blockraw)+'\n')
        # Nonzero pair overlaps despite an empty all-subject intersection.
        disjoint=[raw[0]]
        for sub,line in enumerate(raw[1:]):
            name=f'disjoint{sub}.1D'
            (root/name).write_text('\n'.join(str(int(t%6!=sub)) for t in range(48))+'\n')
            disjoint.append(line.replace(f's{sub+1:02d}.censor.1D',name))
        (root/'disjoint.txt').write_text('\n'.join(disjoint)+'\n')

        (root/'none.1D').write_text('0\n'*48)
        (root/'empty_source.txt').write_text((root/'censor_subjects.txt').read_text().replace('s01.censor.1D','none.1D'))
        (root/'empty_all.txt').write_text('\n'.join([raw[0]]+[
            line.replace(f's{sub+1:02d}.censor.1D','none.1D') for sub,line in enumerate(raw[1:])])+'\n')
        # Half precision must scale from retained samples, not from arbitrarily
        # large artifacts hidden behind a source's censor mask.
        _, artifact=image(root/'s01.nii')
        keep=[int(x) for x in (root/'s01.censor.1D').read_text().split()]
        for t,k in enumerate(keep):
            if not k: artifact[t*24:(t+1)*24]=[1.e30]*24
        nifti(root/'artifact.nii',artifact,48)
        (root/'artifact.txt').write_text((root/'censor_subjects.txt').read_text().replace('s01.nii','artifact.nii'))

        truncated=[raw[0]]
        for sub,line in enumerate(raw[1:]):
            count=38+2*sub
            flat=image(root/f's{sub+1:02d}.nii')[1]
            name=f'truncated{sub}.nii'; nifti(root/name,flat[:24*count],count)
            censor=f'truncated{sub}.censor.1D'
            (root/censor).write_text('\n'.join((root/f's{sub+1:02d}.censor.1D').read_text().split()[:count])+'\n')
            truncated.append(line.replace(f's{sub+1:02d}.nii',name).replace(f's{sub+1:02d}.censor.1D',censor))
        (root/'truncated.txt').write_text('\n'.join(truncated)+'\n')

        nifti(root/'short.nii', image(root/'s01.nii')[1][:24],1)
        (root/'short.censor.1D').write_text((root/'s01.censor.1D').read_text().split()[0]+'\n')
        (root/'short.txt').write_text((root/'censor_subjects.txt').read_text().replace('s01.nii','short.nii').replace('s01.censor.1D','short.censor.1D'))

        def run(opts=(), source_table='censor_subjects.txt', threads=1, error=None):
            nonlocal executions, comparisons
            executions += 1
            prefix=f'run{executions:03d}'
            env=dict(os.environ, AFNI_DECONFLICT='NO', AFNI_NOMMAP='YES', OMP_NUM_THREADS=str(threads))
            cmd=[program,'-quiet',*missing,'-prefix',prefix,'-save_counts',prefix+'.counts.tsv',*opts,'-dataTableFile',source_table]
            r=subprocess.run(cmd,cwd=root,env=env,text=True,capture_output=True)
            if error:
                assert r.returncode and error in r.stderr,(cmd,r.stderr)
                assert not (root/(prefix+'.counts.tsv')).exists(),r.stderr
                return
            assert r.returncode==0,(cmd,r.stderr)
            def value(key,default): return opts[opts.index(key)+1] if key in opts else default
            method=value('-isc_method','pairwise'); metric=value('-correlation','pearson')
            kind=value('-summary','median'); blockkind=value('-block_summary','median')
            blockpolicy=value('-block_missing','require_all')
            min_n=int(value('-min_valid',3)); fraction=float(value('-min_valid_frac',0))
            min_b=int(value('-min_blocks',1)); frac_b=float(value('-min_block_frac',0))
            min_others=int(value('-loo_min_others',1))
            tol=.002 if '-float16' in opts else 6e-6
            rows=table(root/(prefix+'.counts.tsv'))
            schedule=table(root/(prefix+'.selections.tsv'))
            bricks=table(root/(prefix+'.bricks.tsv'))
            inputs={}
            for row in schedule:
                key=(row['Subject'],row['Condition'])
                if key in inputs: continue
                shape,data=image(root/row['InputFile']); nv=math.prod(shape[:3])
                keep=[int(x) for x in (root/row['Censor']).read_text().split()] if row['Censor']!='NA' else [1]*48
                if '-censor' in opts:
                    global_keep=[int(x) for x in (root/value('-censor','')).read_text().split()]
                    keep=[a*b for a,b in zip(keep,global_keep)]
                mask=image(root/value('-mask','mask.nii'))[1] if '-mask' in opts else [1]*nv
                if '-zcensor' in opts:
                    keep=[k*int(any(data[t*nv+v]!=0 for v in range(nv) if mask[v])) for t,k in enumerate(keep)]
                data += [math.nan]*(48*nv-len(data)); keep += [0]*(48-len(keep))
                inputs[key]=(data,nv,keep,row['Group'])
            atlas=image(root/value('-atlas','atlas.nii'))[1] if '-atlas' in opts else None
            cache={}
            def expected(sel,cond,unitkind,unit):
                key=(sel,cond,unitkind,unit)
                if key in cache: return cache[key]
                subjects=sorted(sub for sub,c in inputs if c==cond)
                group={sub:inputs[sub,cond][3] for sub in subjects}
                members={g:[sub for sub in subjects if group[sub]==g] for g in set(group.values())}
                segments=[list(range(int(e['OriginalStartTR']),int(e['OriginalStopTR'])+1)) for e in schedule if e['SelectionID']==sel and e['Condition']==cond and e['Subject']==subjects[0]]
                aggregate=any(e['Role']=='block_aggregate_component' for e in schedule if e['SelectionID']==sel) and '-combine_blocks' not in opts
                timeline=[t for segment in segments for t in segment]
                series={}; observed={}
                for sub in subjects:
                    data,nv,keep,_=inputs[sub,cond]
                    mask=image(root/value('-mask','mask.nii'))[1] if '-mask' in opts else [1]*nv
                    vox=[unit] if unitkind=='voxel' else [v for v in range(nv) if atlas[v]==unit and mask[v]]
                    series[sub]=[f32(sum(data[t*nv+v] for v in vox)/len(vox)) if keep[t] and all(math.isfinite(data[t*nv+v]) for v in vox) else math.nan for t in range(48)]
                    observed[sub]=list(map(math.isfinite,series[sub]))
                    if args.interp:
                        starts=list(map(int,value('-run_starts','0').split(',')))
                        if '-xmat_blocks' in opts:starts=[0,24]
                        series[sub]=interpolate(series[sub],starts,args.interp)
                components=[('pair',a,b) for a,b in itertools.combinations(subjects,2)]
                if method=='loo': components += [('loo',a,'NA') for a in subjects]
                def one(component,parts):
                    typ,a,b=component
                    xs=[];ys=[];refs=[];real=0
                    for part in parts:
                        xx=[];yy=[]
                        for t in part:
                            x=series[a][t]
                            if not math.isfinite(x): continue
                            if typ=='pair':
                                y=series[b][t]; genuine=observed[a][t] and observed[b][t]
                            else:
                                others=[series[sub][t] for sub in members[group[a]] if sub!=a and math.isfinite(series[sub][t])]
                                if len(others)<min_others: continue
                                y=f32(statistics.mean(others)); refs.append(len(others))
                                genuine=observed[a][t] and all(observed[sub][t] for sub in members[group[a]] if sub!=a and math.isfinite(series[sub][t]))
                            if math.isfinite(y): xx.append(x); yy.append(y); real+=genuine
                        if '-segment_center' in opts and xx:
                            mx,my=statistics.mean(xx),statistics.mean(yy)
                            xx=[f32(x-f32(mx)) for x in xx]; yy=[f32(y-f32(my)) for y in yy]
                        xs+=xx;ys+=yy
                    n=len(xs); nominal=sum(map(len,parts))
                    corr=pearson(xs,ys,metric) if real>=min_n and real/nominal>=fraction else math.nan
                    return dict(r=f32(corr), overlap=real, used=real if math.isfinite(corr) else 0,
                                samples=n,used_samples=n if math.isfinite(corr) else 0,
                                eligible=int(math.isfinite(corr)), blocks=int(math.isfinite(corr)), others=min(refs,default=0))
                if not aggregate: result={component:one(component,segments) for component in components}
                else:
                    perblock=[{component:one(component,[segment]) for component in components} for segment in segments]
                    wanted=[c for c in components if (c[0]=='pair' and method=='pairwise' and group[c[1]]==group[c[2]]) or (c[0]=='loo' and method=='loo')]
                    common=[all(math.isfinite(block[c]['r']) for c in wanted) for block in perblock]
                    result={}
                    for component in components:
                        vals=[block[component] for block in perblock]
                        admitted=[v for index,v in enumerate(vals) if math.isfinite(v['r']) and (blockpolicy!='drop_common' or common[index])]
                        eligible=sum(math.isfinite(v['r']) for v in vals)
                        good=len(admitted)>=min_b and len(admitted)/len(segments)>=frac_b and (blockpolicy!='require_all' or len(admitted)==len(segments))
                        block_r=(admitted[0]['r'] if len(admitted)==1 else weighted([v['r'] for v in admitted],[v['used'] for v in admitted],blockkind) if value('-block_weight','equal')=='valid' else summary([v['r'] for v in admitted],blockkind)) if good else math.nan
                        result[component]=dict(r=f32(block_r),
                            samples=sum(v['samples'] for v in vals),used_samples=sum(v['used_samples'] for v in admitted) if good else 0,
                            overlap=sum(v['overlap'] for v in vals),used=sum(v['used'] for v in admitted) if good else 0,
                            eligible=eligible,blocks=len(admitted) if good else 0,others=min((v['others'] for v in vals if v['others']),default=0))
                own={sub:sum(observed[sub][t] for t in timeline) for sub in subjects}
                filled_own={sub:sum(math.isfinite(series[sub][t]) and not observed[sub][t] for t in timeline) for sub in subjects}
                cache[key]=(result,own,members,aggregate,filled_own)
                return cache[key]
            def close(a,b,context):
                nonlocal comparisons
                comparisons+=1
                if math.isfinite(a) and math.isfinite(b):
                    mode='float16' if '-float16' in opts else 'float32'
                    maximum_error[mode]=max(maximum_error[mode],abs(a-b))
                assert (math.isnan(a) and math.isnan(b)) or (math.isfinite(a) and math.isfinite(b) and abs(a-b)<=tol),(context,a,b)
            for row in rows:
                result,own,_,_,filled_own=expected(row['SelectionID'],row['Condition'],row['UnitKind'],int(row['Unit']))
                if row['Type']=='subject':
                    assert int(row['OverlapTR'])==own[row['Subj1']]
                    if args.interp:
                        assert int(row['FilledTR'])==filled_own[row['Subj1']]
                        assert int(row['SamplesTR'])==int(row['OverlapTR'])+int(row['FilledTR'])
                    continue
                exp=result[row['Type'],row['Subj1'],row['Subj2']]
                close(float(row['Correlation']),exp['r'],row)
                for col,key in [('OverlapTR','overlap'),('UsedTR','used'),('BlocksEligible','eligible'),('BlocksUsed','blocks'),('MinOthers','others')]:
                    assert int(row[col])==exp[key],(col,row,exp)
                assert int(row['Valid'])==math.isfinite(exp['r'])
                if args.interp:
                    assert int(row['SamplesTR'])==exp['samples']
                    assert int(row['UsedSamplesTR'])==exp['used_samples']
                    assert int(row['FilledTR'])==exp['samples']-exp['overlap']
            if '-save_matrix' in opts:
                lookup={(r['SelectionID'],r['Condition'],r['Unit'],r['Subj1'],r['Subj2']):r
                    for r in rows if r['UnitKind']=='ROI' and r['Type']=='pair'}
                for r in table(root/value('-save_matrix','')):
                    ref=lookup[r['SelectionID'],r['Condition'],r['ROI'],r['Subj1'],r['Subj2']]
                    close(float(r['Correlation']),float(ref['Correlation']),r)
                    assert r['Valid']==ref['Valid']
            # Check group statistics/counts and subject coverage in actual maps,
            # not just the diagnostic exporter. ROI-only values must be painted.
            output={b['File']:image(root/b['File']) for b in bricks if b['Kind']=='result'}
            for key,(result,own,members,_,filled_own) in cache.items():
                sel,cond,unitkind,unit=key
                if unitkind=='ROI' and '-roi_only' not in opts: continue
                for b in bricks:
                    if b['SelectionID']!=sel or b['Kind']!='result': continue
                    label=b['Label']; target=None
                    for group,subjects in members.items():
                        comp=[('pair',a,c) for a,c in itertools.combinations(subjects,2)] if method=='pairwise' else [('loo',a,'NA') for a in subjects]
                        vals=[result[c]['r'] for c in comp if math.isfinite(result[c]['r'])]
                        suffix=group if len({c for _,c in inputs})==1 else cond+'_'+group
                        if label=='ISC_'+suffix:
                            weights=[result[c]['used'] for c in comp if math.isfinite(result[c]['r'])]
                            target=(weighted(vals,weights,kind) if value('-summary_weight','equal')=='valid' else summary(vals,kind)) if vals else 0.
                        if label=='N_'+suffix: target=len(vals)
                        if label==('MinPairTR_' if method=='pairwise' else 'MinLOOTR_')+cond+'_'+group: target=min(result[c]['overlap'] for c in comp)
                    if args.between_groups:
                        for ga,gb in itertools.combinations(sorted(members),2):
                            cross=[result['pair',*sorted((a,b))] for a in members[ga] for b in members[gb]]
                            finite=[x for x in cross if math.isfinite(x['r'])]
                            rr=(weighted([x['r'] for x in finite],[x['used'] for x in finite],kind) if value('-summary_weight','equal')=='valid' else summary([x['r'] for x in finite],kind)) if finite else 0.
                            values={'CrossISC':rr,'CrossN':len(finite),'CrossValid':int(bool(finite)),
                                    'CrossComplete':int(len(finite)==len(cross)),
                                    'CrossMinTR':min(x['overlap'] for x in cross),
                                    'CrossMinUsedTR':min(x['used'] for x in cross)}
                            for metric,expected_value in values.items():
                                if label==metric+'_'+cond+'_'+ga+'_'+gb:target=expected_value
                    for sub,n in own.items():
                        if label=='ValidTR_'+cond+'_'+sub: target=n
                        if label=='FilledTR_'+cond+'_'+sub: target=filled_own[sub]
                    if target is None: continue
                    shape,data=output[b['File']]; nv=math.prod(shape[:3])
                    units=[unit] if unitkind=='voxel' else [v for v in range(nv) if atlas[v]==unit]
                    for v in units: close(data[int(b['Brick'])*nv+v],target,(b,key))
            return (root/(prefix+'.counts.tsv')).read_text()

        for method,metric,kind in itertools.product(('pairwise','loo'),('pearson','spearman'),('median','fisher_mean')):
            opts=['-isc_method',method,'-correlation',metric,'-summary',kind]
            run(opts)
            run(opts, 'subjects.txt')
            run(opts, 'raw.txt')
        a=run(['-zcensor','-window','12','-step','12','-run_starts','0,24'])
        assert a==run(['-zcensor','-window','12','-step','12','-run_starts','0,24'],threads=2)
        run(['-isc_method','loo','-loo_min_others','2','-min_valid','10','-min_valid_frac','.8','-window','12','-step','12'], 'raw.txt')
        run(['-min_valid_frac','.95'])
        run([], 'disjoint.txt')
        run([], 'empty_source.txt')
        run(['-isc_method','loo'], 'empty_source.txt')
        run(['-isc_method','loo','-loo_min_others','2'], 'empty_source.txt')
        run([], 'empty_all.txt')
        run(['-isc_method','loo'], 'empty_all.txt')
        run(([] if args.interp else ['-float16'])+['-mask','mask.nii'], 'artifact.txt')
        run(['-zcensor','-mask','mask.nii']+([] if args.interp else ['-float16']))
        run(['-atlas','atlas.nii','-roi_only','-zcensor'])
        run(['-atlas','atlas.nii','-window','24','-step','24','-save_matrix','roi_pairs.tsv'])
        run(['-zcensor'], 'conditions_censor.txt')
        run(['-xmat_blocks','by_stim','-xmat_threshold','.5','-block_missing','drop_component'], 'xmat_subjects.txt')
        for method,policy in itertools.product(('pairwise','loo'),('require_all','drop_common','drop_component')):
            run(['-isc_method',method,'-block_missing',policy,'-min_valid','15','-min_blocks','1'], 'rawblocks.txt')
            run(['-isc_method',method,'-block_missing',policy,'-block_summary','fisher_mean','-min_block_frac','.75'], 'blocks.txt')
        run(['-combine_blocks','pool','-segment_center','mean','-correlation','spearman'], 'rawblocks.txt')
        run(['-combine_blocks','pool','-segment_center','mean','-isc_method','loo','-loo_min_others','2'], 'rawblocks.txt')
        for method,kind in itertools.product(('pairwise','loo'),('median','fisher_mean')):
            run(['-isc_method',method,'-summary',kind,'-summary_weight','valid'], 'truncated.txt')
            run(['-isc_method',method,'-summary',kind,'-summary_weight','valid','-block_weight','valid',
                 '-block_summary',kind,'-block_missing','drop_component'], 'rawblocks.txt')
        run(['-window','12','-step','12','-min_valid_frac','.8'], 'truncated.txt')
        run(['-zcensor','-save_missing','short_missing.tsv'], 'short.txt')
        for row in table(root/'short_missing.tsv'):
            assert int(row['NPadded'])==int(row['Group']=='A' and int(row['OrigTR'])>=1),row
            assert int(row['NNonfinite'])==0,row
        for opts,error in [(['-temporal_null','timeshift','-nnull','20'],'joint_shift'),
            (['-loo_min_others','1'],'requires'),
            (['-isc_method','loo','-loo_min_others','3'],'exceeds available'),(['-loo_min_others','0'],'integer'),
            (['-edges','-atlas','atlas.nii'],'edge modes'),(['-missing','common'],'-save_counts requires'),
            (['-memory_limit','.000001'],'exceeds limit'),(['-check_table'],'-save_counts requires')]:
            if args.between_groups and '-missing' in opts:error='-between_groups requires'
            elif args.interp and '-missing' in opts: error='-interp_maxgap requires'
            run(opts,error=error)
        if args.between_groups:
            raw=(root/'censor_subjects.txt').read_text().splitlines()
            extra=[line.replace(f's{i+1:02d} ',f's{i+7:02d} ',1).replace(' A ',' C ') for i,line in enumerate(raw[1:4])]
            (root/'three_groups.txt').write_text('\n'.join(raw+extra)+'\n')
            run(['-window','12','-step','12'],'three_groups.txt')
            (root/'one_group.txt').write_text('\n'.join(raw).replace(' B ',' A ')+'\n')
            run([], 'one_group.txt',error='at least two groups')
        # Saving counts is optional; the estimates/maps must be identical.
        result=subprocess.run([program,'-quiet',*missing,'-prefix','no_counts',
            '-dataTableFile','censor_subjects.txt'],cwd=root,text=True,capture_output=True,
            env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_DECONFLICT='NO'))
        executions+=1
        assert result.returncode==0,result.stderr
        assert image(root/'no_counts.sel000001.nii.gz')==image(root/'run001.sel000001.nii.gz')
    print(f'PASS: {executions} source-local executions; {comparisons} scalar/map comparisons; max absolute errors {maximum_error}')


if __name__=='__main__':
    main()
