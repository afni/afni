#!/usr/bin/env python3
"""Sparse surface domain, volume parity, all supported modes and worker counts."""
from test_mvpa_regression import *
createico,convert=map(lambda p:str(Path(p).resolve()),sys.argv[2:4])

def command(args):
    r=subprocess.run(list(map(str,args)),env=env,text=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    assert r.returncode==0,r.stdout+r.stderr
    return r.stdout

with tempfile.TemporaryDirectory(prefix='mvpa-reg-surface-') as folder:
    os.chdir(folder);rng=random.Random(35);n=24;nodes=[0,2,5,9]
    x=[[f32(rng.gauss(0,1)+i*.1) for j in range(4)] for i in range(n)]
    design=[[f32(rng.gauss(0,1)) for j in range(4)] for i in range(n)]
    target=[[f32(row[0]-.3*row[1]),f32(row[2]+row[3])] for row in x]
    nifti('x.nii',x);nifti('mask.nii',[[1]*4])
    command([createico,'-ld','1','-rad','10','-prefix','mesh.gii'])
    Path('nodes.1D').write_text('\n'.join(map(str,nodes))+'\n');Path('values.1D').write_text('\n'.join(' '.join(map(str,col)) for col in zip(*x))+'\n')
    Path('mask.1D').write_text('1\n'*4)
    command([convert,'-o_niml_bi','-input','values.1D','-node_index_1D','nodes.1D','-prefix','x.niml.dset'])
    command([convert,'-o_niml_bi','-input','mask.1D','-node_index_1D','nodes.1D','-prefix','mask.niml.dset'])
    for surface in [False,True]:
        Path('surface.1D' if surface else 'volume.1D').write_text('Subj Run Sample Y0 Y1 X0 X1 X2 X3 InputFile\n'+''.join(
            f's0 r{i//6} t{i} '+' '.join(f'{v:.17g}' for v in target[i]+design[i])+f' {"x.niml.dset" if surface else "x.nii"}[{i}]\n' for i in range(n)))
    Path('groups.1D').write_text('Group Column\ng0 X0\ng1 X1\ng0 X2\ng1 X3\n');Path('penalties.1D').write_text('g0 g1\n.2 3\n4 .1\n1 1\n')
    for method,(options,params) in METHODS.items():
        for mode in (['decode'] if method=='svr' else ['encode'] if method=='banded_ridge' else ['decode','encode']):
            for score in (['pattern','voxel'] if mode=='encode' else ['pattern']):
                tag=f'{method}_{mode}_{score}'
                base=['-mode',mode,'-method',method,'-cv_column','Run','-reg_tol','1e-8','-reg_max_iter','50000']+options
                base+=['-design_columns','X0,X1,X2,X3','-score',score] if mode=='encode' else ['-target_columns','Y0,Y1']
                for worker in (1,2):
                    prefix=f'{tag}_{worker}.niml.dset'
                    # voxel encoding uses sparse surface mask ROIs without a searchlight.
                    spatial=[] if score=='voxel' else ['-surf','mesh.gii','-searchlight','100']
                    run(base+['-dataTable','@surface.1D','-mask','mask.niml.dset']+spatial+['-prefix',prefix,'-nperm','5','-permute_within','Run','-seed','31','-save_predictions',prefix+'.pred'],workers=worker)
                    rows=[list(map(float,line.split())) for line in command([convert,'-o_1Dp_stdout','-input',prefix,'-prepend_node_index_1D']).splitlines() if line.strip() and not line.startswith('#')]
                    assert [int(row[0]) for row in rows]==nodes
                    if worker==1:reference=rows
                    else:assert reference==rows
                for suffix in ['pred','continuous.1D','regression.1D','regression_candidates.1D','inference.1D']+(['sparse_features.1D'] if method=='elastic_net' else ['group_contributions.1D'] if method=='banded_ridge' else []):
                    assert read(f'{tag}_1.niml.dset.'+suffix)==read(f'{tag}_2.niml.dset.'+suffix)
                run(base+['-dataTable','@volume.1D','-mask','mask.nii','-prefix',tag+'_volume','-save_predictions',tag+'_volume.pred'])
                volume=read(tag+'_volume.pred');surf=read(tag+'_1.niml.dset.pred')
                lookup={(r['Response'],r['Sample']):r for r in volume}
                for row in surf:
                    response=row['Response']
                    if mode=='encode':response='voxel_'+str(nodes.index(int(response.split('_')[1])))
                    expected=lookup[(response,row['Sample'])]
                    for key in ['Predicted','Observed','TrainingMean']:close(row[key],expected[key],2e-6)
                if method=='elastic_net' and mode=='decode':assert {r['Feature'] for r in read(tag+'_1.niml.dset.sparse_features.1D')}=={f'node_{v}' for v in nodes}
                if method=='banded_ridge':assert {r['Group'] for r in read(tag+'_1.niml.dset.group_contributions.1D')}=={'g0','g1'}
                print('PASS sparse-surface domain, workers, permutation restore, volume parity:',tag)
