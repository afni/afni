#!/usr/bin/env python3
"""Enumerate an exact 64-slot blocked null, independently refitting/tuning models."""
from test_mvpa_regression import *

with tempfile.TemporaryDirectory(prefix='mvpa-reg-null-') as folder:
    os.chdir(folder);rng=random.Random(31);n=12
    x=[[f32(rng.gauss(0,1)),f32(rng.gauss(0,1))] for _ in range(n)]
    y=[[f32(2*row[0]+rng.gauss(0,.5)),f32(row[1]-row[0]+rng.gauss(0,.5))] for row in x]
    z=[f32(v) for v in range(n//2) for _ in range(2)];groups=[i//2 for i in range(n)];train=list(range(8));test=list(range(8,12))
    nifti('x.nii',x);nifti('y.nii',y);nifti('mask.nii',[[1,1]])
    Path('groups.1D').write_text('Group Column\ng0 X0\ng1 X1\n');Path('penalties.1D').write_text('g0 g1\n.2 3\n4 .1\n1 1\n')
    for method,(options,params) in METHODS.items():
        encoding=method=='banded_ridge'
        if method=='pls':options=['-pls_components_grid','1,2'];params=[1,2]
        Path('data.1D').write_text('Subj Sample Run Block Split Motion X0 X1 Y0 Y1 InputFile\n'+''.join(
            f's0 t{i} r{groups[i]} b{groups[i]} '+('train' if i<8 else 'test')+f' {z[i]:.17g} '+' '.join(f'{v:.17g}' for v in x[i]+y[i])+f' {"y.nii" if encoding else "x.nii"}[{i}]\n' for i in range(n)))
        base=['-mode','encode' if encoding else 'decode','-method',method,'-dataTable','@data.1D','-design_columns' if encoding else '-target_columns',
              'X0,X1' if encoding else 'Y0,Y1','-partition_column','Split','-inner_cv_column','Run','-mask','mask.nii',
              '-reg_tol','1e-9','-reg_max_iter','50000','-nuisance_columns','Motion']+options+(['-score','pattern'] if encoding else [])
        nulls={metric:[] for metric in ['MAE','RMSE','pearson']}
        for swaps in itertools.product([0,1],repeat=6):
            perm=[i^swaps[i//2] for i in range(n)];px=[x[i] for i in perm] if encoding else x;py=y if encoding else [y[i] for i in perm]
            candidates=[]
            for param in params:
                truth=[];pred=[];baseline=[]
                for group in range(4):
                    tr=[i for i in train if groups[i]!=group];te=[i for i in train if groups[i]==group]
                    xx=preprocess(px,tr,z=z);model,_=fit(method,[xx[i] for i in tr],[py[i] for i in tr],param)
                    truth += [py[i] for i in te];pred += model([xx[i] for i in te]);baseline += [mean([py[i] for i in tr])]*len(te)
                candidates.append(loss(truth,pred,baseline,'normalized_mse'))
            chosen=min(range(len(params)),key=lambda i:candidates[i]);xx=preprocess(px,train,z=z);model,_=fit(method,[xx[i] for i in train],[py[i] for i in train],params[chosen]);prediction=model([xx[i] for i in test])
            for metric in nulls:
                values=[]
                for o in range(2):
                    truth=[py[i][o] for i in test];pr=[row[o] for row in prediction];errors=[a-b for a,b in zip(truth,pr)]
                    if metric=='MAE':v=sum(map(abs,errors))/len(errors)
                    elif metric=='RMSE':v=math.sqrt(dot(errors,errors)/len(errors))
                    else:
                        a=[v-sum(truth)/len(truth) for v in truth];b=[v-sum(pr)/len(pr) for v in pr];den=dot(a,a)*dot(b,b)
                        v=dot(a,b)/math.sqrt(den) if den else math.nan
                    values.append(v)
                nulls[metric].append([sum(values)/len(values)] if encoding else values)
        for metric,values in nulls.items():
            tag=method+'_'+metric
            run(base+['-metric',metric,'-nperm','999','-permute_within','Block','-prefix',tag,'-save_predictions',tag+'.pred'])
            run(base+['-metric',metric,'-prefix',tag+'_observed','-save_predictions',tag+'_observed.pred'])
            for suffix in ['pred','regression.1D','regression_candidates.1D']+(['sparse_features.1D'] if method=='elastic_net' else ['group_contributions.1D'] if encoding else []):
                assert read(tag+'.'+suffix)==read(tag+'_observed.'+suffix)
            for o,row in enumerate(read(tag+'.inference.1D')):
                scores=[v[o] for v in values];observed=scores[0];finite=[v for v in scores if math.isfinite(v)]
                # Undefined nulls conservatively count as exceedances.
                count=sum(not math.isfinite(v) or (v>=observed-1e-7 if metric=='pearson' else v<=observed+1e-7) for v in scores)
                close(row['SCORE'],observed);close(row['NULL_MEAN'],sum(finite)/len(finite));close(row['PVAL'],count/64)
                close(row['PFWE'],row['PVAL']);assert int(float(row['NPERM']))==64
        print('PASS exact 64-slot refit/tune null, both score tails:',method)
