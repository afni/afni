#!/usr/bin/env python3
"""Finite Monte Carlo check of missing-data joint-shift nulls.

Random/clustered/group-dependent masks are circularly stationary and independent
of signals. Stimulus-related masks depend jointly on a fixed task and each
subject's signal: this deliberately violates ignorability/stationarity, and is
reported as a sensitivity experiment, not a calibration acceptance case.
"""
import argparse,json,math,os,random,subprocess,tempfile
from pathlib import Path
from insync_window_fixtures import nifti
from run_insync_estimation import image
from run_insync_pairwise_missing import table


def main():
    ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--program',required=True)
    ap.add_argument('--replicates',type=int,default=100);ap.add_argument('--method',choices=('pairwise','loo'),default='pairwise');ap.add_argument('--weighted',action='store_true');args=ap.parse_args()
    program=str(Path(args.program).resolve());rng=random.Random(20261118);results={}
    def interval(k,n):
        z=1.96;p=k/n;den=1+z*z/n;mid=(p+z*z/(2*n))/den
        half=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
        return [mid-half,mid+half]
    with tempfile.TemporaryDirectory(prefix='insync-missing-null-') as tmp:
        root=Path(tmp);nifti(root/'mask.nii',[1]+[0]*23)
        for scenario in ('random','clustered','group_dependent','stimulus_related'):
            anyfwe=point=invalid=0
            for rep in range(args.replicates):
                lines=['Subj Group Censor InputFile']
                for sub in range(6):
                    # Circular moving average: exact rotation-invariant joint law.
                    noise=[rng.gauss(0,1) for _ in range(48)]
                    signal=[sum(.7**k*noise[(t-k)%48] for k in range(8)) for t in range(48)]
                    if scenario=='random':keep=[int(rng.random()>.2) for t in range(48)]
                    elif scenario=='clustered':
                        start=rng.randrange(48);keep=[int((t-start)%48>=10) for t in range(48)]
                    elif scenario=='group_dependent':keep=[int(rng.random()>(.1 if sub<3 else .35)) for t in range(48)]
                    else:keep=[int(signal[t]*(1 if t%24<12 else -1)>-.2) for t in range(48)]
                    nifti(root/f's{sub}.nii',[x for x in signal for _ in range(24)],48)
                    (root/f's{sub}.1D').write_text('\n'.join(map(str,keep))+'\n')
                    lines.append(f's{sub} {"AB"[sub//3]} s{sub}.1D s{sub}.nii')
                (root/'table.txt').write_text('\n'.join(lines)+'\n')
                prefix=f'{scenario}{rep}'
                cmd=[program,'-quiet','-missing','pairwise','-isc_method',args.method,'-summary_weight','valid' if args.weighted else 'equal','-mask','mask.nii','-window','24','-step','24',
                    '-temporal_null','timeshift','-temporal_censor','joint_shift','-nnull','100',
                    '-min_shift','1','-prefix',prefix,'-seed',str(3000+rep),'-dataTableFile','table.txt']
                proc=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS='1',AFNI_DECONFLICT='NO'),text=True,capture_output=True)
                assert proc.returncode==0,proc.stderr
                outputs={};cache={}
                for row in table(root/(prefix+'.inference.tsv')):
                    fn=row['File'];cache.setdefault(fn,image(root/fn)[1]);outputs[row['SelectionID'],row['Label']]=cache[fn][int(row['Brick'])*24]
                point+=outputs['sel000001','TP_All_A']<=.05
                invalid+=not outputs['sel000001','TInferValid_All_A']
                anyfwe+=any(v<=.05 for (sid,label),v in outputs.items() if label.startswith('TPFWE_'))
                for path in root.glob(prefix+'.*'):path.unlink()
            results[scenario]={'replicates':args.replicates,'point_rejections':point,'family_rejections':anyfwe,
                'point_rate':point/args.replicates,'family_rate':anyfwe/args.replicates,'family_wilson95':interval(anyfwe,args.replicates),'invalid_observed':invalid,
                'interpretation':'assumptions violated; sensitivity only' if scenario=='stimulus_related' else 'stationary independent-mask null'}
            # A broad finite-simulation alarm, not proof of nominal calibration.
            if scenario!='stimulus_related':assert interval(anyfwe,args.replicates)[0]<=.05,results[scenario]
    print(json.dumps({'seed':20261118,'alpha':.05,'draws':100,'method':args.method+', median, '+('UsedTR' if args.weighted else 'equal')+' weights, one-sided joint shifts','results':results},indent=2))

if __name__=='__main__':main()
