#!/usr/bin/env python3
"""Check coverage diagnostics and missing-aware 3dISC pair/count maps independently."""
import argparse,itertools,math,os,statistics,subprocess,tempfile
from pathlib import Path
from insync_window_fixtures import write_fixture,nifti,support
from run_insync_estimation import image,f32
from run_insync_pairwise_missing import table,pearson,weighted,interpolate


def main():
    ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--program',required=True)
    ap.add_argument('--interp',type=int,default=0)
    args=ap.parse_args();program=str(Path(args.program).resolve());checks=runs=0
    missing=['-missing','interp','-interp_maxgap',str(args.interp)] if args.interp else ['-missing','pairwise']
    with tempfile.TemporaryDirectory(prefix='insync-coverage-export-') as tmp:
        root=write_fixture(Path(tmp)/'fixture')
        def run(opts,filename='censor_subjects.txt',error=None):
            nonlocal runs
            runs+=1;p=f'r{runs:03d}'
            proc=subprocess.run([program,'-quiet',*missing,'-prefix',p,*[x.replace('{p}',p) for x in opts],'-dataTableFile',filename],cwd=root,text=True,capture_output=True,env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_DECONFLICT='NO'))
            if error:assert proc.returncode and error in proc.stderr,(proc.args,proc.stderr)
            else:assert not proc.returncode,(proc.args,proc.stderr)
            return p
        def close(a,b):
            nonlocal checks
            checks+=1;assert abs(a-b)<4e-6,(a,b)
        # Unequal group fractions exercise the actual permutation statistic.
        for sub in range(6):
            keep=[int((t+sub)%11>=sub) for t in range(48)]
            (root/f's{sub+1:02d}.censor.1D').write_text('\n'.join(map(str,keep))+'\n')
        p=run(['-missing_diagnostics','20'])
        frac=[]
        for r in table(root/(p+'.missing_subjects.tsv')):
            keep=[int(v) for v in (root/(r['Subject']+'.censor.1D')).read_text().split()]
            close(float(r['ValidTR']),sum(keep));close(float(r['MissingFrac']),1-sum(keep)/48);frac.append(1-sum(keep)/48)
        obs=3*((statistics.mean(frac[:3])-statistics.mean(frac))**2+(statistics.mean(frac[3:])-statistics.mean(frac))**2)
        null=[]
        for a in itertools.combinations(range(6),3):
            b=[i for i in range(6) if i not in a]
            null.append(3*((statistics.mean(frac[i] for i in a)-statistics.mean(frac))**2+(statistics.mean(frac[i] for i in b)-statistics.mean(frac))**2))
        r=table(root/(p+'.missing_tests.tsv'))[0];close(float(r['Statistic']),obs);close(float(r['P']),sum(x>=obs-64*2.220446049250313e-16 for x in null)/20)
        # Xmat baseline columns are constant within runs; stimulus tests have
        # independently reconstructed splitmix64 rotations on original time.
        p=run(['-missing_diagnostics','40'],'xmat_subjects.txt')
        shifts=[];mask64=(1<<64)-1
        for runid in range(2):
            state=(1234567+1000003*(runid+1))%2147483647;offset=[0]
            for draw in range(1,40):
                while True:
                    state=(state+0x9e3779b97f4a7c15)&mask64;z=state
                    z=((z^(z>>30))*0xbf58476d1ce4e5b9)&mask64;z=((z^(z>>27))*0x94d049bb133111eb)&mask64;z^=z>>31
                    if z>=((1<<64)%24):break
                offset.append(z%24)
            shifts.append(offset)
        # Xmat GoodList supplies the mask, rather than the unrelated censor files.
        import re
        sources={}
        for line in (root/'xmat_subjects.txt').read_text().splitlines()[1:]:
            cols=line.split();xm=next(x for x in cols if x.endswith('.xmat.1D') and 'full' not in x)
            good=re.search(r'GoodList = "([^"]+)"',(root/xm).read_text()).group(1)
            censor=list(map(int,(root/(cols[0]+'.censor.1D')).read_text().split()))
            sources[cols[0]]={t for t in map(int,good.split(',')) if censor[t]}
        for r in table(root/(p+'.missing_tests.tsv')):
            if r['Test']!='xmat':continue
            if r['Column'].startswith('Run#'):assert r['Valid']=='0' and r['P']=='1';continue
            keep=sources[r['Subject']];col=0 if r['Column']=='Vis#0' else 1;vals=[]
            for draw in range(40):
                x=[];y=[]
                for runid in range(2):
                    xx=[float(runid*24+(t+shifts[runid][draw])%24 not in keep) for t in range(24)]
                    yy=[support(t)[col] for t in range(24)]
                    x += [v-statistics.mean(xx) for v in xx];y += [v-statistics.mean(yy) for v in yy]
                vals.append(pearson(x,y,'pearson'))
            close(float(r['Statistic']),vals[0]);close(float(r['P']),sum(abs(x)>=abs(vals[0])-64*2.220446049250313e-16 for x in vals)/40)
        # Ordered cross-condition exports: each edge uses its own source pair.
        lines=['Subj Group Condition Censor InputFile']
        for sub in range(6):
            for c in range(2):lines.append(f's{sub+1:02d} {"AB"[sub//3]} C{c+1} s{sub+1:02d}.censor.1D s{sub+1:02d}{"_C2" if c else ""}.nii')
        (root/'conditions_masked.txt').write_text('\n'.join(lines)+'\n')
        blocks=[lines[0]+' Selection Segment Run StartTR StopTR']
        for line in lines[1:]:
            for b,start in enumerate((0,24)):blocks.append(f'{line} joined b{b} 1 {start} {start+15}')
        (root/'blocks_masked.txt').write_text('\n'.join(blocks)+'\n')
        for roi,block in itertools.product((False,True),(False,True)):
            opts=['-save_pairwise','{p}_pairs']+(['-atlas','atlas.nii','-roi_only'] if roi else [])+(['-block_weight','valid','-block_summary','fisher_mean','-block_missing','drop_component'] if block else [])
            p=run(opts,'blocks_masked.txt' if block else 'conditions_masked.txt');bundle=root/(p+'_pairs.isc')
            schedule=table(root/(p+'.selections.tsv'));atlas=image(root/'atlas.nii')[1]
            for job in table(bundle/'jobs.tsv'):
                pairs=image(bundle/job['Pairs'])[1];counts=image(bundle/job['Support'])[1];mask=image(bundle/job['Mask'])[1]
                parts=[list(range(int(r['OriginalStartTR']),int(r['OriginalStopTR'])+1)) for r in schedule if r['SelectionID']==job['SelectionID'] and r['Subject']=='s01' and r['Condition']=='C1']
                series={};own={};observed={};fills={}
                for c,cond in enumerate((job['Condition1'],job['Condition2'])):
                    for sub in range(6):
                        flat=image(root/f's{sub+1:02d}{"_C2" if cond=="C2" else ""}.nii')[1]
                        keep=list(map(int,(root/f's{sub+1:02d}.censor.1D').read_text().split()))
                        for v in range(24):
                            vv=[i for i in range(24) if atlas[i]==atlas[v]] if roi else [v]
                            series[c,sub,v]=[f32(statistics.mean(flat[t*24+i] for i in vv)) if keep[t] else math.nan for t in range(48)]
                            observed[c,sub,v]=list(map(math.isfinite,series[c,sub,v]))
                            if args.interp:series[c,sub,v]=interpolate(series[c,sub,v],[0],args.interp)
                            fills[c,sub,v]=sum(math.isfinite(series[c,sub,v][t]) and not observed[c,sub,v][t] for part in parts for t in part)
                            own[c,sub,v]=sum(keep[t] for part in parts for t in part)
                for v in range(24):
                    if roi and not atlas[v]:continue
                    assert mask[v]==1
                    for e,(a,b) in enumerate(itertools.combinations(range(6),2)):
                        rs=[];nn=[];aug=[]
                        for part in parts:
                            kept=[t for t in part if math.isfinite(series[0,a,v][t]) and math.isfinite(series[1,b,v][t])]
                            rs.append(f32(pearson([series[0,a,v][t] for t in kept],[series[1,b,v][t] for t in kept],'pearson')));nn.append(sum(observed[0,a,v][t] and observed[1,b,v][t] for t in kept));aug.append(len(kept))
                        r=rs[0] if len(rs)==1 else f32(weighted(rs,nn,'fisher_mean'))
                        close(pairs[e*24+v],math.atanh(max(f32(-.999999),min(f32(.999999),r))))
                        close(counts[2*e*24+v],sum(nn));close(counts[(2*e+1)*24+v],sum(nn))
                        if args.interp:
                            close(counts[(42+2*e)*24+v],sum(aug));close(counts[(43+2*e)*24+v],sum(aug))
                    for c in range(2):
                        for sub in range(6):
                            close(counts[(30+c*6+sub)*24+v],own[c,sub,v])
                            if args.interp:close(counts[(72+c*6+sub)*24+v],fills[c,sub,v])
        run(['-missing_diagnostics','20','-check_table'],error='requires sample loading')
    print(f'PASS: {runs} coverage/export executions; {checks} independent comparisons')

if __name__=='__main__':main()
