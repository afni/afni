#!/usr/bin/env python3
"""M8 explicit-surrogate runtime/RSS/output-size benchmark, isolated child RSS."""
import argparse,json,math,os,struct,subprocess,sys,tempfile,time
from pathlib import Path
from insync_window_fixtures import nifti

def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True); p.add_argument('--output',required=True)
    args=p.parse_args(); program=str(Path(args.program).resolve()); records=[]
    with tempfile.TemporaryDirectory(prefix='insync-m8-benchmark-') as tmp:
        root=Path(tmp); nv=2048; nt=96
        nifti(root/'header.nii',[0.]*24); header=bytearray((root/'header.nii').read_bytes()[:352])
        struct.pack_into('<8h',header,40,4,16,16,8,nt,1,1,1)
        for s in range(6):
            values=[math.sin(t*.31+v%13*.01)+.3*math.cos(t*.11+s)+.1*math.sin((s+1)*t*.7+v%5) for t in range(nt) for v in range(nv)]
            (root/f's{s}.nii').write_bytes(header+struct.pack('<%df'%len(values),*values))
        struct.pack_into('<8h',header,40,3,16,16,8,1,1,1,1)
        (root/'atlas.nii').write_bytes(header+struct.pack('<%df'%nv,*[v%16+1 for v in range(nv)]))
        (root/'subjects.txt').write_text('Subj Group InputFile\n'+''.join(f's{s} {"AB"[s//3]} s{s}.nii\n' for s in range(6)))
        wrapper='import subprocess,resource,json,sys,time; t=time.monotonic(); r=subprocess.run(sys.argv[1:],capture_output=True,text=True); print(json.dumps(dict(code=r.returncode,log=r.stderr,seconds=time.monotonic()-t,rss=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))'
        for mode in ('timeshift','phase'):
            for roi in (False,True):
                tag=mode+('_roi' if roi else '_voxel'); out=root/tag; out.mkdir()
                command=[program,'-prefix',str(out/'result'),'-window','24','-step','12','-run_starts','0,48','-temporal_null',mode,'-nnull','40','-save_pairwise',str(out/'pairs')]+(['-atlas','atlas.nii','-roi_only'] if roi else [])+['-dataTableFile','subjects.txt']
                proc=subprocess.run([sys.executable,'-c',wrapper]+command,cwd=root,env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_NOMMAP='YES'),capture_output=True,text=True,check=True)
                result=json.loads(proc.stdout); assert result['code']==0,result
                record=dict(mode=mode,space='16 parcels' if roi else '2048 voxels',subjects=6,ntime=96,runs=2,selections=6,draws=40,threads=2,seconds=result['seconds'],peak_rss_bytes=result['rss'] if sys.platform=='darwin' else result['rss']*1024,output_bytes=sum(f.stat().st_size for f in out.rglob('*') if f.is_file()),model_export_bytes=sum(f.stat().st_size for f in (out/'pairs.isc').rglob('*') if f.is_file()))
                records.append(record); print(json.dumps(record),flush=True)
        Path(args.output).write_text(json.dumps(records,indent=2)+'\n')
if __name__=='__main__': main()
