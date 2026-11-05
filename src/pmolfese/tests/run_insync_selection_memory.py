#!/usr/bin/env python3
"""Measure bounded M5 source/result storage as selection count increases."""
import argparse
import json
import math
import os
from pathlib import Path
import re
import struct
import subprocess
import sys
import tempfile
from insync_window_fixtures import nifti


def main():
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('--program',required=True); p.add_argument('--models',action='store_true')
    args=p.parse_args(); program=str(Path(args.program).resolve())
    with tempfile.TemporaryDirectory(prefix='insync-m5-memory-') as tmp:
        root=Path(tmp); nvox=8192; nt=96
        nifti(root/'header.nii',[0.]*24)
        header=bytearray((root/'header.nii').read_bytes()[:352])
        struct.pack_into('<8h',header,40,4,32,16,16,nt,1,1,1)
        for s in range(6):
            values=[math.sin(t*.31+v%13*.01)+.3*math.cos(t*.11+s)+.1*math.sin((s+1)*t*.7+v%5) for t in range(nt) for v in range(nvox)]
            (root/f's{s}.nii').write_bytes(header+struct.pack('<%df'%len(values),*values))
        (root/'subjects.txt').write_text('Subj Group InputFile\n'+''.join(f's{s} {"A" if s<3 else "B"} s{s}.nii\n' for s in range(6)))
        records=[]
        # Child wrapper yields this run's peak RSS, independent of earlier runs.
        wrapper='import subprocess,resource,json,sys; r=subprocess.run(sys.argv[1:],capture_output=True,text=True); print(json.dumps(dict(code=r.returncode,log=r.stderr,rss=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))'
        for step in (96,6,1):
            cmd=[sys.executable,'-c',wrapper,program,'-prefix',f'w{step}','-window','24','-step',str(step),'-progress','off']+(['-save_pairwise',f'p{step}'] if args.models else [])+['-dataTableFile','subjects.txt']
            r=subprocess.run(cmd,cwd=root,env=dict(os.environ,OMP_NUM_THREADS='2',AFNI_NOMMAP='YES',AFNI_DECONFLICT='NO'),capture_output=True,text=True,check=True)
            record=json.loads(r.stdout); assert record['code']==0,record['log']
            record['rss_bytes']=record['rss'] if sys.platform=='darwin' else record['rss']*1024
            record['ledger']=re.search(r'sources (\d+) bytes; result/support (\d+) bytes; ROI matrices (\d+) bytes; schedule rows (\d+) \(metadata estimate (\d+) bytes\)',record['log']).groups()
            if args.models:
                record['model_bytes']=int(re.search(r'model export memory (\d+) bytes',record['log'])[1])
                assert (root/f'p{step}.isc/jobs.tsv').read_text().endswith('# Status: complete\n')
            records.append(record)
        assert len({r['ledger'][:3] for r in records})==1
        if args.models: assert len({r['model_bytes'] for r in records})==1
        assert [int(r['ledger'][3]) for r in records]==[6,78,438]
        # Storing all 73 output buckets would add >25 MiB; one-result-at-a-time
        # runs should stay within 16 MiB of the one-window process peak.
        assert records[-1]['rss_bytes']<records[0]['rss_bytes']+16*1024**2,[(r['rss_bytes'],r['ledger']) for r in records]
        for n,r in zip((1,13,73),records):
            print(json.dumps(dict(selections=n,peak_rss_bytes=r['rss_bytes'],source_bytes=int(r['ledger'][0]),result_bytes=int(r['ledger'][1]),schedule_bytes=int(r['ledger'][4]),batch_size=1,model_batch_bytes=r.get('model_bytes',0))))
    print('PASS: fixed-batch memory benchmark (1, 13, 73 selections)'+(' with M6 model exports' if args.models else ''))

if __name__=='__main__': main()
