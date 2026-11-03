#!/usr/bin/env python3
"""Run the complete 3dRSA numerical audit against the numpy oracle.

    RSA_BIN=/path/to/3dRSA python3 run_all.py [workdir]

Each test writes its own synthetic data under workdir and a results.json.
Exit status is nonzero if any strict check fails.  Convention differences
(documented in the audit report) are printed but never counted as failures.
"""
import os, sys, importlib
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

work = sys.argv[1] if len(sys.argv) > 1 else '/tmp/rsa_audit'
ok = True
for mod in ['test1_first_order', 'test2_crossnobis', 'test3_group', 'test4_inference', 'test5_group_crossnobis']:
    print('=' * 78); print(mod); print('=' * 78)
    m = importlib.import_module(mod)
    ok &= bool(m.main(os.path.join(work, mod)))
print('=' * 78)
print('ALL STRICT CHECKS PASSED' if ok else 'SOME STRICT CHECKS FAILED')
sys.exit(0 if ok else 1)
