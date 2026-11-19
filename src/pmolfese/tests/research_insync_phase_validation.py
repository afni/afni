#!/usr/bin/env python3
"""Expanded research-only calibration of two censored-phase candidates.

Uses the independently checked estimator/completion in research_insync_phase_missing.
No production inference is changed. Fixed independent masks and Gaussian circular
processes provide exact phase controls; finite AR and non-Gaussian processes are
separate robustness tests. Family = 3 voxels x full/half windows. Candidate A
scores observations; B reapplies censoring/interpolation before augmented scoring.
Screen with one seed, confirm selected failures with another; do not select a
production domain retrospectively by retaining only passing scenarios.

How to read this file: CONFIG declares each experiment; generate() constructs
independent subjects and mask() removes signal-independent samples. main()
applies identical phase draws to the controls and candidates, then counts
pointwise and familywise rejections. selfcheck() verifies data/mask invariants.
"""
import argparse
import json
from pathlib import Path
import numpy as np
import research_insync_phase_missing as base

# nt, subjects, spectrum/process, mask, missing fraction, maximum gap.
CONFIG = {
    'white_low': (120, 6, 'white', 'random', .10, 1),
    'red_low': (120, 6, 'red', 'random', .10, 2),
    'red_many': (120, 12, 'red', 'shared', .20, 3),
    'band_random': (120, 6, 'band', 'random', .20, 2),
    'band_shared': (120, 6, 'band', 'shared', .20, 2),
    'band_local': (120, 6, 'band', 'local', .20, 2),
    'narrow_shared': (120, 6, 'narrow', 'shared', .20, 2),
    'narrow_local': (120, 6, 'narrow', 'local', .20, 2),
    'finite_ar': (120, 6, 'finite', 'random', .20, 2),
    'heavy_tail': (120, 6, 'heavy', 'random', .20, 2),
    'long_band': (300, 6, 'band', 'shared', .20, 2),
    'localized_white': (96, 6, 'white', 'dense_local', 1/3, 2),
}


def generate(rng, nt, ns, process, shared):
    """Independent subjects; correlated voxels within each subject.

    Circular Gaussian spectra are exact complete-phase controls. Band-pass is
    smooth (roughly .01-.10 Hz at TR=2 s); narrow spectra concentrate near .08
    Hz and are stress tests, not a full physiological BOLD model. Finite AR(1)
    uses a burn-in rather than a circular boundary. Heavy-tailed innovations
    are t(3); exact phase exchangeability is not claimed for that process.
    """
    length = nt + 600 if process == 'finite' else nt
    draw = (lambda shape: rng.standard_t(3, size=shape)) if process == 'heavy' else (lambda shape: rng.normal(size=shape))
    x = (draw((3, ns, length)) + draw((1, ns, length))) / np.sqrt(2)
    freq = np.fft.rfftfreq(nt)
    if process == 'finite':
        for t in range(1, length):
            x[..., t] += .8 * x[..., t-1]
        x = x[..., -nt:]
    elif process != 'white':
        if process == 'band':
            filt = (1-np.exp(-(freq/.02)**4)) * np.exp(-(freq/.20)**8)
        elif process == 'narrow':
            filt = np.exp(-.5*((freq-.16)/.015)**2) + .02
        else:
            filt = 1 / (1-.8*np.exp(-2j*np.pi*freq))
        x = np.fft.irfft(np.fft.rfft(x)*filt, n=nt)
    x /= x.std(-1, keepdims=True)
    if shared:
        common = rng.normal(size=(3, 1, nt))
        common = np.fft.irfft(np.fft.rfft(common)/(1-.8*np.exp(-2j*np.pi*freq)), n=nt)
        x += shared * common / common.std(-1, keepdims=True)
    return x


def mask(rng, nt, ns, kind, rate, gap):
    """Signal-independent bounded gaps, observed run endpoints; no retries on data."""
    keep = np.ones((ns, nt), bool)
    if kind == 'dense_local':
        keep[:, 1:nt//2] = np.arange(1, nt//2)%3 == 0
        return keep
    for s in range(ns):
        upper = nt//2 if kind == 'local' else nt-1
        for t in rng.permutation(np.arange(1, upper-gap+1)):
            if (~keep[s]).sum() >= int(rate*nt):
                break
            if keep[s, t-1:t+gap+1].all():
                keep[s, t:t+gap] = False
    if kind in ('shared', 'local'):
        keep[:] = keep[0]
    return keep



def selfcheck():
    """Check all scenario shapes, original anchors and completion identity.

    The shared base check compares Pearson/LOO estimates with a scalar oracle.
    Here every configured mask must have observed endpoints and only fillable
    gaps; reapplying the same operator to identity must not change its values.
    """
    base.selfcheck()
    for nt, ns, process, kind, rate, gap in CONFIG.values():
        rng = np.random.default_rng(103)
        x = generate(rng, nt, ns, process, 0)
        keep = mask(rng, nt, ns, kind, rate, gap)
        filled = base.completion(x, keep, gap)
        assert x.shape == (3, ns, nt) and np.isfinite(filled).all()
        assert keep[:, [0, -1]].all()
        assert np.array_equal(filled[:, keep], x[:, keep])
        assert np.allclose(base.completion(filled, keep, gap), filled)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--replicates', type=int, default=300)
    ap.add_argument('--draws', type=int, default=200, help='includes identity')
    ap.add_argument('--seed', type=int, default=20261124)
    ap.add_argument('--scenarios', nargs='+', choices=CONFIG, default=list(CONFIG))
    ap.add_argument('--method', choices=('pairwise', 'loo'), default='pairwise')
    ap.add_argument('--shared', type=float, default=0)
    ap.add_argument('--output', type=Path, required=True)
    args = ap.parse_args()
    if args.draws < 20 or args.replicates < 1:
        ap.error('need >=20 draws and >=1 replicate')
    selfcheck()
    report = dict(seed=args.seed, replicates=args.replicates, draws=args.draws,
                  method=args.method, shared=args.shared, numpy=np.__version__,
                  alpha=.05, family='3 voxels x full/first-half/second-half', results={})
    for name in args.scenarios:
        nt, ns, process, kind, rate, gap = CONFIG[name]
        rng = np.random.default_rng(np.random.SeedSequence([args.seed, list(CONFIG).index(name)]))
        counts = {m: dict(point=0, family=0, invalid=0, evaluated=0)
                  for m in ('complete', 'oracle', 'observed', 'refilled')}
        fraction = 0.
        for rep in range(args.replicates):
            x = generate(rng, nt, ns, process, args.shared)
            keep = mask(rng, nt, ns, kind, rate, gap)
            fraction += 1-keep.mean()
            fill = base.completion(x, keep, gap)
            assert np.isfinite(fill).all()
            factors = np.exp(1j*rng.uniform(-np.pi, np.pi, (args.draws, 1, ns, nt//2+1)))
            # Preserve real-valued DC/Nyquist terms and synchronize phases over
            # voxels/windows. Draw zero is identity, giving the +1 p-value rule.
            factors[..., 0] = factors[..., -1] = 1
            factors[0] = 1
            truth = base.phase(x, factors)
            truth[0] = x
            surrogate = base.phase(fill, factors)
            surrogate[0] = fill
            real = keep[None, None, ...]
            draws = {'complete': (truth, np.ones_like(real)),
                     'oracle': (np.where(real, truth, np.nan), real),
                     'observed': (np.where(real, surrogate, np.nan), real),
                     'refilled': (base.completion(surrogate, keep, gap), real)}
            for m, (data, observed) in draws.items():
                stat = base.estimate(data, observed, args.method)
                finite = np.isfinite(stat)
                obs = stat[0]
                # Never drop a failed null cell from the maximum: infinity
                # prevents it making significance easier. Report invalid
                # observed cells separately instead of hiding lost coverage.
                null = np.where(finite, stat, np.inf)
                maximum = null.reshape(args.draws, -1).max(-1)
                p = (null >= obs).mean(0)
                pf = (maximum[:, None, None] >= obs).mean(0)
                c = counts[m]
                c['evaluated'] += 1
                c['invalid'] += not finite[0].all()
                c['point'] += bool(finite[0, 0, 0] and p[0, 0] <= .05)
                c['family'] += bool(np.any(finite[0] & (pf <= .05)))
        for c in counts.values():
            for metric in ('point', 'family'):
                c[metric+'_rate'] = c[metric]/c['evaluated']
                c[metric+'_wilson95'] = base.wilson(c[metric], c['evaluated'])
        interpretation = ('power' if args.shared else
                          'robustness stress test' if process in ('finite', 'heavy') else
                          'independent circular Gaussian signals, signal-independent masks')
        report['results'][name] = dict(config=CONFIG[name], interpretation=interpretation,
                                      missing_fraction=fraction/args.replicates, methods=counts)
        args.output.write_text(json.dumps(report, indent=2)+'\n')
        print(name, {m: (c['point'], c['family'], c['invalid']) for m, c in counts.items()}, flush=True)


if __name__ == '__main__':
    main()
