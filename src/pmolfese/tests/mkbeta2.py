"""Noisier version of the classic-RSA test so the across-subject t is finite,
which exercises the real t-test path rather than the degenerate branch.
Also plants a subject-level effect to test -mode beta -rdm_over subj."""
import numpy as np, nibabel as nib, os
from scipy.stats import spearmanr, ttest_1samp, rankdata

rng = np.random.default_rng(99)
OUT = os.path.dirname(os.path.abspath(__file__))
NSUB, NCOND = 15, 8
NX, NY, NZ = 8, 8, 4
NVOX = NX * NY * NZ

atlas = np.zeros(NVOX, dtype=np.int16)
atlas[0:80] = 1; atlas[80:170] = 2; atlas[170:] = 3
group = np.array([0, 0, 0, 0, 1, 1, 1, 1])

# behavior, used for the -rdm_over subj check
behav = rng.normal(size=NSUB) * 5 + 20
rk = rankdata(behav)
theta = (rk - 1) / (NSUB - 1) * (np.pi / 2)
P1 = rng.normal(size=NVOX); P2 = rng.normal(size=NVOX)

data = np.zeros((NSUB, NVOX, NCOND), dtype=np.float32)
for s in range(NSUB):
    G = rng.normal(size=(2, NVOX))
    for c in range(NCOND):
        # heavy noise -> per-subject correlations genuinely vary
        base = G[group[c]] + 2.5 * rng.normal(size=NVOX)
        # plus a subject-level pattern rotated by behavioral rank
        subj_pat = np.cos(theta[s]) * P1 + np.sin(theta[s]) * P2
        for v in range(NVOX):
            data[s, v, c] = (base[v] + 1.5 * subj_pat[v]) if atlas[v] == 1 else rng.normal()

aff = np.diag([3.0, 3.0, 3.0, 1.0])
nib.save(nib.Nifti1Image(atlas.reshape(NX, NY, NZ), aff), f"{OUT}/b2atlas.nii.gz")
for s in range(NSUB):
    nib.save(nib.Nifti1Image(data[s].reshape(NX, NY, NZ, NCOND), aff), f"{OUT}/b2sub{s:02d}.nii.gz")
with open(f"{OUT}/b2table.txt", "w") as f:
    f.write("Subj behav InputFile\n")
    for s in range(NSUB):
        f.write(f"s{s:02d} {behav[s]:.6f} {OUT}/b2sub{s:02d}.nii.gz\n")

model = (group[:, None] == group[None, :]).astype(float)
iu8 = np.triu_indices(NCOND, k=1)
print("== classic RSA (-rdm_over brick), reference ==")
print(f"{'ROI':>4} {'mean_r':>10} {'t':>9} {'p':>10}")
for roi in (1, 2, 3):
    vsel = np.where(atlas == roi)[0]
    zs = np.array([np.arctanh(spearmanr(np.corrcoef(data[s][vsel, :].T)[iu8],
                                        model[iu8]).statistic) for s in range(NSUB)])
    t, p = ttest_1samp(zs, 0.0)
    print(f"{roi:>4} {np.tanh(zs.mean()):>10.6f} {t:>9.4f} {p:>10.3e}")

# -mode beta -rdm_over subj : feature vector is the whole ROI pattern, all conditions
iuS = np.triu_indices(NSUB, k=1)
d = np.abs(rk[:, None] - rk[None, :]); nn = 1.0 - d / d.max()
print("\n== IS-RSA on patterns (-mode beta -rdm_over subj), reference ==")
print(f"{'ROI':>4} {'nn_r':>10}")
for roi in (1, 2, 3):
    vsel = np.where(atlas == roi)[0]
    # 3dRSA flattens as [condition][voxel]
    feat = np.array([data[s][vsel, :].T.reshape(-1) for s in range(NSUB)])
    print(f"{roi:>4} {spearmanr(np.corrcoef(feat)[iuS], nn[iuS]).statistic:>10.6f}")
