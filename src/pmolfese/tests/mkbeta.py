"""Classic within-subject RSA: 8 conditions in 2 groups of 4.  ROI 1 has spatial
patterns that cluster by group; ROIs 2-3 are noise.  Also serves as the test for
-mode beta -rdm_over subj."""
import numpy as np, nibabel as nib, os
from scipy.stats import spearmanr

rng = np.random.default_rng(1234)
OUT = os.path.dirname(os.path.abspath(__file__))
NSUB, NCOND = 15, 8
NX, NY, NZ = 8, 8, 4
NVOX = NX * NY * NZ

atlas = np.zeros(NVOX, dtype=np.int16)
atlas[0:80] = 1; atlas[80:170] = 2; atlas[170:] = 3
group = np.array([0, 0, 0, 0, 1, 1, 1, 1])

data = np.zeros((NSUB, NVOX, NCOND), dtype=np.float32)
for s in range(NSUB):
    G = rng.normal(size=(2, NVOX))          # this subject's two group patterns
    for c in range(NCOND):
        base = G[group[c]] + 0.6 * rng.normal(size=NVOX)
        for v in range(NVOX):
            data[s, v, c] = base[v] if atlas[v] == 1 else rng.normal()

aff = np.diag([3.0, 3.0, 3.0, 1.0])
nib.save(nib.Nifti1Image(atlas.reshape(NX, NY, NZ), aff), f"{OUT}/batlas.nii.gz")
for s in range(NSUB):
    nib.save(nib.Nifti1Image(data[s].reshape(NX, NY, NZ, NCOND), aff), f"{OUT}/bsub{s:02d}.nii.gz")

# model matrix: 1 if same condition group, 0 if different
model = (group[:, None] == group[None, :]).astype(float)
np.savetxt(f"{OUT}/condmodel.1D", model, fmt="%.1f")

with open(f"{OUT}/btable.txt", "w") as f:
    f.write("Subj InputFile\n")
    for s in range(NSUB):
        f.write(f"s{s:02d} {OUT}/bsub{s:02d}.nii.gz\n")

# ---- reference: per-subject condition RDM vs model, then t-test on Fisher z --
iu = np.triu_indices(NCOND, k=1)
mtri = model[iu]
print(f"{'ROI':>4} {'mean_r':>10} {'t':>9}")
for roi in (1, 2, 3):
    vsel = np.where(atlas == roi)[0]
    zs = []
    for s in range(NSUB):
        pat = data[s][vsel, :].T                 # NCOND x nvox
        neural = np.corrcoef(pat)
        zs.append(np.arctanh(spearmanr(neural[iu], mtri).statistic))
    zs = np.array(zs)
    t = zs.mean() / (zs.std(ddof=1) / np.sqrt(NSUB))
    print(f"{roi:>4} {np.tanh(zs.mean()):>10.6f} {t:>9.3f}")
