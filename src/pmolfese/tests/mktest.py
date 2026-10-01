"""Generate synthetic IS-RSA data with a known ground truth, then compute the
reference answer with numpy/scipy so 3dRSA's output can be checked against it."""
import numpy as np, nibabel as nib, os
from scipy.stats import rankdata, spearmanr

rng = np.random.default_rng(20260726)
OUT = os.path.dirname(os.path.abspath(__file__))

NSUB, NT = 20, 60
NX, NY, NZ = 8, 8, 4
NVOX = NX * NY * NZ

# ---- atlas: three ROIs -------------------------------------------------
atlas = np.zeros(NVOX, dtype=np.int16)
atlas[0:80] = 1          # will carry the effect
atlas[80:170] = 2        # null
atlas[170:] = 3          # null

# ---- behavior ----------------------------------------------------------
behav = rng.normal(size=NSUB) * 10 + 50
rk = rankdata(behav)

# ROI 1: subject i's signal is a rotation by an angle set by their rank, so
# corr(i,j) = cos(theta_i - theta_j) falls off with |rank_i - rank_j|.
# That is exactly a "nearest neighbor" IS-RSA effect.
theta = (rk - 1) / (NSUB - 1) * (np.pi / 2)
S1 = rng.normal(size=NT); S1 -= S1.mean(); S1 /= np.linalg.norm(S1)
S2 = rng.normal(size=NT); S2 -= S2.dot(S1) * S1; S2 -= S2.mean(); S2 /= np.linalg.norm(S2)

data = np.zeros((NSUB, NVOX, NT), dtype=np.float32)
for i in range(NSUB):
    sig = np.cos(theta[i]) * S1 + np.sin(theta[i]) * S2
    for v in range(NVOX):
        if atlas[v] == 1:
            data[i, v] = sig + 0.05 * rng.normal(size=NT)   # effect
        else:
            data[i, v] = rng.normal(size=NT)                # null

# ---- write NIfTIs ------------------------------------------------------
aff = np.diag([3.0, 3.0, 3.0, 1.0])
nib.save(nib.Nifti1Image(atlas.reshape(NX, NY, NZ), aff), f"{OUT}/atlas.nii.gz")
for i in range(NSUB):
    vol = data[i].reshape(NX, NY, NZ, NT)
    nib.save(nib.Nifti1Image(vol.astype(np.float32), aff), f"{OUT}/sub{i:02d}.nii.gz")

with open(f"{OUT}/table.txt", "w") as f:
    f.write("Subj behav InputFile\n")
    for i in range(NSUB):
        f.write(f"s{i:02d} {behav[i]:.6f} {OUT}/sub{i:02d}.nii.gz\n")

# ---- reference implementation -----------------------------------------
def nn_model(x):
    r = rankdata(x)
    d = np.abs(r[:, None] - r[None, :])
    m = 1.0 - d / d.max()
    np.fill_diagonal(m, 1.0)
    return m

def annak_model(x):
    r = rankdata(x)
    m = (r[:, None] + r[None, :]) / 2.0 / len(x)
    np.fill_diagonal(m, 1.0)
    return m

iu = np.triu_indices(NSUB, k=1)
print(f"{'ROI':>4} {'nvox':>6} {'nn_r':>10} {'nn_p':>10} {'annak_r':>10}")
for roi in (1, 2, 3):
    vsel = np.where(atlas == roi)[0]
    roimean = data[:, vsel, :].mean(axis=1)        # NSUB x NT, as 3dRSA does
    neural = np.corrcoef(roimean)
    ntri = neural[iu]
    rn, pn = spearmanr(ntri, nn_model(behav)[iu])
    ra, _ = spearmanr(ntri, annak_model(behav)[iu])
    print(f"{roi:>4} {len(vsel):>6} {rn:>10.6f} {pn:>10.2e} {ra:>10.6f}")
