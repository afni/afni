"""200 pure-noise ROIs.  If the Mantel permutation is implemented correctly the
p-values must be ~uniform; the classic error (shuffling triangle entries instead
of permuting labels) makes them dramatically anticonservative."""
import numpy as np, nibabel as nib, os

rng = np.random.default_rng(7)
OUT = os.path.dirname(os.path.abspath(__file__))
NSUB, NT, NROI, VPR = 20, 60, 200, 10
NVOX = NROI * VPR
NX, NY, NZ = 20, 20, 5
assert NX * NY * NZ == NVOX

atlas = (np.arange(NVOX) // VPR + 1).astype(np.int16)
aff = np.diag([3.0, 3.0, 3.0, 1.0])
nib.save(nib.Nifti1Image(atlas.reshape(NX, NY, NZ), aff), f"{OUT}/natlas.nii.gz")

behav = rng.normal(size=NSUB)
for i in range(NSUB):
    vol = rng.normal(size=(NVOX, NT)).astype(np.float32).reshape(NX, NY, NZ, NT)
    nib.save(nib.Nifti1Image(vol, aff), f"{OUT}/nsub{i:02d}.nii.gz")

with open(f"{OUT}/ntable.txt", "w") as f:
    f.write("Subj behav InputFile\n")
    for i in range(NSUB):
        f.write(f"s{i:02d} {behav[i]:.6f} {OUT}/nsub{i:02d}.nii.gz\n")
print("wrote", NROI, "null ROIs,", NSUB, "subjects")
