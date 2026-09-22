"""Synthetic flat-grid GIFTI surface + node-indexed subject datasets, for
testing the mask-optional surface searchlight.  A GxG grid of nodes on a
regular triangulated mesh, spacing 3mm; a planted IS-RSA effect (same
construction as mktest.py) covers only the CENTER of the grid, so a whole-mesh
search (no -mask) must find it exactly where a matching explicit mask would.
"""
import numpy as np, nibabel as nib, os
from nibabel.gifti import GiftiImage, GiftiDataArray
from scipy.stats import rankdata

rng = np.random.default_rng(20260802)
OUT = os.path.dirname(os.path.abspath(__file__))

G = 8                      # G x G node grid
NNODE = G * G
NSUB, NT = 16, 40

# ---- mesh: regular grid, two triangles per quad -------------------------
xs, ys = np.meshgrid(np.arange(G), np.arange(G))
coords = np.stack([xs.ravel(), ys.ravel(), np.zeros(NNODE)], axis=1).astype(np.float32) * 3.0

tris = []
for j in range(G - 1):
    for i in range(G - 1):
        n00 = j * G + i
        n10 = j * G + i + 1
        n01 = (j + 1) * G + i
        n11 = (j + 1) * G + i + 1
        tris.append([n00, n10, n11])
        tris.append([n00, n11, n01])
tris = np.array(tris, dtype=np.int32)

mesh = GiftiImage(darrays=[
    GiftiDataArray(coords, intent="NIFTI_INTENT_POINTSET", datatype="NIFTI_TYPE_FLOAT32"),
    GiftiDataArray(tris, intent="NIFTI_INTENT_TRIANGLE", datatype="NIFTI_TYPE_INT32"),
])
nib.save(mesh, os.path.join(OUT, "flatmesh.gii"))

# ---- planted effect only in the center 4x4 block of the grid ------------
center = np.zeros(NNODE, dtype=bool)
for j in range(G):
    for i in range(G):
        if 2 <= i < 6 and 2 <= j < 6:
            center[j * G + i] = True

behav = rng.normal(size=NSUB) * 10 + 50
rk = rankdata(behav)
theta = (rk - 1) / (NSUB - 1) * (np.pi / 2)
S1 = rng.normal(size=NT); S1 -= S1.mean(); S1 /= np.linalg.norm(S1)
S2 = rng.normal(size=NT); S2 -= S2.dot(S1) * S1; S2 -= S2.mean(); S2 /= np.linalg.norm(S2)

for i in range(NSUB):
    sig = np.cos(theta[i]) * S1 + np.sin(theta[i]) * S2
    darrays = []
    for t in range(NT):
        v = np.where(center, sig[t] + 0.05 * rng.normal(size=NNODE),
                     rng.normal(size=NNODE)).astype(np.float32)
        darrays.append(GiftiDataArray(v, intent="NIFTI_INTENT_NONE",
                                       datatype="NIFTI_TYPE_FLOAT32"))
    nib.save(GiftiImage(darrays=darrays), os.path.join(OUT, "surf_s%02d.gii" % i))

# an explicit mask matching the planted center exactly, for the "same result
# with an equivalent -mask" comparison
mdar = GiftiDataArray(center.astype(np.int32), intent="NIFTI_INTENT_NONE",
                      datatype="NIFTI_TYPE_INT32")
nib.save(GiftiImage(darrays=[mdar]), os.path.join(OUT, "centermask.gii"))
# an all-ones mask, for the "no -mask == explicit all-ones mask" comparison
adar = GiftiDataArray(np.ones(NNODE, dtype=np.int32), intent="NIFTI_INTENT_NONE",
                      datatype="NIFTI_TYPE_INT32")
nib.save(GiftiImage(darrays=[adar]), os.path.join(OUT, "allmask.gii"))

with open(os.path.join(OUT, "surftab.txt"), "w") as f:
    f.write("Subj behav InputFile\n")
    for i in range(NSUB):
        f.write("s%02d %.6f %s\n" % (i, behav[i], os.path.join(OUT, "surf_s%02d.gii" % i)))

print("wrote %d-node flat mesh, %d subjects, center block = %d nodes" %
      (NNODE, NSUB, int(center.sum())))
