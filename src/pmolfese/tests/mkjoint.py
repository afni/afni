"""Reference for -model_joint: z-scored OLS of the neural triangle on several
model triangles, plus partial correlations.  Uses correlated predictors on
purpose -- that is the case the whole facility exists for."""
import numpy as np, os
from scipy.stats import rankdata, spearmanr

OUT = os.path.dirname(os.path.abspath(__file__))
rng = np.random.default_rng(4242)
NSUB, NT = 20, 60

behav = np.loadtxt(f"{OUT}/table.txt", skiprows=1, usecols=1)
behav2 = behav + rng.normal(size=NSUB) * behav.std() * 0.4   # correlated with behav
conf   = rng.normal(size=NSUB) * 3 + 10                      # unrelated confound

with open(f"{OUT}/jtable.txt", "w") as f:
    f.write("Subj behav behav2 conf modelfile InputFile\n")
    for i in range(NSUB):
        f.write(f"s{i:02d} {behav[i]:.6f} {behav2[i]:.6f} {conf[i]:.6f} "
                f"{OUT}/sub{i:02d}.nii.gz {OUT}/sub{i:02d}.nii.gz\n")

def nn(x):
    r = rankdata(x); d = np.abs(r[:, None] - r[None, :])
    m = 1.0 - d / d.max(); np.fill_diagonal(m, 1.0); return m

def z(v):
    return (v - v.mean()) / v.std()

atlas = np.zeros(8*8*4, dtype=int); atlas[0:80]=1; atlas[80:170]=2; atlas[170:]=3
data = np.stack([__import__("nibabel").load(f"{OUT}/sub{i:02d}.nii.gz")
                 .get_fdata().reshape(-1, NT) for i in range(NSUB)])

iu = np.triu_indices(NSUB, 1)
X = np.column_stack([z(nn(behav)[iu]), z(nn(behav2)[iu]), z(nn(conf)[iu])])

print("model-model correlations (spearman, as 3dRSA prints them):")
names = ["behav_nn", "behav2_nn", "conf_nn"]
tri = [nn(behav)[iu], nn(behav2)[iu], nn(conf)[iu]]
for i in range(3):
    print("   ", " ".join(f"{spearmanr(tri[i],tri[j]).statistic:+7.3f}" for j in range(3)))

print(f"\n{'ROI':>4} " + " ".join(f"{n+'_b':>11} {n+'_pr':>11}" for n in names))
for roi in (1, 2, 3):
    vs = np.where(atlas == roi)[0]
    y = z(np.corrcoef(data[:, vs, :].mean(axis=1))[iu])
    beta = np.linalg.pinv(X) @ y
    pr = []
    for j in range(3):
        Zc = np.delete(X, j, axis=1)
        ry = y - Zc @ (np.linalg.pinv(Zc) @ y)
        rx = X[:, j] - Zc @ (np.linalg.pinv(Zc) @ X[:, j])
        pr.append(np.corrcoef(ry, rx)[0, 1])
    print(f"{roi:>4} " + " ".join(f"{beta[j]:>11.6f} {pr[j]:>11.6f}" for j in range(3)))
