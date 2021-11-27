#!/usr/bin/env python

# python3 status: ready

import sys

# whine about execution as a main program
if __name__ == '__main__':
   print('** %s: not a main program' % sys.argv[0].split('/')[-1])
   sys.exit(1)

# paired 'orig' vs. 'bids' output files (e.g., tedana --convention orig)
g_m_tedana_files = [
 [ 'adaptive_mask.nii.gz',         'desc-adaptiveGoodSignal_mask.nii.gz'],
 [ 'betas_OC.nii.gz',              'desc-ICA_stat-z_components.nii.gz'],
 [ 'betas_hik_OC.nii.gz',          'desc-ICAAccepted_components.nii.gz'],
 [ 'dataset_description.jzon',     'dataset_description.jzon'],
 [ 'dn_ts_OC.nii.gz',              'desc-optcomDenoised_bold.nii.gz'],
 [ 'feats_OC2.nii.gz',             'desc-ICAAccepted_stat-z_components.nii.gz'],
 [ 'figures',                      'figures'],
 [ 'hik_ts_OC.nii.gz',             'desc-optcomAccepted_bold.nii.gz'],
 [ 'ica_components.nii.gz',        'desc-ICA_components.nii.gz'],
 [ 'ica_decomposition.jzon',       'desc-ICA_decomposition.jzon'],
 [ 'ica_metrics.jzon',             'desc-tedana_metrics.jzon'],
 [ 'ica_metrics.tsv',              'desc-tedana_metrics.tsv'],
 [ 'ica_mixing.tsv',               'desc-ICA_mixing.tsv'],
 [ 'lowk_ts_OC.nii.gz',            'desc-optcomRejected_bold.nii.gz'],
 [ 'pca_components.nii.gz',        'desc-PCA_stat-z_components.nii.gz'],
 [ 'pca_decomposition.jzon',       'desc-PCA_decomposition.jzon'],
 [ 'pca_metrics.jzon',             'desc-PCA_metrics.jzon'],
 [ 'pca_metrics.tsv',              'desc-PCA_metrics.tsv'],
 [ 'pca_mixing.tsv',               'desc-PCA_mixing.tsv'],
 [ 'report.txt',                   'report.txt'],
 [ 's0vG.nii.gz',                  'S0map.nii.gz'],
 [ 't2svG.nii.gz',                 'T2starmap.nii.gz'],
 [ 'tedana_2021-11-19T195806.tsv', 'tedana_2021-11-19T183932.tsv'],
 [ 'tedana_report.html',           'tedana_report.html'],
 [ 'ts_OC.nii.gz',                 'desc-optcom_bold.nii.gz'],
]

