__author__ = 'Joshua'

import RetroTS as rts

a = rts.retro_ts(respiration_file='Resp_epiRT_scan_14.dat',
                 cardiac_file='ECG_epiRT_scan_14.dat',
                 phys_fs=50,
                 number_of_slices=20,
                 volume_tr=2,
                 show_graphs=1,
                 quiet=1
                 )
