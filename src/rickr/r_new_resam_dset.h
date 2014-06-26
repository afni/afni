#ifndef _R_NEW_RESAM_DSET_H_
#define _R_NEW_RESAM_DSET_H_

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif
int resam_str2mode ( char * modestr );
int resam_str2bound( char * str );
THD_3dim_dataset * r_new_resam_dset_eng ( THD_3dim_dataset * din,
        THD_3dim_dataset * min, double dx, double dy, double dz,
        char orient [], int resam_mode, int * sublist, int get_data,
        int killwarpinfo, int bound_type );
THD_3dim_dataset * r_new_resam_dset ( THD_3dim_dataset * din,
        THD_3dim_dataset * min, double dx, double dy, double dz,
        char orient [], int resam_mode, int * sublist, int get_data,
        int killwarpinfo );
int     r_dxyz_mod_dataxes    ( double dx, double dy, double dz,
                   THD_dataxes * daxin, THD_dataxes * daxout, int bound_type );
int     r_fill_resampled_data_brick( THD_3dim_dataset * dset, int resam_mode );
int     r_orient_str2vec      ( char ostr [], THD_ivec3 * ovec );
Boolean r_is_valid_orient_str ( char ostr [] );

#ifdef  __cplusplus
}
#endif

#endif  /* _R_NEW_RESAM_DSET_H_ */
