
static char * g_history[] =
{
    "----------------------------------------------------------------------\n"
    " history:\n"
    "\n",
    " 1.0  Jul  5, 2005 [rickr] - initial release\n",
    " 1.1  Jul 13, 2005 [rickr] - process run of fewer than 3 slices\n",
    " 1.2  Jul 22, 2005 [rickr] - use IOCHAN_CLOSENOW() in realtime.c\n",
    " 1.3  Jul 25, 2005 [rickr] - force tcp close for multiple term signals\n",
    " 2.0  Jul 29, 2005 [rickr] - DICOM file organizer\n"
    "      - add -dicom_org option, to try to organize the image files\n"
    "      - enable GERT_Reco option for DICOM files\n",
    " 2.1  Aug 23, 2005 [rickr]\n"
    "      - added -sort_by_num_suffix option and routines\n"
    "      - output actual TR in to3d command of GERT_Reco script\n",
    " 2.2  Aug 29, 2005 [rickr] - added options -rev_org_dir, -rev_sort_dir\n",
    " 2.3  Sep 01, 2005 [rickr] - added option -tr\n",
    " 2.4  Nov 20, 2006 [rickr] - added option -epsilon\n",
    " 2.5  Mar 10, 2008 [rickr]\n"
    "      - if 1 run, GERT_Reco_dicom is named by it\n",
    "      - apply -gert_outdir in the case of dicom images\n",
    " 2.6  Mar 17, 2008 [rickr]\n"
    "      - if 1 volume, GERT_Reco_dicom does not specify nt=1 in to3d\n",
    " 2.7  Mar 24, 2008 [rickr] - new GERT_Reco options\n"
    "      - added -gert_filename, -gert_nz, -gert_to3d_prefix (for D Glen)\n",
    " 2.8  Jul 10, 2008 [rickr] - handle oblique datasets\n"
    "      - pass oblique transform matrix to plug_realtime\n",
    " 2.9  Jul 11, 2008 [rickr]\n"
    "      - slight change in sleeping habits (no real effect)\n"
    "      - pass all 16 elements of oblique transform matrix\n",
    " 2.10 Jul 14, 2008 [rickr] - control over real-time sleep habits\n"
    "      - added -sleep_init, -sleep_vol, -sleep_frac\n",
    " 2.11 Jul 25, 2008 [rickr]\n"
    "      - allow -sleep_vol to be very small w/out early run termination\n"
    "      - limit search failures to 1, again\n",
    " 2.12 Jul 31, 2008 [rickr]\n"
    "      - added full real-time testing example to help (example E)\n",
    "      - added -num_slices option\n",
    " 2.13 Aug 14, 2008 [rickr]\n"
    "      - moved num_slices check to separate function\n"
    " 2.14 Aug 18, 2008 [rickr] - help update\n"
    " 2.15 Aug 18, 2008 [rickr] - suggest -num_slices with -sleep_init\n"
    " 2.16 Sep  3, 2008 [rickr] - added -drive_wait option\n"
    " 2.17 Nov 24, 2008 [rickr] - added -infile_list and -show_sorted_list\n"
    " 2.18 Jun 25, 2009 [rickr] - fixed dz sent to RT plugin for oblique data\n"
    " 2.19 Nov  4, 2009 [rickr] - small change to sort test\n"
    " 2.20 May  6, 2010 [rickr]\n",
    "      - look for field 0054 1330 (Image Index) in image sorting\n"
    "        (for S Kippenhan, S Wei, G Alarcon)\n"
    "      - allow negatives in -sort_by_num_suffix\n"
    " 2.21 Oct 20, 2010 [rickr] - added -sort_by_acq_time for -dicom_org\n"
    "                             (for Manjula)\n"
    " 3.0  Oct 20, 2010 [rickr] - handle Siemens Mosaic formatted files\n",
    "      - Dimon now depends on libmri.a to make the processing consistent\n"
    "        (many changes to DICOM processing in libmri)\n"
    "      - get MRI_IMARR from mri_read_dicom (data or not)\n"
    "      - modifications for oblique data processing\n"
    "      - for mosaic: figure mosaic origin, nslices, but not orients\n"
    " 3.1  Jan 13, 2011 [rickr]\n"
    "      - added GERT_Reco execution and naming options\n",
    "      - added -gert_write_as_nifti and -gert_create_dataset\n"
    "        (requested by V Roopchansingh)\n"
    " 3.2  Apr 15, 2011 [rickr]\n"
    "      - added FROM_IMAGE as the default slice timing pattern for to3d\n"
    "        for the case of Siemens mosaic\n",
    " 3.3  Apr 25, 2011 [rickr]\n"
    "      - if Siemens timing: pass TPATTERN explicit to RT plugin\n"
    " 3.4  Aug 30, 2011 [rickr]\n"
    "      - update volume delta to mean dz, to accomodate truncated initial\n"
    "        values leading to 'volume toasted' (problem noted by B Benson)\n",
    " 3.5  Sep  6, 2011 [rickr]\n"
    "      - added -fast: short for -sleep_init 50 -sleep_vol 50\n"
    " 3.6  Sep  6, 2011 [rickr]\n"
    "      - allow -save_file_list to apply even with -infile_list\n"
    " 3.7  Jan 17, 2012 [rickr]\n"
    "      - using -gert_create_dataset implies -GERT_Reco and -quit\n"
    " 3.8  Jan 19, 2012 [rickr]\n",
    "      - made -quit more agressive (never wait for new files)\n"
    " 3.7  Jan 25, 2012 [rickr] : back out changes for 3.8 and ponder\n"
    " 3.8  Feb  7, 2012 [rickr] : added -no_wait (more forceful than -quit)\n"
    "      - also, suppress new glob warning\n"
    " 3.9  Feb 14, 2012 [rickr]\n"
    "      - if -no_wait, terminate on volume_match failure\n"
    " 3.10 Feb 16, 2012 [rickr]\n",
    "      - added -max_images\n"
    "      - do not init vol search state to 2, would limit volumes to 40\n"
    "      - include fl_start in no_wait test\n"
    "      - look for new vol worth of images, but no volume match\n"
    " 3.11 Mar 14, 2012 [rickr]\n",
    "      - added -num_chan for J Evans\n"
    "      - added -max_quiet_trs for V Roopchansingh\n"
    "      - default is to sleep 1.1*TR\n"
    " 3.12 Aug  8, 2012 [rickr]\n",
    "      - added -use_slice_loc\n"
    "      - fixed application of use_last_elem in library\n"
    " 3.13 Jan 22, 2013 [rickr]\n",
    "      - replaced -use_imon with -file_type\n"
    "      - made many changes in prep for adding AFNI file type\n"
    " 3.14 Jan 24, 2013 [rickr]\n",
    "      - now handles -file_type AFNI\n"
    " 3.15 Jul 10, 2013 [rickr]\n",
    "      - if unsigned short is detected, pass -ushort2float to to3d\n"
    " 3.16 Sep  3, 2013 [rickr]\n",
    "      - if im_is_volume and single volume, get dz from image\n"
    "        (problem reported by A Nilsen)\n"
    "      - also, pass along option TR in volume case\n"
    " 4.00 Aug 12, 2014 [rickr]\n",
    "      - no (real) change should be noticed\n"
    "      - this was an internal re-write to allow for realtime sorting\n"
    " 4.01 Aug 13, 2014 [rickr] : minor changes\n",
    " 4.02 Aug 22, 2014 [rickr] :\n",
    "      - added -sort_method option (particularly for geme_index)\n"
    "        (this is for real-time sorting based on GE_ME_INDEX fields)\n"
    "      - added -save_detail, for save the details of image files\n"
    " 4.03 Aug 28, 2014 [rickr] :\n",
    "      - if -sort_method geme_index, test SOP IUID index order\n"
    "      - if -save_details, make separate file for GEME index sort info\n"
    " 4.04 Sep  8, 2014 [rickr] :\n",
    "      - num_chan > 1 needs ACQ type 3D+t\n"
    " 4.05 Sep 10, 2014 [rickr] : handle num_chan > 1 in GERT_Reco\n",
    " 4.06 Sep 25, 2014 [rickr] : fixed add_to_string_list() usage\n",
    " 4.07 Oct  8, 2014 [rickr]\n",
    "      - added option -save_errors, to go with -save_details\n"
    "      - allow more chances for error recovery\n"
    "      - fixed interaction between -sort_by_num_suffix and realtime sort\n"
    " 4.08 Oct 16, 2014 [rickr]\n",
    "      - added sort_methods: none, acq_time, default, num_suffix, zposn\n"
    " 4.09 Oct 27, 2014 [rickr]\n",
    "      - fixed -sort_by_num_suffix strcmp trap\n"
    "      - expanded -sort_by_num_suffix to find any final integer\n"
    " 4.10 Nov 19, 2014 [rickr]:\n",
    "      - do not allow num_suffix entries to be read as octal\n"
    "      - show version upon execution\n"
    " 4.11 Mar 13, 2015 [rickr]: added option -te_list for VR\n",
    " 4.12 Aug  3, 2015 [rickr]: \n",
    "      - lost timing info in change to 3D+t (version 4.04, above)\n"
    "      - add it back via new sequence type 3D+timing\n"
    " 4.13 Mar 24, 2016 [rickr]: \n",
    "      - added option -use_obl_origin for oblique data\n"
    "      - run to3d via 'tcsh -x' to see the command\n"
    " 4.14 Apr 19, 2016 [rickr]: \n",
    "      - no sorting was incorrectly returning an error\n"
    " 4.15 Jul  7, 2016 [rickr]: add -order_as_zt: convert tz ordering to zt\n"
    " 4.16 Jul  8, 2016 [rickr]: add -read_all: remove limit on images read\n"
    " 4.17 Nov  8, 2016 [rickr]: maybe override DICOM orient if sorting\n"
    " 4.18 Nov  9, 2016 [rickr]: add -gert_chan_prefix\n"
    " 4.19 May  9, 2017 [rickr]:\n",
    "      - if NIFTI prefix, whine about and clear any write_as_nifti\n"
    " 4.20 Jun 19, 2017 [rickr]: add -assume_dicom_mosaic\n"
    " 4.21 Sep  6, 2017 [rickr]:\n"
    "      - apply xim in realtime.c, so not sending short end of run\n"
    " 4.22 Dec 10, 2017 [rickr]: apply -gert_to3d_prefix for GEMS\n"
    " 4.23 May 23, 2018 [rickr]: add -ushort2float\n"
    "----------------------------------------------------------------------\n"
};

#define DIMON_VERSION "version 4.23 (May 23, 2018)"

/*----------------------------------------------------------------------
 * Dimon - monitor real-time aquisition of Dicom or I-files
 *
 *     This program is intended to be run during a scanning session
 *     on a GE scanner, to monitor the collection of 2D image files.
 *     The user will be notified of any missing slice or any slice that
 *     is acquired out of order.
 *
 *     It is recommended that the user runs 'Dimon' when scanning
 *     begins, and then watches for error messages during the
 *     scanning session.  The user should terminate the program
 *     whey they are done with all runs.
 *
 *     - When acquiring GEMS 5.x I-files, users should run Dimon
 *       once for the entire scanning session.
 *
 *     - When acquiring DICOM image files, users should run Dimon
 *       once per run.
 *
 *     At the present time, the user must use <ctrl-c> to terminate
 *     the program.
 *
 *   usage: Dimon [options] -infile_pattern input_file_format
 *
 *   examples:    Dimon -infile_pattern 's12345/i*'
 *                Dimon -help
 *                Dimon -version
 *                Dimon -infile_prefix s12345/i -gert_create_dataset
 *                Dimon -infile_list my_files.txt -quit
 *                Dimon -infile_prefix 's12345/i' -rt -host pickle -quit
 *----------------------------------------------------------------------
*/

/* rcr - todo:
 *
 * AFNI dset:
 *    - no way to know run (effect of -nt?)
 *    - how to pass slice pattern?
 *    - oblique info
 */

#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAIN
#define IFM_PROG_NAME   "Dimon"

#include "mrilib.h"
#include "Dimon.h"      /* broken off from Imon.h */
#include "realtime.h"

/***********************************************************************/
/* globals */

IFM_debug gD;           /* debug information         */
param_t   gP;           /* main parameter struct     */
stats_t   gS;           /* general run information   */
ART_comm  gAC;          /* afni communication struct */

float     gD_epsilon       = IFM_EPSILON;
int       g_dicom_sort_dir = 1;  /* can use to swap sort direction          */
int       g_sort_by_atime  = 0;  /* choose to sort by acquisition time      */
int       g_sort_type      = 0;  /* bitmask: note fields applied in sorting */
int       g_num_slices     = 0;  /* num_slices for sort by ZPOSN            */

/***********************************************************************/

/* ---------------------------------------------------------------------------
 * file type macros/functions: GEMS, DICOM, AFNI/NIFTI     22 Jan 2013 [rickr]
 */

#ifdef IM_IS_GEMS
#undef IM_IS_GEMS
#endif
#ifdef IM_IS_DICOM
#undef IM_IS_DICOM
#endif
#ifdef IM_IS_AFNI
#undef IM_IS_AFNI
#endif

#define IM_IS_APPROX(a,b) ( fabs((a)-(b)) < gD_epsilon )

#define IM_IS_GEMS(ftype)  (ftype == IFM_IM_FTYPE_GEMS5 )
#define IM_IS_DICOM(ftype) (ftype == IFM_IM_FTYPE_DICOM )
#define IM_IS_AFNI(ftype)  (ftype == IFM_IM_FTYPE_AFNI )

static char * get_ftype_str(int ftype)
{
   if      ( ftype == IFM_IM_FTYPE_DICOM ) return "DICOM";
   else if ( ftype == IFM_IM_FTYPE_GEMS5 ) return "GEMS5";
   else if ( ftype == IFM_IM_FTYPE_AFNI  ) return "AFNI";

   return "UNKNOWN";
}

static int show_ftype(int ftype)
{
   fprintf(stderr,"-- image file type considered as '%s'\n",
           get_ftype_str(ftype));
   return 0;
}

/* set p->ftype based on name, default is DICOM */
static int set_ftype(param_t * p, char * name)
{
   if ( !name || !strcmp(name, "DICOM")) p->ftype = IFM_IM_FTYPE_DICOM;
   else if ( ! strcmp( name, "GEMS" )  ) p->ftype = IFM_IM_FTYPE_GEMS5;
   else if ( ! strcmp( name, "AFNI" )  ) p->ftype = IFM_IM_FTYPE_AFNI;
   else {
      fprintf(stderr,"** illegal file_type '%s'\n", name);
      p->ftype = IFM_IM_FTYPE_NONE;
      return 1;
   }

   if ( gD.level > 1 ) show_ftype(p->ftype);

   return 0;
}

/* ---------------------------------------------------------------------------
 * link to libmri for consistent DICOM processing           4 Jan 2010 [rickr]

#include "l_mcw_glob.h"
#include "thd_iochan.h"
#include "mri_image.h"
#include "dbtrace.h"

extern char  DI_MRL_orients[8];
extern float DI_MRL_tr;

extern struct dimon_stuff_t { int study, series, image, image_index;
                              float acq_time; } gr_dimon_stuff;
 * ---------------------------------------------------------------------------
 */

/* globals from mri_read_dicom.c */
extern int          obl_info_set;
extern int          g_is_oblique;
extern oblique_info obl_info;
extern int          g_image_ori_ind[3];
extern float        g_image_posn[3];
extern int          g_ge_me_index;
extern int          g_ge_nim_acq;
extern int          g_sop_iuid_maj;
extern int          g_sop_iuid_min;

static int          read_obl_info = 1;  /* only process obl_info once */
static int          want_ushort2float = 0;  /* 9 Jul 2013 */

static int         g_compare_by_geh = 0;

int compare_finfo_z(const void * v0, const void * v1);

static int         clear_float_zeros( char * str );
int                compare_finfo( const void * v0, const void * v1 );
int                compare_finfo_num_suff(const void * v0, const void * v1);
int                compare_by_num_suff( const void * v0, const void * v1 );
int                compare_by_geme( const void * v0, const void * v1 );
int                compare_by_sindex( const void * v0, const void * v1 );
static int         copy_dset_data(finfo_t * fp, THD_3dim_dataset * dset);
static int         copy_image_data(finfo_t * fp, MRI_IMARR * imarr);
static int         finfo_order_as_zt(param_t * p, finfo_t * flist, int n2sort);
static int         get_num_suffix( char * str );
static int         read_afni_image( char *pathname, finfo_t *fp, int get_data);
static int         read_dicom_image( char *pathname, finfo_t *fp, int get_data);
static int         sort_by_num_suff( char ** names, int nnames);
int                get_sop_iuid_index(ge_extras * ep);

/* oblique function protos */
extern void   mri_read_dicom_reset_obliquity();
extern int    mri_read_dicom_get_obliquity(float *, int);

int disp_obl_info(char * mesg);


/*----------------------------------------------------------------------*/
/* local function declarations */

static int append_new_files_to_fim_list(param_t  * p);
int append_new_finfo_entry(param_t * p, char * fname);

int compare_finfo_t            (const void * v0, const void * v1);
int nfim_in_state              (param_t * p, int start, int end, int state);
int nfim2proc                  (param_t * p);

static int check_error         ( int * retry, float tr, char * note );
static int check_im_byte_order ( int * order, vol_t * v, param_t * p );
static int check_stalled_run   ( int run, int seq_num, int naps, int max_naps,
                                 int nap_time );
static int complete_orients_str( vol_t * v, param_t * p );
int        create_dimon_file_lists(param_t * p, char * ret_fname, stats_t * ST,
                                   int rind);
static int create_file_list    ( param_t * p, char * fname, int details,
                                 char * mesg );
static int create_gert_script  ( stats_t * s, param_t * p );
static int create_gert_reco    ( stats_t * s, opts_t * opts );
static int create_gert_dicom   ( stats_t * s, param_t * p );
static int dir_expansion_form  ( char * sin, char ** sexp );
static int disp_ftype          ( char * info, int ftype );
static char * ftype_string     ( int ftype );
static char * image_state_string( int state );
static int find_first_volume   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_more_volumes   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_next_zoff      ( param_t * p, int start, float zoff, int nim,
                                 int warn );
int        free_image_memory   ( param_t * p, int first, int nim );
FILE *     get_file_pointer    (char * fname, char * mesg, int index);
static int get_sorted_file_names( param_t  * p );
static int init_extras         ( param_t * p, ART_comm * ac );
static int init_options        ( param_t * p, ART_comm * a, int argc,
                                 char * argv[] );
static int make_sorted_fim_list( param_t  * p );
static int nap_time_in_ms      ( float, float );
int        nap_for_ms          (int ms);
static int num_slices_ok       ( int, int, char * );
static int path_to_dir_n_suffix( char * dir, char * suff, char * path );
static int read_image_files    ( param_t * p );
static int read_ge_image       ( char * pathname, finfo_t * fp,
                                 int get_image, int need_memory );
static int read_file_list      ( char * inlist, char *** fnames, int * nfiles );
static int read_new_images     ( param_t * p );
static int set_nice_level      ( int level );
static int set_starting_fim    ( param_t * p );
static int set_volume_stats    ( param_t * p, stats_t * s, vol_t * v );
static int show_run_stats      ( stats_t * s );
static int str_char_count      ( char * str, int len, char target );
static int swap_4              ( void * ptr );

static void hf_signal          ( int signum );
static int string_lists_differ(string_list * L0, string_list * L1);

int        sort_by_geme_index  (param_t * p);
int        geme_find_block_end (param_t * p, int start);
int        geme_set_range_n_state(param_t * p, int start, int * pmin,
                                  int * pmax, int * nacq);
int        geme_set_sort_indices(param_t * p, int_list * ilist, int ngeme,
                                 int memin);
int        guess_predefined_nacq(param_t * p, int ind);
int        show_all_sort_methods(char * mesg, FILE * stream);
int        sort_method         (char * method);
char     * sort_method_str     (int method);
int        test_sop_iuid_index_order(param_t * p);

/* volume scanning */
int        check_one_volume    (param_t *p, int bound, int state,
                                int * r_first, int * r_last, float * r_delta);
int        update_max2read     (param_t * p, int max);
int        update_states       (param_t * p, int first, int last, int state,
                                int freemem);
static int volume_match  ( vol_t * vin, vol_t * vout, param_t * p );
static int volume_search ( vol_t * V, param_t * p, int * state );

/* information functions */
static int idisp_opts_t         ( char * info, opts_t * opt );
static int idisp_param_t        ( char * info, param_t * p );
static int idisp_finfo_t        ( char * info, finfo_t * p );
static int idisp_vol_t          ( char * info, vol_t * v );
static int idisp_ge_extras      ( char * info, ge_extras * E );
static int idisp_ge_header_info ( char * info, ge_header_info * I );
static int idisp_mosaic_info    ( char * info, mosaic_info * I );

static int usage                ( char * prog, int level );

/* local copy of AFNI function */
unsigned long l_THD_filesize( char * pathname );
static   char * l_strdup(char * text);

/*----------------------------------------------------------------------*/

/***********************************************************************/
int main( int argc, char * argv[] )
{
    ART_comm * ac = &gAC;               /* access AFNI comm as local   */
    param_t  * p  = &gP;                /* access the global as local  */
    vol_t      baseV;                   /* base volume - first scanned */
    int        ret_val;

    mainENTRY("Dimon");

    /* validate inputs and init options structure */
    if ( (ret_val = init_options( p, ac, argc, argv )) != 0 ) {
        if( ret_val > 0 ) return 0;
        else              return 1;
    }

    if ( (ret_val = init_extras( p, ac )) != 0 )
        return ret_val;

    if ( (ret_val = find_first_volume( &baseV, p, ac )) != 0 )
        return ret_val;

    if ( (ret_val = find_more_volumes( &baseV, p, ac )) != 0 )
        return ret_val;

    return 0;
}
/***********************************************************************/

/*----------------------------------------------------------------------
 * find_first_volume:   scan p->fim_o for a complete volume
 *
 * This function runs until either a volume is located, or an
 * error occurs.
 *
 * return:     0 : on success
 *          else : error
 *----------------------------------------------------------------------
*/
static int find_first_volume( vol_t * v, param_t * p, ART_comm * ac )
{
    int ret_val, n2read, n2proc;
    int sleep_ms = -1;  /* has not been set from data yet */
    int vs_state = 0;   /* state for volume search, can reset */
    int nfiles;         /* from read_image_files */

    if ( gD.level > 0 ) fprintf( stderr, "-- scanning for first volume\n" );

    /* clear initial volume */
    memset(v, 0, sizeof(vol_t));

    if ( p->opts.no_wait ) sleep_ms = 0;  /* no sleeping in this case */

    mri_read_dicom_reset_obliquity();     /* to be sure */
    MCW_set_glob_whine(0);                /* quiet any glob warnings */

    ret_val = 0;
    while ( ret_val == 0 )
    {
        /* try to read and then note what we have */
        nfiles = read_image_files( p );
        n2read = nfim_in_state(p, 0, 0, IFM_FSTATE_TO_READ);
        n2proc = nfim2proc(p);
        ret_val = nfiles;

        if ( ret_val > 0 || n2proc > 0 )
        {
            ret_val = volume_search( v, p, &vs_state );
            if(gD.level>1)
                fprintf(stderr,"-- volume search returns %d\n", ret_val);

            if ( (ret_val == 0 || ret_val == -1) && n2read > 0 ) {
               /* have images, no volume, but more to read: do it */
               if( gD.level > 2 )
                  fprintf(stderr,"-d read ims, no err, no vol, more to read\n");
               update_max2read(p, p->max2read + IFM_MAX_IM_ALLOC);
               /* skip nap if there was no error, but there is more to read */
               if( ret_val == 0 ) continue;
            }

            /* if there was an error, maybe save the current list */
            if ( ret_val < 0 && p->opts.save_errors && p->opts.flist_details )
               create_file_list(p, p->opts.flist_details, 1, "err_vsearch");

            /* try to recover from a data error */
            if ( ret_val == -1 ) ret_val = 0;
        }

        /* if no volume, sleep and continue loop */
        if ( ret_val == 0 )   /* we are not done yet */
        {
            /* no_wait: if ret_val ever repeats (including start at 0), fail
                      (might go 40, 80, 120...)          16 Feb 2012 [rickr] */
            if ( p->opts.no_wait ) {
                if ( nfiles == 0 ) { /* no new files read */
                    fprintf(stderr,
                            "\n** no_wait: no volume found in %d files\n",
                            p->nfim);
                    if( gP.opts.flist_details )
                       create_file_list(p, p->opts.flist_details, 1, "no_vol");
                    else fprintf(stderr, "   (consider -save_details)\n");
                    fputc('\n', stderr);
                    return -1;
                }
            }

            if ( p->nfim > p->opts.max_images && ! p->opts.dicom_org ) {
                fprintf(stderr,"** cannot find a volume in %d image files\n"
                               "   (max allowable = %d, see -max_images)\n",
                               p->nfim, p->opts.max_images);
                return -1;
            }

            if ( gD.level > 0 ) fprintf( stderr, "." ); /* pacifier */

            /* try to update nap time (either given or computed from TR) */
            if( sleep_ms < 0 ) {
                if( p->opts.sleep_init > 0 )
                    sleep_ms = p->opts.sleep_init;
                else if ( p->fim_update && p->fim_start > 0 )
                    /* TR option overrides image */
                    sleep_ms = nap_time_in_ms(p->opts.tr,
                                   p->fim_o[p->fim_start].geh.tr);
                else
                    sleep_ms = nap_time_in_ms(p->opts.tr, 0.0);
            }

            if( sleep_ms > 0 ) nap_for_ms(sleep_ms);
        }
        else if ( ret_val > 0 )         /* success - we have a volume! */
        {
            if ( gD.level > 0 ) {
                fprintf(stderr,"\n-- first volume found (%d slices)\n",v->nim);
                if ( gD.level > 1 ) idisp_vol_t( "+d first volume : ", v );
            }

            /* allow reading up to 4 volumes at a time */
            if( p->max2read > 0 && p->max2read < 4 * v->nim )
               update_max2read(p, 4*v->nim);

            /* use this volume to complete the geh.orients string */
            if ( complete_orients_str( v, p ) < 0 )
                return -1;

            /* use this volume to note the byte order of image data */
            if ( check_im_byte_order( &ac->byte_order, v, p ) < 0 )
                return -1;

            v->seq_num = 1;

            /* if wanted, verify afni link, send image info and first volume */
            if ( ac->state == ART_STATE_TO_OPEN )
                ART_open_afni_link( ac, 5, 0, gD.level );

            if ( ac->state == ART_STATE_TO_SEND_CTRL )
                ART_send_control_info( ac, v, gD.level );

            if ( ac->state == ART_STATE_IN_USE )
                ART_send_volume( ac, v, gD.level );

            /* update status on included images, and set new search point */
            update_states(p, v->fs_1, v->fs_1+v->nim-1, IFM_FSTATE_DONE, 1);
            p->fim_start = v->fs_1+v->nim;

            if ( gD.level > 2 ) {
               idisp_param_t( "-d first vol - new params : ", p );
               disp_ftype("-d ftype: ", p->ftype);
               ART_idisp_ART_comm( "-- first vol ", ac );
            }
        }
        else
            return ret_val; /* terminal failure */
    }

    if ( ret_val > 0 ) return 0;
    else               return ret_val;
}

int update_max2read(param_t * p, int max)
{
   if( gD.level > 2 )
      fprintf(stderr,"+d updating max2read from %d to %d\n", p->max2read, max);

   if( max <= 0 ) return 1;

   p->max2read = max;

   return 0;
}


/*----------------------------------------------------------------------
 * find_more_volumes:   given first volume, keep scanning for others
 *
 * This function runs until a fatal error occurs.
 *
 * return:     0 : on success
 *          else : error
 *----------------------------------------------------------------------
*/
static int find_more_volumes( vol_t * v0, param_t * p, ART_comm * ac )
{
    vol_t vn;
    float tr;
    int   ret_val, vmrval, done, n2proc, n2read;
    int   run, seq_num;
    int   naps;                         /* keep track of consecutive naps */
    int   nap_time;                     /* sleep time, in milliseconds    */
    int   tr_naps, max_naps;            /* naps per TR                    */
    int   ms_nap_time;                  /* nap time in ms */

    done     = 0;
    naps     = 0;

    run      = v0->run;
    seq_num  = v0->seq_num = 1;         /* set first seq_num to 1 */

    /* nap time is either given or computed */
    if      ( p->opts.tr > 0.0 ) tr = p->opts.tr;
    else if ( v0->geh.tr > 0.0 ) tr = v0->geh.tr;
    else                         tr = 2.0;

    ms_nap_time = nap_time_in_ms(p->opts.tr, v0->geh.tr);
    if( p->opts.sleep_vol > 0 ) nap_time = p->opts.sleep_vol;
    else nap_time = ms_nap_time;

    /* compute the number of naps per TR, for stalled run checks */
    tr_naps = (int)(0.9 + tr*1000.0/nap_time); /* TR to ms, then round up */

    if ( p->opts.max_quiet_trs > 0 )
         max_naps = tr_naps*p->opts.max_quiet_trs;
    else max_naps = tr_naps*IFM_MAX_RUN_NAPS;

    if ( gD.level > 0 )         /* status */
    {
        fprintf( stderr, "-- scanning for additional volumes...\n" );
        fprintf( stderr, "-- run %d: %d ", run, seq_num );
    }
    if ( gD.level > 2 )
      fprintf(stderr,"++ nap time = %d, tr_naps = %d (max quiet time %.3fs)\n",
                     nap_time, tr_naps, max_naps*nap_time/1000.0);

    /* give stats when user quits */
    signal( SIGHUP,  hf_signal );
    signal( SIGINT,  hf_signal );
    signal( SIGQUIT,  hf_signal );
    signal( SIGTRAP,  hf_signal );
    signal( SIGABRT,  hf_signal );
    signal( SIGTERM, hf_signal );
    signal( SIGSEGV,  hf_signal );

    if ( set_volume_stats( p, &gS, v0 ) ) return -1;

    while ( ! done )
    {
        /* check for volumes in all images previously read */
        vmrval = 1;
        while ( vmrval == 1 ) {
            vmrval = volume_match( v0, &vn, p );
            if(gD.level>2) fprintf(stderr,"-- vol match returns %d\n", vmrval);

            if      ( vmrval <  -3 ) return vmrval; /* bail on fatal error */
            else if ( vmrval == -3 ) continue;   /* new starting point */
            else if ( vmrval == -2 || vmrval == -1 || vmrval == 0 )
                break;                            /* new sort or more data */

            /* else, have volume */

            if ( gD.level > 3 ) idisp_vol_t( "-- new volume: ", &vn );

            if ( vn.run != run ||            /* new run?                */
                 (p->opts.nt > 1 && seq_num >= p->opts.nt) )
            {
                /* pass run and seq_num before they are updated */
                if ( ac->state == ART_STATE_IN_USE )
                    ART_send_end_of_run( ac, run, seq_num, gD.level );

                run = vn.run;               /* reset                   */
                seq_num = 1;

                if ( gD.level > 2 ) disp_obl_info("+d new run obl info: ");
                if ( gD.level > 0 )
                    fprintf( stderr, "\n-- run %d: %d ", run, seq_num );
            }
            else
            {
                seq_num++;
                if ( gD.level > 0 ) fprintf( stderr, "%d ", seq_num );
            }

            vn.seq_num = seq_num;

            if ( set_volume_stats( p, &gS, &vn ) ) return -1;

            if ( complete_orients_str( &vn, p ) < 0 ) return -1;

            if ( ac->state == ART_STATE_TO_SEND_CTRL )
                ART_send_control_info( ac, &vn, gD.level );

            /* only send good volumes to afni   - 2003.03.10 */
            if ( ac->state == ART_STATE_IN_USE )
                ART_send_volume( ac, &vn, gD.level );

            /* update status on included images, and set new search point */
            update_states(p, vn.fs_1, vn.fs_1+vn.nim-1, IFM_FSTATE_DONE, 1);
            p->fim_start = vn.fs_1+vn.nim;

            naps = 0;                       /* reset on existing volume */

            if( p->opts.pause > 0 ) nap_for_ms(p->opts.pause);
        }

        /* maybe nap before getting more data */
        n2read = nfim_in_state(p, p->fim_start, p->nfim-1, IFM_FSTATE_TO_READ);
        /* write as negation of positive no-nap tests, for clarity */
        if( ! ((vmrval == 0 && n2read > 0) || vmrval == -1 || p->opts.no_wait))
           nap_for_ms( nap_time );

        /* now we need new data - skip past last file name index */

        ret_val = read_image_files( p );
        n2proc = nfim2proc(p);
        naps = 0;                       /* do not accumulate for stall check */

        if( vmrval == -1 ) continue;    /* no napping before next check */

        /* wait while no fatal error and no full volume,
         * also, wait if we have not processed new images (full volume error?)*/
        while ( (n2proc >= 0 && n2proc < v0->nim ) || ret_val == 0 )
        {
            if ( ret_val > 0 )
                naps = 0;   /* then start over */
            else if ( p->opts.no_wait || naps > max_naps )
            {
                if ( p->opts.quit )         /* then we are outta here */
                {
                    if ( ac->state == ART_STATE_IN_USE )
                        ART_send_end_of_run( ac, run, seq_num, gD.level );

                    show_run_stats( &gS );

                    if( n2proc > 0 ) {
                        fprintf(stderr,"\n** have %d unprocessed image(s)\n",
                                n2proc);

                        if( gP.opts.flist_details )
                           create_file_list(p,p->opts.flist_details,1,"unproc");
                        else fprintf(stderr, "   (consider -save_details)\n");
                        fputc('\n', stderr);
                    }

                    return 0;
                }

                /* continue, regardless */
                if( check_stalled_run(run,seq_num,naps,max_naps,nap_time )>0 )
                    if ( ac->state == ART_STATE_IN_USE )
                        ART_send_end_of_run( ac, run, seq_num, gD.level );
            }

            if ( gD.level > 0 && !(naps % tr_naps) )
                fprintf( stderr, ". " ); /* pacifier */

            if ( ! p->opts.no_wait )
                nap_for_ms( nap_time );   /* wake after a couple of TRs */
            naps ++;

            ret_val = read_image_files( p );
            n2proc = nfim2proc(p);
        }

        if ( ret_val < 0 )               /* aaaaagh!  panic!  wet pants! */
        {
            fprintf( stderr, "\n** failure: IFM:RIF fatal error\n" );
            return -1;
        }
    }

    return 0;   /* success */
}

int nap_for_ms(int ms)
{
   if( gD.level > 2 ) fprintf(stderr,"-d napping for %d ms...\n", ms);
   iochan_sleep(ms);
   return 0;
}

/* maybe we should let volume search fail multiple times */
#undef MAX_SEARCH_FAILURES
#define MAX_SEARCH_FAILURES 1

/*----------------------------------------------------------------------
 * volume_search:   scan p->fim_o for a complete volume
 *
 *     - start should be at the expected beginning of a volume!
 *     - *fl_start may be returned as a new starting index into fnames
 *
 * state:     0 :  < 2 slices found (in the past)
 *            1 : >= 2 slices (new 'bound')
 *            2 : >= 2 slices (repeated 'bound', may be one volume run)
 *
 * return:   -2 : on programming error
 *           -1 : on data error
 *            0 : on success - no volume yet
 *            1 : on success - volume read
 *----------------------------------------------------------------------
*/
static int volume_search(
        vol_t   * V,                    /* volume to fill                 */
        param_t * p,                    /* master structure               */
        int     * state )               /* assumed state of search        */
{
    float      delta;
    int        bound;                   /* upper bound on image slice  */
    static int prev_first = -1;         /* note previous 'bound' value */
    static int prev_bound = -1;         /* note previous 'bound' value */
    static int bound_cnt  =  0;         /* allow some number of checks */
    int        first;                   /* first image                 */
    int        last;                    /* final image in volume       */
    int        n2proc, rv;

    first = p->fim_start;
    n2proc = nfim2proc(p);
    bound = p->fim_start + n2proc;

    if ( ( n2proc < 1) ||           /* from 3              8 Jul 2005 */
         ((n2proc < 4) && IM_IS_GEMS(p->ftype)) )
        return 0;                   /* not enough data to work with   */

    if( gD.level > 2 || (gD.level > 1 && prev_first >= 0) )
       fprintf(stderr,"-- VS state %d, bound %d, prev %d, first %d, pf %d\n",
               *state, bound, prev_bound, first, prev_first);

    /* maintain the state, check for 'bad' repeated entry state */
    if ( bound == prev_bound && first == prev_first) {
       if ( *state == 1 ) {
          bound_cnt++;
          /* if hit limit, try to finish */
          if( bound_cnt >= MAX_SEARCH_FAILURES ) *state = 2;
       } else if ( *state < 1 )
          *state = 1;  /* continue mode, but do not lose 2 */
    } else { /* good news, revert state and count */
       /* if no wait, exit sooner, but allow for list to grow */
       if( p->opts.no_wait ) *state = 1;
       else                  *state = 0;
       bound_cnt = 0;
    }
    prev_first = first;
    prev_bound = bound;

    rv = check_one_volume(p,bound,*state, &first,&last,&delta);

    if ( rv == 1 )
    {
        /* One volume exists from slice 'first' to slice 'last'. */

        /* note obliquity */
        if( read_obl_info ) {
            read_obl_info = 0;
            mri_read_dicom_get_obliquity(gAC.oblique_xform, gD.level>1);
            if (gD.level>2)disp_obl_info("postV mri_read_dicom_get_obliquity ");
        }

        V->geh      = p->fim_o[first].geh;         /* copy GE structure  */
        V->gex      = p->fim_o[first].gex;         /* copy GE extras     */
        V->minfo    = p->fim_o[first].minfo;       /* copy mosaic info   */
        V->nim      = last - first + 1;
        V->fs_1     = first;
        V->first_file = p->fim_o[first].fname;
        V->last_file  = p->fim_o[last].fname;
        V->z_first  = p->fim_o[first].geh.zoff;
        V->z_last   = p->fim_o[last].geh.zoff;
        V->z_delta  = delta;
        V->image_dz = V->geh.dz;
        V->oblique  = g_is_oblique;
        V->seq_num  = -1;                               /* uninitialized */
        V->run      = V->geh.uv17;

        /* store obliquity */
        if( V->oblique != gAC.is_oblique ) {
            if( gD.level > 1 )
               fprintf(stderr,"-- data is %soblique\n",V->oblique ? "":"not ");
            gAC.is_oblique = V->oblique;
        }

        return 1;
    }
    else if ( rv == 0 ) {
        return 0;                           /* we did not finish a volume */
    }
    else return -1;                         /* had some reported error */
}

/* updates states from first to last
 * (if new state is not TO_PROC and we have an image, free it) */
int update_states(param_t * p, int first, int last, int state, int freemem)
{
   int ind;

   /* update status on included images, and set new search point */
   for( ind = first; ind <= last; ind++ ) {
      p->fim_o[ind].state = state;
      if( freemem && state != IFM_FSTATE_TO_PROC && p->fim_o[ind].imdata ) {
         free(p->fim_o[ind].imdata);
         p->fim_o[ind].imdata = NULL;
      }
   }
   p->fim_start = last+1;

   return 0;
}


/* updates states from first to last */
int free_image_memory(param_t * p, int first, int nim)
{
   int ind;

   if( first < 0 ) return 1;

   /* update status on included images, and set new search point */
   for( ind = first; ind < first+nim && ind < p->nfim ; ind++ )
      if( p->fim_o[ind].imdata ) {
         free(p->fim_o[ind].imdata);
         p->fim_o[ind].imdata = NULL;
      }

   return 0;
}

/*----------------------------------------------------------------------
 * check_one_volume:   scan p->fim_o for an entire volume
 *
 * upon success, fill r_first, r_last and r_delta
 *
 * upon error (return < 0), adjust fim_start
 *
 * return:   -2 : general error
 *           -1 : found direction change
 *            0 : no error, no volume yet
 *            1 : found volume
 *----------------------------------------------------------------------
*/
int check_one_volume(param_t *p, int bound, int state,
                     int * r_first, int * r_last, float * r_delta)
{
    static int errs=0;
    finfo_t  * fp;
    float      delta, prev_z, dz;
    int        run0, run1, first, next, last;
    double     zsum;
    int        zcount, start = p->fim_start, testc;

    *r_first = *r_last = -1;  /* init return values to junk */
    *r_delta = -1.0;

    /* if state is 2 and we have only 1 or 2 images, return quickly */
    if( state == 2 && (bound-start) < 3 )
    {
        if( gD.level > 1 )
            fprintf(stderr,"-d stall after only %d slices\n", bound-start);
        *r_first = start;
        if( (bound-start) == 1 ) /* then only one slice */
        {
            *r_last = start;
            *r_delta = 1.0;  /* doesn't really matter, but 0 may be bad */
        }
        else /* 2 slices, so 1 volume of 2 slices or 2 volumes of 1 slice */
        {
            delta = p->fim_o[start+1].geh.zoff - p->fim_o[start].geh.zoff;
            if ( fabs(delta) < gD_epsilon )  /* one slice per volume */
            {
                *r_last = start;
                *r_delta = 1.0;
            }
            else                              /* two slices per volume */
            {
                *r_last = start+1;
                *r_delta = delta;
            }
        }
        return 1;  /* done */
    }
    else if ( bound-start < 3 )
        return 0;

    first = start;
    delta = p->fim_o[first+1].geh.zoff - p->fim_o[first].geh.zoff;

    run0  = p->fim_o[first  ].geh.uv17;
    run1  = p->fim_o[first+1].geh.uv17;

    /* if apparent 1-slice volume, skip and start over */
    if ( (fabs(delta) < gD_epsilon) || (run1 != run0) )
    {
        /* consider this a single slice volume */
        if ( ! IM_IS_GEMS(p->ftype) )
        {
            if( gD.level > 1 ) fprintf(stderr,"+d found single slice volume\n");
            *r_first = *r_last = first;
            *r_delta = 1.0;   /* make one up, zero may be bad */

            /* if we have a mosaic, try to set dz properly */
            if( p->fim_o[first].minfo.im_is_volume )
                *r_delta = p->fim_o[first].geh.dz;

            return 1;         /* success */
        }

        if ( gD.level > 1 )
            fprintf( stderr, "-- skipping single slice volume <%s>\n",
                     p->fim_o[first].fname );

        first++;
        delta = p->fim_o[first+1].geh.zoff - p->fim_o[first].geh.zoff;
        run0  = run1;

        if ( fabs(delta) < gD_epsilon )
        {
            fprintf( stderr, "Error: 3 slices with 0 delta, beginning with"
                     "file <%s>\n", p->fim_o[start].fname);
            p->fim_start = first + 2;
            errs = 1;
            return -1;
        }
    }

    /* --- okay, see if we can find a volume --- */

    fp = p->fim_o + first;                      /* initialize flist posn  */

    /* set current values at position (first+1) */
    fp++;
    prev_z = fp->geh.zoff;
    run1   = fp->geh.uv17;
    dz     = delta;

    /* also, modify detla to be an average across 'valid' dz, to prevent
     * exacerbating a poor initial value (for B Benson)      30 Aug 2011 */
    zsum = 0.0;
    zcount = 0;

    /* scan for volume break */
    next = first + 2;                           /* next z to look at      */
    while ( (next < bound) && (fabs(dz - delta) < gD_epsilon) &&
            (run1 == run0) )
    {
        fp++;                             /* good index so get new values */

        zsum += dz;  zcount++;            /* accumulate good values */

        dz     = fp->geh.zoff - prev_z;
        run1   = fp->geh.uv17;
        prev_z = fp->geh.zoff;

        next++;
    }

    /* now modify delta to be the mean dz, not just the first  30 Aug 2011 */
    if( zcount > 0 ) {
        if( gD.level > 2 )
            fprintf(stderr,"++ updating delta (%f) to mean dz (%f)\n",
                    delta, zsum/zcount);
        delta = zsum/zcount;
    }

    /* note final image in current volume -                        */
    /* if we left the current volume, next is too far by 2, else 1 */
    if ( (fabs(dz - delta) > gD_epsilon) || (run1 != run0) ) last = next - 2;
    else                                                     last = next - 1;

    /* set return values */
    *r_first = first;
    *r_last  = last;
    *r_delta = delta;  /* was just delta  30 Aug 2011 */

    if( gD.level > 1 )
        fprintf(stderr,"+d cov: first, last, delta, state = %d, %d, %g, %d\n",
                first, last, delta, state);

    /* If we have found the same slice location, we are done. */
    if ( fabs(fp->geh.zoff - p->fim_o[first].geh.zoff) < gD_epsilon )
    {
        /* maybe verify that we have the correct number of slices */
        if ( ! num_slices_ok(p->opts.num_slices,last-first+1,"same location") )
            return 0;

        if ( gD.level > 1 ) {
            fprintf(stderr,"+d found first slice of second volume\n");
            if( errs ) fprintf(stderr,"   (recovered after skipping images)\n");
        }
        return 1;  /* success */
    }

    /* If we are still waiting for the same location, either keep waiting
     * or figure there is only one volume to read. */
    if ( (fabs(dz - delta) < gD_epsilon) && (run1 == run0) ) {

        if( state < 2 ) return 0;  /* not done yet, keep waiting */

        /* done: we seem to have only one volume to read */

        /* maybe verify that we have the correct number of slices */
        if ( ! num_slices_ok(p->opts.num_slices,last-first+1,"data stall") )
            return 0;

        if ( gD.level > 1 )
            fprintf(stderr,"+d no new data after finding sufficient slices\n"
                           "   --> assuming completed single volume\n");
        state = 0;
        return 1;
    }

    if ( dz * delta < 0.0 ) {

        if( state < 2 ) return 0;  /* not done yet, keep waiting */

        fprintf( stderr, "\n"
                "*************************************************\n"
                "Error: missing slice(s) in first volume!\n"
                "       attempting to re-start at file: %s\n"
                "*************************************************\n",
                p->fim_o[last+1].fname);
        p->fim_start = last+1;
        errs = 1;
        state = 0;
        return -1;   /* wrong direction */
    }

    /* right direction, but bad delta, look farther for first zoff */

    for ( testc = last; testc < bound; testc++ )
       if ( abs( p->fim_o[first].geh.zoff -
                 p->fim_o[testc].geh.zoff ) < gD_epsilon )
       {
          if( state < 2 ) return 0;  /* not done yet, keep waiting */

          /* aaaaagh!  we are missing data from the first volume!   */
          /* print error, and try to skip this volume               */
          fprintf( stderr, "\n"
                  "*************************************************\n"
                  "Error: missing slice in first volume!\n"
                  "       detected    at file: %s\n"
                  "       re-starting at file: %s\n"
                  "*************************************************\n",
                  p->fim_o[last+1].fname, p->fim_o[testc].fname );

          /* try to skip this volume and recover */
          p->fim_start = testc;
          errs = 1;

          state = 0;
          return -1;
       }

    /* did not find original zoff, wait for more files */
    if ( gD.level > 1 )
        fprintf(stderr,"-d bad delta in %d of %d images from first volume\n"
                       "   --> waiting for more data\n", last, p->nfim);

    return 0;
}


/*----------------------------------------------------------------------
 * check that num_slices matches the number seen
 *
 * Setting num_slices prevents early termination of the first volume when
 * the first slice of second volume is seen before the entire first volume.
 *----------------------------------------------------------------------
*/
static int num_slices_ok( int num_slices, int nfound, char * mesg )
{
    int ok = 0;

    if( gD.level > 2 )
        fprintf(stderr,"-- num_slices_ok (%s): checking %d against %d...\n",
                mesg ? mesg : "no mesg", num_slices, nfound);

    if( num_slices <= 0 ) return 1;     /* no use means ok */

    ok = num_slices == nfound;

    if ( gD.level > 1 ) {
        if( ok )
            fprintf(stderr,"+d (%s) num_slices found matches option, %d\n",
                    mesg ? mesg : "no mesg", nfound);
        else
            fprintf(stderr,"+d (%s) num_slices found (%d) does not match"
                           " option (%d), still waiting...\n",
                    mesg ? mesg : "no mesg", nfound, num_slices);
    }

    return ok;
}


/*----------------------------------------------------------------------
 * volume_match:   scan p->fim_o for a matching volume
 *
 *     - start should be at the expected beginning of a volume!
 *
 * return:   -4 : fatal error: die
 *           -3 : small error: no data check needed
 *           -2 : small error: want to nap
 *           -1 : small error: no nap needed
 *            0 : success:     no volume yet
 *            1 : success:     volume read
 *----------------------------------------------------------------------
*/
static int volume_match( vol_t * vin, vol_t * vout, param_t * p )
{
    static int   retry = IFM_NUM_RETRIES, errs=0;       /* v2.12 */
    finfo_t    * fp=NULL;
    finfo_t    * fp_test;
    char       * estr;
    float        z, zind;
    int          count, start, next_start = -1;
    int          n2proc;

    if ( vin == NULL || vout == NULL || p == NULL )
    {
        fprintf( stderr, "failure: FMV: bad parameter data\n" );
        return -4;
    }

    start = p->fim_start;
    n2proc = nfim2proc(p);

    if ( n2proc < vin->nim )        /* enough files to scan? */
        return 0;

    /* now 'everything' should match */

    /* just see whether all slices match */
    for ( count = 0; count < vin->nim; count++ ) {
        fp = p->fim_o + start + count;
        z = vin->z_first + count * vin->z_delta;        /* note expected z */

        /* if all is well, continue */
        if ( fabs( z - fp->geh.zoff ) <= gD_epsilon ) continue;

        /**************** Houston, we have a problem ****************/

        /* if there was an error, maybe save the current list */
        if ( p->opts.save_errors && p->opts.flist_details )
           create_file_list(p, p->opts.flist_details, 1, "err_vmatch");

        /* note the index we appear to be looking at */
        /* (z_delta != 0, else we would not be in loop) */
        zind = (fp->geh.zoff - vin->z_first)/vin->z_delta;

        if ( p->opts.no_wait || !retry ) {
           fprintf(stderr,"\n** volume match failure (bad zoff %f != %f)\n"
                          "   (estimated index %f, want index %d)\n",
                   fp->geh.zoff, z, zind, count);
        }

        if ( p->opts.no_wait ) {
         fprintf(stderr,"   (for more details, replace -no_wait with -quit)\n");
         return -3;
        }

        /* if we seem to be outside volume, whine and banish slice */
        if( zind < -0.2 || zind > (vin->nim-0.8) ) {
           fprintf(stderr, "   - zoffset %f outside volume range of %f to %f\n"
                           "   - banishing image %s to FAILED state\n"
                           "   --> will try to continue...\n\n",
                   fp->geh.zoff, vin->z_first,
                   vin->z_first + (vin->nim-1)*vin->z_delta, fp->fname);
           fp->state = IFM_FSTATE_FAILED;
           errs = 2; /* printed some error */
           return -2;
        }

        /* search for a next starting point */
        next_start = find_next_zoff(p, start+count, vin->z_first, vin->nim, 1);
        fp_test = fp + 1;

        if ( fabs( z - fp_test->geh.zoff ) < gD_epsilon )
           estr = "slice out of order";
        else if ( fabs(z + vin->z_delta - fp->geh.zoff) < gD_epsilon )
           estr = "slice missing";
        else        /* unknown error - find start of next volume */
           estr = "volume severely toasted";

        errs = 1; /* error not yet printed */

        /* if there are retries, wait and see whether we recover */
        if ( !check_error(&retry, vin->geh.tr, estr) ) return -2;

        /* report error and try to recover */
        IFM_BIG_ERROR_MESG( estr, fp->fname, z, fp->geh.zoff,
                            fp->geh.uv17, count + 1, vin->nim );

        errs = 2; /* printed some error */

        if( next_start >= 0 ) {
            fprintf(stderr,"++ attempting re-start at image %s\n"
                    "   (banishing %d images to FAILED state, from index %d)\n",
                    p->fim_o[next_start].fname, next_start-start, start);
            update_states(p, start, next_start-1, IFM_FSTATE_FAILED, 1);
            return -1;
        } else if ( gD.level > 2 )
            fprintf(stderr,"-d doing nothing; hope life is better later...\n");

        return -3;
    }

    /* check mosaic case */
    if( fp->minfo.im_is_volume ) {
        if( fp->minfo.nslices != vin->minfo.nslices ||
            fp->minfo.mos_nx  != vin->minfo.mos_nx  ||
            fp->minfo.mos_ny  != vin->minfo.mos_ny  ) {
            fprintf(stderr, "** volume mis-match, not sure how to proceed!\n");
            fprintf(stderr, "   im_is_volume = %d, %d\n",
                    fp->minfo.im_is_volume, vin->minfo.im_is_volume);
            fprintf(stderr, "   nslices   = %d, %d\n",
                    fp->minfo.nslices, vin->minfo.nslices);
            fprintf(stderr, "   (nx, ny)  = (%d, %d), (%d, %d)\n",
                    fp->minfo.mos_nx, fp->minfo.mos_ny,
                    vin->minfo.mos_nx, vin->minfo.mos_ny);
            return -4;
        }
    }

    if ( errs ) { /* just state recovery */
      if( gD.level > 0 ) {
         if( errs == 1 ) fprintf(stderr," (retry OK - no errors)\n");
         else fprintf(stderr,"++ recovered after skipping images\n");
      }
      errs = 0;

      if ( p->opts.save_errors && p->opts.flist_details )
         create_file_list(p, p->opts.flist_details, 1, "recover_vmatch");
    }

    retry = IFM_NUM_RETRIES;    /* next error starts over */

    /* fill volume structure */

    vout->geh      = p->fim_o[start].geh;
    vout->gex      = p->fim_o[start].gex;
    vout->minfo    = p->fim_o[start].minfo;
    vout->nim      = vin->nim;
    vout->fs_1     = start;
    vout->first_file = p->fim_o[start].fname;
    vout->last_file  = p->fim_o[start+vout->nim-1].fname;
    vout->z_first  = vin->z_first;
    vout->z_last   = vin->z_last;
    vout->z_delta  = vin->z_delta;
    vout->image_dz = vout->geh.dz;
    vout->oblique  = g_is_oblique;
    vout->seq_num  = -1;                                /* uninitialized */
    vout->run      = vout->geh.uv17;

    if( gD.level > 2 )
       fprintf(stderr,"++ volume match: have volume from index %d to %d\n",
               start, start+vout->nim-1);

    return 1;
}

/*----------------------------------------------------------------------
 * check_error:         only report error on second failure     v2.12
 *
 * return:  < 0  : on programming error
 *            0  : do not report error yet
 *            1  : report error
 *----------------------------------------------------------------------
*/
static int check_error( int * retry, float tr, char * note )
{
    int nap_time;

    if ( retry < 0 ) return -1;

    if ( *retry > 0 )
    {
        /* let user know we're checking */
        if ( gD.level > 0 )
            fprintf(stderr," (volume retry test for warning '%s'...)\n",
                    CHECK_NULL_STR(note));

        *retry -= 1;

        /* sleep time is either given or computed from some TR */
        nap_time = (gP.opts.sleep_vol > 0) ? gP.opts.sleep_vol :
                                             nap_time_in_ms(gP.opts.tr, tr);

        if ( ! gP.opts.no_wait ) nap_for_ms( nap_time );
        return 0;
    }

    /* so calling function should print error, but start over */

    *retry = IFM_NUM_RETRIES;

    return 1;
}


/*----------------------------------------------------------------------
 * read_image_files:
 *
 * - read file list (MCW_file_expand or name list)
 *    - possibly sort (based on name only)
 *    - maintain previous (sorted?) result
 * - for each new file name, append to list of unsorted fileimage structs
 * - read new images (those added, and any previous failures)
 *    - limit number of failures
 *    - limit number of newly read (based on nz * nchan, else default?)
 *    - a hard limit on number read would be possible with frequent reading
 *      (so a TR's worth could be accumulated, say)
 * - apply any sort based on image info: fim_o => fim->s
 * - process unprocessed images
 *
 * return: < 0 : on error
 *           0 : nothing to process
 *           1 : something new happened
 *----------------------------------------------------------------------
*/
static int read_image_files(param_t * p)
{
    static int first = 1;    /* note first pass */
    int        newstuff;

    newstuff = get_sorted_file_names(p);
    if( newstuff < 0 ) return -1; /* failure */

    if( newstuff && string_lists_differ(&p->fnames_cur, &p->fnames_prev) ) {
       newstuff = append_new_files_to_fim_list(p); /* newstuff = num added */
       if( newstuff < 0 ) return -1;
    }

    /* maybe we are waiting for a starting file */
    if( newstuff && p->fim_skip < 0 ) {
       if( set_starting_fim(p) < 0 ) return -1;
    }

    if( p->fim_skip < 0 ) return 0; /* still waiting for starting file */

    /* dicom_org: remove any limit on images processed */
    if( p->opts.dicom_org ) p->max2read = -1;

    /* now actually try to read new images, starting from fim_skip    */
    /* note: this implies wherether there is something new to process */
    newstuff = read_new_images(p);
    if( newstuff < 0 ) return -1;

    if( gP.opts.flist_details && newstuff && first )
       create_file_list(p, p->opts.flist_details, 1, "first_read");

    /* fill fim_o, set fim_start */
    if( make_sorted_fim_list(p) < 0 ) return -1;

    if( gP.opts.flist_details && newstuff && first )
       create_file_list(p, p->opts.flist_details, 1, "first_sort");

    if(gD.level > 1)
       fprintf(stderr,
          "-- read_image_files: %d images, start = %d, proc = %d, tot = %d\n",
          newstuff, p->fim_start, nfim2proc(p), p->nfim);
    if ( gD.level > 3 ) idisp_param_t( "end read_image_files : ", p );

    if( newstuff ) first = 0;

    /* may be negative for an error condition */
    return newstuff;
}


/*----------------------------------------------------------------------
 * sort fim_o and set fim_start
 *
 * - fim_o should be left static for the list of processed images
 *    - i.e. any index below fim_start
 *    - includes FAILED, SKIP, UNKNOWN, DONE
 * - beyond that, apply sort
 *    - states <= DONE should trickle to the beginning
 *      (every such image should be an error condition)
 *    - TO_READ should trickle to the end (for reading later)
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
static int make_sorted_fim_list(param_t  * p)
{
   static int first = 1;
   finfo_t  * fp;
   int        index, n2sort, method;

   /* The first fim_start pointers should be left untouched.
    * There should be no _DONE structures above that point, as
    * fim_start should be incremented with completed volumes.
    * Sort the remaining pointers pulling any "completed" ones
    * down early (FAILED, SKIP, UNKNOWN, DONE (should not happen)).
    *
    * For now, warn about any images that are not TO_PROC or _READ.
    */

   n2sort = p->nfim - p->fim_start;

   if( n2sort <= 0 ) {
      if( gD.level > 2 ) fprintf(stderr,"-- fim_o: no sorting to do\n");
      return 0;
   }

   method = sort_method(p->opts.sort_method);
   /* if unspecified, other options might be applied */
   if( method == IFM_SORT_UNSPEC ) {
      if( p->opts.sort_acq_time )      method = IFM_SORT_ACQ_TIME;
      else if( p->opts.sort_num_suff ) method = IFM_SORT_NUM_SUFF;
      else if( p->opts.dicom_org )     method = IFM_SORT_DEFAULT;
   }

   if( gD.level > 2 || (first && gD.level > 1) ) {
      fprintf(stderr,"-- fim_o: sorting image list via method %s\n"
                     "   (%d images from %d to %d)\n",
              sort_method_str(method), n2sort, p->fim_start, p->nfim-1);
      first = 0;
   }

   /*-- sort from offset fim_start --*/
   fp = p->fim_o + p->fim_start;

   /* apply requested sorting method            15 Aug 2014 [rickr] */
   switch( method ) {
      /* return SUCCESS on no sort              19 Apr 2016 [rickr] */
      default: return 0; /* UNKNOWN, UNSPEC or NONE: no sorting */

      case IFM_SORT_ACQ_TIME: {
         g_sort_by_atime = 1;
         qsort(fp, n2sort, sizeof(finfo_t), compare_finfo_t);
         break;
      }
      case IFM_SORT_GEME:    {
         sort_by_geme_index(p);
         break;
      }
      case IFM_SORT_DEFAULT: {
         qsort(fp, n2sort, sizeof(finfo_t), compare_finfo_t);
         break;
      }
      case IFM_SORT_NUM_SUFF:{
         qsort(fp, n2sort, sizeof(finfo_t), compare_finfo_num_suff);
         break;
      }
      case IFM_SORT_ZPOSN:   {
         /* need nz or just 1 vol */
         g_num_slices = p->opts.num_slices;
         qsort(fp, n2sort, sizeof(finfo_t), compare_finfo_z);
         break;
      }
   }

   if( p->opts.order_as_zt ) finfo_order_as_zt(p, fp, n2sort);

   /* after sorting, increment starting index to first TO_PROC (or later)
    * image (if not found, fim_start should be left unchanged) */
   fp = p->fim_o + p->fim_start; /* might have changed since first set */
   for( index = 0; index < n2sort; index++, fp++ ) {
      if( fp->state >= IFM_FSTATE_TO_PROC ) {
         /* if we skipped something, whine one last time */
         if( index > 0 ) {
            if( gD.level > 1 )
               fprintf(stderr,"-- incrementing post-sort fim_start by %d\n",
                       index);
            p->fim_start += index;
         }
         break;
      }
      if( gD.level > 0 )
        fprintf(stderr,
                "** bad image state: ind %d, errs %d, state %d (%s), file %s\n",
                fp->findex, fp->bad_reads, fp->state,
                image_state_string(fp->state), fp->fname);
   }

   return 0;
}

/* change order from slice-major (time changes fast, slices slow) to
 * time-major order (slices change fast, time slow)
 *
 * - allocate and free memory to perform the change
 * - return 0 on success
 */
static int finfo_order_as_zt(param_t * p, finfo_t * flist, int n2sort)
{
   static int   nentry=0;
   finfo_t    * fnew;
   float        zoff;
   int          nt, ns, iold, inew, tind, ok;

   if( n2sort < 3 ) return 0;

   zoff = flist[0].geh.zoff;
   for( nt=1; nt < n2sort && IM_IS_APPROX(zoff, flist[nt].geh.zoff); nt++ )
      /* nada */ ;
   ns = n2sort / nt;  /* apparent number of slices (truncated ratio) */
   ok = (p->opts.num_slices == 0) || (p->opts.num_slices == ns);
   if( ns*nt != n2sort) ok = 0;

   /* debug info */
   if( gD.level > 0 ) {
      if( gD.level > 3 || ! ok)
         fprintf(stderr,"\n++ order_as_zt: have nt=%d, n2sort=%d, ns=%d\n",
                 nt, n2sort, ns);
      if( ! ok && p->opts.num_slices != ns )
         fprintf(stderr,"** order_as_zt: num_slices=%d, but computed ns=%d\n",
                 p->opts.num_slices, ns);
      else if( ! ok )
         fprintf(stderr,"** order_as_zt: (ns=%d*nt=%d = %d) != n2sort %d\n",
                 ns, nt, ns*nt, n2sort);
   }

   /* noting to do? */
   if( nt < 2 ) return 0;

   /* whine if we try to do things more than once */
   nentry++;
   if( nentry > 1 && nentry <= 10 ) {
      fprintf(stderr,"** warning, entering order_as_zt %d times\n", nentry);
      if( nentry == 1 && p->max2read > 0 )
         fprintf(stderr,"   ==> consider using -dicom_org or -read_all\n");
      if( nentry == 10 )
         fprintf(stderr,"   no more such warnings will be printed ...\n");
   }

   /* allocate temporary copy */
   fnew = (finfo_t *)calloc(n2sort, sizeof(finfo_t));
   if( !fnew ) {
      fprintf(stderr,"** failed to allocate %d structs for order_as_zt\n",
              n2sort);
      return 1;
   }

   /* re-order from slice-major to time-major order */
   tind = 0;  /* saved time index, will go from 0 to nt-1 */
   inew = 0;  /* new position, will go from 0 to n2sort-1 */
   while( inew < n2sort ) {
      for( iold=tind; iold < n2sort; iold+=nt, inew++ ) {
         fnew[inew] = flist[iold];
         if( gD.level > 3 ) fprintf(stderr,"   %d -> %d\n", iold, inew);
      }
      tind++;
   }

   if( inew != n2sort )
      fprintf(stderr,"** order_as_zt: counted %d moves in list of %d\n",
              inew, n2sort);

   /* move back to original list */
   memcpy(flist, fnew, n2sort * sizeof(finfo_t));

   free(fnew);

   return 0;
}


/* sort finto_t structs by GE multi-echo index scheme
 *
 * alphabetical, but for each grouping of ge_me_index values, sort by that
 *
 * - find minimum (or repeat)
 */
int sort_by_geme_index(param_t * p)
{
   static int_list ilist = { 0, 0, NULL };
   static int      nwarn=1, s_geme_min = -1;
   static int      snacq = -1;       /* read or computed ge_nim_acq */
   int             n2sort, min, max, ngeme, nt;

   if( gD.level > 2 )fprintf(stderr,"-- sorting by GE ME index...\n");
   n2sort = p->nfim - p->fim_start;
   if( n2sort <= 0 ) return 0;  /* nothing to do */

   /* set geme value range and sates to TO_SORT (if TO_PROC) */
   if( geme_set_range_n_state(p, p->fim_start, &min, &max, &snacq) ) return 0;
   ngeme = max-min+1;
   if( ngeme <= 0 ) return 0;
   if( ngeme < snacq ) {
      if( nwarn > 0 ) {
        fprintf(stderr,"-- GE ME index count, only %d of %d, waiting...\n",
                       ngeme, snacq);
        nwarn--;
      }
      return 0;
   }
   if( ngeme > 10000 ) {
      if( gD.level > 0 ) fprintf(stderr,"** SBGI: bad ngeme = %d\n", ngeme);
      return 0;
   }
   if( nwarn == 0 ) {
      fprintf(stderr,"-- GE ME index count, have all\n");
      if( gD.level > 1 ) nwarn = 1; /* start over */
      else               nwarn--;   /* no more messages */
   }

   /* be sure we have enough space for counters */
   if( snacq > ilist.nall ) resize_int_list(&ilist, snacq);

   /* and track overall minimum */
   if( s_geme_min == -1 ) {
      if( gD.level > 1 ) fprintf(stderr,"-- have initial geme_min = %d\n", min);
      s_geme_min = min;
   }
   else if ( min < s_geme_min ) {
      fprintf(stderr,"-- update in geme_min from %d to %d\n", s_geme_min, min);
      s_geme_min = min;
   } else if ( min > s_geme_min ) {
      fprintf(stderr,"** found geme_min of %d, expected %d\n", min,s_geme_min);
      /* rcr - check that max-s_geme_min+1 <= ilist.nall ? */
   }

   clear_int_list(&ilist);
   ilist.num = ngeme;
   /* set sindex values and states of ready image sets to TO_PROC */
   nt = geme_set_sort_indices(p, &ilist, ngeme, min);
   if( nt < 0 ) return -1;

   /* and finally sort */
   qsort(p->fim_o+p->fim_start, n2sort, sizeof(finfo_t), compare_by_sindex);

   return 0;
}

/* set sort indices and then set "ready" images to TO_PROC state
 *
 *   for all images with state == TO_SORT:
 *      mebase = geme_index - memin
 *      set sindex to list[mebase]*ngeme + mebase
 *         (offset sindex by p->fim_start)
 *      list[mebase]++
 *
 *   nfull = min(ilist->list from 0 to ngeme-1)
 *
 *   for all images with state == TO_SORT:
 *      if sindex <= nfull*ngeme, set state = TO_PROC
 *
 * return nfull
 */
int geme_set_sort_indices(param_t * p, int_list * ilist, int ngeme, int memin)
{
   finfo_t * fp;
   int ind, mebase, nfull, nset, max_sind;

   fp = p->fim_o + p->fim_start;
   for( ind = p->fim_start; ind < p->nfim; ind++, fp++ ) {
      if( fp->state != IFM_FSTATE_TO_SORT ) {
if( fp->state < IFM_FSTATE_TO_SORT )
   fprintf(stderr,"== GSSI: skip state %d image %s\n", fp->state, fp->fname);
continue;
}
      mebase = fp->gex.ge_me_index - memin;

      /* this should be a real, global sorted index (of TO_PROC images) */
      fp->sindex = ilist->list[mebase]*ngeme + mebase + p->fim_start;
      ilist->list[mebase]++;
   }

   /* get minimum ilist value, which is the number of full sort groups */
   /* (i.e. the number of complete TRs) */
   nfull = ilist->list[0];
   for( ind = 1; ind < ngeme; ind++ )
      if( ilist->list[ind] < nfull ) nfull = ilist->list[ind];

   fp = p->fim_o + p->fim_start;
   nset = 0;
   max_sind = nfull*ngeme + p->fim_start;
   for( ind = p->fim_start; ind < p->nfim; ind++, fp++ ) {
      if( fp->state == IFM_FSTATE_TO_PROC )
         fprintf(stderr,"** GSSI: unexpected TO_PROC for %s\n", fp->fname);

      if( fp->state == IFM_FSTATE_TO_SORT && fp->sindex < max_sind ) {
         fp->state = IFM_FSTATE_TO_PROC;
         nset++;
      }
   }

   if( gD.level > 1 )
      fprintf(stderr,"-- GEME: preparing %d apparent TRs for sorting\n", nfull);
   if( (nfull && gD.level > 1) || gD.level > 2 )
      fprintf(stderr,"   (memin %d, ngeme %d, fim_start %d, nfim %d, nset %d,"
                     " max_sind %d\n",
              memin, ngeme, p->fim_start, p->nfim, nset, max_sind);

   return nfull;
}


/* for all images, if state >= TO_PROC and state < TO_READ,
 * set state to TO_SORT
 * if nacq == -1, try to init from gex.ge_nim_acq
 *
 * note: there should really not be any TO_PROCs from previous sorts,
 *       as anything set to that should have a complete volume
 *
 * return min and maximum ge_me_index
 */
int geme_set_range_n_state(param_t * p, int start, int * pmin, int * pmax,
                           int * nacq)
{
   int ind, min, max, newnacq, meind, state;

   if( start >= p->nfim ) return 0;     /* nothing to do */

   /* see if there is a starting point */
   for( ind = start; ind < p->nfim; ind++ ) {
      state = p->fim_o[ind].state;
      if( state >= IFM_FSTATE_TO_PROC && state < IFM_FSTATE_TO_READ )
         break;
   }
   if( ind >= p->nfim ) {
      *pmin = -1;
      *pmax = -1;
      return 0;       /* no starting point */
   }

   /* if we need nacq, see if it is available */
   if( *nacq < 0 ) *nacq = guess_predefined_nacq(p, ind);

   /* we have at least one image to work with, get min,max */
   meind = p->fim_o[ind].gex.ge_me_index;
   min = max = meind;
   /* ind is still set from above */
   for( /* nada */ ; ind < p->nfim; ind++ ) {
      /* maybe we skip this entry */
      state = p->fim_o[ind].state;
      if( state < IFM_FSTATE_TO_PROC || state >= IFM_FSTATE_TO_READ )
         continue;

      /* set state, get ge_me_index */
      p->fim_o[ind].state = IFM_FSTATE_TO_SORT;
      meind = p->fim_o[ind].gex.ge_me_index;

      /* maybe we init min/max */
      if     ( meind  < min ) min = meind;
      else if( meind  > max ) max = meind;
   }

   *pmin = min;
   *pmax = max;

   /* maybe set nacq based on range of values */
   newnacq = max - min + 1;
   if( newnacq > 0 ) {
      if( *nacq < 0 ) {
         if( gD.level > 0 )
            fprintf(stderr,"++ setting nacq: have ngeme = %d\n", newnacq);
         *nacq = newnacq;
      } else if ( newnacq > *nacq ) {
         if( gD.level > 1 )
           fprintf(stderr,"++ setting nacq: have updated ngeme = %d\n",newnacq);
         *nacq = newnacq;
      } else if ( gD.level > 2 )
         fprintf(stderr, "-- GSRNS: have new nacq = %d, over ims %d to %d\n",
                 newnacq, start, p->nfim);
   }

   return 0;
}

int guess_predefined_nacq(param_t * p, int ind)
{
   int nacq = -1, tacq;

   if( p->fim_o[ind].gex.ge_nim_acq > 0 ) {
      nacq = p->fim_o[ind].gex.ge_nim_acq;
      if( gD.level > 0 )
         fprintf(stderr,"++ setting nacq: have ge_nim_acq = %d\n", nacq);
   }

   if( p->opts.num_slices > 0 && p->opts.num_chan > 0 ) {
      tacq = p->opts.num_slices * p->opts.num_chan;
      if( nacq < 0 ) {
         if( gD.level > 0 )
            fprintf(stderr,"++ setting nacq: slices x nchan = %d\n", tacq);
      } else if ( tacq != nacq )
         fprintf(stderr,"++ setting nacq: slices x nchan != ge_nim_acq "
                 "(%d x %d != %d)\n"
                 "   --> using slices x nchan = %d\n",
                 p->opts.num_slices, p->opts.num_chan, nacq, tacq);
      nacq = tacq;
   }

   if( nacq > 10000 )
      fprintf(stderr,"** ge_nim_acq = %d, this seems too large\n", nacq);

   return nacq;
}

/* find repeat of minimum, or end
 *
 * if a repeat of min or max is found,
 *    min: back up until to previous RIN
 *    max: go forward until different RIN
 * (else, return nim-1)
 */
int geme_find_block_end(param_t * p, int start)
{
   int c, c2, meind = -1, memin, memax, imax;

   /* rcr - enhance this to deal with missing data */

   if( start >= p->nfim ) return start-1;

   memin = p->fim_o[start].gex.ge_me_index;
   memax = memin;
   imax  = start;

   for( c = start+1; c < p->nfim; c++ ) {
      meind = p->fim_o[c].gex.ge_me_index;

      if      ( meind  < memin ) memin = meind;
      else if ( meind  > memax ) {
         memax = meind;
         imax = c;      /* also need to know previous max location */
      }
      else if ( meind == memin ) break;
      else if ( meind == memax ) break;
   }

   /* if end of list, we are done */
   if( c >= p->nfim ) return p->nfim-1;

   /* if min match, back up until previous RIN */
   if( meind == memin ) {
      c2 = c;
      c--; /* was previously beyond a good return value */
      if( c2 < p->nfim ) {
         while( c > start &&
                p->fim_o[c].geh.index == p->fim_o[c2].geh.index ) c--;
      }
      return c;
   }

   /* if max match, start from previous and go forward until RIN */
   if( meind == memax ) {
      c2 = imax;
      c = c2+1;
      while( c < p->nfim-1 &&
             p->fim_o[c].geh.index == p->fim_o[c2].geh.index ) c++;
      if( c < p->nfim-1 ) c--;  /* last before change */
      return c;
   }

   /* error */
   fprintf(stderr,"** GEME_FBE: bad finish, s=%d, c=%d, n=%d\n",
           start,c,p->nfim);

   return -1;
}


/* count states from start to end (use defaults if start==end) */
int nfim_in_state(param_t * p, int start, int end, int state)
{
   int ind, scount;
   int i0, i1;

   if( start == end ) {
      i0 = p->fim_start;
      i1 = p->nfim-1;
   }
   else {
      i0 = start;
      if( i0 < 0 ) i0 = 0;
      i1 = end;
      if( i1 >= p->nfim-1 ) i1 = p->nfim-1;
   }

   if( p->nfim <= 0 || start >= p->nfim ) return 0;

   scount = 0;
   for( ind = i0; ind <= i1; ind++ )
      if( p->fim_o[ind].state == state ) scount++;

   return scount;
}


/* like above, but be faster assuming sort order */
int nfim2proc(param_t * p)
{
   int ind;
   if( p->nfim <= 0 || p->fim_start >= p->nfim ) return 0;

   for( ind = p->fim_start; ind < p->nfim; ind++ )
      if( p->fim_o[ind].state != IFM_FSTATE_TO_PROC ) break;
   return ind - p->fim_start;
}


/*----------------------------------------------------------------------
 * compare function for 2 pointers to finfo_t structs
 *
 * The purpose is mostly to sort TO_PROC images.
 *   - if FAILED, SKIP, UNKNOWN (?) or DONE, we are done (so go first)
 *   - TO_READ, we are not ready to sort (so go last)
 *   - else, TO_PROC, where the real sorting occurs
 *
 * - if states differ, sort on state
 *   (state values are arranged to make sorting appropriate)
 * - if state != TO_PROC, sort on findex (i.e. leave as found)
 * -
 * state: FAILED, SKIP, DONE, UNKNOWN (should not happen) TO_PROC, TO_READ
 *
 * return < 0, 0, > 0, according to direction of p0 vs p1
 *----------------------------------------------------------------------
*/
int compare_finfo_t(const void * v0, const void * v1)
{
   finfo_t * p0 = (finfo_t *)v0;
   finfo_t * p1 = (finfo_t *)v1;

   /* if states differ, just sort by state */
   if( p0->state != p1->state ) return p0->state - p1->state;

   /* if state is not TO_PROC, sort by findex */
   if( p0->state != IFM_FSTATE_TO_PROC ) return p0->findex - p1->findex;

   /* both states are TO_PROC, apply desired sorting here */

   /* default to the old comparisons, if requested (via -dicom_org) */
   return compare_finfo(p0, p1);
}

/* sort on z-coordinates (multi-volume uses modulo nz) */
int compare_finfo_z(const void * v0, const void * v1)
{
   finfo_t * p0 = (finfo_t *)v0;
   finfo_t * p1 = (finfo_t *)v1;
   int       dir = g_dicom_sort_dir;

   /* if states differ, just sort by state */
   if( p0->state != p1->state ) return p0->state - p1->state;

   /* if state is not TO_PROC, sort by findex */
   if( p0->state != IFM_FSTATE_TO_PROC ) return p0->findex - p1->findex;

   /* both states are TO_PROC, apply desired sorting here */

   /* if we know the number of slices, only compare z in same slice set */
   if( g_num_slices > 0 ) {
      int v0 = p0->findex / g_num_slices;
      int v1 = p1->findex / g_num_slices;
      if( v0 < v1 ) return -dir;
      if( v0 > v1 ) return  dir;
   }

   /* otherwise, just return based on the difference */
   if( p0->geh.zoff < p1->geh.zoff ) return -dir;
   if( p0->geh.zoff > p1->geh.zoff ) return  dir;

   return p0->findex - p1->findex; /* backup case */
}



/*----------------------------------------------------------------------
 * like comp_by_num_suff, but via finfo_t struct
 *
 *   - find names
 *   - return compare_by_num_suff()
 *
 * return < 0, 0, > 0, according to direction of p0 vs p1
 *----------------------------------------------------------------------
*/
int compare_finfo_num_suff(const void * v0, const void * v1)
{
   finfo_t * p0 = (finfo_t *)v0;
   finfo_t * p1 = (finfo_t *)v1;

   /* if states differ, just sort by state (but allow in TO_PROC..TO_READ) */
   if( p0->state != p1->state ) {
      if( p0->state < IFM_FSTATE_TO_PROC || p0->state > IFM_FSTATE_TO_READ )
         return p0->state - p1->state;
      if( p1->state < IFM_FSTATE_TO_PROC || p1->state > IFM_FSTATE_TO_READ )
         return p0->state - p1->state;

      return compare_by_num_suff(&p0->fname, &p1->fname);
   }

   /* so same state: if not TO_PROC, sort by findex */
   if( p0->state != IFM_FSTATE_TO_PROC ) return p0->findex - p1->findex;

   return compare_by_num_suff(&p0->fname, &p1->fname);
}


/*----------------------------------------------------------------------
 * any updates from fnames_prev to fnames_cur get appended to fim_o
 *
 * If fnames_cur is not a superset of fnames_prev, it is an error.
 *
 * return -1 on error, else the number of new files
 *----------------------------------------------------------------------
*/
static int append_new_files_to_fim_list(param_t  * p)
{
   char ** f_cur, ** f_prev;
   int     cind, pind, nadded = 0;

   if( gD.level > 4 ) fprintf(stderr,"-- append fnames...\n");

   f_cur  = p->fnames_cur.list;  /* convenience */
   f_prev = p->fnames_prev.list; /* convenience */

   /* for each entry in prev, find in cur, adding those missing */
   cind = pind = 0;
   while( pind < p->fnames_prev.num ) {
      /* go until we see the old fname */
      while( cind < p->fnames_cur.num && strcmp(f_cur[cind], f_prev[pind]) ) {
         nadded++;
         if( append_new_finfo_entry(p, f_cur[cind]) ) return -1;
         cind++;
      }
      if( cind >= p->fnames_cur.num ) {
         fprintf(stderr, "** old fname '%s' not found in new list\n",
                 f_prev[pind]);
         return -1;
      }
      pind++;
      cind++;
   }

   /* and add the rest */
   nadded += p->fnames_cur.num - cind;
   while( cind < p->fnames_cur.num )
      if( append_new_finfo_entry(p, f_cur[cind++]) ) return -1;

   if( gD.level > 3 || (gD.level > 2 && nadded) )
      fprintf(stderr,"-- appended %d fnames to fim_o\n", nadded);

   return nadded;
}

/* append new finfo entry to both fim_o and fim_o
 *
 * The fim_o list never changes besides these appends.
 *
 * The fim_o list gets sorted starting at fim_start.
 * Note that append should always work.
 *
 */
int append_new_finfo_entry(param_t * p, char * fname)
{
   if( gD.level > 3 ) fprintf(stderr,"++ creating new finfo for %s\n", fname);

   /* might need to allocate space */
   if( p->nfim >= p->nfalloc ) {
      int step = 50;
      p->nfalloc += step;
      p->fim_o = (finfo_t *)realloc(p->fim_o, p->nfalloc*sizeof(finfo_t));
      if( ! p->fim_o ) {
         fprintf(stderr,"** failed to alloc %d finfo structs\n", p->nfalloc);
         return 1;
      }
      /* clear the new memory */
      memset(p->fim_o + p->nfim, '\0', step*sizeof(finfo_t));
   }

   /* init next struct */
   p->fim_o[p->nfim].findex = p->nfim;
   p->fim_o[p->nfim].sindex = -1;
   p->fim_o[p->nfim].state  = IFM_FSTATE_TO_READ;
   p->fim_o[p->nfim].fname  = strdup(fname);
   p->fim_o[p->nfim].imdata = NULL;

   p->nfim++;

   return 0;
}


/*----------------------------------------------------------------------
 * get_sorted_file_names:
 *
 * populate fnames_prev and _cur
 * possibly set fnames_done
 *
 * return -1 on error
 *         0 if nothing was done
 *         1 something was read
 *----------------------------------------------------------------------
*/
static int get_sorted_file_names ( param_t  * p )
{
    char ** fnames = NULL;
    int     nfiles;

    if( gD.level > 3 ) fprintf(stderr,"-- getting sorted fnames...\n");

    /* maybe there is nothing to do here */
    if( p->fnames_done ) return 0;

    /* get alphabetical file list */
    if ( p->opts.infile_list ) {
       if ( read_file_list(p->opts.infile_list, &fnames, &nfiles) ) return -1;
       p->fnames_done = 1;  /* now don't do it, again */
    } else
       MCW_file_expand( 1, &p->glob_dir, &nfiles, &fnames );

    if( nfiles == 0 ) return 0;

    /* possibly sort by name */
    if( p->opts.sort_num_suff && (sort_by_num_suff(fnames, nfiles) < 0) )
       return -1;

    /* populate fnames lists */
    free_string_list(&p->fnames_prev);
    p->fnames_prev = p->fnames_cur;     /* struct copy */
    p->fnames_cur.num = p->fnames_cur.nall = nfiles;
    p->fnames_cur.list = fnames;

    if( gD.level > 2 ) fprintf(stderr,"-- sorted fnames: from %d to %d\n",
                               p->fnames_prev.num, p->fnames_cur.num);

    return 1;
}

/* allow for NULL struct pointers and list elements */
static int string_lists_differ(string_list * L0, string_list * L1)
{
   char ** l0p, ** l1p;
   int ind;

   /* check both unset or exactly one unset */
   if( ! L0 && ! L1 ) return 0;
   if( ! L0 || ! L1 ) return 1;

   /* if lengths differ, the lists do */
   if( L0->num != L1->num ) return 1;

   l0p = L0->list;
   l1p = L1->list;
   for( ind = 0; ind < L0->num; ind++, l0p++, l1p++ ) {
      if( ! *l0p && ! *l1p ) continue;
      if( ! *l0p || ! *l1p ) return 1;
      if( strcmp(*l0p, *l1p) ) return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * read_file_list:
 *
 * Get the list of files from the input file.
 * Populate fnames and nfiles.
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
static int read_file_list (char * inlist, char *** fnames, int * nfiles)
{
    FILE   * fp;
    char   * fname, *text;
    char  ** newnames = NULL;   /* placeholder for return value */
    size_t   nread;
    int      flen, nalloc;
    int      newfiles = 0;      /* placeholder */

    *fnames = NULL;
    *nfiles = 0;

    if ( ! inlist ) return 1;

    /* first, allocate memory for the file text */
    flen = l_THD_filesize(inlist);

    if ( flen <= 0 ) {
        fprintf(stderr,"** no text in -file_list file, '%s'\n", inlist);
        return 1;
    }

    fp = fopen(inlist, "r");
    if( !fp ) {
        fprintf(stderr,"** failed to open -file_list file, '%s'\n", inlist);
        return 1;
    }

    if( gD.level > 1 )
        fprintf(stderr,"++ reading file list from %s, size %d\n",inlist,flen);

    text = (char *)malloc((flen+1)*sizeof(char));
    if( !text ) {
        fprintf(stderr,"** failed to alloc for read of %d byte file '%s'\n",
                flen, inlist);
        fclose(fp);
        return 1;
    }

    nread = fread(text, sizeof(char), flen, fp);

    if( nread != flen ) {
        fprintf(stderr,"** RFL: read %d of %d bytes from %s\n",
                (int)nread, flen, inlist);
        free(text);
        fclose(fp);
        return 1;
    }

    /* now actually parse the file */

    newfiles = 0;
    newnames = NULL;
    nalloc = 0;
    fname = strtok(text, " \n\r\t\f");
    while( fname ) {
        if( nalloc <= newfiles ) {
            nalloc += 1000;
            newnames = (char  **)realloc(newnames, nalloc*sizeof(char *));
            if( !newnames ){
                fprintf(stderr,"** RFL: failed realloc of %d ptrs\n", nalloc);
                free(text);
                fclose(fp);
            }
        }
        newnames[newfiles] = l_strdup(fname);
        newfiles++;
        fname = strtok(NULL, " \n\r\t\f");
    }

    if( gD.level > 1 )
        fprintf(stderr,"++ read %d filenames from '%s'\n", newfiles, inlist);

    free(text);
    fclose(fp);

    /* pass back the list */
    *fnames = newnames;
    *nfiles = newfiles;

    return 0;
}

/* return an allocated string */
static char * l_strdup(char * text)
{
    char * ret;
    int    len;

    if( !text ) return NULL;
    len = strlen(text);
    ret = (char *)malloc((len+1)*sizeof(char));
    if( ! ret ) {
        fprintf(stderr,"** failed to alloc %d bytes for l_strdup\n", len);
        return NULL;
    }

    strcpy(ret, text);

    return ret;
}


/*----------------------------------------------------------------------
 * read pending images into finfo_t structs
 *
 *   - start at index fim_skip
 *   - read elements in state TO_READ (too many errors -> FAILED)
 *
 * return -1 on failure, else the number of images read
 *----------------------------------------------------------------------
*/
static int read_new_images( param_t * p )
{
   finfo_t * fp;
   char      back5[6] = { '\x8', '\x8', '\x8', '\x8', '\x8', '\x0' };
   int       index, rv, nread, nerrs;
   int       n2read, stat=0;

   if( p->nfim <= 0 )              return 0;    /* no names to read */
   if( p->fim_skip < 0 )           return 0;    /* waiting to start */
   if( p->nfim <= p->fim_skip )    return 0;    /* waiting for more names */

   /* make sure initial files to skip are not set for reading */
   fp = p->fim_o;
   nread = 0;
   for( index = 0; index < p->fim_skip && index < p->nfim; index++ )
      if( fp->state == IFM_FSTATE_TO_READ ) {
         fp->state = IFM_FSTATE_SKIP;
         nread++;
      }
   if( nread > 0 && gD.level > 2 )
      fprintf(stderr,"-- RNI: skipped %d initial image files", nread);

   /* read any file that is ready, possibly up to a limit */
   fp = p->fim_o + p->fim_skip;
   nread = 0;
   nerrs = 0;

   n2read = nfim_in_state(p, p->fim_skip, p->nfim-1, IFM_FSTATE_TO_READ);

   if( n2read > 300 && p->max2read <= 0 ) stat = n2read/100;
   if( stat && gD.level )
      fprintf(stderr,"-- reading %d image files ...  00%%", n2read);

   for( index = p->fim_skip; index < p->nfim; index++, fp++ ) {
      if( fp->state != IFM_FSTATE_TO_READ ) continue;

      if( gD.level > 3 )
         fprintf(stderr,"-- RNI: reading %s image file '%s'...\n",
                 get_ftype_str(p->ftype), fp->fname);

      if( IM_IS_DICOM(p->ftype) )
         rv = read_dicom_image( fp->fname, fp, 1 );
      else if( IM_IS_AFNI(p->ftype) )
         rv = read_afni_image( fp->fname, fp, 1 );
      else
         rv = read_ge_image( fp->fname, fp, 1, 1 );

      if( gD.level > 3 ) fprintf(stderr,"   image read status = %d\n", rv);

      if( rv == 0 ) {   /* success */
         fp->state = IFM_FSTATE_TO_PROC;
         nread++;

         /* babble, and maybe break out */
         if( gD.level > 3 ) idisp_finfo_t("++ have new image ", fp);

         /* if( stat && gD.level && !(nread%stat) ) fputc('.', stderr); */
         if( stat && gD.level && !(nread%stat) )
            fprintf(stderr,"%s %3d%%", back5, (int)(100.0*nread/n2read));

         if( p->max2read > 0 && nread >= p->max2read ) {
            if( gD.level > 2 )
               fprintf(stderr, "-- RNI: read max of %d images", nread);
            break;
         }
      } else {          /* error */
         nerrs++;
         fp->bad_reads++;
         if( fp->bad_reads > IFM_MAX_READ_ERRORS ) {
            fprintf(stderr,"** %d failures to read %s, no more attempts\n",
                    fp->bad_reads, fp->fname);
            fp->state = IFM_FSTATE_FAILED;
         }
      }
   }

   if( stat && gD.level ) fprintf(stderr,"%s 100%%\n", back5);

   if(gD.level > 1) fprintf(stderr,"-- RNI: read %d new images (nerrs = %d)\n",
                            nread, nerrs);

   p->fim_update = (nread > 0);

   return nread;
}


/*----------------------------------------------------------------------
 * compare filenames by numerical suffix (must be .nnn)
 *
 * return  0 on success, or -1 on failure
 *----------------------------------------------------------------------
*/
static int sort_by_num_suff(char ** names, int nnames)
{
    int c, val, errs = 0;

    if( gD.level > 2 ) fprintf(stderr,"-- sort_by_num_suff...\n");

    if( !names || nnames < 0 || !*names ) return -1;
    if( nnames == 0 ) return 0;

    /* sort the names */
    qsort(names, nnames, sizeof(char *), compare_by_num_suff);

    /* test the sort */
    for( c = 0; c < nnames-1; c++ )
    {
        val = compare_by_num_suff(names+c, names+c+1);
        if( val == -2 )
        {
            fprintf(stderr,"** bad numerical suffix for sorting in pair:\n"
                           "   (%s , %s)\n", names[c], names[c+1]);
            errs++;
        }
        else if( val == 0 )
        {
            fprintf(stderr,"** suffix not unique for sorting in pair:\n"
                           "   (%s , %s)\n", names[c], names[c+1]);
            errs++;
        }
    }

    if( gD.level > 4 )
    {
        fprintf(stderr,"+d names, sorted by suffix :\n");
        for( c = 0; c < nnames; c++ )
            fprintf(stderr,"      #%04d : suff %04d : %s\n",
                    c, get_num_suffix(names[c]), names[c]);
    }

    if( gD.level > 2 ) fprintf(stderr,"-- sort_by_num_suff: errs = %d\n",errs);

    if( errs > 0 ) return -1;

    return 0;
}


#ifdef DEATH_VALUE
#undef DEATH_VALUE
#endif
#define DEATH_VALUE -99999
/*----------------------------------------------------------------------
 * compare filenames by numerical suffix (must be .nnn)
 *----------------------------------------------------------------------
*/
int compare_by_num_suff( const void * v0, const void * v1 )
{
    int n0 = get_num_suffix(*(char **)v0);
    int n1 = get_num_suffix(*(char **)v1);
    int cmp;

    if ( n0 == DEATH_VALUE || n1 == DEATH_VALUE ) return -2; /* error */

    if ( n0 < n1 ) return -1;
    if ( n0 > n1 ) return  1;

    /* if equal, sort alphabetically (15 Aug 2014) */

    /* return explicit value, since strcmp can return -2 */
    cmp = strcmp(*(char **)v0, *(char **)v1);
    if( cmp < 0 ) return -1;
    else if ( cmp > 0 ) return 1;
    return 0;

}


/*----------------------------------------------------------------------
 *  find a numerical suffix (previously required to be non-negative)
 *                                                6 May 2010 [rickr]
 *
 *  return DEATH_VALUE : some error
 *         else        : numerical suffix
 *----------------------------------------------------------------------
*/
static int get_num_suffix( char * str )
{
    char * cp;
    int    len, val = DEATH_VALUE+1;

    if ( !str ) return DEATH_VALUE;
    len = strlen( str );
    if ( len <= 0 ) return DEATH_VALUE;

    cp = strrchr(str, '.');


    /* if we did not find an integer, search backwards */
    if( !cp || (cp >= (str + len - 1)) || !isdigit(*(cp+1)) ) {
       /* from right, find first digit, then beginning of integer */
       cp = str + len - 1;
       while( cp > str && !isdigit(*cp) ) cp--;
       while( cp > str &&  isdigit(*cp) ) cp--;
       if( !isdigit(*cp) && isdigit(*(cp+1)) ) cp++;

       /* so we are either looking at the beginning of the final integer,
          or did not find one */
       if( !isdigit(*cp) ) return DEATH_VALUE;

    } else {
       /* point to what should be the start of an integer */
       cp++;
    }

    len = sscanf(cp, "%d", &val);

    if ( len != 1 ) return DEATH_VALUE;   /* bad things man, bad things */

    return val;
}


/*----------------------------------------------------------------------
 * compare run:index values from ge_header structs
 *
 * if sorting by acquisition time, try that after run number
 *----------------------------------------------------------------------
*/
int compare_finfo( const void * v0, const void * v1 )
{
    ge_header_info * h0  = &((finfo_t *)v0)->geh;
    ge_header_info * h1  = &((finfo_t *)v1)->geh;
    int              dir;

    /* check for non-DICOM files first */
    if     ( h1->uv17 < 0 ) { g_sort_type |= 1; return -1; }
    else if( h0->uv17 < 0 ) { g_sort_type |= 1; return 1; }

    dir = g_dicom_sort_dir;

    /* check the run */
    if( h0->uv17 != h1->uv17 )
    {
        if( h0->uv17 < h1->uv17 ) { g_sort_type |= 2; return -1; }
        { g_sort_type |= 2; return 1; }
    }

    if( g_sort_by_atime ) {
        if     ( h0->atime < h1->atime ) { g_sort_type |= 4; return -dir; }
        else if( h0->atime > h1->atime ) { g_sort_type |= 4; return dir; }
    }

    /* 0054 1330: IMAGE INDEX */
    if     ( h0->im_index < h1->im_index ) { g_sort_type |= 8; return -dir; }
    else if( h0->im_index > h1->im_index ) { g_sort_type |= 8; return dir; }

    /* 0020 0013: REL Instance Number */
    if     ( h0->index < h1->index ) { g_sort_type |= 16; return -dir; }
    else if( h0->index > h1->index ) { g_sort_type |= 16; return dir; }

    return 0;  /* equal */
}


/*----------------------------------------------------------------------
 * compare structures by GE_ME field, else input index
 *----------------------------------------------------------------------
*/
int compare_by_geme( const void * v0, const void * v1 )
{
    finfo_t * f0  = (finfo_t *)v0;
    finfo_t * f1  = (finfo_t *)v1;

    if( f0->gex.ge_me_index != f1->gex.ge_me_index )
        return f0->gex.ge_me_index-f1->gex.ge_me_index;

    return f0->findex - f1->findex;
}


int compare_by_sindex( const void * v0, const void * v1 )
{
    finfo_t * f0  = (finfo_t *)v0;
    finfo_t * f1  = (finfo_t *)v1;

    /* state major sort */
    if( f0->state  != f1->state  ) return f0->state-f1->state;

    /* sindex only applies if state <= TO_PROC */
    if( f0->state <= IFM_FSTATE_TO_PROC && f0->sindex != f1->sindex )
       return f0->sindex-f1->sindex;

    /* minor order */
    return f0->findex - f1->findex;
}


/* init param_t struct elements */
static int init_param_t( param_t * p )
{
   memset(p, '\0', sizeof(param_t));

   /* option stuff first */
   p->opts.ep = IFM_EPSILON;           /* allow user to override     */
   p->opts.max_images = IFM_MAX_VOL_SLICES;   /* allow user override */
   p->opts.sleep_frac = 1.1;           /* fraction of TR to sleep    */
   p->opts.te_list = NULL;

   init_string_list( &p->opts.drive_list, 0, 0 );   /* no allocation */
   init_string_list( &p->opts.wait_list, 0, 0 );
   init_string_list( &p->opts.rt_list, 0, 0 );

   /* param_t struct initialization */
   init_string_list(&p->fnames_prev, 0, 1);       /* fake allocation */
   init_string_list(&p->fnames_cur, 0, 1);
   p->fim_o = NULL;

   p->max2read = IFM_MAX_IM_ALLOC;   /* max images to read at a time */

   return 0;
}

/*----------------------------------------------------------------------
 * init_options:
 *
 *     1. check usage: Imon -start_dir DIR
 *     2. do initial allocation of data structures
 *
 * return  0 on success, with continuation
 *         1 on success, but terminate
 *        -1 on error
 *----------------------------------------------------------------------
*/
static int init_options( param_t * p, ART_comm * A, int argc, char * argv[] )
{
    int ac, errors = 0;

    if ( p == NULL )
        return -1;

    if ( argc < 2 )
    {
        usage( IFM_PROG_NAME, IFM_USE_LONG );
        return 1;
    }

    /* basic initialization of data structures */
    init_param_t(p);
    memset( &gD, 0, sizeof(gD)  );      /* debug struct     */
    memset( &gS, 0, sizeof(gS)  );      /* stats struct     */
    memset(  A,  0, sizeof(gAC) );      /* afni comm struct */
    memset( &g_dicom_ctrl, 0,sizeof(g_dicom_ctrl) );/* from mri_dicom_stuff.c */

    ART_init_AC_struct( A );            /* init for no real-time comm */
    A->param = p;                       /* store the param_t pointer  */

    /* debug level 1 is now the default - by order of Wen-Ming :) */
    gD.level = 1;

    for ( ac = 1; ac < argc; ac++ )
    {
        if ( ! strncmp( argv[ac], "-debug", 4 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -debug LEVEL\n", stderr );
                return -1;
            }

            p->opts.debug = atoi(argv[ac]);
            gD.level      = p->opts.debug;
            if ( gD.level < 0 || gD.level > IFM_MAX_DEBUG )
            {
                fprintf( stderr, "error: debug level must be in [0,%d]\n",
                         IFM_MAX_DEBUG );
                errors++;
            }
        }
        else if ( ! strcmp( argv[ac], "-assume_dicom_mosaic") )
        {
            assume_dicom_mosaic = 1 ; /* global in mri_read_dicom.c */
            p->opts.assume_dicom_mosaic = 1 ;
        }
        else if ( ! strncmp( argv[ac], "-dicom_org", 10 ) )
        {
            p->opts.dicom_org = 1;
        }
        else if ( ! strncmp( argv[ac], "-epsilon", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -epsilon EPSILON\n", stderr );
                return -1;
            }

            p->opts.ep = atof(argv[ac]);
            if( p->opts.ep < 0 )
            {
                fprintf(stderr,"error: epsilon must be non-negative\n");
                errors++;
            }
        }
        else if ( ! strcmp( argv[ac], "-fast") )
        {
            /* equiv to -sleep_init 50 -sleep_vol 50 */
            p->opts.sleep_init = 50;
            p->opts.sleep_vol = 50;
        }
        else if ( ! strncmp( argv[ac], "-gert_create_dataset", 20) )
        {
            p->opts.gert_exec = 1;      /* execute GERT_Reco script  */

            p->opts.gert_reco = 1;      /* these options are implied */
            p->opts.no_wait = 1;
            p->opts.quit = 1;
        }
        else if ( ! strncmp( argv[ac], "-gert_filename", 10 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_filename FILENAME\n", stderr );
                return -1;
            }

            p->opts.gert_filename = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-gert_nz", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_nz NZ\n", stderr );
                return -1;
            }

            p->opts.gert_nz = atoi(argv[ac]);
            if( p->opts.gert_nz <= 0 )
            {
                fprintf(stderr,"gert_nz error: NZ must be positive (have %d)\n",
                        p->opts.gert_nz);
                return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-GERT_Reco", 7 ) )
        {
            p->opts.gert_reco = 1;      /* output script at the end */
        }
        else if ( ! strncmp( argv[ac], "-gert_chan_prefix", 14 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_chan_prefix PREFIX\n", stderr );
                return -1;
            }

            p->opts.chan_prefix = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-gert_to3d_prefix", 14 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_to3d_prefix PREFIX\n", stderr );
                return -1;
            }

            p->opts.gert_prefix = argv[ac];
        }
        else if ( ! strcmp( argv[ac], "-gert_write_as_nifti") )
        {
            p->opts.gert_format = 1;    /* NIFTI format */
        }
        else if ( ! strcmp( argv[ac], "-gert_quit_on_err") )
        {
            p->opts.gert_quiterr = 1;    /* Don't comeup in interactive mode */
        }
        else if ( ! strncmp( argv[ac], "-help", 5 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_LONG );
            return 1;
        }
        else if ( ! strncmp( argv[ac], "-hist", 5 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_HIST );
            return 1;
        }
        else if ( ! strncmp( argv[ac], "-infile_list", 12 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -infile_list FILE\n", stderr );
                return -1;
            }
            /* just append a '*' to the PREFIX */
            p->opts.infile_list = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-infile_pattern", 11 ) ||
                  ! strncmp( argv[ac], "-dicom_glob", 9 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -infile_pattern FILE_PATTERN\n", stderr );
                return -1;
            }

            p->opts.dicom_glob = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-infile_prefix", 11 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -infile_prefix FILE_PREFIX\n", stderr );
                return -1;
            }
            /* just append a '*' to the PREFIX */
            p->opts.dicom_glob = calloc(strlen(argv[ac])+2, sizeof(char));
            strcpy(p->opts.dicom_glob, argv[ac]);
            strcat(p->opts.dicom_glob, "*");
        }
        else if ( ! strncmp( argv[ac], "-max_images", 7 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -max_images NUM_IMAGES\n", stderr );
                return -1;
            }

            p->opts.max_images = atoi(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-max_quiet_trs", 7 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -max_quiet_trs NUM_TRs\n", stderr );
                return -1;
            }

            p->opts.max_quiet_trs = atoi(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-nice", 5 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -nice INCREMENT\n", stderr );
                return -1;
            }

            p->opts.nice = atoi(argv[ac]);
            if ( (p->opts.nice < IFM_MIN_NICE_INC) ||
                 (p->opts.nice > IFM_MAX_NICE_INC) )
            {
                fprintf( stderr, "error: nice incrment must be in [%d,%d]\n",
                         IFM_MIN_NICE_INC, IFM_MAX_NICE_INC );
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-no_wait", 8 ) )
        {
            p->opts.no_wait = 1;
            p->opts.quit = 1;
        }
        else if ( ! strncmp( argv[ac], "-nt", 3 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -nt VOLUMES_PER_RUN\n", stderr );
                return -1;
            }

            p->opts.nt = atoi(argv[ac]);
            if ( p->opts.nt < 0 || p->opts.nt > IFM_MAX_NT )
            {
                fprintf( stderr,
                    "option usage: -nt VOLUMES_PER_RUN\n"
                    "       error: VOLUMES_PER_RUN must be in [%d,%d]\n",
                    0, IFM_MAX_NT );
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-num_slices", 7 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -num_slices NUM_SLICES\n", stderr );
                return -1;
            }

            p->opts.num_slices = atoi(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-od", 3 ) ||
                  ! strncmp( argv[ac], "-gert_outdir", 9 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_outdir OUTPUT_DIR\n", stderr );
                return -1;
            }

            p->opts.gert_outdir = argv[ac];
        }
        else if ( ! strcmp( argv[ac], "-order_as_zt") )
        {
            p->opts.order_as_zt = 1;    /* just note the user option */
            p->opts.read_all = 1;       /* implied                   */
        }
        else if ( ! strncmp( argv[ac], "-pause", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -pause milliseconds\n", stderr );
                return -1;
            }

            p->opts.pause = atoi(argv[ac]);
            if ( p->opts.pause < 0 )
            {
                fprintf(stderr,"** illegal -pause time: %s\n",argv[ac]);
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-quiet", 6 ) )
        {
            /* only go quiet if '-debug' option has not changed it */
            if ( gD.level == IFM_DEBUG_DEFAULT )
                gD.level = 0;
        }
        else if ( ! strcmp( argv[ac], "-quit" ) )
        {
            p->opts.quit = 1;
        }
        else if ( ! strcmp( argv[ac], "-read_all" ) )
        {
            p->opts.read_all = 1;       /* just note the option */
        }
        else if ( ! strncmp( argv[ac], "-rev_org_dir", 8 ) )
        {
            p->opts.rev_org_dir = 1;
        }
        else if ( ! strncmp( argv[ac], "-rev_sort_dir", 9 ) )
        {
            p->opts.rev_sort_dir = 1;
        }
        else if ( ! strncmp( argv[ac], "-save_errors", 9 ) )
        {
            p->opts.save_errors = 1; /* goes with -save_details */
        }
        else if ( ! strncmp( argv[ac], "-save_details", 9 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -save_details PREFIX\n", stderr );
                return -1;
            }

            p->opts.flist_details = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-save_file_list", 10 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -save_file_list FILENAME\n", stderr );
                return -1;
            }

            p->opts.flist_file = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-show_sorted_list", 12 ) )
        {
            p->opts.show_sorted_list = 1;
        }
        else if ( ! strncmp( argv[ac], "-sleep_frac", 11 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sleep_vol TIME\n", stderr );
                return -1;
            }

            p->opts.sleep_frac = atof(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-sleep_init", 11 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sleep_init TIME\n", stderr );
                return -1;
            }

            p->opts.sleep_init = atoi(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-sleep_vol", 10 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sleep_vol TIME\n", stderr );
                return -1;
            }

            p->opts.sleep_vol = atoi(argv[ac]);
        }
        else if ( ! strcmp( argv[ac], "-sort_by_acq_time" ) )
        {
            p->opts.sort_acq_time = 1;
        }
        else if ( ! strncmp( argv[ac], "-sort_by_num_suffix", 12 ) )
        {
            p->opts.sort_num_suff = 1;
        }
        else if ( ! strncmp( argv[ac], "-sort_method", 10 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sort_method METHOD\n", stderr );
                return -1;
            }
            p->opts.sort_method = argv[ac];
            if( sort_method(p->opts.sort_method) == IFM_SORT_UNKNOWN ) {
               fprintf(stderr,"** invalid sort method: %s\n", argv[ac]);
               show_all_sort_methods("   valid methods: ", stderr);
               return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-sp", 3 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sp PATTERN\n", stderr );
                return -1;
            }

            p->opts.sp = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-start_dir", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -start_dir DIRECTORY\n", stderr );
                return -1;
            }

            p->opts.start_dir = argv[ac];
            p->fim_skip = -1; /* wait for starting point */
        }
        else if ( ! strncmp( argv[ac], "-start_file", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -start_file DIR/FIRST_FILE\n", stderr );
                return -1;
            }

            p->opts.start_file = argv[ac];
            p->fim_skip = -1; /* wait for starting point */
        }
        else if ( ! strncmp( argv[ac], "-tr", 3 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -tr TR  (in seconds)\n", stderr );
                return -1;
            }

            p->opts.tr = atof(argv[ac]);
            if ( p->opts.tr <= 0 || p->opts.tr > 30 )
            {
                fprintf(stderr,"bad value for -tr: %g (from '%s')\n",
                        p->opts.tr, argv[ac]);
                return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-version", 2 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_VERSION );
            return 1;
        }
        /* real-time options */
        else if ( ! strncmp( argv[ac], "-drive_afni", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -drive_afni COMMAND\n", stderr );
                return -1;
            }

            if ( add_to_string_list( &p->opts.drive_list, argv[ac], 0 ) <= 0 )
            {
                fprintf(stderr,"** failed add '%s' to drive_list\n",argv[ac]);
                return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-drive_wait", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -drive_wait COMMAND\n", stderr );
                return -1;
            }

            if ( add_to_string_list( &p->opts.wait_list, argv[ac], 0 ) <= 0 )
            {
                fprintf(stderr,"** failed add '%s' to drive_wait\n",argv[ac]);
                return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-host", 4 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -host HOSTNAME\n", stderr );
                return -1;
            }

            p->opts.host = argv[ac];    /* note and store the user option   */
            strncpy( A->host, argv[ac], ART_NAME_LEN-1 );
            A->host[ART_NAME_LEN-1] = '\0';     /* just to be sure */
        }
        else if ( ! strncmp( argv[ac], "-num_chan", 7 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -num_chan NUM_CHANNELS\n", stderr );
                return -1;
            }

            p->opts.num_chan = atoi(argv[ac]);
        }
        else if ( ! strncmp( argv[ac], "-rev_byte_order", 4 ) )
        {
            p->opts.rev_bo = 1;           /* note to send reverse byte order */
        }
        else if ( ! strncmp( argv[ac], "-rt_cmd", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -rt_cmd COMMAND\n", stderr );
                return -1;
            }

            if ( add_to_string_list( &p->opts.rt_list, argv[ac], 0 ) <= 0 )
            {
                fprintf(stderr,"** failed add '%s' to rt_list\n",argv[ac]);
                return -1;
            }
        }
        else if ( ! strncmp( argv[ac], "-rt", 3 ) )
        {
            A->state = ART_STATE_TO_OPEN; /* real-time is open for business */
            p->opts.rt = 1;               /* just note the user option      */
        }
        else if ( ! strcmp( argv[ac], "-swap" ) )
        {
            A->swap = 1;                /* do byte swapping before sending  */
            p->opts.swap = 1;           /* just note the user option        */
        }
        else if ( ! strncmp( argv[ac], "-te_list", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -te_list TE1 TE2 ...\n", stderr );
                return -1;
            }

            p->opts.te_list = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-file_type", 7 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -file_type TYPE\n", stderr );
                return -1;
            }
            p->opts.file_type = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-use_imon", 7 ) )
        {
            /* still run as Imon */
            p->opts.file_type = "GEMS";
        }
        else if ( ! strncmp( argv[ac], "-use_last_elem", 11 ) )
        {
            p->opts.use_last_elem = 1;
            g_dicom_ctrl.use_last_elem = 1;        /* for external function */
        }
        else if ( ! strncmp( argv[ac], "-use_slice_loc", 12 ) )
        {
            p->opts.use_slice_loc = 1;
        }
        else if ( ! strncmp( argv[ac], "-use_obl_origin", 12 ) )
        {
            p->opts.use_obl_origin = 1;
        }
        else if ( ! strncmp( argv[ac], "-zorder", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -zorder ORDER\n", stderr );
                return -1;
            }

            A->zorder = argv[ac];
        }
        else if ( ! strcmp( argv[ac], "-ushort2float" ) )
        {
            p->opts.ushort2float = 1;
            want_ushort2float = 1;
        }
        else
        {
            fprintf( stderr, "error: invalid option <%s>\n\n", argv[ac] );
            return -1;
        }
    }

    if( p->opts.save_errors && ! p->opts.flist_details ) {
       fputs("** -save_errors requires -save_details for file prefix\n",stderr);
       return -1;
    }

    /* apply read_all */
    if( p->opts.read_all ) p->max2read = -1;

    if ( errors > 0 )          /* check for all minor errors before exiting */
    {
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return -1;
    }

    gD_epsilon = p->opts.ep;    /* store new epsilon globally, for dimon_afni */

    /* set the p->ftype parameter, based on any file_type option  22 Jan 2013 */
    if ( set_ftype( p, p->opts.file_type ) ) return -1;

    if ( IM_IS_GEMS(p->ftype) && p->opts.start_dir == NULL )
    {
        fputs( "error: missing '-start_dir DIR' option\n", stderr );
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return -1;
    }

    if ( ! IM_IS_GEMS(p->ftype) &&   p->opts.show_sorted_list &&
                                   ! p->opts.dicom_org )
    {
        fputs( "error: -dicom_org is required with -show_sorted_list", stderr );
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return -1;
    }

    if ( p->opts.rev_bo && p->opts.swap )
    {
        fprintf( stderr, "error: options '-rev_byte_order' and '-swap' "
                 "cannot both be used\n");
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return -1;
    }

    if ( A->zorder )
    {
        if ( strcmp(A->zorder, "alt") && strcmp(A->zorder, "seq") )
        {
            fprintf(stderr,"** order '%s' is invalid for '-zorder' option,\n"
                    "   must be either 'alt' or 'seq'\n", A->zorder);
            return -1;
        }
    }

    if ( ! IM_IS_GEMS(p->ftype) )
    {
        if( ! p->opts.dicom_glob && ! p->opts.infile_list )
        {
            fprintf(stderr,"** missing -infile_pattern option\n");
            return -1;
        }
    }

    if ( p->opts.sleep_init > 0 && p->opts.sleep_init < 1000
             && p->opts.num_slices == 0 && gD.level > 0 )
        fprintf(stderr,"\n** consider using -num_slices with -sleep_init");

    /* done processing argument list */

    /* if dicom, start_dir is not used for globbing */
    if ( ! IM_IS_GEMS(p->ftype) )
    {
        p->glob_dir = p->opts.dicom_glob;
    }
    else
    {
        if ( dir_expansion_form(p->opts.start_dir, &p->glob_dir) ) return -1;
    }

    /* if user gives a NIFTI prefix, forget write_as_nifti */
    if( nifti_find_file_extension(p->opts.gert_prefix) != NULL &&
        p->opts.gert_format == 1) {
       fprintf(stderr,
               "** -gert_write_as_nifti is not needed with NIFTI prefix\n");
       p->opts.gert_format = 0;    /* do not try to add NIFTI extension */
    }

    /* save command arguments to add as a NOTE to any AFNI datasets */
    p->opts.argv = argv;
    p->opts.argc = argc;

    if ( gD.level > 2 )
    {
        idisp_opts_t ( "end init_options : ", &p->opts );
        idisp_param_t( "end init_options : ", p );
    }

    /* large-to-small sort is -1 */
    if ( p->opts.rev_sort_dir ) rglob_set_sort_dir( -1 );
    if ( p->opts.rev_org_dir )  g_dicom_sort_dir = -1;

    if ( gD.level > 0 )
        fprintf( stderr, "\n%s %s running, use <ctrl-c> to quit...\n\n",
                 IFM_PROG_NAME, DIMON_VERSION );

    return 0;
}

int show_all_sort_methods(char * mesg, FILE * stream)
{
   int method = IFM_SORT_NONE;

   if( mesg ) fputs(mesg, stream);

   /* first method, then comma delimited */
   fprintf(stream, "%s", sort_method_str(method));
   for( method++; method <= IFM_SORT_NUM_METHODS; method++ )
      fprintf(stream, ", %s", sort_method_str(method));
   fputc('\n', stream);
   return 0;
}

/* given method from user option, return method index */
int sort_method(char * method)
{
   if( ! method ) return IFM_SORT_UNSPEC;

   if( ! strcmp(method, "none")       ) return IFM_SORT_NONE;
   if( ! strcmp(method, "acq_time")   ) return IFM_SORT_ACQ_TIME;
   if( ! strcmp(method, "default")    ) return IFM_SORT_DEFAULT;
   if( ! strcmp(method, "geme_index") ) return IFM_SORT_GEME;
   if( ! strcmp(method, "num_suffix") ) return IFM_SORT_NUM_SUFF;
   if( ! strcmp(method, "zposn")      ) return IFM_SORT_ZPOSN;

   return IFM_SORT_UNKNOWN;
}

/* given method index, return correponding string */
char * sort_method_str(int method)
{
   switch( method ) {
      case IFM_SORT_UNSPEC:   return "UNSPECIFIED";
      case IFM_SORT_NONE:     return "none";
      case IFM_SORT_ACQ_TIME: return "acq_time";
      case IFM_SORT_DEFAULT:  return "default";
      case IFM_SORT_GEME:     return "geme_index";
      case IFM_SORT_NUM_SUFF: return "num_suffix";
      case IFM_SORT_ZPOSN:    return "zposn";
   }

   return "UNKNOWN";
}


/*----------------------------------------------------------------------
 * initialize:
 *     - nice level
 *     - afni communications
 *     - verify that any passed 'start_file' "matches" 'start_dir'
 *       (for now, just do this by comparing directory depth)
 *----------------------------------------------------------------------
*/
static int init_extras( param_t * p, ART_comm * ac )
{
    if ( p->opts.nice && set_nice_level(p->opts.nice) )
        return 1;

    if ( ac->state == ART_STATE_TO_OPEN )            /* open afni comm link */
    {
        atexit( ART_exit );
        ac->mode = AFNI_OPEN_CONTROL_MODE;
        ART_open_afni_link( ac, 2, 1, gD.level );
    }

    /* check directory depth of start_file against glob_dir */
    if ( p->opts.start_file != NULL )
    {
        char * sf     = p->opts.start_file;
        char * gd     = p->glob_dir;
        int    flevel = str_char_count( sf, strlen(sf), (char)'/' );

        /* check whether the number of slashes match */
        if ( flevel != str_char_count( gd, strlen(gd), (char)'/' ) )
        {
            fprintf( stderr,
                     "** warning : relative path to       : '-start_dir  %s'\n"
                     "             does not seem to match : '-start_file %s'\n"
                     "             (so 'start_file' may never be found)\n\n",
                     p->opts.start_file, p->opts.start_dir );
        }
        else if ( gD.level > 1 )
            fprintf( stderr, "-- '-start_file %s' and\n"
                             "   '-start_dir  %s' match at dir level %d\n",
                             p->opts.start_file, p->opts.start_dir, flevel );
    }

    return 0;
}


/*----------------------------------------------------------------------
 *  set_nice_level              - set to given "nice" value
 *
 *  returns:   0  : success
 *           else : failure
 *----------------------------------------------------------------------
*/
static int set_nice_level( int level )
{
    int rv;

    rv = nice( level );
    if ( rv != 0 )
    {
        if ( level < 0 )
            fprintf( stderr, "error: only root may decrement nice value\n"
                             "       (errno = %d, rv = %d)\n", errno, rv );
        else
            fprintf( stderr,
                     "error: failure to adjust nice by %d\n"
                     "       (errno = %d, rv = %d)\n", level, errno, rv );
    }
    else if ( gD.level > 1 )
        fprintf( stderr, "-- nice value incremented by %d\n", level );

    return rv;
}


/*------------------------------------------------------------
 *  dir_expansion_form
 *
 *  sin    : must be in the form "...0[01]n", where n is a digit
 *           (note that "...0[01]n/..." is okay)
 *  sexp   : memory is allocated for the output string of the
 *           form: "...[0-9][02468]n/I.*"
 *
 *  returns:    0 : on success
 *           else : error
 *------------------------------------------------------------
*/
int dir_expansion_form( char * sin, char ** sexp )
{
    char * out;
    char * cp;
    char   d0, d1, d2;                  /* the three relevant digits */
    int    len;

    if ( (sin == NULL) || (sexp == NULL) )
        return -1;

    *sexp = NULL;
    len = strlen(sin);

    out = (char *)malloc((len + IFM_PAD_LEN) * sizeof(char));
    if ( out == NULL )
    {
        fprintf( stderr, "failure: dir_expansion_form malloc\n" );
        return -1;
    }

    *sexp = out;                        /* save resulting malloc'd address */

    strcpy( out,sin );

    cp = out + len - 1;                         /* point to end */

    /* we'd better find 0[01]n - ignore the rest??? */
    while ( (cp > (out+2)) && !isdigit( *cp ) )
        cp--;

    if ( !isdigit(*cp) )                        /* didn't find even one */
    {
        fprintf( stderr, "error: dir <%s> is not of the form 00n (e.g. 003)\n",
                 sin );
        free(out);
        return -1;
    }

    cp -= 2;                                    /* should be first zero  */

    d0 = cp[0];                                 /* note the three digits */
    d1 = cp[1];
    d2 = cp[2];

    if ( (d0 != '0') ||                         /* first is not a zero   */
         ( (d1 != '0') && (d1 != '1')) )        /* second is not 0 or 1  */
    {
        fprintf( stderr, "error: dir <%s> is not of the form 0[01]n"
                         " (e.g. 003)\n", sin );
        free(out);
        return -1;
    }

    /* woohooo!  we're good to go! */
    /* set to "...[0-9][02468]n/I.*" (or with [13579]) */

    strcpy( cp, "[0-9]" );                      /* add and skip "[0-9]" */
    cp += strlen( "[0-9]" );

    if ( d1 == '0' )                            /* adding 2 to each     */
        strcpy( cp, "[02468]" );
    else
        strcpy( cp, "[13579]" );
    cp += strlen( "[02468]" );

    *cp++ = d2;                                 /* insert final digit */

    /* allow either I.* or i.*  - v2.11 */
    strcpy( cp, "/[Ii].*" );                    /* the big finish */

    return 0;
}


/*----------------------------------------------------------------------
 *  read_afni_image
 *---------------------------------------------------------------------- */
static int read_afni_image( char * pathname, finfo_t * fp, int get_data )
{
    THD_3dim_dataset * dset;
    ge_header_info   * gehp = &fp->geh;
    ge_extras        * gexp = &fp->gex;
    mosaic_info      * mp   = &fp->minfo;
    int                rv = 0;

    dset = THD_open_dataset(pathname);
    if ( ! dset ) {
        fprintf(stderr,"** failed to read file '%s' as AFNI dset\n", pathname);
        return 1;
    }

    /* process oblique info only once */
    /* rcr - todo, exactly how do we pass the matrix along, check form */
    if( obl_info_set == 2 && read_obl_info ) {
       static int nwarn = 1;
       if( nwarn ) {
          fprintf(stderr,"** have oblique AFNI data, maxtrix is lost\n");
          nwarn--;
       }
    }

    /* fill the finfo_t struct (always based on first image) */

    gehp->good  = ISVALID_DSET(dset);
    gehp->nx    = DSET_NX(dset);
    gehp->ny    = DSET_NY(dset);
    gehp->uv17  = 1;    /* no way to know */
    gehp->index = 1;    /* no way to know */
    gehp->im_index = 1; /* no way to know */
    gehp->atime = 0.0;  /* no way to know */
    gehp->dx    = DSET_DX(dset);
    gehp->dy    = DSET_DY(dset);
    gehp->dz    = DSET_DZ(dset);
    gehp->zoff  = DSET_ZORG(dset);
    gehp->slice_loc = DSET_ZORG(dset); /* rcr - same? what about mosaic? */

    /* TR of single volume dataset is probably 0.0   3 Sep 2013 */
    gehp->tr = DSET_TR(dset);
    if ( gehp->tr == 0.0 && gP.opts.tr > 0.0 ) gehp->tr = gP.opts.tr;
    gehp->te = 0; /* no way to know */

    /* orients, go backwards from daxes->xxorient values to LRPAIS, say */
    THD_fill_orient_str_6(dset->daxes, gehp->orients);

    /* ge_extras */
    gexp->bpp    = DSET_BRICK(dset, 0)->pixel_size;
    gexp->cflag  = 0;
    gexp->hdroff = -1;
    gexp->skip   = -1;
    gexp->swap   = 0;
    gexp->kk     = 0;
    gexp->ge_me_index  = -1;
    gexp->ge_nim_acq   = -1;
    gexp->sop_iuid_maj = -1;
    gexp->sop_iuid_min = -1;
    gexp->xorg   = DSET_XORG(dset);
    gexp->yorg   = DSET_YORG(dset);
    memset(gexp->xyz, 0, 9*sizeof(float)); /* skip xyz[9] */

    /* mosaic/volume info (volume in this case) */
    memset(mp, 0, sizeof(mosaic_info));
    mp->nslices      = DSET_NZ(dset);
    /* im_is_volume = 2: flag as AFNI volume      3 Sep 2013 */
    mp->im_is_volume = mp->nslices > 1 ? 2 : 0;
    mp->mos_nx       = gehp->nx;
    mp->mos_ny       = gehp->ny;

    if( gD.level > 3 ) idisp_mosaic_info( "AFNI volume ", mp );

    if( get_data ) {
        DSET_load(dset);
        rv = copy_dset_data(fp, dset); /* fill fp->bytes and fp->image */
    }

    DSET_delete(dset);  /* image data has been copied */

    return rv;
}


/*----------------------------------------------------------------------
 *  read_dicom_image
 *---------------------------------------------------------------------- */
static int read_dicom_image( char * pathname, finfo_t * fp, int get_data )
{
    MRI_IMARR      * imarr;
    MRI_IMAGE      * im;
    ge_header_info * gehp = &fp->geh;
    ge_extras      * gexp = &fp->gex;
    mosaic_info    * mp = &fp->minfo;

    static int       check_timing = 1, tverb = 1;             /* 15 Apr 2011 */
    int              rv = 0, ind;

    /* now use mri_read_dicom() directly                     4 Jan 2011 */
    /* im = r_mri_read_dicom( pathname, gD.level,
                              get_data ? &fp->image : NULL);            */

    /* init globals to be used in mri_read_dicom.c           4 Jan 2011 */
    if( ! g_dicom_ctrl.init ) {
        g_dicom_ctrl.init    = 1;
        g_dicom_ctrl.verb    = gD.level - 1; /* be quieter at the DICOM level */
        g_dicom_ctrl.rescale = 1;
    }

    g_dicom_ctrl.read_data = get_data;        /* do we actually want data?    */

    if( check_timing && gD.level > 2 ) {    /* set verb for timing info */
       tverb = mri_sst_get_verb();
       mri_sst_set_verb(3);
    }

    imarr = mri_read_dicom( pathname ); /* return a whole MRI_IMARR     */
    if ( !imarr || !imarr->imarr || !imarr->imarr[0] )
    {
        fprintf(stderr,"** failed to read file '%s' as dicom\n", pathname);
        return 1;
    }

    im = imarr->imarr[0];       /* for convenience */

    /* check whether short overflow to unsigned                           */
    /* If the user requested it, it is already set.   23 May 2018 [rickr] */
    if( MRILIB_dicom_s16_overflow ) want_ushort2float = 1;

    /* process oblique info only once */
    if( obl_info_set == 2 && read_obl_info ) {
        read_obl_info = 0;
        mri_read_dicom_get_obliquity(gAC.oblique_xform, gD.level>1);
        if ( gD.level > 2 ) disp_obl_info("post mri_read_dicom_get_obliquity ");
    }

    /* --------------------------------------------------------------- */
    /* process any siemens timing info only once           15 Apr 2011
     *
     * Process the times by calling populate_g_siemens_times() after
     * reading a single DICOM file.  If valid, g_siemens_timing_nused
     * and g_siemens_timing_times[] will be set.
     * Use valid_g_siemens_times() to check against nslices and TR.    */
    if( check_timing ) {
        check_timing = 0;
        populate_g_siemens_times(UNITS_SEC_TYPE);

        /* finish with verbose stuff ... */
        if( gD.level > 0 && g_siemens_timing_nused > 0 ) {
           fprintf(stderr, "\n-- Siemens timing (%d entries):",
                   g_siemens_timing_nused);
           for(ind = 0; ind < g_siemens_timing_nused; ind++ )
              fprintf(stderr," %.3f", g_siemens_timing_times[ind]);
           fputc('\n', stderr);
        }
        if( gD.level > 2 ) mri_sst_set_verb(tverb);  /* and reset verb */
    }

    /* print lots of image info */
    if ( gD.level > 3 )
    {
        fprintf(stderr,"+d dinfo (%s):\n   std, ser, im, im_ind, time = "
                "(%d, %d, %3d, %3d, %.3f)\n"
                "   is_obl=%d, is_mos=%d, (mnx, mny, mnslice) = (%d, %d, %d)\n",
            pathname,
            g_image_info.study, g_image_info.series,
            g_image_info.image, g_image_info.image_index, g_image_info.acq_time,
            g_image_info.is_obl, g_image_info.is_mosaic,
            g_image_info.mos_nx, g_image_info.mos_ny, g_image_info.mos_nslice
               );
        fprintf(stderr,"   GIPx, GIPy, GIPz (M_z) (%6.1f, %6.1f, %6.1f (%f))\n",
                g_image_posn[0], g_image_posn[1], g_image_posn[2], MRILIB_zoff);
        fprintf(stderr,"   OIPx, OIPy, OIPz       (%6.1f, %6.1f, %6.1f)\n",
                obl_info.dfpos1.xyz[0], obl_info.dfpos1.xyz[1],
                obl_info.dfpos1.xyz[2]);
        fprintf(stderr,"   ior, jor, kor (%d, %d, %d)\n",
                g_image_ori_ind[0], g_image_ori_ind[1], g_image_ori_ind[2]);
        fprintf(stderr,"   g_is_oblique = %d\n", g_is_oblique);
    } else if ( gD.level>1 && (g_image_info.is_obl||g_image_info.is_mosaic))
        fprintf(stderr,"-- is_oblique = %d, im_is_volume = %d\n",
                g_image_info.is_obl, g_image_info.is_mosaic);

    /* fill the finfo_t struct (always based on first image) */

    gehp->good  = 1;
    gehp->nx    = im->nx;
    gehp->ny    = im->ny;
    gehp->uv17  = g_image_info.series;
    gehp->index = g_image_info.image;            /* image number */
    gehp->im_index = g_image_info.image_index;   /* image index number */
    gehp->atime = g_image_info.acq_time;         /* acquisition time */
    gehp->dx    = im->dx;
    gehp->dy    = im->dy;
    gehp->dz    = im->dz;
    gehp->zoff = g_image_posn[2];
    gehp->slice_loc = g_image_info.slice_loc;
    /* maybe the slice location is desired, instead   7 Aug 2012 [rickr] */
    /* (leave option of replacing zoff with slice_loc)                   */
    if ( gP.opts.use_slice_loc ) {
       if ( gP.opts.debug > 1 && gehp->zoff != gehp->slice_loc )
          fprintf(stderr,"++ applying slice loc %.4f over image_posn %.4f\n",
                  gehp->slice_loc, gehp->zoff);
       gehp->zoff = gehp->slice_loc;
    }

    /* get some stuff from mrilib */
    gehp->tr = MRILIB_tr;
    gehp->te = 0;
    strncpy(gehp->orients, MRILIB_orients, 7);
    gehp->orients[7] = '\0';

    /* ge_extras */
    gexp->bpp    = im->pixel_size;
    gexp->cflag  = 0;
    gexp->hdroff = -1;
    gexp->skip   = -1;
    gexp->swap   = im->was_swapped;
    gexp->kk     = 0;
    gexp->ge_me_index  = g_ge_me_index;
    gexp->ge_nim_acq   = g_ge_nim_acq;
    gexp->sop_iuid_maj = g_sop_iuid_maj;
    gexp->sop_iuid_min = g_sop_iuid_min;
    gexp->xorg   = g_image_posn[0];
    gexp->yorg   = g_image_posn[1];
    memset(gexp->xyz, 0, 9*sizeof(float)); /* skip xyz[9] */

    /* mosaic info use Tr_dicom field for given axis order */
    memset(mp, 0, sizeof(mosaic_info));
    if( g_image_info.is_mosaic ) {
        if( ! g_image_ori_ind[0] || ! obl_info_set ) {
            if( gD.level > 0 )
                fprintf(stderr, "** mosaic missing ori_end or obl_info\n");
        }
        gexp->xorg = obl_info.Tr_dicom[abs(g_image_ori_ind[0])-1][3];
        gexp->yorg = obl_info.Tr_dicom[abs(g_image_ori_ind[1])-1][3];
        gehp->zoff = obl_info.Tr_dicom[abs(g_image_ori_ind[2])-1][3];

        if( gD.level > 2 )
           fprintf(stderr,"-- setting origin from Tr_dicom: %f, %f, %f\n",
                   gexp->xorg, gexp->yorg, gehp->zoff);

        mp->im_is_volume = g_image_info.is_mosaic;
        mp->nslices      = g_image_info.mos_nslice;
        mp->mos_nx       = g_image_info.mos_nx;
        mp->mos_ny       = g_image_info.mos_ny;
    }

    if( get_data )
        rv = copy_image_data(fp, imarr); /* fill fp->bytes and fp->image */

    DESTROY_IMARR(imarr);  /* image data has been copied */

    return rv;
}


/*----------------------------------------------------------------------
 *  copy_dset_data     (set fp->nbytes and imdata, or just fill image)
 *
 *  copy first volume of an AFNI dataset
 *
 *  If image is already allocated, just check that bytes is correct.
 *  Else, allocate it.
 *
 *  return: 0 on success
 *          1 on failure
 *----------------------------------------------------------------------
*/
static int copy_dset_data(finfo_t * fp, THD_3dim_dataset * dset)
{
    int nz, nvox, pix_size, arrbytes;

    if( !dset || !fp ) {
       fprintf(stderr,"** CDD: missing finfo or dset: %p, %p\n", fp, dset);
       return 1;
    }

    if( DSET_NVALS(dset) > 1 ) {
       fprintf(stderr,"** warning, dset has %d sub-bricks (using #0)\n",
               DSET_NVALS(dset));
       return 1;
    }

    nz = DSET_NZ(dset);
    nvox = DSET_NVOX(dset);
    pix_size = DSET_BRICK(dset, 0)->pixel_size;
    arrbytes = nvox * pix_size;

    if( gD.level > 3)
        fprintf(stderr,
                "-- CID: dset nz, nvox, pixbytes = %d, %d, %d (prod %d)\n",
                nz, nvox, pix_size, arrbytes);

    /* allocate image space */
    fp->nbytes = arrbytes;
    fp->imdata = malloc( fp->nbytes );
    if( ! fp->imdata ) {
       fprintf(stderr,"** CID: failed to alloc %d bytes for image %s\n",
               fp->nbytes, fp->fname);
       return 1;
    }

    /* now copy data */
    memcpy(fp->imdata, DBLK_ARRAY(dset->dblk, 0), arrbytes);

    return 0;
}


/*----------------------------------------------------------------------
 *  copy_image_data     (set fp->nbytes and imdata)
 *
 *  Allocate and copy an image.
 *
 *  If a mosaic, copy all images into a sequence of them
 *  else, just copy the single image over.
 *
 *  return: 0 on success
 *          1 on failure
 *----------------------------------------------------------------------
*/
static int copy_image_data(finfo_t * fp, MRI_IMARR * imarr)
{
    MRI_IMAGE * im = imarr->imarr[0];
    static int  nwarn=2;
    void      * dp = NULL;
    int         ind, imbytes;
    int64_t     arrbytes;

    im = imarr->imarr[0];
    imbytes = im->nvox * im->pixel_size;        /* image bytes */
    arrbytes = imarr->num * imbytes;            /* image array bytes */

    if( gD.level > 3)
        fprintf(stderr,"   num, nvox, pix_size = %d, %d, %d (prod %d)\n",
                imarr->num, (int)im->nvox, im->pixel_size, (int)arrbytes );

    /* verify num images against mosaic */
    if( imarr->num > 1 && ! fp->minfo.im_is_volume && nwarn > 0 ) {
        fprintf(stderr,"** CID: have non-mosaic %s with %d images\n",
                fp->fname, imarr->num);
        nwarn--;
        if( !nwarn ) fprintf(stderr,"   no more warnings will be printed...\n");
    }

    /* allocate space */
    fp->nbytes = (int)arrbytes;
    fp->imdata = malloc( fp->nbytes );
    if( ! fp->imdata ) {
       fprintf(stderr,"** CID: failed to alloc %d bytes for image %s\n",
               fp->nbytes, fp->fname);
       return 1;
    }

    /* copy data */
    for( ind = 0; ind < imarr->num; ind++ ) {
        /* point to data destination, offset by any mosaic image index */
        dp = (char *)fp->imdata + ind*imbytes;
        memcpy(dp, mri_data_pointer(imarr->imarr[ind]), imbytes);
    }

    return 0;
}


/*----------------------------------------------------------------------
 *  read_ge_image  (basically from Ifile.c, but use file_tool.c version)
 *
 *  returns:   0  : success
 *           < 0  : failure
 *----------------------------------------------------------------------
*/
static int read_ge_image( char * pathname, finfo_t * fp,
                          int get_image, int need_memory )
{
   ge_header_info * gehp  = &fp->geh;
   ge_extras      * gexp  = &fp->gex;

   FILE *imfile ;
   int  length , skip , swap=0 ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff ;
   float uv17 = -1.0;

   /* nuke mosaic structs */
   memset(&fp->minfo, 0, sizeof(mosaic_info));

   if( gehp == NULL ) return -1;            /* bad */
   gehp->good = 0 ;                         /* not good yet */
   if( pathname    == NULL ||
       pathname[0] == '\0'   ) return -1; /* bad */

   length = l_THD_filesize( pathname ) ;
   if( length < 1024 ) return -1;         /* bad */

   imfile = fopen( pathname , "r" ) ;
   if( imfile == NULL ) return -1;        /* bad */

   strcpy(str,"JUNK") ;     /* initialize string */
   fread(str,1,4,imfile) ;  /* check for "IMGF" at start of file */

   if( str[0]!='I' || str[1]!='M' || str[2]!='G' || str[3]!='F' ){ /* bad */
      fclose(imfile) ; return -2;
   }

   /*-- read next 5 ints (after the "IMGF" string) --*/

   fread( &skip , 4,1, imfile ) ; /* offset into file of image data */
   fread( &nx   , 4,1, imfile ) ; /* x-size */
   fread( &ny   , 4,1, imfile ) ; /* y-size */
   fread( &bpp  , 4,1, imfile ) ; /* bits per pixel (should be 16) */
   fread( &cflag, 4,1, imfile ) ; /* compression flag (1=uncompressed)*/

        /*-- check if nx is funny --*/

   if( nx < 0 || nx > 8192 ){      /* have to byte swap these 5 ints */
     swap = 1 ;                    /* flag to swap data, too */
     swap_4(&skip); swap_4(&nx); swap_4(&ny); swap_4(&bpp); swap_4(&cflag);
   } else {
     swap = 0 ;  /* data is ordered for this CPU */
   }
   if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ){  /* bad */
      fclose(imfile) ; return -1;
   }

   gehp->nx = nx ;
   gehp->ny = ny ;

   if( skip+2*nx*ny >  length ||               /* file is too short */
       skip         <= 0      ||               /* bizarre  */
       cflag        != 1      ||               /* data is compressed */
       bpp          != 16        ){
      fclose(imfile); return -1;    /* data is not shorts */
   }

   /*-- try to read image header data as well --*/

   fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
   fread( &hdroff , 4,1 , imfile ) ;   /* location of image header */
   if( swap ) swap_4(&hdroff) ;

   if( hdroff > 0 && hdroff+256 < length ){   /* can read from image header */
       float dx,dy,dz, xyz[9], zz ; int itr, ii,jj,kk ;

       /*-- get voxel grid sizes --*/

       fseek( imfile , hdroff+26 , SEEK_SET ) ;    /* dz */
       fread( &dz , 4,1 , imfile ) ;

       fseek( imfile , hdroff+50 , SEEK_SET ) ;    /* dx and dy */
       fread( &dx , 4,1 , imfile ) ;
       fread( &dy , 4,1 , imfile ) ;

       if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

       gehp->dx = dx ; gehp->dy = dy ; gehp->dz = dz ;

       /* grid orientation: from 3 sets of LPI corner coordinates: */
       /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
       /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
       /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
       /* GEMS coordinate orientation here is LPI                  */

       fseek( imfile , hdroff+154 , SEEK_SET ) ;  /* another magic number */
       fread( xyz , 4,9 , imfile ) ;
       if( swap ){
          swap_4(xyz+0); swap_4(xyz+1); swap_4(xyz+2);
          swap_4(xyz+3); swap_4(xyz+4); swap_4(xyz+5);
          swap_4(xyz+6); swap_4(xyz+7); swap_4(xyz+8);
       }

       /* x-axis orientation */
       /* ii determines which spatial direction is x-axis  */
       /* and is the direction that has the biggest change */
       /* between the TLHC and TRHC                        */

       dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
       dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
       dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
       dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
       switch( ii ){
        case  1: orients[0]= 'L'; orients[1]= 'R'; break;
        case -1: orients[0]= 'R'; orients[1]= 'L'; break;
        case  2: orients[0]= 'P'; orients[1]= 'A'; break;
        case -2: orients[0]= 'A'; orients[1]= 'P'; break;
        case  3: orients[0]= 'I'; orients[1]= 'S'; break;
        case -3: orients[0]= 'S'; orients[1]= 'I'; break;
        default: orients[0]='\0'; orients[1]='\0'; break;
       }

       /* y-axis orientation */
       /* jj determines which spatial direction is y-axis  */
       /* and is the direction that has the biggest change */
       /* between the BRHC and TRHC                        */

       dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
       dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
       dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
       dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
       switch( jj ){
         case  1: orients[2] = 'L'; orients[3] = 'R'; break;
         case -1: orients[2] = 'R'; orients[3] = 'L'; break;
         case  2: orients[2] = 'P'; orients[3] = 'A'; break;
         case -2: orients[2] = 'A'; orients[3] = 'P'; break;
         case  3: orients[2] = 'I'; orients[3] = 'S'; break;
         case -3: orients[2] = 'S'; orients[3] = 'I'; break;
         default: orients[2] ='\0'; orients[3] ='\0'; break;
       }

       orients[4] = '\0' ;   /* terminate orientation string */

       kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis   */
                                    /* where 1=LR, 2=PA, 3=IS               */
                                    /* (can't tell orientation from 1 slice) */

       zz = xyz[kk-1] ;             /* z-coordinate of this slice */

       gehp->zoff = zz ;
       strcpy(gehp->orients,orients) ;

       /* similarly (with zoff), store x and y origins in ge_extras */
       /*                                       2003 Jun 25 [rickr] */
       gexp->xorg = xyz[abs(ii)-1];
       gexp->yorg = xyz[abs(jj)-1];

       /*-- get TR in seconds --*/

       fseek( imfile , hdroff+194 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* note itr is an int */
       if( swap ) swap_4(&itr) ;
       gehp->tr = 1.0e-6 * itr ;        /* itr is in microsec */

       /*-- get TE in milliseconds --*/

       fseek( imfile , hdroff+202 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* itr is an int, in microsec */
       if( swap ) swap_4(&itr) ;
       gehp->te = 1.0e-6 * itr ;

       /* zmodify: get User Variable 17, a likely indicator of a new scan,
        * info by S. Marrett, location from S. Inati's matlab function
        * GE_readHeaderImage.m
        */

        /* printf ("\nuv17 = \n"); */
        fseek ( imfile , hdroff+272+202, SEEK_SET ) ;
        fread( &uv17 , 4, 1 , imfile ) ;
        if( swap ) swap_4(&uv17) ;
        /* printf ("%d ", (int)uv17);  */
        gehp->uv17 = (int)uv17;
        /* printf ("\n"); */

        /* store the ge_extra info */
        gexp->bpp    = bpp;
        gexp->cflag  = cflag;
        gexp->hdroff = hdroff;
        gexp->skip   = skip;
        gexp->swap   = swap;
        gexp->kk     = kk;
        gexp->ge_me_index = -1;
        gexp->ge_nim_acq = -1;

        memcpy( gexp->xyz, xyz, sizeof(xyz) );

        gehp->good = 1 ;                  /* this is a good file */

    } /* end of actually reading image header */

    /* read image in as well */
    if ( get_image )
    {
        int elements = gehp->nx * gehp->ny;

        fp->nbytes = elements * 2;                       /* bpp == 16 */

        if ( need_memory )
            fp->imdata = malloc( fp->nbytes );

        if ( fp->imdata == NULL )
        {
            fprintf(stderr, "** RGI: no memory for %d byte image %s\n",
                            fp->nbytes, fp->fname);
            gehp->good = 0;
            return -1;
        }

        fseek ( imfile, skip, SEEK_SET );
        if ( fread( fp->imdata , 2, elements, imfile ) != elements )
        {
            fprintf( stderr, "** RGI: failed to read %d shorts from %s\n",
                     elements, pathname );
            gehp->good = 0;               /* signal file problem */
            return -1;
        }
    }

    fclose(imfile);
    return 0;
}


/*------------------------------------------------------------
 * Reverse the order of the 4 bytes at this address.
 *------------------------------------------------------------
*/
static int swap_4( void * ptr )            /* destructive */
{
    unsigned char * addr = ptr;

    addr[0] ^= addr[3]; addr[3] ^= addr[0]; addr[0] ^= addr[3];
    addr[1] ^= addr[2]; addr[2] ^= addr[1]; addr[1] ^= addr[2];

    return 0;
}


/*------------------------------------------------------------
 * print out the contents of the param_t struct
 *------------------------------------------------------------
*/
static int idisp_param_t( char * info, param_t * p )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( p == NULL )
    {
        fprintf( stream, "idisp_param_t: p == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "param_t struct :\n"
            "   ftype             = %d\n"
            "   glob_dir          = %s\n"
            "   fnames_done       = %d\n"
            "   nfim              = %d\n"
            "   nfalloc           = %d\n"
            "   fim_update        = %d\n"
            "   fim_skip          = %d\n"
            "   fim_start         = %d\n"
            "   max2read          = %d\n",
            p->ftype, CHECK_NULL_STR(p->glob_dir),
            p->fnames_done, p->nfim, p->nfalloc, p->fim_update,
            p->fim_skip, p->fim_start, p->max2read);

    return 0;
}


/*------------------------------------------------------------
 * print out the contents of the opts_t struct
 *------------------------------------------------------------
*/
static int idisp_opts_t( char * info, opts_t * opt )
{
    FILE * stream = stderr;

    if ( info ) fputs( info, stream );

    if ( opt == NULL )
    {
        fprintf( stream, "idisp_opts_t: opt == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "opts_t struct :\n"
            "   start_file         = %s\n"
            "   start_dir          = %s\n"
            "   dicom_glob         = %s\n"
            "   infile_list        = %s\n"
            "   sp                 = %s\n"
            "   gert_outdir        = %s\n"
            "   file_type          = %s\n"
            "   argc               = %d\n"
            "   tr, ep             = %g, %g\n"
            "   nt, num_slices     = %d, %d\n"
            "   max_images         = %d\n"
            "   max_quiet_trs      = %d\n"
            "   nice, pause        = %d, %d\n"
            "   sleep_frac         = %g\n"
            "   sleep_init         = %d\n"
            "   sleep_vol          = %d\n"
            "   debug              = %d\n"
            "   quit, no_wait      = %d, %d\n"
            "   assume_dicom_mosaic= %d\n"
            "   use_last_elem      = %d\n"
            "   use_slice_loc      = %d\n"
            "   use_obl_origin     = %d\n"
            "   ushort2float       = %d\n"
            "   show_sorted_list   = %d\n"
            "   gert_reco          = %d\n"
            "   gert_filename      = %s\n"
            "   gert_prefix        = %s\n"
            "   chan_prefix        = %s\n"
            "   gert_nz            = %d\n"
            "   gert_format        = %d\n"
            "   gert_exec          = %d\n"
            "   gert_quiterr       = %d\n"
            "   dicom_org          = %d\n"
            "   sort_num_suff      = %d\n"
            "   sort_acq_time      = %d\n"
            "   order_as_zt        = %d\n"
            "   read_all           = %d\n"
            "   rev_org_dir        = %d\n"
            "   rev_sort_dir       = %d\n"
            "   save_errors        = %d\n"
            "   flist_file         = %s\n"
            "   flist_details      = %s\n"
            "   sort_method        = %s\n"
            "   (rt, swap, rev_bo) = (%d, %d, %d)\n"
            "   num_chan           = %d\n"
            "   host               = %s\n"
            "   drive_list(u,a,p)  = %d, %d, %p\n"
            "   wait_list (u,a,p)  = %d, %d, %p\n"
            "   rt_list   (u,a,p)  = %d, %d, %p\n",
            CHECK_NULL_STR(opt->start_file),
            CHECK_NULL_STR(opt->start_dir),
            CHECK_NULL_STR(opt->dicom_glob),
            CHECK_NULL_STR(opt->infile_list),
            CHECK_NULL_STR(opt->sp),
            CHECK_NULL_STR(opt->gert_outdir),
            CHECK_NULL_STR(opt->file_type),
            opt->argc,
            opt->tr, opt->ep, opt->nt, opt->num_slices, opt->max_images,
            opt->max_quiet_trs, opt->nice, opt->pause,
            opt->sleep_frac, opt->sleep_init, opt->sleep_vol,
            opt->debug, opt->quit, opt->no_wait,
            opt->assume_dicom_mosaic,
            opt->use_last_elem, opt->use_slice_loc, opt->use_obl_origin,
            opt->ushort2float, opt->show_sorted_list, opt->gert_reco,
            CHECK_NULL_STR(opt->gert_filename),
            CHECK_NULL_STR(opt->gert_prefix),
            CHECK_NULL_STR(opt->chan_prefix),
            opt->gert_nz, opt->gert_format, opt->gert_exec, opt->gert_quiterr,
            opt->dicom_org, opt->sort_num_suff, opt->sort_acq_time,
            opt->order_as_zt, opt->read_all,
            opt->rev_org_dir, opt->rev_sort_dir, opt->save_errors,
            CHECK_NULL_STR(opt->flist_file), CHECK_NULL_STR(opt->flist_details),
            CHECK_NULL_STR(opt->sort_method),
            opt->rt, opt->swap, opt->rev_bo, opt->num_chan,
            CHECK_NULL_STR(opt->host),
            opt->drive_list.num, opt->drive_list.nall, opt->drive_list.list,
            opt->wait_list.num, opt->wait_list.nall, opt->wait_list.list,
            opt->rt_list.num, opt->rt_list.nall, opt->rt_list.list
            );

    return 0;
}



/*------------------------------------------------------------
 * print out a string corresponding to the file type
 *------------------------------------------------------------
*/
static char * image_state_string( int state )
{
    if      ( state == IFM_FSTATE_FAILED  ) return "FSTATE_FAILED";
    else if ( state == IFM_FSTATE_SKIP    ) return "FSTATE_SKIP";
    else if ( state == IFM_FSTATE_UNKNOWN ) return "FSTATE_UNKNOWN";
    else if ( state == IFM_FSTATE_DONE    ) return "FSTATE_DONE";
    else if ( state == IFM_FSTATE_TO_PROC ) return "FSTATE_TO_PROC";
    else if ( state == IFM_FSTATE_TO_SORT ) return "FSTATE_TO_SORT";
    else if ( state == IFM_FSTATE_TO_READ ) return "FSTATE_TO_READ";

    return "FSTATE_INVALID";
}


/*------------------------------------------------------------
 * print out a string corresponding to the file type
 *------------------------------------------------------------
*/
static char * ftype_string( int ftype )
{
    if( ftype == IFM_IM_FTYPE_GEMS5 )           return "GEMS";
    else if ( ftype == IFM_IM_FTYPE_DICOM )     return "DICOM";
    else if ( ftype == IFM_IM_FTYPE_AFNI )      return "AFNI";

    return "UNKNOWN";
}

/*------------------------------------------------------------
 * print out a string corresponding to the file type
 *------------------------------------------------------------
*/
static int disp_ftype( char * info, int ftype )
{
    FILE * stream = stderr;

    if ( info ) fputs(info, stream);

    fprintf(stream, "%s (%d)\n", ftype_string(ftype), ftype);

    fflush(stream);

    return 0;
}


static int idisp_finfo_t( char * info, finfo_t * p )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( ! p ) {
        fprintf( stream, "idisp_finfo_t: p == NULL\n" );
        return -1;
    }

    fprintf( stream,
            "finfo_t struct:\n"
            "   findex              = %d\n"
            "   sindex              = %d\n"
            "   state               = %d\n"
            "   bad_reads           = %d\n"
            "   nbytes              = %d\n"
            "   fname               = %s\n"
            "   imdata              = %s\n",
            p->findex, p->sindex, p->state, p->bad_reads, p->nbytes,
            CHECK_NULL_STR(p->fname),
            p->imdata ? "<set>" : "<not set>"
            );

    idisp_ge_header_info( "   ===", &p->geh );
    idisp_ge_extras(      "   ===", &p->gex );
    idisp_mosaic_info(    "   ===", &p->minfo );

    return 0;
}

/*------------------------------------------------------------
 * print out the contents of the vol_t struct
 *------------------------------------------------------------
*/
static int idisp_vol_t( char * info, vol_t * v )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( v == NULL )
    {
        fprintf( stream, "idisp_vol_t: v == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "vol_t struct at :\n"
            "   nim                 = %d\n"
            "   fs_1                = %d\n"
            "   first_file          = %s\n"
            "   last_file           = %s\n"
            "   (z_first, z_last)   = (%g, %g)\n"
            "   z_delta, image_dz   = (%g, %g)\n"
            "   oblique             = %d\n"
            "   (seq_num, run)      = (%d, %d)\n",
            v->nim, v->fs_1, v->first_file, v->last_file,
            v->z_first, v->z_last, v->z_delta, v->image_dz,
            v->oblique, v->seq_num, v->run );

    idisp_ge_header_info( info, &v->geh );
    idisp_ge_extras( info, &v->gex );
    idisp_mosaic_info( info, &v->minfo );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge_extras struct.
 *  (copied from file_tool.c)
 *------------------------------------------------------------
*/
static int idisp_ge_extras( char * info, ge_extras * E )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( E == NULL )
    {
        fprintf( stream, "idisp_ge_extras: E == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "ge_extras :\n"
            "    bpp              = %d\n"
            "    cflag            = %d\n"
            "    hdroff           = %d\n"
            "    skip             = %d\n"
            "    swap             = %d\n"
            "    kk               = %d\n"
            "    ge_me_index      = %d\n"
            "    ge_nim_acq       = %d\n"
            "    sop_iuid_maj     = %d\n"
            "    sop_iuid_min     = %d\n"
            "    xorg             = %g\n"
            "    yorg             = %g\n"
            "    (xyz0,xyz1,xyz2) = (%g,%g,%g)\n"
            "    (xyz3,xyz4,xyz5) = (%g,%g,%g)\n"
            "    (xyz6,xyz7,xyz8) = (%g,%g,%g)\n",
            E->bpp, E->cflag, E->hdroff, E->skip, E->swap, E->kk,
            E->ge_me_index, E->ge_nim_acq, E->sop_iuid_maj,  E->sop_iuid_min,
            E->xorg,   E->yorg,
            E->xyz[0], E->xyz[1], E->xyz[2],
            E->xyz[3], E->xyz[4], E->xyz[5],
            E->xyz[6], E->xyz[7], E->xyz[8]
          );
    return 0;
}

/*------------------------------------------------------------
 *  Display the contents of the ge_header_info struct.
 *  (copied from file_tool.c)
 *------------------------------------------------------------
*/
static int idisp_ge_header_info( char * info, ge_header_info * I )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( I == NULL )
    {
        fprintf( stream, "idisp_ge_header_info: I == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "ge_header_info at :\n"
            "    good        = %d\n"
            "    (nx,ny)     = (%d,%d)\n"
            "    uv17        = %d\n"
            "    index       = %d\n"
            "    im_index    = %d\n"
            "    atime       = %f\n"
            "    slice_loc   = %f\n"
            "    (dx,dy,dz)  = (%g,%g,%g)\n"
            "    zoff        = %g\n"
            "    (tr,te)     = (%g,%g)\n"
            "    orients     = %-8s\n",
            I->good, I->nx, I->ny, I->uv17, I->index, I->im_index,
            I->atime, I->slice_loc,
            I->dx, I->dy, I->dz, I->zoff, I->tr, I->te,
            CHECK_NULL_STR(I->orients)
          );

    return 0;
}

/*------------------------------------------------------------
 *  Display the contents of the ge_header_info struct.
 *  (copied from file_tool.c)
 *------------------------------------------------------------
*/
static int idisp_mosaic_info( char * info, mosaic_info * I )
{
    FILE * stream = stderr;

    if ( info ){ fputs( info, stream ); fputc(' ', stream); }

    if ( I == NULL )
    {
        fprintf(stream, "idisp_mosaic_info: I == NULL\n" );
        return -1;
    }

    fprintf(stream,
            "mosaic_info :\n"
            "    im_is_volume  = %d\n"
            "    nslices       = %d\n"
            "    mos_nx, ny    = %d, %d\n",
            I->im_is_volume, I->nslices, I->mos_nx, I->mos_ny);

    return 0;
}

/*----------------------------------------------------------------------
 * usage
 *----------------------------------------------------------------------
*/
static int usage ( char * prog, int level )
{
    if ( level == IFM_USE_SHORT )
    {
        fprintf( stderr,
            "usage: %s [options] -infile_prefix prefix\n"
            "usage: %s -help\n",
            prog, prog );
        return 0;
    }
    else if ( level == IFM_USE_LONG )
    {
printf(
"\n"
"%s - monitor real-time acquisition of DICOM image files\n"
"    (or GEMS 5.x I-files, as 'Imon')\n"
"\n"
"    This program is intended to be run during a scanning session\n"
"    on a scanner, to monitor the collection of image files.  The\n"
"    user will be notified of any missing slice or any slice that\n"
"    is acquired out of order.\n"
"\n"
"    When collecting DICOM files, it is recommended to run this\n"
"    once per run, only because it is easier to specify the input\n"
"    file pattern for a single run (it may be very difficult to\n"
"    predict the form of input filenames runs that have not yet\n"
"    occurred.\n"
"\n"
"    This program can also be used off-line (away from the scanner)\n"
"    to organize the files, run by run.  If the DICOM files have\n"
"    a correct DICOM 'image number' (0x0020 0013), then Dimon can\n"
"    use the information to organize the sequence of the files, \n"
"    particularly when the alphabetization of the filenames does\n"
"    not match the sequencing of the slice positions.  This can be\n"
"    used in conjunction with the '-GERT_Reco' option, which will\n"
"    write a script that can be used to create AFNI datasets.\n"
"\n"
"    See the '-dicom_org' option, under 'other options', below.\n"
"\n"
"    If no -quit option is provided (and no -no_wait), the user should\n"
"    terminate the program when it is done collecting images according\n"
"    to the input file pattern.\n"
"\n"
"    Dimon can be terminated using <ctrl-c>.\n"
"\n"
"  ---------------------------------------------------------------\n"
"  comments for using Dimon with various image file types\n"
"\n"
"     DICOM : this is the intended and default use\n"
"             - provide at least -infile_prefix\n"
"\n"
"     GEMS 5x. : GE Medical Systems I-files\n"
"             - requires -start_dir and -file_type GEMS\n"
"             - works as the original Imon program\n"
"\n"
"     AFNI : AFNI/NIfTI volume datasets\n"
"             - requires -file_type AFNI\n"
"             - use -sp to specify slice timing pattern\n"
"             - if datasets are 4D, please use rtfeedme\n"
"\n"
"  ---------------------------------------------------------------\n"
"  realtime notes for running afni remotely:\n"
"\n"
"    - The afni program must be started with the '-rt' option to\n"
"      invoke the realtime plugin functionality.\n"
"\n"
"    - If afni is run remotely, then AFNI_TRUSTHOST will need to be\n"
"      set on the host running afni.  The value of that variable\n"
"      should be set to the IP address of the host running %s.\n"
"      This may set as an environment variable, or via the .afnirc\n"
"      startup file.\n"
"\n"
"    - The typical default security on a Linux system will prevent\n"
"      %s from communicating with afni on the host running afni.\n"
"      The iptables firewall service on afni's host will need to be\n"
"      configured to accept the communication from the host running\n"
"      %s, or it (iptables) will need to be turned off.\n"
"  ---------------------------------------------------------------\n"
"  usage: %s [options] -infile_prefix PREFIX\n"
"     OR: %s [options] -infile_pattern \"PATTERN\"\n"
"     OR: %s [options] -infile_list FILES.txt\n"
"\n"
"  ---------------------------------------------------------------\n"
"  notes regarding Siemens mosaic images:\n"
"\n"
"    - Final run slices will be reported as 1 (since there is only 1\n"
"      actual image), but mos_nslices will show the mosaic slice count.\n"
"\n"
"    - Acquisition timing for the slices will depend on the number of\n"
"      slices (parity), as well as the mosiac ordering.  So users may\n"
"      need to rely on reading slice timing from the DICOM headers.\n"
"\n"
"    - If slice timing is detected, \n"
"\n",
prog, prog, prog, prog, prog, prog, prog);

printf(
"  ---------------------------------------------------------------\n"
"  examples:\n"
"\n"
"  A. no real-time options:\n"
"\n"
"    %s -infile_prefix   s8912345/i\n"
"    %s -infile_pattern 's8912345/i*'\n"
"    %s -infile_list     my_files.txt\n"
"    %s -help\n"
"    %s -infile_prefix   s8912345/i  -quit\n"
"    %s -infile_prefix   s8912345/i  -nt 120 -quit\n"
"    %s -infile_prefix   s8912345/i  -debug 2\n"
"    %s -infile_prefix   s8912345/i  -dicom_org -GERT_Reco -quit\n"
"\n"
"  A2. investigate a list of files: \n"
"\n"
"    %s -infile_pattern '*' -dicom_org -show_sorted_list -quit\n"
"    %s -infile_prefix run1/im -sort_by_num_suffix -quit \\\n"
"          -save_details DETAILS -save_errors\n"
"\n"
"  A3. save a sorted list of files and check it later: \n"
"\n"
"    %s -infile_prefix data/im -dicom_org -save_file_list sorted.files\n"
"    %s -infile_list sorted.files ... \n"
"\n"
"  A4. sort by geme_index with 3-echo EPI data\n"
"      (and check sort against iuid 0008,0018)\n"
"\n"
"    %s -infile_pre data/im -sort_by_num_suffix -no_wait -num_chan 3 \\\n"
"          -sort_method geme_index\n"
"\n"
"  B. for GERT_Reco:\n"
"\n"
"    %s -infile_prefix run_003/image -gert_create_dataset\n"
"    %s -infile_prefix run_003/image -dicom_org -GERT_Reco -no_wait\n"
"    %s -infile_prefix 'run_00[3-5]/image' -GERT_Reco -quit\n"
"    %s -infile_prefix anat/image -GERT_Reco -no_wait\n"
"    %s -infile_prefix epi_003/image -dicom_org -no_wait \\\n"
"          -GERT_Reco -gert_to3d_prefix run3 -gert_nz 42\n"
"\n"
"  B2. Deal with Philips data (names are not sorted, and image numbers\n"
"      are in slice-major order).  Sort by acq time, then inst num.\n"
"      See -sort_by_acq_time in help output for details.\n"
"\n"
"    %s -infile_pattern 'data/*.dcm' -GERT_Reco -quit \\\n"
"          -use_last_elem -use_slice_loc -dicom_org -sort_by_acq_time\n"
"\n"
"  B3. Simple examples for NIH scanners (GE or Siemens).\n"
"\n"
"      o  create GERT_Reco script to put data into AFNI format\n"
"      o  create GERT_Reco script AND execute it (running to3d)\n"
"         (-gert_create_dataset implies -GERT_Reco and -quit)\n"
"      o  create and execute script, but make a NIfTI dataset\n"
"      o  also, store the datasets under a 'MRI_dsets' directory\n"
"\n"
"    %s -infile_pattern 'mr_0015/*.dcm' -GERT_Reco -quit \n"
"    %s -infile_prefix 'mr_0003/image' -gert_create_dataset\n"
"    %s -infile_pattern 'mr_0003/*.dcm' -gert_create_dataset\n"
"          -gert_write_as_nifti \n"
"    %s -infile_pattern 'mr_0003/*.dcm' -gert_create_dataset\n"
"          -gert_outdir MRI_dsets -gert_to3d_prefix EPI_003.nii\n"
"\n"
"  C. with real-time options:\n"
"\n"
"    %s -infile_prefix s8912345/i -rt \n"
"\n"
"    %s -infile_pattern 's*/i*' -rt \n"
"    %s -infile_pattern 's*/i*' -rt -nt 120\n"
"    %s -infile_pattern 's*/i*' -rt -quit\n"
"    %s -infile_prefix s8912345/i -rt -num_chan 2 -quit\n"
"\n"
"    %s -infile_pre run1/i -rt -num_chan 3 -quit -sort_method geme_index\n"
"\n"
"    ** detailed real-time example:\n"
"\n"
"    %s                                    \\\n"
"       -infile_pattern 's*/i*'               \\\n"
"       -rt -nt 120                           \\\n"
"       -host some.remote.computer            \\\n"
"       -rt_cmd \"PREFIX 2005_0513_run3\"     \\\n"
"       -num_slices 32                        \\\n"
"       -max_quiet_trs 3                      \\\n"
"       -sleep_frac 0.4                       \\\n"
"       -quit                                 \n"
"\n"
"    This example scans data starting from directory 003, expects\n"
"    120 repetitions (TRs), and invokes the real-time processing,\n"
"    sending data to a computer called some.remote.computer.name\n"
"    (where afni is running, and which considers THIS computer to\n"
"    be trusted - see the AFNI_TRUSTHOST environment variable).\n"
"    The time to wait for new data is 1.1*TR, and 32 slices are\n"
"    required for a volume\n"
"\n"
"    Note that -num_slices can be important in a real-time setup,\n"
"    as scanners do not always write the slices in order.   Slices\n"
"    from volume #1 can appear on disk before all slices from volume\n"
"    #0, in which case Dimon might determine an incorrect number of\n"
"    slices per volume.\n"
"\n"
"  -------------------------------------------\n"
"    Multiple DRIVE_AFNI commands are passed through '-drive_afni'\n"
"    options, one requesting to open an axial image window, and\n"
"    another requesting an axial graph, with 160 data points.\n"
"\n"
"    Also, '-drive_wait' options may be used like '-drive_afni',\n"
"    except that the real-time plugin will wait until the first new\n"
"    volume is processed before executing those DRIVE_AFNI commands.\n"
"    One advantage of this is opening an image window for a dataset\n"
"    _after_ it is loaded, allowing afni to approriately set the\n"
"    window size.\n"
"\n"
"    See README.driver for acceptable DRIVE_AFNI commands.\n"
"\n"
"    Also, multiple commands specific to the real-time plugin are\n"
"    passed via '-rt_cmd' options.  The PREFIX command sets the\n"
"    prefix for the datasets output by afni.  The GRAPH_XRANGE and\n"
"    GRAPH_YRANGE commands set the graph dimensions for the 3D\n"
"    motion correction graph (only).  And the GRAPH_EXPR command\n"
"    is used to replace the 6 default motion correction graphs with\n"
"    a single graph, according to the given expression, the square\n"
"    root of the average squared entry of the 3 rotation params,\n"
"    roll, pitch and yaw, ignoring the 3 shift parameters, dx, dy\n"
"    and dz.\n"
"\n"
"    See README.realtime for acceptable DRIVE_AFNI commands.\n"
"\n"
"  example D (drive_afni):\n"
"\n"
"    %s                                                   \\\n"
"       -infile_pattern 's*/i*.dcm'                         \\\n"
"       -nt 160                                             \\\n"
"       -rt                                                 \\\n"
"       -host some.remote.computer.name                     \\\n"
"       -drive_afni 'OPEN_WINDOW axialimage'                \\\n"
"       -drive_afni 'OPEN_WINDOW axialgraph pinnum=160'     \\\n"
"       -rt_cmd 'PREFIX eat.more.cheese'                    \\\n"
"       -rt_cmd 'GRAPH_XRANGE 160'                          \\\n"
"       -rt_cmd 'GRAPH_YRANGE 1.02'                         \\\n"
"       -rt_cmd 'GRAPH_EXPR sqrt(d*d+e*e+f*f)'\n"
"\n"
"  -------------------------------------------\n"
"\n"
"  example E (drive_wait):\n"
"\n"
"    Close windows and re-open them after data has arrived.\n"
"\n"
"    Dimon                                                    \\\n"
"       -infile_prefix EPI_run1/8HRBRAIN                      \\\n"
"       -rt                                                   \\\n"
"       -drive_afni 'CLOSE_WINDOW axialimage'                 \\\n"
"       -drive_afni 'CLOSE_WINDOW sagittalimage'              \\\n"
"       -drive_wait 'OPEN_WINDOW axialimage geom=+20+20'      \\\n"
"       -drive_wait 'OPEN_WINDOW sagittalimage geom=+520+20'  \\\n"
"       -rt_cmd 'PREFIX brie.would.be.good'                   \\\n"
"\n"
"  -------------------------------------------\n"
"  example F (for testing complete real-time system):\n"
"\n"
"    ** consider AFNI_data6/realtime.demos/demo.2.fback.*\n"
"\n"
"    Use Dimon to send volumes to afni's real-time plugin, simulating\n"
"    TR timing with Dimon's -pause option.  Motion parameters and ROI\n"
"    averages are then sent on to realtime_receiver.py (for subject\n"
"    feedback).\n"
"    \n"
"    a. Start afni in real-time mode, but first set some environment\n"
"       variables to make it explicit what might be set in the plugin.\n"
"       Not one of these variables is actually necessary, but they \n"
"       make the process more scriptable.\n"
"    \n"
"       See Readme.environment for details on any variable.\n"
"    \n"
"           setenv AFNI_TRUSTHOST              localhost\n"
"           setenv AFNI_REALTIME_Registration  3D:_realtime\n"
"           setenv AFNI_REALTIME_Graph         Realtime\n"
"           setenv AFNI_REALTIME_MP_HOST_PORT  localhost:53214\n"
"           setenv AFNI_REALTIME_SEND_VER      YES\n"
"           setenv AFNI_REALTIME_SHOW_TIMES    YES\n"
"           setenv AFNI_REALTIME_Mask_Vals     ROI_means\n"
"    \n"
"           afni -rt\n"
"    \n"
"       Note: in order to send ROI averages per TR, the user must\n"
"             choose a mask in the real-time plugin.\n"
"    \n"
"    b. Start realtime_receiver.py to show received data.\n"
"    \n"
"           realtime_receiver.py -show_data yes\n"
"    \n"
"    c. Run Dimon from the AFNI_data3 directory, in real-time mode,\n"
"       using a 2 second pause to simulate the TR.  Dicom images are\n"
"       under EPI_run1, and the files start with 8HRBRAIN.\n"
"    \n"
"           Dimon -rt -pause 2000 -infile_prefix EPI_run1/8HRBRAIN\n"
"    \n"
"       Note that Dimon can be run many times at this point.\n"
"\n"
"    --------------------\n"
"\n"
"    c2. alternately, set some env vars via Dimon\n"
"\n"
"         Dimon -rt -pause 2000 -infile_prefix EPI_run1/8          \\\n"
"           -drive_afni 'SETENV AFNI_REALTIME_Mask_Vals=ROI_means' \\\n"
"           -drive_afni 'SETENV AFNI_REALTIME_SEND_VER=Yes'        \\\n"
"           -drive_afni 'SETENV AFNI_REALTIME_SHOW_TIMES=Yes'\n"
"\n"
"       Note that plugout_drive can also be used to set vars at\n"
"       run-time, though plugouts must be enabled to use it.\n"
"\n"
"\n"
"  -------------------------------------------\n"
"  example G: when reading AFNI datasets\n"
"\n"
"    Note that single-volume AFNI datasets might not contain the.\n"
"    TR and slice timing information (since they are not considered\n"
"    to be time series).  So it may be necessary to specify such\n"
"    information on the command line.\n"
"\n"
"    %s -rt                                                  \\\n"
"       -infile_pattern EPI_run1/vol.*.HEAD                     \\\n"
"       -file_type AFNI -sleep_vol 1000 -sp alt+z -tr 2.0 -quit\n"
"\n"
"  ---------------------------------------------------------------\n",
prog, prog, prog, prog, prog, prog, prog,
prog, prog, prog, prog, prog, prog, prog, prog, prog, prog,
prog, prog, prog, prog, prog, prog, prog,
prog, prog, prog, prog, prog, prog, prog, prog );

printf(
    "  notes:\n"
    "\n"
    "    - Once started, unless the '-quit' option is used, this\n"
    "      program exits only when a fatal error occurs (single\n"
    "      missing or out of order slices are not considered fatal).\n"
    "      Otherwise, it keeps waiting for new data to arrive.\n"
    "\n"
    "      With the '-quit' option, the program will terminate once\n"
    "      there is a significant (~2 TR) pause in acquisition.\n"
    "\n"
    "    - To terminate this program, use <ctrl-c>.\n"
    "\n"
    "  ---------------------------------------------------------------\n"
    "  main options:\n"
    "\n"
    "    For DICOM images, either -infile_pattern or -infile_prefix\n"
    "    is required.\n"
    "\n"
    "    -infile_pattern PATTERN : specify pattern for input files\n"
    "\n"
    "        e.g. -infile_pattern 'run1/i*.dcm'\n"
    "\n"
    "        This option is used to specify a wildcard pattern matching\n"
    "        the names of the input DICOM files.  These files should be\n"
    "        sorted in the order that they are to be assembled, i.e.\n"
    "        when the files are sorted alphabetically, they should be\n"
    "        sequential slices in a volume, and the volumes should then\n"
    "        progress over time (as with the 'to3d' program).\n"
    "\n"
    "        The pattern for this option must be within quotes, because\n"
    "        it will be up to the program to search for new files (that\n"
    "        match the pattern), not the shell.\n"
    "\n"
    "    -infile_prefix PREFIX   : specify prefix matching input files\n"
    "\n"
    "        e.g. -infile_prefix run1/i\n"
    "\n"
    "        This option is similar to -infile_pattern.  By providing\n"
    "        only a prefix, the user need not use wildcard characters\n"
    "        with quotes.  Using PREFIX with -infile_prefix is\n"
    "        equivalent to using 'PREFIX*' with -infile_pattern (note\n"
    "        the needed quotes).\n"
    "\n"
    "        Note that it may not be a good idea to use, say 'run1/'\n"
    "        for the prefix, as there might be a readme file under\n"
    "        that directory.\n"
    "\n"
    "        Note also that it is necessary to provide a '/' at the\n"
    "        end, if the prefix is a directory (e.g. use run1/ instead\n"
    "        of simply run1).\n"
    "\n"
    "    -infile_list MY_FILES.txt : filenames are in MY_FILES.txt\n"
    "\n"
    "        e.g. -infile_list subject_17_files\n"
    "\n"
    "        If the user would rather specify a list of DICOM files to\n"
    "        read, those files can be enumerated in a text file, the\n"
    "        name of which would be passed to the program.\n"
    "\n"
    "  ---------------------------------------------------------------\n"
    "  real-time options:\n"
    "\n"
    "    -rt                : specify to use the real-time facility\n"
    "\n"
    "        With this option, the user tells '%s' to use the real-time\n"
    "        facility, passing each volume of images to an existing\n"
    "        afni process on some machine (as specified by the '-host'\n"
    "        option).  Whenever a new volume is acquired, it will be\n"
    "        sent to the afni program for immediate update.\n"
    "\n"
    "        Note that afni must also be started with the '-rt' option\n"
    "        to make use of this.\n"
    "\n"
    "        Note also that the '-host HOSTNAME' option is not required\n"
    "        if afni is running on the same machine.\n"
    "\n"
    "    -drive_afni CMND   : send 'drive afni' command, CMND\n"
    "\n"
    "        e.g.  -drive_afni 'OPEN_WINDOW axialimage'\n"
    "\n"
    "        This option is used to pass a single DRIVE_AFNI command\n"
    "        to afni.  For example, 'OPEN_WINDOW axialimage' will open\n"
    "        such an axial view window on the afni controller.\n"
    "\n"
    "        Note: the command 'CMND' must be given in quotes, so that\n"
    "              the shell will send it as a single parameter.\n"
    "\n"
    "        Note: this option may be used multiple times.\n"
    "\n"
    "        See README.driver for more details.\n"
    "\n"
    "    -drive_wait CMND   : send delayed 'drive afni' command, CMND\n"
    "\n"
    "        e.g.  -drive_wait 'OPEN_WINDOW axialimage'\n"
    "\n"
    "        This option is used to pass a single DRIVE_AFNI command\n"
    "        to afni.  For example, 'OPEN_WINDOW axialimage' will open\n"
    "        such an axial view window on the afni controller.\n"
    "\n"
    "        This has the same effect as '-drive_afni', except that\n"
    "        the real-time plugin will wait until the next completed\n"
    "        volume to execute the command.\n"
    "\n"
    "        An example of where this is useful is so that afni 'knows'\n"
    "        about a new dataset before opening the given image window,\n"
    "        allowing afni to size the window appropriately.\n"
    "\n"
    "    -fast              : process data very quickly\n"
    "\n"
    "        short for:  -sleep_init 50 -sleep_vol 50\n"
    "\n"
    "    -host HOSTNAME     : specify the host for afni communication\n"
    "\n"
    "        e.g.  -host mycomputer.dot.my.network\n"
    "        e.g.  -host 127.0.0.127\n"
    "        e.g.  -host mycomputer\n"
    "        the default host is 'localhost'\n"
    "\n"
    "        The specified HOSTNAME represents the machine that is\n"
    "        running afni.  Images will be sent to afni on this machine\n"
    "        during the execution of '%s'.\n"
    "\n"
    "        Note that the environment variable AFNI_TRUSTHOST must be\n"
    "        set on the machine running afni.  Set this equal to the\n"
    "        name of the machine running Imon (so that afni knows to\n"
    "        accept the data from the sending machine).\n"
    "\n"
    "    -num_chan CHANNELS : specify number of channels to send over\n"
    "\n"
    "        e.g.  -num_chan 8\n"
    "\n"
    "        This option tells the realtime plugin how many channels to\n"
    "        break incoming data into.  Each channel would then get its\n"
    "        own dataset.\n"
    "\n"
    "        Note that this simply distributes the data as it is read\n"
    "        across multiple datasets.  If 12 volumes are seen in some\n"
    "        directory and -num_chan 2 is specified, then volumes 0, 2,\n"
    "        4, 6, 8 and 10 would go to one dataset (e.g. channel 1),\n"
    "        while volumes 1,3,5,7,9,11 would go to another.\n"
    "\n"
    "        A sample use might be for multi-echo data.  If echo pairs\n"
    "        appear to Dimon sequentially over the TRs, then -num_chan\n"
    "        could be used to send each echo type to its own dataset.\n"
    "        This option was added for J Evans.\n"
    "\n"
    "        Currently, -num_chan only affects the realtime use.\n"
    "\n"
    "    -pause TIME_IN_MS : pause after each new volume\n"
    "\n"
    "        e.g.  -pause 200\n"
    "\n"
    "        In some cases, the user may wish to slow down a real-time\n"
    "        process.  This option will cause a delay of TIME_IN_MS\n"
    "        milliseconds after each volume is found.\n"
    "\n"
    "    -rev_byte_order   : pass the reverse of the BYTEORDER to afni\n"
    "\n"
    "        Reverse the byte order that is given to afni.  In case the\n"
    "        detected byte order is not what is desired, this option\n"
    "        can be used to reverse it.\n"
    "\n"
    "        See the (obsolete) '-swap' option for more details.\n"
    "\n"
    "    -rt_cmd COMMAND   : send COMMAND(s) to realtime plugin\n"
    "\n"
    "        e.g.  -rt_cmd 'GRAPH_XRANGE 120'\n"
    "        e.g.  -rt_cmd 'GRAPH_XRANGE 120 \\n GRAPH_YRANGE 2.5'\n"
    "\n"
    "        This option is used to pass commands to the realtime\n"
    "        plugin.  For example, 'GRAPH_XRANGE 120' will set the\n"
    "        x-scale of the motion graph window to 120 (repetitions).\n"
    "\n"
    "        Note: the command 'COMMAND' must be given in quotes, so\n"
    "        that the shell will send it as a single parameter.\n"
    "\n"
    "        Note: this option may be used multiple times.\n"
    "\n"
    "        See README.realtime for more details.\n"
    "\n"
    "    -show_sorted_list  : display -dicom_org info and quit\n"
    "\n"
    "        After the -dicom_org has taken effect, display the list\n"
    "        of run index, image index and filenames that results.\n"
    "        This option can be used as a simple review of the files\n"
    "        under some directory tree, say.\n"
    "\n"
    "        See the -show_sorted_list example under example A2.\n"
    "\n"
    "    -sleep_init MS    : time to sleep between initial data checks\n"
    "\n"
    "        e.g.  -sleep_init 500\n"
    "\n"
    "        While Dimon searches for the first volume, it checks for\n"
    "        files, pauses, checks, pauses, etc., until some are found.\n"
    "        By default, the pause is approximately 3000 ms.\n"
    "\n"
    "        This option, given in milliseconds, will override that\n"
    "        default time.\n"
    "\n"
    "        A small time makes the program seem more responsive.  But\n"
    "        if the time is too small, and no new files are seen on\n"
    "        successive checks, Dimon may think the first volume is\n"
    "        complete (with too few slices).\n"
    "\n"
    "        If the minimum time it takes for the scanner to output\n"
    "        more slices is T, then 1/2 T is a reasonable -sleep_init\n"
    "        time.  Note: that minimum T had better be reliable.\n"
    "\n"
    "        The example shows a sleep time of half of a second.\n"
    "\n"
    "        See also -fast.\n"
    "\n"
    "    -sleep_vol MS     : time to sleep between volume checks\n"
    "\n"
    "        e.g.  -sleep_vol 1000\n"
    "\n"
    "        When Dimon finds some volumes and there still seems to be\n"
    "        more to acquire, it sleeps for a while (and outputs '.').\n"
    "        This option can be used to specify the amount of time it\n"
    "        sleeps before checking again.  The default is 1.5*TR.\n"
    "\n"
    "        The example shows a sleep time of one second.\n"
    "\n"
    "        See also -fast.\n"
    "\n"
    "    -sleep_frac FRAC  : new data search, fraction of TR to sleep\n"
    "\n"
    "        e.g.  -sleep_frac 0.5\n"
    "\n"
    "        When Dimon finds some volumes and there still seems to be\n"
    "        more to acquire, it sleeps for a while (and outputs '.').\n"
    "        This option can be used to specify the amount of time it\n"
    "        sleeps before checking again, as a fraction of the TR.\n"
    "        The default is 1.5 (as the fraction).\n"
    "\n"
    "        The example shows a sleep time of one half of a TR.\n"
    "\n"
    "    -swap  (obsolete) : swap data bytes before sending to afni\n"
    "\n"
    "        Since afni may be running on a different machine, the byte\n"
    "        order may differ there.  This option will force the bytes\n"
    "        to be reversed, before sending the data to afni.\n"
    "\n"
    "        ** As of version 3.0, this option should not be necessary.\n"
    "           '%s' detects the byte order of the image data, and then\n"
    "           passes that information to afni.  The realtime plugin\n"
    "           will (now) decide whether to swap bytes in the viewer.\n"
    "\n"
    "           If for some reason the user wishes to reverse the order\n"
    "           from what is detected, '-rev_byte_order' can be used.\n"
    "\n"
    "    -te_list 'TE TE TE ...' : specify a list of echo times\n"
    "\n"
    "        e.g. -te_list '13.9 31.7 49.5'\n"
    "\n"
    "        This optins is used to pass along a list of echo times to the\n"
    "        realtime plugin.  The list should be enclosed in quotes to be\n"
    "        a single program argument.  It is passed to plug_realtime as\n"
    "        ECHO_TIMES TE TE TE ...\n"
    "\n"
    "    -zorder ORDER     : slice order over time\n"
    "\n"
    "        e.g. -zorder alt\n"
    "        e.g. -zorder seq\n"
    "        the default is 'alt'\n"
    "\n"
    "        This options allows the user to alter the slice\n"
    "        acquisition order in real-time mode, similar to the slice\n"
    "        pattern of the '-sp' option.  The main differences are:\n"
    "            o  only two choices are presently available\n"
    "            o  the syntax is intentionally different (from that\n"
    "               of 'to3d' or the '-sp' option)\n"
    "\n"
    "        ORDER values:\n"
    "            alt   : alternating in the Z direction (over time)\n"
    "            seq   : sequential in the Z direction (over time)\n"
    "\n"
    "  ---------------------------------------------------------------\n"
    "  other options:\n"
    "\n"
    "    -debug LEVEL       : show debug information during execution\n"
    "\n"
    "        e.g.  -debug 2\n"
    "        the default level is 1, the domain is [0,3]\n"
    "        the '-quiet' option is equivalent to '-debug 0'\n"
    "\n"
    "    -dicom_org         : organize files before other processing\n"
    "\n"
    "        e.g.  -dicom_org\n"
    "\n"
    "        When this flag is set, the program will attempt to read in\n"
    "        all files subject to -infile_prefix or -infile_pattern,\n"
    "        determine which are DICOM image files, and organize them\n"
    "        into an ordered list of files per run.\n"
    "\n"
    "        This may be necessary since the alphabetized list of files\n"
    "        will not always match the sequential slice and time order\n"
    "        (which means, for instance, that '*.dcm' may not list\n"
    "        files in the correct order.\n"
    "\n"
    "        In this case, if the DICOM files contain a valid 'image\n"
    "        number' field (0x0020 0013), then they will be sorted\n"
    "        before any further processing is done.\n"
    "\n"
    "        Notes:\n"
    "\n"
    "        - This does not work in real-time mode, since the files\n"
    "          must all be organized before processing begins.\n"
    "\n"
    "          ** As of version 4.0, this _is_ a real-time option.\n"
    "\n"
    "        - The DICOM images need valid 'image number' fields for\n"
    "          organization to be possible (DICOM field 0x0020 0013).\n"
    "\n"
    "        - This works will in conjunction with '-GERT_Reco', to\n"
    "          create a script to make AFNI datasets.  There will be\n"
    "          a single file per run that contains the image filenames\n"
    "          for that run (in order).  This is fed to 'to3d'.\n"
    "\n"
    "        - This may be used with '-save_file_list', to store the\n"
    "          list of sorted filenames in an output file.\n"
    "\n"
    "        - The images can be sorted in reverse order using the\n"
    "          option, -rev_org_dir.\n"
    "\n"
    "    -epsilon EPSILON   : specify EPSILON for 'equality' tests\n"
    "\n"
    "        e.g.  -epsilon 0.05\n"
    "        the default is 0.01\n"
    "\n"
    "        When checking z-coordinates or differences between them\n"
    "        for 'equality', a check of (difference < EPSILON) is used.\n"
    "        This option lets the user specify that cutoff value.\n"
    "\n"
    "    -file_type TYPE    : specify type of image files to be read\n"
    "\n"
    "        e.g.  -file_type AFNI\n"
    "        the default is DICOM\n"
    "\n"
    "        Dimon will currently process GEMS 5.x or DICOM files\n"
    "        (single slice or Siemens mosaic).\n"
    "\n"
    "        possible values for TYPE:\n"
    "\n"
    "           GEMS      : GE Medical Systems GEMS 5.x format\n"
    "           DICOM     : DICOM format, possibly Siemens mosaic\n"
    "           AFNI      : AFNI or NIfTI formatted datasets\n"
    "\n"
    "    -help              : show this help information\n"
    "\n"
    "    -hist              : display a history of program changes\n"
    "\n"
    "    -max_images NUM    : limit on images (slices per volume)\n"
    "\n"
    "        e.g.  -max_images 256\n"
    "        default = 3000\n"
    "\n"
    "        This variable is in case something is very messed up with\n"
    "        the data, and prevents the program from continuing after\n"
    "        failing to find a volume in this number of images.\n"
    "\n"
    "    -max_quiet_trs TRS : max number of TRs without data (if -quit)\n"
    "\n"
    "        e.g.  -max_quiet_trs 4\n"
    "        default = 2\n"
    "\n"
    "        This variable is to specify the number of TRs for which\n"
    "        having no new data is okay.  After this number of TRs, it\n"
    "        is assumed that the run has ended.\n"
    "\n"
    "        The TR (duration) comes from either the image files or\n"
    "        the -tr option.\n"
    "\n"
    "    -nice INCREMENT    : adjust the nice value for the process\n"
    "\n"
    "        e.g.  -nice 10\n"
    "        the default is 0, and the maximum is 20\n"
    "        a superuser may use down to the minimum of -19\n"
    "\n"
    "        A positive INCREMENT to the nice value of a process will\n"
    "        lower its priority, allowing other processes more CPU\n"
    "        time.\n"
    "\n"
    "    -no_wait           : never wait for new data\n"
    "\n"
    "        More forceful than -quit, when using this option, the\n"
    "        program should never wait for new data.  This option\n"
    "        implies -quit and is implied by -gert_create_dataset.\n"
    "\n"
    "        This is appropriate to use when the image files have\n"
    "        already been collected.\n"
    "\n"
    "    -nt VOLUMES_PER_RUN : set the number of time points per run\n"
    "\n"
    "        e.g.  -nt 120\n"
    "\n"
    "        With this option, if a run stalls before the specified\n"
    "        VOLUMES_PER_RUN is reached (notably including the first\n"
    "        run), the user will be notified.\n"
    "\n"
    "        Without this option, %s will compute the expected number\n"
    "        of time points per run based on the first run (and will\n"
    "        allow the value to increase based on subsequent runs).\n"
    "        Therefore %s would not detect a stalled first run.\n"
    "\n"
    "    -num_slices SLICES  : slices per volume must match this\n"
    "\n"
    "        e.g.  -num_slices 34\n"
    "\n"
    "        Setting this puts a restriction on the first volume\n"
    "        search, requiring the number of slices found to match.\n"
    "\n"
    "        This prevents odd failures at the scanner, which does not\n"
    "        necessarily write out all files for the first volume\n"
    "        before writing some file from the second.\n"
    "\n"
    "    -quiet             : show only errors and final information\n"
    "\n"
    "    -quit              : quit when there is no new data\n"
    "\n"
    "        With this option, the program will terminate once a delay\n"
    "        in new data occurs (an apparent end-of-run pause).\n"
    "\n"
    "        This option is implied by -no_wait.\n"
    "\n"
    "    -order_as_zt       : change order from -time:tz to -time_zt\n"
    "\n"
    "        e.g.  -rev_org_dir\n"
    "\n"
    "        Assuming the images are initially sorted in to3d's -time:tz\n"
    "        order (meaning across images, time changes first and slice\n"
    "        position changes next, i.e. all time points for the first slice\n"
    "        come first, then all time points for the next slice), re-order\n"
    "        the images into the -time:zt order (meaning all slices at the\n"
    "        first time point come first, then all slices at the next, etc).\n"
    "        \n"
    "        Note that -time:zt is the usual order expected with Dimon, since\n"
    "        it was intended for real-time use (when all slices for a given\n"
    "        time point come together).\n"
    "\n"
    "        This option implies -read_all.\n"
    "\n"
    "      * This is a post-sort operation.  Images will be initially sorted\n"
    "        based on the other options, then they will be shuffled into the\n"
    "        slice-minor order (volumes of slices grouped over time).\n"
    "\n"
    "      * This should probably not be used on a real-time system.\n"
    "\n"
    "        See 'to3d -help' for the -time options.\n"
    "\n"
    "    -read_all          : read all images at once\n"
    "\n"
    "        e.g.  -read_all\n"
    "\n"
    "        There is typically a limit on the number of images initially\n"
    "        read or stored at any one time.  This option is to remove that\n"
    "        limit.\n"
    "\n"
    "        It uses more memory, but is particularly important if sorting\n"
    "        should be done over a complete image list.\n"
    "\n"
    "    -rev_org_dir       : reverse the sort in dicom_org\n"
    "\n"
    "        e.g.  -rev_org_dir\n"
    "\n"
    "        With the -dicom_org option, the program will attempt to\n"
    "        organize the DICOM files with respect to run and image\n"
    "        numbers.  Normally that is an ascending sort.  With this\n"
    "        option, the sort is reversed.\n"
    "\n"
    "        see also: -dicom_org\n"
    "\n"
    "    -rev_sort_dir      : reverse the alphabetical sort on names\n"
    "\n"
    "        e.g.  -rev_sort_dir\n"
    "\n"
    "        With this option, the program will sort the input files\n"
    "        in descending order, as opposed to ascending order.\n"
    "\n"
    "    -save_file_list FILENAME : store the list of sorted files\n"
    "\n"
    "        e.g.  -save_file_list dicom_file_list\n"
    "\n"
    "        With this option the program will store the list of files,\n"
    "        sorted via -dicom_org, in the output file, FILENAME.  The\n"
    "        user may wish to have a separate list of the files.\n"
    "\n"
    "        Note: this option no longer requires '-dicom_org'.\n"
    "\n"
    "    -save_details FILE_PREFIX : save details about images\n"
    "\n"
    "        e.g.  -save_defails dicom_details\n"
    "\n"
    "        With this option the program will store the list of files,\n"
    "        along with many details for each image file.\n"
    "\n"
    "        It is akin to -save_file_list, only with extra information.\n"
    "\n"
    "        Fields:\n"
    "\n"
    "           index     : current index\n"
    "           findex    : index in main finfo_t list (as found)\n"
    "           sindex    : sorting index (-1 if not used)\n"
    "           state     : current state (<=0:bad, 1=good, >1=todo)\n"
    "           errs      : reading errors\n"
    "\n"
    "           zoff      : slice coordinate\n"
    "           diff      : difference from previous coordinate\n"
    "           data      : have data\n"
    "           run       : apparent run index\n"
    "           IIND      : image index (DICOM 0054 1330)\n"
    "           RIN       : image instance number (DICOM 0020 0013)\n"
    "           GEMEIND   : GE multi-echo index (DICOM RawDataRunNumber)\n"
    "           ATIME     : Acquisition time (DICOM 0008 0032)\n"
    "\n"
    "    -save_errors          : save 'details' files on search/match errors\n"
    "\n"
    "        e.g.  -save_errors -save_details dicom_details\n"
    "\n"
    "        For use with -save_details, the option causes extra details\n"
    "        files to be written upon any volume_search or volume_match\n"
    "        errors.\n"
    "\n"
    "    -sort_by_acq_time  : sort files by acquisition time\n"
    "\n"
    "        e.g.  -dicom_org -sort_by_acq_time\n"
    "\n"
    "        When this option is used with -dicom_org, the program will\n"
    "        sort DICOM images according to:\n"
    "           run, acq time, image index and image number\n"
    "\n"
    "        For instance, Philips files may have 0020 0013 (Inst. Num)\n"
    "        fields that are ordered as slice-major (volume minor).\n"
    "        But since slice needs to be the minor number, Acquisition\n"
    "        Time may be used for the major sort, before Instance Num.\n"
    "        So sort first by Acquisition Num, then by Instance.\n"
    "\n"
    "        Consider example B2.\n"
    "\n"
    "    -sort_by_num_suffix : sort files according to numerical suffix\n"
    "\n"
    "        e.g.  -sort_by_num_suffix\n"
    "\n"
    "        With this option, the program will sort the input files\n"
    "        according to the trailing '.NUMBER' in the filename.  This\n"
    "        NUMBER will be evaluated as a positive integer, not via\n"
    "        an alphabetic sort (so numbers need not be zero-padded).\n"
    "\n"
    "        This is intended for use on interleaved files, which are\n"
    "        properly enumerated, but only in the filename suffix.\n"
    "        Consider a set of names for a single, interleaved volume:\n"
    "\n"
    "          im001.1  im002.3  im003.5  im004.7  im005.9  im006.11\n"
    "          im007.2  im008.4  im009.6  im010.8  im011.10\n"
    "\n"
    "        Here the images were named by 'time' of acquisition, and\n"
    "        were interleaved.  So an alphabetic sort is not along the\n"
    "        slice position (z-order).  However the slice ordering was\n"
    "        encoded in the suffix of the filenames.\n"
    "\n"
    "        NOTE: the suffix numbers must be unique\n"
    "\n"
    "    -sort_method METHOD : apply METHOD for real-time sorting\n"
    "\n"
    "        e.g. -sort_method geme_index\n"
    "\n"
    "        This option is used to specify the sorting method to apply\n"
    "        to image structures after they have been read in.\n"
    "\n"
    "        methods:\n"
    "\n"
    "           none            : do not apply any real-time sorting\n"
    "           acq_time        : by acqusition time, if set\n"
    "           default         : sort by run, [ATIME], IIND, RIND\n"
    "           geme_index      : by GE multi-echo index\n"
    "           num_suffix      : based on numeric suffix\n"
    "           zposn           : based on z-coordinate and input order\n"
    "\n"
    "        more detailed method descriptions:\n"
    "\n"
    "           none\n"
    "\n"
    "             Do not perform any real-time sorting.  One can still apply\n"
    "             a pre-read name-based sort, such as -sort_by_num_suffix.\n"
    "\n"
    "           acq_time\n"
    "\n"
    "             Try to sort by acquisition time, if set.  This may apply\n"
    "             to Philps images.\n"
    "\n"
    "           default\n"
    "\n"
    "             Sort by run, acq_time (maybe), image index (0054 1330),\n"
    "             and REL Instance Number (0020 0013).\n"
    "\n"
    "           geme_index\n"
    "\n"
    "             This is for the GE multi-echo sequence.  Sort the list of\n"
    "             images in groups of nslices*nechos (which should match\n"
    "             'Images in Acquisition' in the Dicom header).  Each such\n"
    "             set of images should have the same GE_ME_INDEX sequence,\n"
    "             starting from some arbitrary offset.\n"
    "\n"
    "             Note that the actual file order is somewhat unspecified,\n"
    "             except that for a given geme_index, the files should be\n"
    "             chronological.\n"
    "\n"
    "           num_suffix\n"
    "\n"
    "             Sort by numerical file suffix (e.g. image.01234).\n"
    "\n"
    "           zposn\n"
    "\n"
    "             Sort by z-coordinate.  This is limited to a single volume\n"
    "             window of images, so num_slices should be set if there is\n"
    "             more than 1 volume.\n"
    "\n"
    "    -start_file S_FILE : have %s process starting at S_FILE\n"
    "\n"
    "        e.g.  -start_file 043/I.901\n"
    "\n"
    "        With this option, any earlier I-files will be ignored\n"
    "        by %s.  This is a good way to start processing a later\n"
    "        run, if it desired not to look at the earlier data.\n"
    "\n"
    "        In this example, all files in directories 003 and 023\n"
    "        would be ignored, along with everything in 043 up through\n"
    "        I.900.  So 043/I.901 might be the first file in run 2.\n"
    "\n"
    "    -tr TR             : specify the TR, in seconds\n"
    "\n"
    "        e.g.  -tr 5.0\n"
    "\n"
    "        In the case where volumes are acquired in clusters, the TR\n"
    "        is different than the time needed to acquire one volume.\n"
    "        But some scanners incorrectly store the latter time in the\n"
    "        TR field.\n"
    "        \n"
    "        This option allows the user to override what is found in\n"
    "        the image files, which is particularly useul in real-time\n"
    "        mode, though is also important to have stored properly in\n"
    "        the final EPI datasets.\n"
    "\n"
    "        Here, TR is in seconds.\n"
    "\n"
    "    -use_imon          : revert to Imon functionality\n"
    "\n"
    "        ** This option is deprecated.\n"
    "           Use -file_type GEMS, instead.\n"
    "\n"
    "    -assume_dicom_mosaic : as stated, useful for 3D format\n"
    "\n"
    "        Siemens 3D DICOM image files use a different type of mosaic\n"
    "        format, missing the indicator string.  This option matches\n"
    "        that for to3d.\n"
    "\n"
    "    -use_last_elem     : use the last elements when reading DICOM\n"
    "\n"
    "        In some poorly created DICOM image files, some elements\n"
    "        are listed incorrectly, before being listed correctly.\n"
    "\n"
    "        Use the option to search for the last occurrence of each\n"
    "        element, not necessarily the first.\n"
    "\n"
    "    -use_slice_loc     : use REL Slice Loc for z offset\n"
    "\n"
    "        REL Slice Location, 0020 1041, is sometimes used for the\n"
    "        z offset, rather than Image Position.\n"
    "        \n"
    "        Use this option to set slice offsets according to SLoc.\n"
    "\n"
    "    -ushort2float      : convert short datasets to float in to3d\n"
    "\n"
    "        By default, if short integer datasets appear to be unsigned\n"
    "        shorts, Dimon will add a similar -ushort2float to the to3d\n"
    "        command when creating AFNI datasets (via -gert_create_dataset).\n"
    "\n"
    "        But if some runs need conversion and others do not, one can\n"
    "        have a mix of types across runs.  Then one basically needs to\n"
    "        decide whether to use floats for all subjects, one subject at a\n"
    "        time, or to perform some conversion that removes the large\n"
    "        shorts.\n"
    "\n"
    "        Applying -ushort2float in Dimon will result in passing it to\n"
    "        any to3d commands (if -gert_create_dataset is applied), which\n"
    "        would have all short datasets converted to float32.\n"
    "\n"
    "    -version           : show the version information\n"
    "\n",
    prog, prog, prog, prog, prog, prog, prog
  );
  printf(
    "  ---------------------------------------------------------------\n"
    "  GERT_Reco options:\n"
    "\n"
    "    -GERT_Reco        : output a GERT_Reco_dicom script\n"
    "\n"
    "        Create a script called 'GERT_Reco_dicom', similar to the\n"
    "        one that Ifile creates.  This script may be run to create\n"
    "        the AFNI datasets corresponding to the I-files.\n"
    "\n"
    "    -gert_create_dataset     : actually create the output dataset\n"
    "\n"
    "        Execute any GERT_Reco script, creating the AFNI or NIfTI\n"
    "        datasets.\n"
    "\n"
    "        This option implies -GERT_Reco and -quit.\n"
    "\n"
    "        See also -gert_write_as_nifti.\n"
    "\n"
    "    -gert_filename FILENAME : save GERT_Reco as FILENAME\n"
    "\n"
    "        e.g. -gert_filename gert_reco_anat\n"
    "\n"
    "        This option can be used to specify the name of the script,\n"
    "        as opposed to using GERT_Reco_dicom.\n"
    "\n"
    "        By default, if the script is generated for a single run,\n"
    "        it will be named GERT_Reco_dicom_NNN, where 'NNN' is the\n"
    "        run number found in the image files.  If it is generated\n"
    "        for multiple runs, then the default it to name it simply\n"
    "        GERT_Reco_dicom.\n"
    "\n"
    "    -gert_nz NZ        : specify the number of slices in a mosaic\n"
    "\n"
    "        e.g. -gert_nz 42\n"
    "\n"
    "        Dimon happens to be able to write valid to3d commands\n"
    "        for mosaic (volume) data, even though it is intended for\n"
    "        slices.  In the case of mosaics, the user must specify the\n"
    "        number of slices in an image file, or any GERT_Reco script\n"
    "        will specify nz as 1.\n"
    "\n"
    "    -gert_outdir OUTPUT_DIR  : set output directory in GERT_Reco\n"
    "\n"
    "        e.g. -gert_outdir subject_A7\n"
    "        e.g. -od subject_A7\n"
    "        the default is '-gert_outdir .'\n"
    "\n"
    "        This will add '-od OUTPUT_DIR' to the @RenamePanga command\n"
    "        in the GERT_Reco script, creating new datasets in the\n"
    "        OUTPUT_DIR directory, instead of the 'afni' directory.\n"
    "\n"
    "    -sp SLICE_PATTERN  : set output slice pattern in GERT_Reco\n"
    "\n"
    "        e.g. -sp alt-z\n"
    "        the default is 'alt+z'\n"
    "\n"
    "        This options allows the user to alter the slice\n"
    "        acquisition pattern in the GERT_Reco script.\n"
    "\n"
    "        See 'to3d -help' for more information.\n"
    "\n"
    "    -gert_to3d_prefix PREFIX : set to3d PREFIX in output script\n"
    "\n"
    "        e.g. -gert_to3d_prefix anatomy\n"
    "        e.g. -gert_to3d_prefix epi.nii.gz\n"
    "\n"
    "        When creating a GERT_Reco script that calls 'to3d', this\n"
    "        option will be applied to '-prefix'.\n"
    "\n"
    "        The default prefix is 'OutBrick_run_NNN', where NNN is the\n"
    "        run number found in the images.\n"
    "\n"
    "        Use a NIFTI suffix to create a NIFTI dataset.\n"
    "\n"
    "      * Caution: this option should only be used when the output\n"
    "        is for a single run.\n"
    "\n"
    "    -gert_chan_prefix PREFIX : use PREFIX instead of _chan_ in dsets\n"
    "\n"
    "        e.g. -gert_chan_prefix _echo_\n"
    "\n"
    "        When creating a GERT_Reco script that calls 'to3d' in the case\n"
    "        of multi-channel (or echo) data, this option overrides the\n"
    "        _chan_ part of the prefix.\n"
    "\n"
    "        Instead of naming the result as in:\n"
    "            OutBrick_run_003_chan_001+orig.HEAD\n"
    "        the name would use PREFIX, e.g. _echo_, in place of _chan_:\n"
    "            OutBrick_run_003_echo_001+orig.HEAD\n"
    "\n"
    "    -gert_write_as_nifti     : output dataset should be in NIFTI format\n"
    "\n"
    "        By default, datasets created by the GERT_Reco script will be in \n"
    "        AFNI format.  Use this option to create them in NIfTI format,\n"
    "        instead.  These merely appends a .nii to the -prefix option of\n"
    "        the to3d command.\n"
    "\n"
    "        This option is not necessary if -gert_to3d_prefix is NIFTI.\n"
    "\n"
    "        See also -gert_create_dataset, -gert_to3d_prefix.\n"
    "\n"
    "    -gert_quit_on_err : Add -quit_on_err option to to3d command\n"
    "                        which has the effect of causing to3d to \n"
    "                        fail rather than come up in interactive\n"
    "                        mode if the input has an error.\n"
    "\n"
    "    -use_obl_origin    : if oblique, pass -oblique_origin to to3d\n"
    "\n"
    "        This will usually apply a more accurate origin to the volume.\n"
    "        Maybe this will become the default operation in the future.\n"
    "\n"
    "  ---------------------------------------------------------------\n"
    "\n"
    "  Author: R. Reynolds - %s\n"
    "\n",
    DIMON_VERSION
  );

        return 0;
    }
    else if ( level == IFM_USE_HIST )
    {
        int c, len = sizeof(g_history)/sizeof(char *);
        for( c = 0; c < len; c++ )
            fputs( g_history[c], stdout );
        return 0;
    }
    else if ( level == IFM_USE_VERSION )
    {
        printf( "%s: %s, compile date: %s\n",
                prog, DIMON_VERSION, __DATE__ );
        return 0;
    }

    fprintf( stderr, "error: usage() called with illegal level <%d>\n", level );

    return -1;
}


/* ----------------------------------------------------------------------
 * signal handler
 * ----------------------------------------------------------------------
*/
static void hf_signal( int signum )
{
    switch ( signum )
    {
        default :
            fprintf( stderr, "\nError: received unknown signal, %d\n",
                     signum );
            break;

        case SIGHUP  :
        case SIGINT  :
        case SIGTERM :
            show_run_stats( &gS );
            break;
    }

    if(gD.level > 1) fprintf(stderr, "\n+d received signal, %d\n", signum);

    if( gAC.state != ART_STATE_NO_USE )
    {
        if( gD.level > 1 )
            fprintf(stderr,"-d closing afni connection, state %d\n", gAC.state);
        ART_exit();
    }

    fflush(stderr);  /* in case it is redirected */
    fflush(stdout);

    exit(0);
}


/* ----------------------------------------------------------------------
 * set_volume_stats
 * ----------------------------------------------------------------------
*/
static int set_volume_stats( param_t * p, stats_t * s, vol_t * v )
{
    run_t * rp;           /* for a little speed, this will be called often */

    if ( v == NULL || v->seq_num < 0 || v->run < 0 )
    {
        fprintf( stderr, "failure: SVS - insufficient data\n\n" );
        idisp_vol_t ( "-- VOLUME FAILURE INFO : ", v );
    }

    /* initialize the stats structure */
    if ( s->nalloc == 0 )
    {
        s->runs = (run_t *)calloc( IFM_STAT_ALLOC, sizeof(run_t) );
        if ( s->runs == NULL )
        {
            fprintf( stderr, "failure: cannot allocate space for run info\n" );
            return -1;
        }

        /* first time caller - fill initial stats info */
        s->slices   = v->nim;
        s->z_first  = v->z_first;
        s->z_last   = v->z_last;
        s->z_delta  = v->z_delta;
        s->image_dz = v->image_dz;

        s->nalloc  = IFM_STAT_ALLOC;
        s->nused   = 0;
        s->nvols   = gP.opts.nt;        /* init with any user input value */
        s->oblique = v->oblique;
        s->mos_nslices = v->minfo.nslices;

        if ( gD.level > 1 )
            fprintf( stderr, "\n-- svs: init alloc - vol %d, run %d, file %s\n",
                     v->seq_num, v->run, v->first_file );
    }

    if ( v->run >= s->nalloc )          /* run is 0-based */
    {
        /* make space for many more runs - we don't want to do this often */
        s->runs = (run_t *)realloc( s->runs, (v->run + IFM_STAT_ALLOC) *
                                             sizeof(run_t) );
        if ( s->runs == NULL )
        {
            fprintf( stderr, "failure: cannot realloc space for run info\n" );
            return -1;
        }

        s->nalloc = v->run + IFM_STAT_ALLOC;

        /* zero out any new memory */
        memset( s->runs + s->nused, 0, (s->nalloc - s->nused)*sizeof(run_t) );

        if ( gD.level > 1 )
            fprintf( stderr,
                "\n-- svs: realloc (%d entries) - vol %d, run %d, file %s\n",
                s->nalloc, v->seq_num, v->run, v->first_file );

    }

    /* we have memory - the current run number is an index into runs */

    rp = s->runs + v->run;

    if ( s->nused < v->run+1 )
        s->nused = v->run+1;

    if ( rp->volumes == 0 )
    {
        rp->f1index = v->fs_1; /* index into fim_o (matching f1name) */
        rp->f1name = v->first_file;
        rp->geh = v->geh;      /* keep a copy of the volume info */
        rp->gex = v->gex;
    }

    rp->volumes = v->seq_num;

    /* update nvols (if the user did not specify it and it is small) */
    if ( (p->opts.nt <= 0) && (s->nvols < v->seq_num) )
        s->nvols = v->seq_num;

    if ( gD.level > 2 )
        fprintf( stderr, "\n-- svs: run %d, seq_num %d\n", v->run, v->seq_num );

    return 0;
}


/* ----------------------------------------------------------------------
 * Create a gert_reco script.
 * ---------------------------------------------------------------------- */
static int create_gert_script( stats_t * s, param_t * p )
{
    if( IM_IS_AFNI(p->ftype) ) {
        fprintf(stderr,"** create AFNI GERT script not currently allowed\n");
        return 1;
    }

    /* for either GEMS I-files or DICOM files */
    if( IM_IS_DICOM(p->ftype) ) return create_gert_dicom(s, p);
    else                        return create_gert_reco (s, &p->opts);
}


/* ----------------------------------------------------------------------
 * Create a gert_reco script for DICOM files.
 * ---------------------------------------------------------------------- */
static int create_gert_dicom( stats_t * s, param_t * p )
{
    opts_t * opts = &p->opts;
    FILE   * fp;                          /* script and name file pointers */
    char     script[32] = IFM_GERT_DICOM; /* output script filename */
    char   * sfile;                       /* pointer to script      */
    char     prefixname[32];              /* output prefix          */
    char   * pname;                       /* pointer to prefix      */
    char     command[64];                 /* command line for chmod */
    char   * spat;                        /* slice acquisition pattern */
    char     outfile[64];                 /* run files */
    char     TR[16];                      /* for printing TR w/out zeros */
    float    tr;
    int      num_valid, c, indent=0;
    int      first_run = -1, nspaces = 0;

    /* If the user did not give a slice pattern string, use the default *
     * (default is "FROM_IMAGE" if siemens timing info).
     * Check that siemens timing info has correct nz.      15 Apr 2011  */

    spat = (g_siemens_timing_nused > 0) ? "FROM_IMAGE" : IFM_SLICE_PAT;
    if ( opts->sp ) spat = opts->sp;

    for ( c = 0, num_valid = 0; c < s->nused; c++ )
        if ( s->runs[c].volumes > 0 )
        {
            if( first_run < 0 ) first_run = c;  /* note first valid run */
            num_valid++;
        }

    if ( num_valid == 0 )
    {
        fprintf( stderr, "-- no runs to use for '%s'\n", script );
        return 0;
    }

    sfile = script;                     /* init to script string   */
    if ( opts->gert_filename )          /* override with user name */
        sfile = opts->gert_filename;
    else if ( num_valid == 1 && first_run >= 0 )    /* name by run */
        sprintf(script, "%s_%03d", IFM_GERT_DICOM, first_run);

    if ( (fp = fopen( sfile, "w" )) == NULL )
    {
        fprintf( stderr, "failure: cannot open '%s' for writing, "
                 "check permissions\n", sfile );
        return -1;
    }

    /* output text casually, uh, borrowed from Ifile.c */
    fprintf( fp,
             "#!/bin/tcsh\n"
             "\n"
             "# This script was automatically generated by '%s'.\n"
             "#\n"
             "# Please modify the following options for your own evil uses.\n"
             "\n"
             "set OutlierCheck = ''         # use '-skip_outliers' to skip\n",
             IFM_PROG_NAME
           );

    if( !opts->gert_prefix )
        fprintf(fp, "set OutPrefix    = 'OutBrick' # prefix for datasets\n");

    /* maybe use an output directory */
    if( opts->gert_outdir )
        fprintf(fp,
             "set OutDir       = '%s'     # output directoy for datasets\n"
             "\n\n"
             "#---------- make sure output directory exists ----------\n"
             "test -d $OutDir || mkdir $OutDir\n",
             opts->gert_outdir );

    fprintf(fp, "\n\n");

    /* create run files, containing lists of all files in a run */
    for ( c = 0; c < s->nused; c++ ) {
        if ( s->runs[c].volumes <= 0 ) continue;

        /*-- create name file, dimon.files.run.001 (maybe with .chan.001) --*/
        if( create_dimon_file_lists(p, outfile, s, c) ) {
            fclose(fp);
            return -1;
        }

        /* remove trailing zeros from TR printing */
        tr = opts->tr > 0 ? opts->tr : s->runs[c].geh.tr;
        sprintf(TR, "%.6f", tr);
        clear_float_zeros(TR);

        /* and write to3d command */
        if( opts->gert_outdir )
            fprintf(fp, "#------- create dataset for run #%d -------\n",c);

        /* if volumes = 1, do not print timing information, 17 Mar 2008 */

        if( ! opts->gert_prefix )
        {
            sprintf(prefixname, "${OutPrefix}_run_%03d", c);
            pname = prefixname;
        } else
            pname = opts->gert_prefix;

        /* if we add a .nii extension, try to adjust backslashes */
        if( opts->gert_format == 1 ) nspaces = 4;
        else                         nspaces = 0;

        if( opts->num_chan > 1 ) {
           indent = 4;
           fprintf(fp, "# process %d channels/echoes\n"
                       "foreach chan ( `count -digits 3 1 %d` )\n",
                       opts->num_chan, opts->num_chan);
        }

        /* if gert_format = 1, write as NIfTI */
        fprintf(fp, "%*sto3d%s%s -prefix %s%s%s%s  \\\n",
                 indent, "",
                 opts->assume_dicom_mosaic==1 ? " -assume_dicom_mosaic" : "",
                 opts->gert_quiterr==1 ? " -quit_on_err" : "",
                 pname,
                 /* if multi-chan, use either prefix or _chan_, else "" */
                 opts->num_chan > 1 ?
                    opts->chan_prefix ? opts->chan_prefix : "_chan_"
                    : "",
                 opts->num_chan > 1 ? "$chan" : "",
                 opts->gert_format==1 ? ".nii" : "" );

        if( s->runs[c].volumes > 1 )
        {
            /* set nslices gert_nz, else mosaic, else volume stats */
            int nslices = s->slices;
            int nvols = s->runs[c].volumes;
            if( opts->num_chan > 1 ) nvols /= opts->num_chan;
            if( opts->gert_nz && s->slices != 1 )
                fprintf(stderr,"** warning: overriding %d slices with %d"
                               " in script %s\n",
                        s->slices, opts->gert_nz, sfile);
            if( opts->gert_nz ) nslices = opts->gert_nz;
            else if ( s->mos_nslices > 1 ) nslices = s->mos_nslices;

            fprintf(fp, "%*s     -time:zt %d %d %ssec %s %*s  \\\n",
                    indent, "", nslices, nvols, TR, spat, nspaces, "");

            /* check siemens timing for errors, just to warn user */
            if( !strcmp(spat, "FROM_IMAGE") )
                valid_g_siemens_times(nslices, tr, 0, 1);
        }

        if( opts->use_last_elem )
           fprintf(fp, "%*s     -use_last_elem                \\\n",indent,"");

        /* have short overflow, convert to floats   9 Jul 2013 */
        if( want_ushort2float )
           fprintf(fp, "%*s     -ushort2float                 \\\n",indent,"");

        /* check -use_obl_origin against oblique   24 Mar 2016 */
        if( s->oblique ) {
          if( opts->use_obl_origin ) {
            fprintf(stderr,"++ oblique data: applying to3d -oblique_origin\n");
            fprintf(fp,"%*s     -oblique_origin               \\\n",indent,"");
          } else fprintf(stderr,
                       "** warning: oblique data, consider -use_obl_origin\n");
        }

        fprintf(fp, "%*s     -@ < %s\n\n", indent, "", outfile);
        if( opts->num_chan > 1 ) fprintf(fp, "end\n\n");

        /* and possibly move output datasets there */
        if( opts->gert_outdir ) {
            fprintf(fp, "mv %s%s $OutDir\n\n", pname,
                    opts->gert_format==1 ? ".nii" : "+orig.*");
        }
    }

    fclose( fp );

    /* warn user about conversion to floats */
    if( want_ushort2float ) {
      if( opts->ushort2float )
        fprintf(stderr,"-- applying requested -ushort2float in GERT_Reco"
                       " to3d command\n\n");
      else
        fprintf(stderr,"** warning, have signed short overflow to unsigned,\n"
                "   applying -ushort2float in GERT_Reco to3d command\n\n");
    }

    /* now make it an executable */
    sprintf(command, "chmod u+x %s", sfile );
    system( command );

    /* and maybe the user wants to actually execute it */
    if( opts->gert_exec ) {
        sprintf(command, "tcsh -x %s", sfile);
        system(command);
    }

    return 0;
}


/* write dimon.files.run.RRR(.chan.CCC), to contain file names
 *
 * populate fn string list with names of those files
 *
 * return 0 on success, else error
 */
int create_dimon_file_lists(param_t *p, char *ret_fname, stats_t *ST, int rind)
{
   FILE  * fp;
   run_t * RT = ST->runs+rind; /* current run_t struct */
   char    fname[64];
   int     nchan, fbase_len, cind, sind, base_sind, vind;

   /* note number of files to create */
   nchan = p->opts.num_chan;
   if( nchan < 1 ) nchan = 1;

   /* initialize fname, and make return list name (maybe with $chan var) */
   sprintf(fname, "dimon.files.run.%03d", rind);
   fbase_len = strlen(fname);

   strcpy(ret_fname, fname);
   if( nchan > 1 ) strcat(ret_fname, ".chan.$chan");

   for( cind = 0; cind < nchan; cind++ ) {

      /* maybe adjust name */
      if( nchan > 1 ) sprintf(fname+fbase_len, ".chan.%03d", cind+1);

      /* open file, write names, close */

      if( (fp = fopen(fname, "w")) == NULL ) {
         fprintf(stderr,"** Dimon files: cannot open '%s' for writing\n",fname);
         return 1;
      }

      if( gD.level > 0 )
         fprintf(stderr,"++ writing dimon file list to %s\n", fname);

      /* write all volumes for this channel (volume cind + k*nchan) */
      for(vind = cind; vind < RT->volumes; vind += nchan) {
         /* the base index for this volume is offset by vind * nslices */
         base_sind = RT->f1index + vind*ST->slices;

         /* write names out, one per line */
         for( sind = 0; sind < ST->slices; sind++ )
            fprintf(fp, "%s\n", p->fim_o[base_sind+sind].fname);
      }

      fclose(fp);
   }

   return 0;
}


/* if index > 0, put index and mesg in name */
FILE * get_file_pointer(char * fname, char * mesg, int index)
{
   char * fnew = NULL;
   FILE * fp   = stderr;
   int    len;

   if     ( ! fname                   ) fp = stderr;
   else if( ! strcmp(fname, "stderr") ) fp = stderr;
   else if( ! strcmp(fname, "stdout") ) fp = stdout;
   else { /* actually try to open */
      if( index ) {
         len = strlen(fname) + (mesg ? strlen(mesg) : 0) + 20;
         fnew = (char *)malloc(len * sizeof(char));
         if( !fnew ) {
            fprintf(stderr,"** failed to allow fnew of length %d\n", len);
            return NULL;
         }
         if(mesg) sprintf(fnew, "%s.%d.%s.txt", fname, index, mesg);
         else     sprintf(fnew, "%s.%d.txt", fname, index);
         if(gD.level>0) fprintf(stderr,"-- writing details to %s...\n", fnew);
      } else fnew = fname;

      fp = fopen(fnew, "w");
      if( !fp ) {
         fprintf(stderr,"** failed to open '%s' for writing\n",fnew);
         return NULL;
      }
      if( fnew != fname ) free(fnew);
   }

    if( gD.level > 2 ) fprintf(stderr,"+d opening file for '%s'...\n",
                               fnew ? fnew : fname ? fname : "def=stderr");


   return fp;
}

/* ----------------------------------------------------------------------
 * Create an output file containing the (sorted?) file list.
 *
 * Either output many details, or just the list of file names.
 * ---------------------------------------------------------------------- */
static int create_file_list( param_t *p, char *fname, int details, char *mesg )
{
    static byte dcount = 0;

    finfo_t   * fip;
    FILE      * fp;
    float       zprev;
    int         c, len, maxlen;

    if( p->nfim <= 0 ) return 0;

    if( gD.level > 0 ) fprintf(stderr,"-- writing file list to %s...\n",
                               CHECK_NULL_STR(fname));
    if( mesg ){ fputs(mesg, stderr); fputs(" : ", stderr); }

    /* open an output file (or use a standard stream) */
    fp = get_file_pointer(fname, mesg, details?dcount:0);
    if( details ) dcount++;

    if( details == 1 ) {
       maxlen = 0;
       for( c = 0; c < p->nfim; c++ ) {
          len = strlen(p->fim_o[c].fname);
          if( len > maxlen ) maxlen = len;
       }
       fprintf(fp, "# %-*s   index findex sindex state  errs", maxlen, "file");
       fprintf(fp, "    zoff       diff    data"
                   "  run   IIND   RIN GEMEIND ATIME\n");
       zprev = p->fim_o[0].geh.zoff;
       for( c = 0, fip = p->fim_o; c < p->nfim; c++, fip++ ) {
          fprintf(fp, "  %-*s   %4d   %4d   %4d   %2d     %d"
                   "  %10.5f  %8.3f     %d"
                   "  %4d   %4d  %4d  %5d  %.3f\n",
                  maxlen, fip->fname, c, fip->findex, fip->sindex,
                  fip->state, fip->bad_reads,
                  fip->geh.zoff, fip->geh.zoff-zprev, fip->imdata?1:0,
                  fip->geh.uv17, fip->geh.im_index, fip->geh.index,
                  fip->gex.ge_me_index, fip->geh.atime);
          zprev = fip->geh.zoff;
       }
    } else if( details == 2 ) {
       int iuid, iuid_prev, order_match = 1;

       iuid_prev = get_sop_iuid_index(&p->fim_o[0].gex)-1;
       maxlen = 0;
       for( c = 0; c < p->nfim; c++ ) {
          len = strlen(p->fim_o[c].fname);
          if( len > maxlen ) maxlen = len;
       }
       fprintf(fp, "# %-*s   index findex sindex  GEME_IND", maxlen, "file");
       fprintf(fp, "  iuid_major  iminor iindex iDIFF\n");

       zprev = p->fim_o[0].geh.zoff;
       for( c = 0, fip = p->fim_o; c < p->nfim; c++, fip++ ) {
          iuid = get_sop_iuid_index(&fip->gex);
          fprintf(fp, "  %-*s   %4d   %4d   %4d     %5d",
                  maxlen, fip->fname, c, fip->findex, fip->sindex,
                  fip->gex.ge_me_index);
          fprintf(fp, "   %8d %6d  %5d   %3d\n", fip->gex.sop_iuid_maj,
                      fip->gex.sop_iuid_min, iuid, iuid-iuid_prev);

          if( order_match ) order_match = (iuid-iuid_prev) == 1;
          iuid_prev = iuid;
       }

       if( gD.level > 1 )
          fprintf(stderr,"-- current IUID order match = %d\n", order_match);
    } else
       for( c = 0; c < p->nfim; c++ )
          fprintf(fp, "%s\n", p->fim_o[c].fname);

    if( fp != stderr && fp != stdout ) fclose(fp);


    if( gD.level > 2 ) fprintf(stderr,"-- file list written for %s\n",
                               fname ? fname : "def=stderr");

    return 0;
}

int test_sop_iuid_index_order(param_t * p)
{
   finfo_t * fip;
   int       c, iuid, iprev;
   int       err = 0;

   if( p->nfim < 1 ) return 0;

   iprev = get_sop_iuid_index(&p->fim_o[0].gex);
   for( c = 1, fip = p->fim_o+1; c < p->nfim; c++, fip++ ) {
      iuid = get_sop_iuid_index(&fip->gex);
      if( (iuid - iprev) != 1 ) {
         fprintf(stderr,"** IUID index failure between %d and %d\n",c-1,c);
         fprintf(stderr,"   image[%d] = %s (iuid %d)\n"
                        "   image[%d] = %s (iuid %d)\n",
                        c-1, p->fim_o[c-1].fname, iprev,
                        c, p->fim_o[c].fname, iuid);
         err = 1;
         break;
      }
      iprev = iuid;
   }

   if( ! err ) fprintf(stderr,"++ %d IUID's are sequential, yay\n", p->nfim);

   return err;
}

/* after subtracting base values for major, minor and geme, return:
 * truncate_to_ngeme_multiple(1000*major-minor) + geme_index; */
int get_sop_iuid_index(ge_extras * ep)
{
   static int base_major=-1, base_minor=-1, base_ngeme=-1, base_geme=-1;
   int        iuid_major, iuid_minor, ngeme, geme_index;
   int        iuid;

   /* extract values from struct */
   iuid_major = ep->sop_iuid_maj;
   iuid_minor = ep->sop_iuid_min;
   ngeme      = ep->ge_nim_acq;
   geme_index = ep->ge_me_index;

   /* maybe init persistent vars */
   if( base_major <= 0 ) {
      base_major = iuid_major;
      base_minor = iuid_minor;
      base_ngeme = ngeme;
      base_geme  = geme_index;
   }

   /* init to 1000*major + minor (with offsets) */
   iuid = 1000*(iuid_major - base_major) + (iuid_minor - base_minor);

   /* truncate to multiple of base_ngene */
   if( base_ngeme > 0 ) iuid -= (iuid % base_ngeme);

   /* return addition of geme (with offset) */
   return iuid + geme_index - base_geme;
}


/*----------------------------------------------------------------------
 * remove trailing zeros from string of printed float
 * return  1 if something was cleared
 *         0 if not
 *----------------------------------------------------------------------*/
static int clear_float_zeros( char * str )
{
   char * dp  = strchr(str, '.'), * valp;
   int    len;

   if( !dp ) return 0;      /* nothing to clear */

   len = strlen(dp);

   /* never clear what is just to the right of '.' */
   for( valp = dp+len-1; (valp > dp+1) && (*valp==' ' || *valp=='0'); valp-- )
       *valp = '\0';     /* clear, so we don't worry about break conditions */

   if( valp < dp + len - 1 ) return 1;
   return 0;
}


/* ----------------------------------------------------------------------
 * Create a gert_reco script for GEMS I-files.
 *
 * Note - stats struct parameters have been checked.
 * ----------------------------------------------------------------------
*/
static int create_gert_reco( stats_t * s, opts_t * opts )
{
    FILE * fp;
    char * spat;                        /* slice acquisition pattern  */
    char   cdir[4], csuff[IFM_SUFFIX_LEN];
    int    num_valid, c;
    char   command[64];                 /* for system command */


    /* if the user did not give a slice pattern string, use the default */
    spat = opts->sp ? opts->sp : IFM_SLICE_PAT;

    for ( c = 0, num_valid = 0; c < s->nused; c++ )
        if ( s->runs[c].volumes > 0 )
            num_valid++;

    if ( num_valid == 0 )
    {
        fprintf( stderr, "-- no runs to use for '%s'\n", IFM_GERT_SCRIPT );
        return 0;
    }

    if ( (fp = fopen( IFM_GERT_SCRIPT, "w" )) == NULL )
    {
        fprintf( stderr, "failure: cannot open '%s' for writing, "
                 "check permissions\n", IFM_GERT_SCRIPT );
        return -1;
    }

    /* output text casually, uh, borrowed from Ifile.c */
    fprintf( fp,
             "#!/bin/tcsh\n"
             "\n"
             "# This script was automatically generated by '%s'.\n"
             "# The script format was, uh, borrowed from Ziad's Ifile.c.\n"
             "#\n"
             "# Please modify the following options for your own evil uses.\n"
             "\n"
             "set OutlierCheck = '-oc'         # use '' to skip outlier check\n"
             "set OutPrefix    = '%s'    # prefix for datasets\n"
             "set OutputDir    = '-od %s'    # where to put output datasets\n"
             "\n"
             "\n",
             IFM_PROG_NAME,
             /* allow user to specify an output prefix  10 Dec 2017 */
             opts->gert_prefix ? opts->gert_prefix : "OutBrick",
             opts->gert_outdir ? opts->gert_outdir : "afni"
           );

    for ( c = 0; c < s->nused; c++ )
        if ( s->runs[c].volumes > 0 )
        {
            if ( path_to_dir_n_suffix(cdir, csuff, s->runs[c].f1name) < 0 )
            {
                fclose( fp );
                return -1;
            }

            fprintf( fp, "@RenamePanga %s %s %d %d $OutPrefix "
                         "-sp %s $OutlierCheck $OutputDir\n",
                     cdir, csuff, s->slices, s->runs[c].volumes, spat );
        }

    fputc( '\n', fp );
    fclose( fp );

    /* now make it an executable */
    system( "chmod u+x " IFM_GERT_SCRIPT );

    /* and maybe the user wants to actually execute it */
    if( opts->gert_exec ) {
        sprintf(command, "tcsh -x %s", IFM_GERT_SCRIPT);
        system(command);
    }

    return 0;
}


/* ----------------------------------------------------------------------
 * - show statistics from the runs
 * - output any requested GERT_Reco file
 * ----------------------------------------------------------------------
*/
static int show_run_stats( stats_t * s )
{
    char * tstr;
    int c;

    if ( s == NULL )
    {
        fprintf( stderr, "failure, SRS - no stats struct!\n" );
        return -1;
    }

    /* check image file type */

    if ( s->mos_nslices > 1 ) {
       if      ( IM_IS_AFNI(gP.ftype)  ) tstr = "(AFNI volume)";
       else if ( IM_IS_DICOM(gP.ftype) ) tstr = "(DICOM mosaic)";
       else                              tstr = "(unknown volume)";
    } else tstr = "";

    if ( s->nalloc <= 0 || s->nused <= 0 )
        return 0;

    printf( "\n\n"
            "final run statistics:\n"
            "    volume info     :\n"
            "        slices      : %d\n"
            "        z_first     : %.4g\n"
            "        z_last      : %.4g\n"
            "        z_delta     : %.4g\n"
            "        oblique     : %s\n"    /* display obliquity  25 Jun 2009 */
            "        mos_nslices : %d %s\n\n",                 /* 30 Dec 2010 */
            s->slices, s->z_first, s->z_last,
            s->oblique ? s->image_dz : s->z_delta,
            s->oblique ? "yes" : "no",
            s->mos_nslices, tstr);

    for ( c = 0; c < s->nused; c++ )
    {
        if ( s->runs[c].volumes > 0 )
            printf( "    run #%4d : volumes = %3d, first file (#%d) = %s\n",
                    c,s->runs[c].volumes,s->runs[c].f1index,s->runs[c].f1name);
    }

    putchar( '\n' );

    if ( gP.opts.gert_reco )
        if ( create_gert_script( s, &gP ) )
            fprintf(stderr,"** failed creating GERT script (and dataset)\n");

    if( gP.opts.flist_file )
       create_file_list(&gP, gP.opts.flist_file, 0, NULL);
    if( gP.opts.flist_details || gD.level > 2 ) {
       create_file_list(&gP, gP.opts.flist_details, 1, "final_list");
       if( sort_method(gP.opts.sort_method) == IFM_SORT_GEME )
          create_file_list(&gP, gP.opts.flist_details, 2, "final_list_geme");
    }

    if( sort_method(gP.opts.sort_method) == IFM_SORT_GEME && gD.level > 1 )
      test_sop_iuid_index_order(&gP);

    fflush( stdout );

    return 0;
}


/* ----------------------------------------------------------------------
 * given tr (in seconds), return a sleep time in ms (approx. 1.1*TR),
 * to make it likely that data is found
 *
 * pass 2 potential trs, and apply the first positive one, else use 4s
 * ----------------------------------------------------------------------
*/
static int nap_time_in_ms( float t1, float t2 )
{
    float tr, fr;
    int   nap_time;

    if     ( t1 > 0.0 ) tr = t1;
    else if( t2 > 0.0 ) tr = t2;
    else                tr = 2.0;

    /* note the fraction of a TR to sleep */
    fr = ( gP.opts.sleep_frac > 0.0 ) ? gP.opts.sleep_frac : 1.1;

    nap_time = (int)(1000.0*fr*tr + 0.5);       /* convert to time, in ms */

    if ( nap_time < 10 )    return 10;          /* 10 ms is minimum */

    if ( nap_time > 30000 ) return 30000;       /* 30 sec is maximum */

    if ( gD.level > 1 )
        fprintf(stderr,"-d computed nap_time is %d ms (TR = %.2f)\n",
                nap_time, tr );

    return( nap_time );
}


/* ----------------------------------------------------------------------
 * find_next_zoff
 *
 * Given p->fim_o, search from index start for an image with
 * geh.zoff equal to zoff.
 *
 * if enough images but we are not finding it, warn user
 *
 * return   index : upon succes         (start <= index <= p->nused)
 *             -1 : not found
 *             -2 : error
 * ----------------------------------------------------------------------
*/
static int find_next_zoff(param_t *p, int start, float zoff, int nim, int warn)
{
    int count;

    if ( (p == NULL) || (start < 0) )
        return -2;

    if ( start >= p->nfim )             /* return not found */
        return -1;

    for ( count = start; count < p->nfim; count++ )
        if ( fabs( zoff - p->fim_o[count].geh.zoff ) < gD_epsilon )
            return count;                       /* found! */

    /* did not find it, but have extra images waiting, so warn user */
    if ( warn && p->nfim-start >= 2*nim )
        fprintf(stderr,
                "** warning: have enough images (%d) for new vol (%d)\n"
                "            but cannot find zoff %.4f, have %.4f (in %s)\n",
                       p->nfim-start, nim, zoff, p->fim_o[start].geh.zoff,
                       p->fim_o[start].fname);

    return -1;
}


/* ----------------------------------------------------------------------
 * Given:  run     > 0
 *         seq_num > 0
 *         naps
 *         tr_naps
 *         naps_time    in ms
 *
 * If naps is too big, and the run is incomplete, print an obnoxious
 * warning message to the user.
 *
 * Too big means naps > max_naps.
 *
 * notes:   - print only 1 warning message per seq_num, per run
 *          - prev_run and prev_seq_num are for the previously found volume
 *
 * returns:
 *          2 : run is stalled - message printed
 *          1 : apparent end of a run
 *          0 : no stall, or if a message has already been printed
 *         -1 : function failure
 * ----------------------------------------------------------------------
*/
static int check_stalled_run ( int run, int seq_num, int naps, int max_naps,
                               int nap_time )
{
    static int func_failure =  0;
    static int prev_run     = -1;
    static int prev_seq     = -1;

    if ( func_failure != 0 )
        return 0;

    if ( (run < 1) || (seq_num < 1) || (naps <= max_naps) ) return 0;

    /* verify that we have already taken note of the previous volume */
    if ( (((gS.nused + 1) < run) || (gS.runs[run].volumes < seq_num)) &&
         ( func_failure == 0 ) )
    {
        fprintf( stderr, "** warning: CSR - stats inconsistancy!\n" );
        func_failure = 1;

        return -1;
    }

    if ( seq_num < gS.nvols )      /* are we done with the run yet? */
    {
        /* if we haven't printed before, this is the first stalled case */
        if ( (run != prev_run) || (seq_num != prev_seq) )
        {
            fprintf( stderr, "\007\n"
                     "****************************************************\n"
                     "Warning: run seems to be stalled\n"
                     "\n"
                     "    run                    : %d\n"
                     "    TRs completed          : %d (of %d)\n"
                     "    approximate idle time  : %d seconds\n"
                     "    first file of this run : %s\n"
                     "****************************************************\n",
                     run, seq_num, gS.nvols,
                     naps*nap_time/1000, gS.runs[run].f1name );

            prev_run = run;
            prev_seq = seq_num;

            return 2;
        }
    }
    /* else (we are done) */
    else if ( (run != prev_run) || (seq_num != prev_seq) )
    {
        /* this is our first visit, note the fact */
        prev_run = run;
        prev_seq = seq_num;

        return 1;
    }

    return 0;
}

/*-------------------------------------------------------*/
/*! Return the file length (-1 if file not found).       */
/*  (local copy from thd_filestuff.c                     */

unsigned long l_THD_filesize( char * pathname )
{
    static struct stat buf ; int ii ;

    if( pathname == NULL ) return -1 ;
        ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;

    return buf.st_size ;
}


/* ----------------------------------------------------------------------
 * Determine the byte order of the image.
 *
 * Note our byte order (LSB_FIRST or MSB_FIRST).
 * If gex.swap is set, reverse it.  If user wants opposite, reverse it.
 *
 * return   0 : success
 *         -1 : on error
 * ----------------------------------------------------------------------
*/
static int check_im_byte_order( int * order, vol_t * v, param_t * p )
{
    int one = 1;

    if ( (order == NULL) || (v == NULL) || (p == NULL) )
    {
        fprintf( stderr, "** invalid parameters to CIBO (%p,%p,%p)\n",
                 order, v, p );
        return -1;
    }

    /* note the order for the current system */
    *order = (*(char *)&one == 1) ? LSB_FIRST : MSB_FIRST;

    if ( gD.level > 1 )
        fprintf( stderr, "-- system order is %s, ",
                 (*order == MSB_FIRST) ? "MSB_FIRST" : "LSB_FIRST" );

    /* are the images the opposite of this?  does the user want the opposite? */
    if ( p->fim_o[v->fs_1].gex.swap ^ p->opts.rev_bo )
        *order = LSB_FIRST + MSB_FIRST - *order;      /* for entertainment */

    if ( gD.level > 1 )
        fprintf( stderr, "image order is %s\n",
                 (*order == MSB_FIRST) ? "MSB_FIRST" : "LSB_FIRST" );

    return 0;
}


/* ----------------------------------------------------------------------
 * Use gex.kk to figure out the z orientation, and complete
 * the v->geh.orients string.
 *
 * orient(kk) = { LR, if kk = 1
 *              { PA, if kk = 2
 *              { IS, if kk = 3
 *
 * return   0 : success
 *         -1 : on error
 * ----------------------------------------------------------------------
*/
static int complete_orients_str( vol_t * v, param_t * p )
{
    int kk;

    if ( (v == NULL) || (p == NULL) )
    {
        fprintf( stderr, "** invalid parameters to COS (%p,%p)\n", v, p );
        return -1;
    }

    if ( IM_IS_AFNI(p->ftype) ) {
        if ( gD.level > 2 )
            fprintf(stderr,"-- have AFNI orient string %s\n", v->geh.orients);
        return 0;
    }

    if ( v->minfo.im_is_volume ) {
        /* orients string should already be complete */
        if ( gD.level > 2 )
            fprintf(stderr,"-- mosaic orients string: %s", v->geh.orients);
        return 0;
    }

    if ( gD.level > 2 )
        fprintf(stderr,"completing orients from '%s' to ", v->geh.orients);

    /* terminate this in case we print it early */
    v->geh.orients[6] = '\0';

    if ( IM_IS_DICOM(p->ftype) ) {
        static int si_report=1;
        char zo = MRILIB_orients[4];
        strncpy(v->geh.orients + 4, MRILIB_orients + 4, 2 );
        /* might have to reverse the slice order after sorting */
        /* inverted images noted by Wenming Luh     8 Nov 2016 */
        if ( ( v->z_delta < 0 && (zo == 'R' || zo == 'A' || zo == 'I') ) ||
             ( v->z_delta > 0 && (zo == 'L' || zo == 'P' || zo == 'S') ) ) {
           v->geh.orients[4] = v->geh.orients[5];
           v->geh.orients[5] = zo;
           if( si_report || gD.level > 3 ) {
              fprintf(stderr,"-- reversing z-orient to '%s'\n",
                      v->geh.orients+4);
              si_report = 0;
           }
        }
    }
    else if ( IM_IS_GEMS(p->ftype) )
    {
        kk = p->fim_o[v->fs_1].gex.kk;

        switch( kk )
        {
            case 1:                                     /* LR */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'L';
                    v->geh.orients[5] = 'R';
                }
                else
                {
                    v->geh.orients[4] = 'R';
                    v->geh.orients[5] = 'L';
                }
                break;

            case 2:                                     /* PA */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'P';
                    v->geh.orients[5] = 'A';
                }
                else
                {
                    v->geh.orients[4] = 'A';
                    v->geh.orients[5] = 'P';
                }
                break;

            case 3:                                     /* IS */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'I';
                    v->geh.orients[5] = 'S';
                }
                else
                {
                    v->geh.orients[4] = 'S';
                    v->geh.orients[5] = 'I';
                }
                break;

            default:
            {
                fprintf(stderr, "** COS failure: kk (%d) not in [1,3]\n", kk);
                return -1;
            }
        }
    }

    if ( gD.level > 2 ) fprintf(stderr,"'%s'\n", v->geh.orients);

    return 0;
}


/* ----------------------------------------------------------------------
 * try to locate the starting file in the fim_o list
 *
 * if found, set all previous entries to SKIP and set fim_skip
 *
 * We can have any combination of start_dir and start_file.
 *
 * return  -1 on error, 0 if not found, 1 if found and set
 * ----------------------------------------------------------------------
*/
static int set_starting_fim( param_t * p )
{
    finfo_t * flp;
    char  * sd, * sf;
    int     index, found = 0;
    int     dlen;

    if( ! p->opts.start_file && ! p->opts.start_dir ) {
       fprintf(stderr,"** no start_file to find\n");
       return -1;
    }

    sd = p->opts.start_dir;
    sf = p->opts.start_file;

    if ( gD.level > 2 )
        fprintf(stderr,"-d searching for initial dir, file: %s, %s\n",
                sd ? sd : "<no start_dir>", sf ? sf : "<no start_file>");

    index = 0;       /* init search params */
    flp = p->fim_o;

    if ( sd ) {
        dlen = strlen(sd);
        for ( ; index < p->nfim; index++ )
            if ( ! strncmp(sd, flp[index].fname, dlen) ) { found = 1; break; }
    }

    if ( sf ) { /* then further continue the search until the file is found */
        found = 0;
        for ( ; index < p->nfim; index++ )
            if ( ! strcmp(flp[index].fname, sf) ) { found = 1; break; }
    }

    if ( gD.level > 2 ) {
        if(found) fprintf(stderr,"-d found starting entry %s at index %d\n",
                          flp[index].fname, index);
        else      fprintf(stderr,"-d no starting entry match found (yet)...\n");
    }

    /* if found, note index and set all previous states to SKIP */
    if( found ) {
       p->fim_skip = index;
       update_states(p, 0, index-1, IFM_FSTATE_SKIP, 0);
    } else {
       /* else, maybe whine a little */
       static int nwarn = 1;
       if ( nwarn && gD.level > 0 ) {
          fprintf(stderr, "-- still searching for start_file, '%s'\n",
                  p->opts.start_file ? p->opts.start_file : p->opts.start_dir );
          nwarn--;
       }
    }

    return found;
}

/* ----------------------------------------------------------------------
 * return the number of occurances of 'target' in 'str' of length 'len'
 * ----------------------------------------------------------------------
*/
static int str_char_count( char * str, int len, char target )
{
    char * cp;
    char * last = str + len;
    int    num = 0;

    if ( (str == NULL) || (len <= 0) )
        return 0;

    for ( cp = str; cp < last; cp++ )
        if ( *cp == target )
            num++;

    return num;
}


/* ----------------------------------------------------------------------
 * Given path, find dir and suff.
 *
 * dir    - 3 character starting directory           (e.g. 003)
 * suff   - 10 char (max) trailing I-file suffix     (e.g. 017, for I.017)
 * path   - first file in run                        (e.g. 003/I.017)
 *
 * return  0 : success
 *        -1 : failure
 * ----------------------------------------------------------------------
*/
static int path_to_dir_n_suffix( char * dir, char * suff, char * path )
{
    char * cp, *cp2;

    if ( (dir == NULL) || (suff == NULL) || (path == NULL) )
    {
        fprintf( stderr, "failure: PTDNS - invalid params (%p,%p,%p)\n",
                 dir, suff, path );
        return -1;
    }

    /* find last '.' */
    for ( cp = path + strlen(path) - 1; (*cp != '.') && (cp > path); cp-- )
        ;

    if ( *cp != '.' )
    {
        fprintf( stderr, "failure: cannot find suffix in '%s'\n", path );
        return -1;
    }
    else if ( strlen( cp ) > IFM_SUFFIX_LEN )          /* '.' not included */
    {
        fprintf( stderr, "failure: suffix too long in '%s'\n", path );
        return -1;
    }

    strcpy( suff, cp+1 );               /* copy null-terminated suffix */

    /* make sure all characters are digits (treat as string, not int) */
    for ( cp2 = suff; (*cp2 != '\0') && isdigit(*cp2); cp2++ )
       ;

    if ( *cp2 != '\0' )
    {
        fprintf( stderr, "failure: suffix not integer in '%s'\n", path );
        return -1;
    }

    /* now get dir prefix - should be nnn/I.mmm */
    cp -= 5;
    if ( ( cp < path )          ||    /* we should have a directory here */
         ( ! isdigit( cp[0] ) ) ||    /* then 3 digits */
         ( ! isdigit( cp[1] ) ) ||
         ( ! isdigit( cp[2] ) ) ||
         (   cp[3] != '/'     ) ||
         (   cp[4] != 'I'     ) )
    {
        fprintf( stderr, "failure: PTDNS - ill-formed path '%s'\n", path );
        return -1;
    }

    /* we are set, just copy the data */
    strncpy( dir, cp, 3 );
    dir[3] = '\0';

    return 0;
}


int disp_obl_info(char * mesg)
{
    int i, j;
    if( mesg ) fputs(mesg, stderr);

    if(! obl_info_set) {
        fprintf(stderr,"** oblique info is not set\n");
        return 1;
    }

    fprintf(stderr,"-- oblique info = %d\n", obl_info_set);

    for(i = 0; i < 4; i++) {
        fprintf(stderr,"    ");
        for(j=0; j<4; j++) fprintf(stderr,"%10.4f  ", obl_info.Tr_dicom[i][j]);
        fputc('\n', stderr);
    }

    return 0;
}
