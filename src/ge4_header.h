
/* ------------------------------------------------------------ */
/* GE MR Signa 4.x header type : GEMS 46-021858                 */

#define GE4_HEADER_LENGTH   0x03800    /*  28 x 256 x 2 ( 28 blocks) */
#define GE4_IMAGE_SIZE      0x20000    /* 256 x 256 x 2 (256 blocks) */

/* ---- display constants ---- */

#define GE4_DISP_NONE	       0x00
#define GE4_DISP_IMAGE	       0x01
#define GE4_DISP_SERIES	       0x02
#define GE4_DISP_STUDY	       0x04
#define GE4_DISP_ALL	       0xff

/* ---- string constants ---- */

#define GE4_IMAGE_TITLE   "IMAGE HEADER  04"
#define GE4_SERIES_TITLE  "SERIES HEADER 04"
#define GE4_STUDY_TITLE   "STUDY HEADER  04"

/* ---- series header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_SER_TITLE		0x1000	/* = (0x1000) 		*/
#define GE4_OFF_SER_SERIES_NUM		0x103e	/* = (0x1000 + 2 * 031) */
#define GE4_OFF_SER_PLANE_TYPE		0x1114	/* = (0x1000 + 2 * 138) */
#define GE4_OFF_SER_PLANE_DESC		0x1116	/* = (0x1000 + 2 * 139) */
#define GE4_OFF_SER_IM_MODE		0x1126	/* = (0x1000 + 2 * 147) */
#define GE4_OFF_SER_PULSE_SEQ		0x112a	/* = (0x1000 + 2 * 149) */
#define GE4_OFF_SER_FOV			0x112e	/* = (0x1000 + 2 * 151) */
#define GE4_OFF_SER_CENTER		0x1132	/* = (0x1000 + 2 * 153) */
#define GE4_OFF_SER_ORIENT		0x113e	/* = (0x1000 + 2 * 159) */
#define GE4_OFF_SER_SCAN_MAT_X		0x118e	/* = (0x1000 + 2 * 199) */
#define GE4_OFF_SER_SCAN_MAT_Y		0x1190	/* = (0x1000 + 2 * 200) */
#define GE4_OFF_SER_IM_MAT		0x1192	/* = (0x1000 + 2 * 201) */

/* ---- string lengths ---- */

#define GE4_L_SER_TITLE		16
#define GE4_L_SER_SER_NUM	 3
#define GE4_L_SER_PL_DESC	12

/* ---- image header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_IMG_TITLE		0x1400
#define GE4_OFF_IMG_IM_NUM		0x1458	/* = (0x1400 + 2 * 044) */
#define GE4_OFF_IMG_IM_LOCN		0x1492	/* = (0x1400 + 2 * 073) */
#define GE4_OFF_IMG_TABLE_POSN		0x1496	/* = (0x1400 + 2 * 075) */
#define GE4_OFF_IMG_IM_THICK		0x149a	/* = (0x1400 + 2 * 077) */
#define GE4_OFF_IMG_IM_SPACING		0x149e	/* = (0x1400 + 2 * 079) */
#define GE4_OFF_IMG_TR			0x14a4	/* = (0x1400 + 2 * 082) */
#define GE4_OFF_IMG_TE			0x14ac	/* = (0x1400 + 2 * 086) */
#define GE4_OFF_IMG_TI			0x14b0	/* = (0x1400 + 2 * 088) */
#define GE4_OFF_IMG_NUM_ECHOS		0x14c4	/* = (0x1400 + 2 * 098) */
#define GE4_OFF_IMG_ECHO_NUM		0x14c6	/* = (0x1400 + 2 * 099) */
#define GE4_OFF_IMG_NEX_INT		0x14ca	/* = (0x1400 + 2 * 101) */
#define GE4_OFF_IMG_NEX_REAL		0x1524	/* = (0x1400 + 2 * 146) */
#define GE4_OFF_IMG_FLIP_ANGLE		0x155e	/* = (0x1400 + 2 * 175) */

/* ---- image header field lengths ---- */

#define GE4_L_IM_TITLE		16
#define GE4_L_IM_NUM		 3

/* ---- study header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_STDY_TITLE		0x0c00
#define GE4_OFF_STDY_NUM		0x0c40	/* = (0x0c00 + 2 * 032) */
#define GE4_OFF_STDY_DATE		0x0c4e	/* = (0x0c00 + 2 * 039) */
#define GE4_OFF_STDY_TIME		0x0c5e	/* = (0x0c00 + 2 * 047) */
#define GE4_OFF_STDY_PAT_NAME		0x0c6c	/* = (0x0c00 + 2 * 054) */
#define GE4_OFF_STDY_PAT_ID		0x0c8c	/* = (0x0c00 + 2 * 070) */
#define GE4_OFF_STDY_AGE		0x0c9c	/* = (0x0c00 + 2 * 078) */
#define GE4_OFF_STDY_SEX		0x0ca0	/* = (0x0c00 + 2 * 080) */

/* ---- study header field lengths ---- */

#define GE4_L_STDY_TITLE	16
#define GE4_L_STDY_NUM		 5
#define GE4_L_STDY_DATE		 9
#define GE4_L_STDY_TIME		 8
#define GE4_L_STDY_PAT_NAME	32
#define GE4_L_STDY_PAT_ID	12
#define GE4_L_STDY_AGE		 3

/* ---------------------------------------------------------------------- */

/* ---- actual data structures ---- */
typedef struct
{
    char    title     [GE4_L_STDY_TITLE    + 1];
    char    num       [GE4_L_STDY_NUM      + 1];
    char    date      [GE4_L_STDY_DATE     + 1];
    char    time      [GE4_L_STDY_TIME     + 1];
    char    pat_name  [GE4_L_STDY_PAT_NAME + 1];
    char    pat_id    [GE4_L_STDY_PAT_ID   + 1];
    char    age       [GE4_L_STDY_AGE      + 1];
    char    sex;
} ge4_study_t;

typedef struct
{
    char    title     [GE4_L_SER_TITLE+1];	/* words 000-015 */
    char    series_num[GE4_L_SER_SER_NUM+1];	/* word  031     */
    short   plane_type;				/* word  138     */
    char    plane_desc[GE4_L_SER_PL_DESC+1];	/* word  139     */
    short   im_mode;				/* word  147     */
    short   pulse_seq;				/* word  149     */
    float   fov;				/* words 151,152 */
    float   center[3];				/* words 153-158 */
    short   orient;				/* word  159     */
    short   scan_mat_x;				/* word  199     */
    short   scan_mat_y;				/* word  200     */
    short   im_mat;				/* word  201     */
} ge4_series_t;

typedef struct
{
    char    title [GE4_L_IM_TITLE+1];		/* word  000     */
    char    im_num[GE4_L_IM_NUM+1];		/* words 044-046 */
    float   im_loc;				/* words 073,074 */
    float   table_posn;				/* words 075,076 */
    float   im_thickness;			/* words 077,078 */
    float   im_spacing;				/* words 079,080 */
    float   tr;			/* uS */	/* words 082,083 */
    float   te;			/* uS */	/* words 082,083 */
    float   ti;			/* uS */	/* words 082,083 */
    short   num_echoes;				/* word  098     */
    short   echo_num;				/* word  099     */
    short   iNEX;				/* word  101     */
    float   fNEX;				/* words 146,147 */
    short   flip_angle;				/* word  175     */
} ge4_image_t;

typedef struct
{
    ge4_study_t		std_h;		/* series header data      */
    ge4_series_t	ser_h;		/* series header data      */
    ge4_image_t 	im_h;		/* image header data       */
    short             * image;		/* image data, if non-NULL */
    int                 im_bytes;	/* size of image, in bytes */
    int			swap;		/* was the data swapped?   */
} ge4_header;


/* global prototypes */
int ge4_read_header	     ( ge4_header * H, char * filename, int get_image );
int idisp_ge4_image_header   ( char * info, ge4_image_t * im );
int idisp_ge4_series_header  ( char * info, ge4_series_t * s );
int idisp_ge4_study_header   ( char * info, ge4_study_t * st );


int ge4_swap_all_bytes	     ( ge4_header * h );
int idisp_ge4_series_header  ( char * info, ge4_series_t * s );
int ge4_validate_header      ( ge4_header * h );

/* ---------------------------------------------------------------------- */

#define GE4M_IND2STR(ind,str_list)					    \
		 ( ind < 0 || (ind > (sizeof(str_list)/sizeof(char *))) ) ? \
			      "out-of-range" : str_list[ind]

