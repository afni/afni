#ifndef _FIELDS_H_
#define _FIELDS_H_

/*------------------------- from nifti_tool.h: -------------------------*/

#define FT_FIELD_NAME_LEN  20       /* more than length of longest name */
#define FT_ANA_NUM_FIELDS  47       /* see mayo_analyze.h: 7+22+18      */
#define FT_DT_STRING      -0xfff    /* some strange number to abuse...  */
#define FT_DT_POINTER     -0xfef    /* some strange number to abuse...  */
#define FT_DT_CHAR_PTR    -0xfee    /* another...                       */

typedef struct{
   int     len;
   char ** list;
} str_list;

typedef struct{
   int     len;
   int   * list;
} int_list;

typedef struct {
   int  type;                    /* one of the DT_* types from nifti1.h */
   int  offset;                  /* bytes from the start of the struct  */
   int  size;                    /* size of one element type            */
   int  len;                     /* number of elements                  */
   char name[FT_FIELD_NAME_LEN]; /* actual structure name used          */
} field_s;

/* for computing the offset from the start of the struct */
#define FT_OFF(str,field) ((int)( ((char *)&str.field) - ((char *)&str) ))

/* call fill_field() for a single type, name and number of elements */
/* nstr is the base struct, and fldp is a field pointer */
#define FT_SFILL(nstr,fldp,type,name,num,rv) do{                   \
           rv=fill_field(fldp,type,FT_OFF(nstr,name),num,#name);   \
           fldp++; } while (0)

/*------------------------- from mayo_analyze.h: -------------------------*/
/* this is a bastardized version of struct dsr, into a single structure   */
/* hopefully none of the compilers will want to shift the offsets (though */
/* that would change the 348 byte size of it, eh?).                       */

typedef struct {
       /* header info fields */
       int sizeof_hdr;                  /* 0 + 4           */
       char data_type[10];              /* 4 + 10          */
       char db_name[18];                /* 14 + 18         */
       int extents;                     /* 32 + 4          */
       short int session_error;         /* 36 + 2          */
       char regular;                    /* 38 + 1          */
       char hkey_un0;                   /* 39 + 1          */

       /* image dimension fields */
       short int dim[8];                /* 0 + 16          */
       short int unused8;               /* 16 + 2          */
       short int unused9;               /* 18 + 2          */
       short int unused10;              /* 20 + 2          */
       short int unused11;              /* 22 + 2          */
       short int unused12;              /* 24 + 2          */
       short int unused13;              /* 26 + 2          */
       short int unused14;              /* 28 + 2          */
       short int datatype;              /* 30 + 2          */
       short int bitpix;                /* 32 + 2          */
       short int dim_un0;               /* 34 + 2          */
       float pixdim[8];                 /* 36 + 32         */

       float vox_offset;                /* 68 + 4          */
       float funused1;                  /* 72 + 4          */
       float funused2;                  /* 76 + 4          */
       float funused3;                  /* 80 + 4          */
       float cal_max;                   /* 84 + 4          */
       float cal_min;                   /* 88 + 4          */
       float compressed;                /* 92 + 4          */
       float verified;                  /* 96 + 4          */
       int glmax,glmin;                 /* 100 + 8         */

       /* data history fields */
       char descrip[80];                /* 0 + 80          */
       char aux_file[24];               /* 80 + 24         */
       char orient;                     /* 104 + 1         */
       char originator[10];             /* 105 + 10        */
       char generated[10];              /* 115 + 10        */
       char scannum[10];                /* 125 + 10        */
       char patient_id[10];             /* 135 + 10        */
       char exp_date[10];               /* 145 + 10        */
       char exp_time[10];               /* 155 + 10        */
       char hist_un0[3];                /* 165 + 3         */
       int views;                       /* 168 + 4         */
       int vols_added;                  /* 172 + 4         */
       int start_field;                 /* 176 + 4         */
       int field_skip;                  /* 180 + 4         */
       int omax, omin;                  /* 184 + 8         */
       int smax, smin;                  /* 192 + 8         */
} ft_analyze_header;

/* protos */
int       get_field_verb        ( void );
void      set_field_verb        ( int );

field_s * make_ana_field_array  ( void );

int       add_string            ( str_list *, char * );
int       check_total_size      ( char *, field_s *, int, int );
int       clear_float_zeros     ( char * str );

int       diff_field            ( field_s *, void *, void *, int );
int       disp_field            ( char *, field_s *, void *, int, int, int );
int       disp_field_s_list     ( char *mesg, field_s * fp, int nfields );
int       disp_raw_data         ( void *, int, int, char, int, int );
char    * field_type_str        ( int type );
int       fill_field            ( field_s *, int, int, int, char * );
void      ft_datatype_sizes     ( int datatype , int *nbyper, int *swapsize );

int       modify_field          ( void * basep, field_s * field, char * data );
int       modify_many_fields    ( void *,str_list *,str_list *,field_s *, int);
int       swap_fields           ( field_s *fieldp, void * str, int nfields );





#endif  /* ifndef _FIELDS_H_ */
