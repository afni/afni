
#include "mrilib.h"
#include "mayo_analyze.h"

void ShowHdr(char *, struct dsr *);
void swap_long(void *);
void swap_short(void *);

int main( int argc , char * argv[] )
{
    struct dsr hdr;
    int size;
    double cmax, cmin;
    FILE *fp;

    if((fp=fopen(argv[1],"r"))==NULL)
    {
        fprintf(stderr,"Can't open:<%s>\n", argv[1]);
        exit(0);
    }
    fread(&hdr,1,sizeof(struct dsr),fp);

    if(hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15)
       swap_hdr(&hdr);

    ShowHdr(argv[1], &hdr);
}


void ShowHdr(char *fileName, struct dsr *hdr)
{
   int i;
   char string[128];
   printf("Analyze Header Dump of: <%s> \n", fileName);
/* Header Key */
   printf("sizeof_hdr: <%d> \n", hdr->hk.sizeof_hdr);
   printf("data_type:  <%s> \n", hdr->hk.data_type);
   printf("db_name:    <%s> \n", hdr->hk.db_name);
   printf("extents:    <%d> \n", hdr->hk.extents);
   printf("session_error: <%d> \n", hdr->hk.session_error);
   printf("regular:  <%c> \n", hdr->hk.regular);
   printf("hkey_un0: <%c> \n", hdr->hk.hkey_un0);

/* Image Dimension */
   for(i=0;i<8;i++)
       printf("dim[%d]: <%d> \n", i, hdr->dime.dim[i]);

#if 0
   strncpy(string,hdr->dime.vox_units,4);
   printf("vox_units:  <%s> \n", string);
#endif

#if 0
   strncpy(string,hdr->dime.cal_units,8);
   printf("cal_units: <%s> \n", string);
   printf("unused1:   <%d> \n", hdr->dime.unused1);
#endif
   printf("datatype:  <%d> \n", hdr->dime.datatype);
   printf("bitpix:    <%d> \n", hdr->dime.bitpix);

   for(i=0;i<8;i++)
       printf("pixdim[%d]: <%6.4f> \n",i, hdr->dime.pixdim[i]);

   printf("vox_offset: <%6.4f> \n",  hdr->dime.vox_offset);
   printf("funused1:   <%6.4f> \n", hdr->dime.funused1);
   printf("funused2:   <%6.4f> \n", hdr->dime.funused2);
   printf("funused3:   <%6.4f> \n", hdr->dime.funused3);
   printf("cal_max:    <%6.4f> \n", hdr->dime.cal_max);
   printf("cal_min:    <%6.4f> \n", hdr->dime.cal_min);
   printf("compressed: <%d> \n", hdr->dime.compressed);
   printf("verified:   <%d> \n", hdr->dime.verified);
   printf("glmax:      <%d> \n", hdr->dime.glmax);
   printf("glmin:      <%d> \n", hdr->dime.glmin);

/* Data History */
   strncpy(string,hdr->hist.descrip,80);
   printf("descrip:  <%s> \n", string);
   strncpy(string,hdr->hist.aux_file,24);
   printf("aux_file: <%s> \n", string);
   printf("orient:   <%d> \n", hdr->hist.orient);

   strncpy(string,hdr->hist.originator,10);
   printf("originator: <%s> \n", string);

   strncpy(string,hdr->hist.generated,10);
   printf("generated: <%s> \n", string);

   strncpy(string,hdr->hist.scannum,10);
   printf("scannum: <%s> \n", string);

   strncpy(string,hdr->hist.patient_id,10);
   printf("patient_id: <%s> \n", string);

   strncpy(string,hdr->hist.exp_date,10);
   printf("exp_date: <%s> \n", string);

   strncpy(string,hdr->hist.exp_time,10);
   printf("exp_time: <%s> \n", string);

   strncpy(string,hdr->hist.hist_un0,10);
   printf("hist_un0: <%s> \n", string);

   printf("views:      <%d> \n", hdr->hist.views);
   printf("vols_added: <%d> \n", hdr->hist.vols_added);
   printf("start_field:<%d> \n", hdr->hist.start_field);
   printf("field_skip: <%d> \n", hdr->hist.field_skip);
   printf("omax: <%d> \n", hdr->hist.omax);
   printf("omin: <%d> \n", hdr->hist.omin);
   printf("smin: <%d> \n", hdr->hist.smax);
   printf("smin: <%d> \n", hdr->hist.smin);

}

swap_hdr(struct dsr *pntr)
{
       swap_long(&pntr->hk.sizeof_hdr) ;
       swap_long(&pntr->hk.extents) ;
       swap_short(&pntr->hk.session_error) ;
       swap_short(&pntr->dime.dim[0]) ;
       swap_short(&pntr->dime.dim[1]) ;
       swap_short(&pntr->dime.dim[2]) ;
       swap_short(&pntr->dime.dim[3]) ;
       swap_short(&pntr->dime.dim[4]) ;
       swap_short(&pntr->dime.dim[5]) ;
       swap_short(&pntr->dime.dim[6]) ;
       swap_short(&pntr->dime.dim[7]) ;
#if 0
       swap_short(&pntr->dime.unused1) ;
#endif
       swap_short(&pntr->dime.datatype) ;
       swap_short(&pntr->dime.bitpix) ;
       swap_long(&pntr->dime.pixdim[0]) ;
       swap_long(&pntr->dime.pixdim[1]) ;
       swap_long(&pntr->dime.pixdim[2]) ;
       swap_long(&pntr->dime.pixdim[3]) ;
       swap_long(&pntr->dime.pixdim[4]) ;
       swap_long(&pntr->dime.pixdim[5]) ;
       swap_long(&pntr->dime.pixdim[6]) ;
       swap_long(&pntr->dime.pixdim[7]) ;
       swap_long(&pntr->dime.vox_offset) ;
       swap_long(&pntr->dime.funused1) ;
       swap_long(&pntr->dime.funused2) ;
       swap_long(&pntr->dime.cal_max) ;
       swap_long(&pntr->dime.cal_min) ;
       swap_long(&pntr->dime.compressed) ;
       swap_long(&pntr->dime.verified) ;
       swap_short(&pntr->dime.dim_un0) ;
       swap_long(&pntr->dime.glmax) ;
       swap_long(&pntr->dime.glmin) ;
}

void swap_long(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
        unsigned char b0, b1, b2, b3;

        b0 = *pntr;
        b1 = *(pntr+1);
        b2 = *(pntr+2);
        b3 = *(pntr+3);

        *pntr = b3;
        *(pntr+1) = b2;
        *(pntr+2) = b1;
        *(pntr+3) = b0;
}

void swap_short(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
        unsigned char b0, b1;

        b0 = *pntr;
        b1 = *(pntr+1);

        *pntr = b1;
        *(pntr+1) = b0;
}
