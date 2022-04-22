#ifndef _MRI_DICOM_ELIST_HEADER_
#define _MRI_DICOM_ELIST_HEADER_

/*----------------------------------------------------------------------------*/

static char *elist[] = {

 "0018 0050" ,  /* Slice thickness */
 "0018 0080" ,  /* Repetition time */
 "0018 0088" ,  /* Spacing between slices */
 "0018 1149" ,  /* Field of view */

 "0020 0020" ,  /* Patient orientation */
 "0020 0032" ,  /* Image position (patient) */
 "0020 0037" ,  /* Image orientation (patient) */
 "0020 1041" ,  /* Slice location */

 "0028 0002" ,  /* Samples per pixel */
 "0028 0008" ,  /* Number of frames */
 "0028 0010" ,  /* Rows */
 "0028 0011" ,  /* Columns */
 "0028 0030" ,  /* Pixel spacing */
 "0028 0100" ,  /* Bits allocated */
 "0028 0101" ,  /* Bits stored */
 "0028 1052" ,  /* Rescale intercept */
 "0028 1053" ,  /* Rescale slope */
 "0028 1054" ,  /* Rescale type */
 "0028 0004" ,  /* Photometric interpretation */
 "0028 0103" ,  /* Pixel representation */
 "0028 0102" ,  /* High bit */
 "0028 1050" ,  /* Window center */
 "0028 1051" ,  /* Window width */

 "0008 0008" ,  /* ID Image type */
 "0008 0032" ,  /* ID Acquisition Time (Philips)        20 Oct 2010 [rickr] */
 "0008 0070" ,  /* ID Manufacturer */
 "0018 1310" ,  /* Acquisition Matrix */

 "0029 1010" ,  /* Siemens addendum #1 */
 "0029 1020" ,  /* Siemens addendum #2 */

 "0002 0010" ,  /* Transfer Syntax [RWC - 05 Jul 2006] */

 /*--- GE multi-echo EPI sequences [RCR - Aug 2014] ---*/
 "0008 0018" ,  /* SOP (service object pair) Instance UID - unique to DICOM file*/
 "0018 0081" ,  /* ACQ Echo Time    (new GE field) [RCR - 24 Mar 2022] */
 "0018 0086" ,  /* ACQ Echo Number  (new GE field) [RCR - 24 Mar 2022] */
 "0019 10a2" ,  /* GE multi-echo index   */
 "0020 1002" ,  /* Images in Acquisition */

 /*--- Siemens 3D mosaic images [RCR/DRG 1 Jun 2016] --*/

 "0018 0023" ,  /* ACQ MR Acquisition Type */
 "0019 100a" ,  /* unnamed MR header num mosaic slices */

 /*--- multi-frame DICOM files [RWC - 02 May 2008] ---*/

 "0020 0105" ,  /* Number of temporal positions */
 "0020 0010" ,  /* study number  */
 "0020 0011" ,  /* series number */
 "0020 0013" ,  /* Instance (image) number */
 "0020 0100" ,  /* Temporal position index identifier */
 "0020 9128" ,  /* Temporal position index */
 "0020 9057" ,  /* Stack position index */

 "0054 1330" ,  /* image index  */


NULL } ;


#define NUM_ELIST (sizeof(elist)/sizeof(char *)-1)

/*----------------------------------------------------------------------------*/

#define E_SLICE_THICKNESS             0
#define E_REPETITION_TIME             1
#define E_SLICE_SPACING               2
#define E_FIELD_OF_VIEW               3

#define E_PATIENT_ORIENTATION         4
#define E_IMAGE_POSITION              5
#define E_IMAGE_ORIENTATION           6
#define E_SLICE_LOCATION              7

#define E_SAMPLES_PER_PIXEL           8
#define E_NUMBER_OF_FRAMES            9
#define E_ROWS                       10
#define E_COLUMNS                    11
#define E_PIXEL_SPACING              12
#define E_BITS_ALLOCATED             13
#define E_BITS_STORED                14
#define E_RESCALE_INTERCEPT          15
#define E_RESCALE_SLOPE              16
#define E_RESCALE_TYPE               17
#define E_PHOTOMETRIC_INTERPRETATION 18
#define E_PIXEL_REPRESENTATION       19
#define E_HIGH_BIT                   20
#define E_WINDOW_CENTER              21
#define E_WINDOW_WIDTH               22

#define E_ID_IMAGE_TYPE              23    /* 28 Oct 2002: for Siemens mosaic */
#define E_ID_ACQUISITION_TIME        24    /* 20 Oct 2010: Philips sorting */
#define E_ID_MANUFACTURER            25
#define E_ACQ_MATRIX                 26

#define E_SIEMENS_1                  27    /* 31 Oct 2002 */
#define E_SIEMENS_2                  28

#define E_TRANSFER_SYNTAX            29    /* 05 Jul 2006 */

#define E_SOP_IUID                   30    /* 28 Aug 2014: GEME sort [rickr] */
#define E_GE_ECHO_TIME               31    /* 24 Mar 2022: echo time [rickr] */
#define E_GE_ECHO_NUMBER             32    /* 24 Mar 2022: echo num  [rickr] */
#define E_GE_ME_INDEX                33    /* 15 Aug 2014: GEME sort [rickr] */
#define E_NIM_IN_ACQ                 34    /* 19 Aug 2014: GE multi-echo sort */

#define E_MR_ACQ_TYPE                35    /* 01 Jun 2016: Siemens 3D acq */
#define E_SIEMENS_3D_NSLICES         36    /* 01 Jun 2016: Siemens 3D acq */

#define E_NUMBER_OF_TIMES            37    /* 02 May 2008 */
#define E_RS_STUDY_NUM               38    /* 10 Feb 2005: for Imon [rickr] */
#define E_RS_SERIES_NUM              39
#define E_INSTANCE_NUMBER            40
#define E_TIME_INDEX_ID              41
#define E_TIME_INDEX                 42
#define E_STACK_INDEX                43

#define E_RS_IMAGE_INDEX             44    /* 06 May 2010: for PET [rickr] */


/*----------------------------------------------------------------------------*/

#endif /* _MRI_DICOM_ELIST_HEADER_ */
