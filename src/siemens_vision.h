#ifndef _SIEMENS_VISION_HEADER_
#define _SIEMENS_VISION_HEADER_

#define u_int unsigned int
#define SIEMENS_HEADERSIZE 6144

struct Siemens_vision_header {
       u_int      SiemensStudyDateYYYY;
       u_int      SiemensStudyDateMM ;
       u_int      SiemensStudyDateDD ;
       u_int      AcquisitionDateYYYY ;
       u_int      AcquisitionDateMM ;
       u_int      AcquisitionDateDD ;
       u_int      ImageDateYYYY ;
       u_int      ImageDateMM ;
       u_int      ImageDateDD ;
       u_int      SiemensStudyTimeHH ;
       u_int      SiemensStudyTimeMM ;
       u_int      SiemensStudyTimeSS;
  u_int i1;
       u_int      AcquisitionTimeHH ;
       u_int      AcquisitionTimeMM ;
       u_int      AcquisitionTimeSS ;
  u_int i2;
       u_int      ImageTimeHH ;
       u_int      ImageTimeMM ;
       u_int      ImageTimeSS ;
  char d1[16];
       char    Manufacturer[7];
  char d2[2];
       char InstitutionName[25] ;
  char d3[638];
       char PatientName[27];
       char PatientID[12];
  char d5[737];
      double     SliceThickness ;
  double x1;
      double     RepetitionTime;
      double     EchoTime;
  double x2,x3;
      double     FrequencyMHz;
  char d6[167];
      char ReceivingCoil[16];     /* offset 1767 */

#if 1                             /* RWCox */
  char d7a[1081];
      u_int DisplayMatrixSize ;   /* offset 2864 */
  char d7b[76] ;
#else
  char d7[1161];
#endif
      char SequencePrgName[65];   /* offset 2944 */
      char SequenceWkcName[65];
      char SequenceAuthor[9];
      char SequenceType[8];
  char d4[653];
      double     FOVRow ;
      double     FOVColumn ;
  double x4;
      double     CenterPointX ;
      double     CenterPointY ;
      double     CenterPointZ ;
      double     NormalVectorX ;
      double     NormalVectorY ;
      double     NormalVectorZ ;
      double     DistanceFromIsocenter ;
  double x5;
      double     RowVectorX ;
      double     RowVectorY ;
      double     RowVectorZ ;
      double     ColumnVectorX ;
      double     ColumnVectorY ;
      double     ColumnVectorZ ;
      char OrientationSet1Top[4];
      char OrientationSet1Left[4];
      char OrientationSet1Back[4];
      char OrientationSet2Down[4];
      char OrientationSet2Right[4];
      char OrientationSet2Front[4];
      char SequenceName[32];
  char d8[1064];
      double     PixelSizeRow ;
      double     PixelSizeColumn ;
  char d9[530];
      char TextImageNumber[4];
  char d10[9];
      char TextDate[12];
      char TextTime[5];
  char d11[230];
      char   TextSlicePosition[8];

} ;
#endif
