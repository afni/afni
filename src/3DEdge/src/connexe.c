/*************************************************************************
 * connexe.c - extraction of connected components
 *
 * $Id$
 *
 * LICENSE:
 * GPL v3.0 (see gpl-3.0.txt for details)
 *
 * AUTHOR:
 * Gregoire Malandain (gregoire.malandain@inria.fr)
 *
 * CREATION DATE:
 * July, 8 1999
 *
 * ADDITIONS, CHANGES
 *
 * * Tue Feb  8 10:47:26 MET 2000
 *   - tmpBuf was erroneously set to inputBuf when typeOut was equal to
 *     either USHORT or SSHORT, it is corrected and set to outputBuf.
 *
 * * Mon Dec 20 18:17:24 MET 1999 (G. Malandain)
 *   - one arg was missing for some fprintf
 *
 * * Tue Dec  7 12:06:32 MET 1999 (G. Malandain)
 *   - add label overflow management
 *   - add some parameters
 *   - redesign some functions
 *
 * * Oct  7, 1999 (G. Malandain)
 *   - add parameters (connectivity, minimal size of connected
 *     components, maximum number of connected components)
 *     and related procedures.
 *   - add verbose mode and related procedures.
 *   - add outputBuffer to CountConnectedComponents()
 *   - add RelabelConnectedComponents() procedure.
 *   - add hysteresis thresholding.
 *
 * * Mon Mar  6 14:28:23 MET 2000 (G. Malandain)
 *   - add the case ( maxNumberOfConnectedComponent == 1 )
 *     which was obviously forgotten in InternalConnectedComponentsExtraction().
 *
 *
 */



#include <connexe.h>




/* variables statiques
 */
static int _VERBOSE_ = 0;
static unsigned short int _low_value_ = (unsigned short int)100;
static unsigned short int _hig_value_ = (unsigned short int)200;
/* the size of the table of equivalence is fixed
   (it is easier to manage) and adapted to the
   the maximal number of labels i.e. 65535
   as labels are stored in an (unsigned short int) array
*/
static int _EQUIVALENCE_ARRAY_SIZE_ = 65536;
static int _MAXIMAL_LABEL_VALUE_ = 65535;

static int _connectivity_ = 26;
static int _minimum_size_of_components_ = 1;
static int _maximum_number_of_components_ = 0;


static int CheckAndEvaluateConnectivity( int connectivity,
					 int dimz );












/* structure describing a connected component
 */

typedef struct {
  int label;
  int pointsAboveLowThreshold;
  int pointsAboveHighThreshold;
  char completelyProcessed;
} typeConnectedComponent;

















/* to change some of the global variables
 */

void Connexe_verbose ( )
{
  if ( _VERBOSE_ <= 0 )
    _VERBOSE_ = 1;
  else
    _VERBOSE_ ++;
}

void Connexe_noverbose ( )
{
  _VERBOSE_ = 0;
}

void Connexe_SetConnectivity( int c )
{
  int lc;
  lc = CheckAndEvaluateConnectivity( c, 2 );
  _connectivity_ = lc;
}

void Connexe_SetMinimumSizeOfComponents( int c )
{
  _minimum_size_of_components_ = c;
}

void Connexe_SetMaximumNumberOfComponents( int c )
{
  _maximum_number_of_components_ = c;
}
























/* fonctions statiques

 */
/*
static void SetBinaryLabelToValidComponent( typeConnectedComponent *components,
					    int typeOut,
					    int binaryLabel );
*/
static int RelabelConnectedComponents( void *inputBuf,
				       bufferType typeIn,
				       int *theDim,
				       unsigned short int *labelsBuf,
				       typeConnectedComponent *components,
				       int outputIsBinary,
				       int binaryLabel );
static int InternalConnectedComponentsExtraction( unsigned short int *inputBuf,
						  int *theDim,
						  typeConnectedComponent **theCc,
						  int connectivity,
						  int minNumberOfPtsAboveLow,
						  int minNumberOfPtsAboveHigh,
						  int maxNumberOfConnectedComponent,
						  int outputIsBinary
						  );

























/* count the number of connected components
 *
 * the input image is relabeled with the
 * connected components labels
 */


int CountConnectedComponents( void *inputBuf,
			      bufferType typeIn,
			      void *outputBuf,
			      bufferType typeOut,
			      int *theDim )
{

  return( CountConnectedComponentsWithAllParams( inputBuf, typeIn, outputBuf, typeOut, theDim,
						 (double)1.0, /* default threshold */
						 _connectivity_,
						 _minimum_size_of_components_,
						 _maximum_number_of_components_,
						 (int)0  /* output is not binary */ ) );
}








/* hysteresis thresholding
 *
 * the input image is relabeled with the
 * connected components labels
 */


int HysteresisThresholding( void *inputBuf,
			    bufferType typeIn,
			    void *outputBuf,
			    bufferType typeOut,
			    int *theDim,
			    double lowThreshold,
			    double highThreshold )
{
  return( HysteresisThresholdingWithAllParams( inputBuf, typeIn, outputBuf, typeOut, theDim,
					       lowThreshold, highThreshold,
					       _connectivity_,
					       _minimum_size_of_components_,
					       (int)1, /* at least one point above the high threshold
							  in each connected component */
					       _maximum_number_of_components_,
					       (int)1 /* output is binary */ ) );
}




















/* FROM NOW ON,
   we should not find any static global variable
   except _VERBOSE_
*/











/* count the number of connected components
 *
 * the input image is relabeled with the
 * connected components labels
 */


int CountConnectedComponentsWithAllParams( void *inputBuf,
					   bufferType typeIn,
					   void *outputBuf,
					   bufferType typeOut,
					   int *theDim,
					   double threshold,
					   int connectivity,
					   int minNumberOfPts,
					   int maxNumberOfConnectedComponent,
					   int outputIsBinary )
{
  char *proc = "CountConnectedComponentsWithAllParams";
  register int i;
  int v = theDim[0] * theDim[1] * theDim[2];
  unsigned short int *tmpBuf = (unsigned short int *)NULL;
  register unsigned short int * resBuf;
  typeConnectedComponent *components = (typeConnectedComponent *)NULL;
  int iThreshold = 0;
  int nbFoundCC;


  /* iThreshold is the nearest integer to threshold
   */
  if ( threshold >= 0.0 ) iThreshold = (int)(threshold + 0.5);
  else                    iThreshold = (int)(threshold - 0.5);



  switch( typeOut ) {
  case USHORT :
  case SSHORT :
    tmpBuf = (unsigned short int *)outputBuf;
    break;
  default :
    tmpBuf = (unsigned short int *)malloc( v * sizeof(unsigned short int) );
    if ( tmpBuf == (unsigned short int *)NULL ) {
      if ( _VERBOSE_)
	fprintf( stderr, "%s: unable to allocate auxiliary buffer\n", proc );
      return( -1 );
    }
  }



  components = (typeConnectedComponent *)malloc( _EQUIVALENCE_ARRAY_SIZE_ * sizeof(typeConnectedComponent) );
  if ( components == (typeConnectedComponent *)NULL ) {
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    if ( _VERBOSE_)
      fprintf( stderr, "%s: unable to allocate equivalence array\n", proc );
    return( -1 );

  }



  /* initialisation
   */
  resBuf = tmpBuf;
  switch( typeIn ) {
  case UCHAR :
    {
      register u8 *theBuf = (u8 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= iThreshold ) *resBuf = _hig_value_;
	else *resBuf = 0;
      }
    }
    break;
  case USHORT :
    {
      register u16 *theBuf = (u16 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= iThreshold ) *resBuf = _hig_value_;
	else *resBuf = 0;
      }
    }
    break;
  case FLOAT :
    {
      register r32 *theBuf = (r32 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= threshold ) *resBuf = _hig_value_;
	else *resBuf = 0;
      }
    }
    break;
  default :
    if ( _VERBOSE_)
      fprintf( stderr, "%s: can not deal with such input image type.\n",proc );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }



  /* on compte
   */
  if ( InternalConnectedComponentsExtraction( tmpBuf, theDim, &components,
					      connectivity, minNumberOfPts, minNumberOfPts,
					      maxNumberOfConnectedComponent, outputIsBinary ) != 1 ) {
    if ( _VERBOSE_ ) {
      fprintf( stderr, "%s: Unable to count the connected components\n", proc );
    }
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }



  nbFoundCC = components[0].label;

  if ( _VERBOSE_ ) {
    fprintf( stderr, "%s: found %d connected components\n", proc, nbFoundCC );
  }

  if ( nbFoundCC <= 0 ) {
    free( components );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );

    switch( typeOut ) {
    case UCHAR :
    case SCHAR :
      break;
    case USHORT :   v *= sizeof( u16 );   break;
    case SSHORT :   v *= sizeof( s16 );   break;
    case FLOAT  :   v *= sizeof( r32 );   break;
    default :
      if ( _VERBOSE_ ) {
	fprintf( stderr, "%s: such output type not handled yet\n", proc );
      }
      return( -1 );
    }
    (void)memset( outputBuf, 0, v );

    return( 0 );
  }




  /* relabeling
   */
  if ( RelabelConnectedComponents( outputBuf, typeOut, theDim,
				   tmpBuf, components, outputIsBinary, (int)0 ) != 1 ) {
    if ( _VERBOSE_)
      fprintf( stderr, "%s: unable to label output image type.\n", proc );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }


  if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
  free( components );

  return( nbFoundCC );
}






















/* hysteresis thresholding
 *
 * the input image is relabeled with the
 * connected components labels
 */


int HysteresisThresholdingWithAllParams( void *inputBuf,
			    bufferType typeIn,
			    void *outputBuf,
			    bufferType typeOut,
			    int *theDim,
			    double lowThreshold,
			    double highThreshold,
			    int connectivity,
			    int minNumberOfPtsAboveLow,
			    int minNumberOfPtsAboveHigh,
			    int maxNumberOfConnectedComponent,
			    int outputIsBinary )
{
  char *proc = "HysteresisThresholdingWithAllParams";
  register int i;
  int v = theDim[0] * theDim[1] * theDim[2];
  unsigned short int *tmpBuf = (unsigned short int *)NULL;
  register unsigned short int * resBuf;
  typeConnectedComponent *components = (typeConnectedComponent *)NULL;
  int nbFoundCC;
  int iLowThreshold = 0;
  int iHighThreshold = 0;



  /* iThreshold is the nearest integer to threshold
   */
  if ( lowThreshold >= 0.0 ) iLowThreshold = (int)(lowThreshold + 0.5);
  else                    iLowThreshold = (int)(lowThreshold - 0.5);
  if ( highThreshold >= 0.0 ) iHighThreshold = (int)(highThreshold + 0.5);
  else                    iHighThreshold = (int)(highThreshold - 0.5);



  switch( typeOut ) {
  case USHORT :
  case SSHORT :
    tmpBuf = (unsigned short int *)outputBuf;
    break;
  default :
    tmpBuf = (unsigned short int *)malloc( v * sizeof(unsigned short int) );
    if ( tmpBuf == (unsigned short int *)NULL ) {
      if ( _VERBOSE_)
	fprintf( stderr, "%s: unable to allocate auxiliary buffer\n", proc );
      return( -1 );
    }
  }


  components = (typeConnectedComponent *)malloc( _EQUIVALENCE_ARRAY_SIZE_ * sizeof(typeConnectedComponent) );
  if ( components == (typeConnectedComponent *)NULL ) {
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    if ( _VERBOSE_)
      fprintf( stderr, "%s: unable to allocate equivalence array\n", proc );
    return( -1 );

  }



  /* initialisation
   *
   * iLowThreshold and iHighThreshold are the nearest integer
   * values of lowThreshold and highThreshold.
   */
  resBuf = tmpBuf;
  switch( typeIn ) {
  case UCHAR :
    {
      register u8 *theBuf = (u8 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= iHighThreshold ) *resBuf = _hig_value_;
	else if ( *theBuf >= iLowThreshold ) *resBuf = _low_value_;
	else *resBuf = 0;
      }
    }
    break;
  case USHORT :
    {
      register u16 *theBuf = (u16 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= iHighThreshold ) *resBuf = _hig_value_;
	else if ( *theBuf >= iLowThreshold ) *resBuf = _low_value_;
	else *resBuf = 0;
      }
    }
    break;
  case SSHORT :
    {
      register s16 *theBuf = (s16 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= iHighThreshold ) *resBuf = _hig_value_;
	else if ( *theBuf >= iLowThreshold ) *resBuf = _low_value_;
	else *resBuf = 0;
      }
    }
    break;
  case FLOAT :
    {
      register r32 *theBuf = (r32 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= highThreshold ) *resBuf = _hig_value_;
	else if ( *theBuf >= lowThreshold ) *resBuf = _low_value_;
	else *resBuf = 0;
      }
    }
    break;
  case DOUBLE :
    {
      register r64 *theBuf = (r64 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	if ( *theBuf >= highThreshold ) *resBuf = _hig_value_;
	else if ( *theBuf >= lowThreshold ) *resBuf = _low_value_;
	else *resBuf = 0;
      }
    }
    break;
  default :
    if ( _VERBOSE_)
      fprintf( stderr, "%s: can not deal with such input image type.\n",proc );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }



  /* on compte
   */
  if ( InternalConnectedComponentsExtraction( tmpBuf, theDim, &components, connectivity,
					      minNumberOfPtsAboveLow, minNumberOfPtsAboveHigh,
					      maxNumberOfConnectedComponent, outputIsBinary ) != 1 ) {
    if ( _VERBOSE_ ) {
      fprintf( stderr, "%s: Unable to count the connected components\n", proc );
    }
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }



  nbFoundCC = components[0].label;

  if ( _VERBOSE_ ) {
    fprintf( stderr, "%s: found %d connected components\n", proc, nbFoundCC );
  }

  if ( nbFoundCC <= 0 ) {
    free( components );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );

    switch( typeOut ) {
    case UCHAR :
    case SCHAR :
      break;
    case USHORT :   v *= sizeof( u16 );   break;
    case SSHORT :   v *= sizeof( s16 );   break;
    case FLOAT  :   v *= sizeof( r32 );   break;
    default :
      if ( _VERBOSE_ ) {
	fprintf( stderr, "%s: such output type not handled yet\n", proc );
      }
      return( -1 );
    }
    (void)memset( outputBuf, 0, v );

    return( 0 );
  }



  /* relabeling
   */
  if ( RelabelConnectedComponents( outputBuf, typeOut, theDim,
				   tmpBuf, components, outputIsBinary, (int)0 ) != 1 ) {
    if ( _VERBOSE_)
      fprintf( stderr, "%s: unable to label output image type.\n", proc );
    if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
    free( components );
    return( -1 );
  }


  if ( (typeOut != USHORT) && (typeOut != SSHORT) ) free( tmpBuf );
  free( components );

  return( nbFoundCC );
}

























static int InitNeighborsOffsets( int array_offset[3][3][2],
				 int offsets[13],
				 int dimx,
				 int dimxy,
				 int connectivity );

static typeConnectedComponent* LabelsOverflowManagement( unsigned short int *inputBuf,
							 int *theDim,
							 typeConnectedComponent *cc,
							 int connectivity,
							 int minNumberOfPtsAboveLow,
							 int minNumberOfPtsAboveHigh,
							 int outputIsBinary,
							 int *used_labels,
							 int x, int y, int z );
static void SortConnectedComponents( typeConnectedComponent *tab,
				     int left,
				     int right );





/* Extracting connected components - the procedure which does the job
 *
 * *inputBuf is a buffer containing 3 values:
 * - 0           : the background
 * - _low_value_ : points with values >= _low_value_ are considered for
 *                 inclusion in a connected component
 * - _hig_value_ : only connected components containing at least one
 *                 point of value >= _hig_value_ are kept
 *
 * This way, this procedure may be used for different task, according
 * some preprocessing of the original buffer into *inputBuf
 * - extraction of connected components
 *   => all points to be considered are set to _hig_value_
 * - hysteresis thresholding
 *   => point above the low threshold (and below the high one)
 *      are set to _low_value, point above the high threshold
 *      are set to _hig_value_
 * - connected components containing seeds
 *   => all points to be considered are set to _low_value_
 *      seeds are set to _hig_value_
 *
 * after computation, it will contain labels of connected components
 * several labels may correspond to the same connected component,
 * this is indicated by the array *cc.
 *
 * *theDim : dimensions of the buffer
 *
 * connectivity : the connectivity used for the components
 * available: 4, 8, 6, 10, 18, and 26
 * 4 and 8 are 2D connectivities, while 6, 10, 18 and 26 are
 * 3D. 10 corresponds to the union of 6 and 8.
 *
 * Once the computation is done, some connected components may be
 * discarded:
 * - the ones which do not contain a point with value >= _hig_value_
 * - some others according to the two last arguments
 *
 * minSize: minimum size of a connected components
 *          connected components with a size < minSize
 *          will be discarded.
 *
 * nbCC: maximal number of connected components
 *       if nbCC > 0
 *       Among the remaining components (the little ones
 *       are already being discarded),
 *       only the nbCC largest ones are kept.
 *
 * *cc contains all the information about the result
 *  cc[0].label : number of valid connected component
 *  for each point i, after computation,
 *  cc[ inputbuf[i] ].label is 0 if the component is discarded
 *                          is the right label else.
 *
 */

static int InternalConnectedComponentsExtraction( unsigned short int *inputBuf,
						  /* input buffer,
						     should contains at most 3 values
						     0, _low_value_, _hig_value_
						  */
						  int *theDim,
						  /* dimension of the input buffer */
						  typeConnectedComponent **theCc,
						  /* equivalence array,
						     should be already allocated */
						  int connectivity,
						  /* connectivity:
						     in 2D: 4,8;
						     in 3D: 6, 10, 18, 26
						  */
						  int minNumberOfPtsAboveLow,
						  /* requested minimum number
						     of points above the low
						     value to validate the connected
						     component */
						  int minNumberOfPtsAboveHigh,
						  /* requested minimum number
						     of points above the high
						     value to validate the connected
						     component */
						  int maxNumberOfConnectedComponent,
						  /* maximal number of connected components.
						     If this value is <= 0 or >= _MAXIMAL_LABEL_VALUE_
						     (=65535), all the valid connected components
						     are kept.
						     For other values, the components are sorted with
						     respect to their size and only the largest ones
						     are kept. As a side effect, if the number of valid
						     connected components is less than the maximal
						     number of connected components, the labeling
						     gives the order with respect to the size.
						  */
						  int outputIsBinary
						  /* this flag (0 or not 0) gives a prior information
						     about the output. E.g. it will be non 0 for a
						     hysteresis thresholding, but 0 for a connected
						     component labeling. It is only used when
						     we have to manage a label overflow, i.e.
						     when too many labels have been attributed.
						  */
						  )
{
  char *proc = "InternalConnectedComponentsExtraction";
  /* sizes
   */
  int dimx = theDim[0];
  int dimy = theDim[1];
  int dimz = theDim[2];
  int dimx1 = dimx-1, dimy1=dimy-1;
  /* offsets
   */
  int array_offset[3][3][2], offsets[13], nb_offsets;


  int i, j, k, x, y, z, _INSIDEY_;
  unsigned short int current_value, *theBuf = (unsigned short int *)(inputBuf);
  int local_connexite, used_labels = 0;
  int current_label;
  int nb_neighbors, label_neighbors[13];
  int valid_labels = 0, total_labels = 0;

  typeConnectedComponent *cc = *theCc;
  typeConnectedComponent *tmpCc = (typeConnectedComponent*)NULL;
  typeConnectedComponent *validCc = (typeConnectedComponent*)NULL;


  /* initialisation de la table des composantes
   */
  cc[0].label = 0;




  /*
   * Computation of a point's label : current_label
   * If this point has some neighbors (i.e. nb_neighbors > 0) then
   *   1. Its label is the minimum value of all the neighbors labels
   *   2. We report the equivalences between all the labels
   * If not (i.e. nb_neighbors == 0) then
   *   we use a new label (potential case of error : label overflow)
   */
#define _LABEL_COMPUTATION {                                     \
      if ( nb_neighbors > 0 ) {                                  \
	/*                                                       \
	 * If there are some neighbors,                          \
	 * the equivalent label for all of them is the minimum   \
	 * of all labels.                                        \
	 */                                                      \
	current_label = label_neighbors[0];                      \
	for ( i = 1; i < nb_neighbors; i ++ )                    \
	  if ( label_neighbors[i] < current_label )              \
            current_label = label_neighbors[i];                  \
	/*                                                       \
	 * we have to report the equivalences inside the equivalence table \
	 */                                                      \
	for ( i = 0; i < nb_neighbors; i ++ ) {                  \
	  if ( label_neighbors[i] != current_label ) {           \
            k = label_neighbors[i];                              \
            /*                                                   \
             * si la classe n'a pas deja ete traitee             \
             */                                                  \
            if ( cc[ k ].label != current_label ) {              \
	      cc[ current_label ].pointsAboveLowThreshold += cc[ k ].pointsAboveLowThreshold; \
              cc[ k ].pointsAboveLowThreshold = 0;               \
	      cc[ current_label ].pointsAboveHighThreshold += cc[ k ].pointsAboveHighThreshold; \
              cc[ k ].pointsAboveHighThreshold = 0;              \
              cc[ k ].label = current_label;                     \
	      for ( j = k+1; j <= used_labels; j ++ )            \
	        if ( cc[ j ].label == k ) cc[ j ].label = current_label; \
            }                                                    \
	  }                                                      \
	}                                                        \
      } else {                                                   \
	/*                                                       \
	 * No neighbors :                                        \
	 * we use a new label for this point.                    \
	 * This is the only case of error in the whole procedure. \
	 * This could be a little bit improved as follows:       \
	 * 1. we re-label the potential equivalence classes (without conditions of \
	 *    validity, i.e. without testing cc[ i ].pointsAboveHighThreshold and  \
         *    cc[ i ].pointsAboveLowThreshold) to get            \
	 *    the effective number of equivalence classes at this time. \
	 * 2. using this re-labeling, we change the image labels up to the current \
	 *    point.                                             \
	 * 3. we re-build the equivalence table to have only one \
         *    entry per equivalence classe.                      \
	 */                                                      \
	current_label = ++used_labels;                           \
	if ( used_labels > (_EQUIVALENCE_ARRAY_SIZE_ - 1) ) {    \
          /* we try to relabel the part of the image which has already been \
             processed. This way, we use only the minimal number of labels  \
             necessary to represent this part. We hope to recover a         \
             sufficient number of labels to end the computation             \
	   */ \
          k = used_labels - 1; \
          tmpCc = LabelsOverflowManagement( inputBuf, theDim, cc, local_connexite, \
					    minNumberOfPtsAboveLow, minNumberOfPtsAboveHigh, \
					    outputIsBinary, &k, x, y, z ); \
          /* does it succeed ?                            \
	   */                                             \
          if ( tmpCc == (typeConnectedComponent*)NULL ||  \
	       k >=  used_labels - 1 ) {                  \
	    /* if not, I give up                          \
	     */                                           \
	    if ( _VERBOSE_ )                              \
	      fprintf( stderr, "%s: too much used labels for connected components computation\n", \
		       proc );                            \
	    if ( tmpCc != (typeConnectedComponent *)NULL ) free( tmpCc ); \
	    return( -1 );                                 \
	  } else {                                        \
	    /* if yes,                                    \
	       we go on with the new table of equivalences \
	       and the new number of used labels          \
	    */                                            \
	    free( cc );                                   \
	    *theCc = cc = tmpCc;                          \
	    used_labels = k;                              \
	    current_label = ++used_labels;                \
	  }                                               \
        }                                                 \
	cc[ current_label ].label = current_label;        \
	cc[ current_label ].pointsAboveLowThreshold = 0;  \
	cc[ current_label ].pointsAboveHighThreshold = 0; \
      }                                                   \
}

  /*
   * Generic tests of all the neighbors
   * we make sure that they belong to the image.
   * We check if the neighbor has a label (value > 0)
   * We are looking for the equivalence classe (i.e. cc[ value ].label)
   * of the neighbors
   */
#define _GENERIC_TEST {                   \
    for ( i = 0; i < nb_offsets; i ++ )   \
      if ( *(theBuf + offsets[i]) > 0 )   \
	label_neighbors[ nb_neighbors++ ] = cc[ (int)( *(theBuf + offsets[i]) ) ].label; \
}

  /*
   * Specific tests for the upper row of the neighborhood of a point
   * We check if the neighbor is connected to the current point
   *          if the neighbor belongs to the image
   *          if the neighbor has a label
   */
#define _UPPER_ROW_TESTS {                \
    if ( y > 0 ) {                        \
      for ( i = -1; i <= 1; i ++ ) {      \
	if ( array_offset[1+i][0][1] == 0 ) continue;  \
	if ( (x+i < 0) || (x+i >= dimx) )   continue;  \
	if ( *(theBuf + array_offset[1+i][0][1]) > 0 ) \
	  label_neighbors[ nb_neighbors++ ] = cc[ (int)( *(theBuf + array_offset[1+i][0][1]) ) ].label; \
      }                                   \
    }                                     \
}

  /*
   * Specific test for the left neighbor of a point
   * We should test if (array_offset[0][1][1] != 0) but we
   * do not because the point (x-1,y,z) always belongs to the
   * neighborhoods we use.
   */
#define _LEFT_NEIGHBOR_TEST {                                                   \
    if ( x > 0 )                                                                \
      if ( *(theBuf - 1) > 0 )                                                  \
	label_neighbors[ nb_neighbors++ ] = cc[ (int)( *(theBuf - 1) ) ].label; \
}

  /*
   * additional informations :
   * - the value of the current label is given to the image buffer
   * - the connect component size is incremented
   * - the validity of the connect component is checked (with respect
   *   to the current value)
   */
#define _ADDITIONAL_INFORMATIONS {                     \
    *theBuf = (unsigned short int)current_label;       \
    cc[ current_label ].pointsAboveLowThreshold ++;    \
    if ( current_value >= _hig_value_ )                \
      cc[ current_label ].pointsAboveHighThreshold ++; \
}


  /*
   * Computation of all points.
   *
   * We will attribute to each point a value (a label). According to this
   * value and to the equivalence table, we know the equivalence classe
   * of the point (i.e. cc[ value ].label).
   *
   * The computation is divided into two parts : the first slice
   * and the others slices. This allows to avoid some tests and
   * to spare some time.
   *
   * The principle of the computation is the following :
   * For each point, we
   * - check if t may belong to a connected component
   *   => ( current_value >= _low_value_)
   * - check if tests are needed for the neighbors, i.e.
   *   if the point is on the image border. This point
   *   is different for the first slice and the other ones.
   *   first slice  -> if ( y == 0 ) _INSIDEY_ = 0;
   *   other slices -> if ( (y == 0) || (y == dimy1) ) _INSIDEY_ = 0;
   *   => ( _INSIDEY_ == 1 ) && ( x > 0 ) && ( x < dimx1 )
   * - depending on the above checking,
   *   we compute the number of neighbors (nb_neighbors) and
   *   extract all the neighbors' values (label_neighbors[]),
   *   either without additionnal tests (_GENERIC_TEST),
   *   or with additionnal tests for the neighbors in the upper slice,
   *   in the upper row (_UPPER_ROW_TESTS) or the left neighbor
   *   (_LEFT_NEIGHBOR_TEST).
   *   The two offsets structures (offsets[] and array_offset[][][])
   *   are necessary because of this choice.
   * - Once we have the number of neighbors and their values, we can
   *   compute the point's label (_LABEL_COMPUTATION)
   *   - if the point have neighbors, its label will be the minimum
   *     of all labels, and we report the equivalence between this
   *     minimum label and the other labels in the neighborhood inside
   *     the equivalence table.
   *   - if the point does not have any neighbor, we use a new label.
   *     we increment the number of used label (++used_labels) and
   *     we initialize a new label.
   *     WARNING: an error may occur if such initialization is not possible
   *     (it depends on the type used for the image).
   * - Once the point's label is computed, we report this value in the
   *   image, increment the connected component size and test if
   *   connected component will be valid (current_value >= _hig_value_)
   */


  /*
   * first slice
   */
  local_connexite = CheckAndEvaluateConnectivity( connectivity, (int)1 );
  nb_offsets = InitNeighborsOffsets( array_offset, offsets, dimx, dimx*dimy, local_connexite );

  z = 0;
  for ( y = 0; y < dimy; y++ ) {
    _INSIDEY_ = 1;
    if ( y == 0 ) _INSIDEY_ = 0;
    for ( x = 0; x < dimx; x++, theBuf++ ) {
      current_value = *theBuf;
      if ( current_value < _low_value_ ) continue;
      nb_neighbors = 0;
      if ( ( _INSIDEY_ == 1 ) && ( x > 0 ) && ( x < dimx1 ) ) {
	_GENERIC_TEST
      } else {
	_UPPER_ROW_TESTS
	_LEFT_NEIGHBOR_TEST
      }
      _LABEL_COMPUTATION
      _ADDITIONAL_INFORMATIONS
    }
  }

  /*
   * other slices
   */
  local_connexite = CheckAndEvaluateConnectivity( connectivity, dimz );
  nb_offsets = InitNeighborsOffsets( array_offset, offsets, dimx, dimx*dimy, local_connexite );
  for ( z = 1; z < dimz; z ++ ) {
    for ( y = 0; y < dimy; y++ ) {
      _INSIDEY_ = 1;
      if ( (y == 0) || (y == dimy1) ) _INSIDEY_ = 0;
      for ( x = 0; x < dimx; x++, theBuf++ ) {
	current_value = *theBuf;
	if ( current_value < _low_value_ ) continue;
	nb_neighbors = 0;
	if ( ( _INSIDEY_ == 1 ) && ( x > 0 ) && ( x < dimx1 ) ) {
	  _GENERIC_TEST
	} else {
	  /*
	   * upper slice tests
	   */
	  for ( j = -1; j <= 1; j ++ ) {
	    if ( (y+j < 0) || (y+j >= dimy) )       continue;
	    for ( i = -1; i <= 1; i ++ ) {
	      if ( (x+i < 0) || (x+i >= dimx) )     continue;
	      if ( array_offset[1+i][1+j][0] == 0 ) continue;
	      if ( *(theBuf + array_offset[1+i][1+j][0]) > 0 )
		label_neighbors[ nb_neighbors++ ] = cc[ (int)( *(theBuf + array_offset[1+i][1+j][0]) ) ].label;
	    }
	  }
	  _UPPER_ROW_TESTS
	  _LEFT_NEIGHBOR_TEST
	}
	_LABEL_COMPUTATION
	_ADDITIONAL_INFORMATIONS
      }
    }
  }






  /*
   * At this point, all the image's points have been processed.
   */

  if ( _VERBOSE_ ) {
    fprintf( stderr, "%s: number of used labels: %5d\n", proc, used_labels );
  }




  /* no components ?
   */
  if ( used_labels == 0 ) {
    cc[0].label = 0;
    cc[0].pointsAboveLowThreshold = 0;
    cc[0].pointsAboveHighThreshold = 0;
    return( 1 );
  }







  /* specific case
     only the largest connected component is required
  */
  if ( maxNumberOfConnectedComponent == 1 ) {

    total_labels = 0;
    valid_labels = -1;

    for ( i = 1; i <= used_labels; i++ ) {
      if ( cc[ i ].label == i ) {
	total_labels ++;
	if ( (cc[ i ].pointsAboveHighThreshold >= minNumberOfPtsAboveHigh)
	     && (cc[ i ].pointsAboveLowThreshold >= minNumberOfPtsAboveLow) ) {
	  if ( valid_labels == -1 ) valid_labels = i;
	  else if ( cc[ valid_labels ].pointsAboveLowThreshold < cc[ i ].pointsAboveLowThreshold )
	    valid_labels = i;
	}
      }
    }

    cc[0].pointsAboveHighThreshold = total_labels;
    cc[0].pointsAboveLowThreshold = used_labels;

    if ( valid_labels > 0 ) {
      cc[0].label = 1;
      if ( _VERBOSE_ ) {
	fprintf( stderr, "%s: number of valid connected components: 1 (out of %d)\n",
		 proc, total_labels );
      }
    }
    else {
      cc[0].label = 0;
      if ( _VERBOSE_ ) {
	fprintf( stderr, "%s: number of valid connected components: 0 (out of %d)\n",
		 proc, total_labels );
      }
      return( 1 );
    }

    for ( i = 1; i <= used_labels; i++ ) {
      if ( cc[ i ].label == valid_labels ) cc[ i ].label = 1;
      else cc[ i ].label = 0;
    }
    return( 1 );
  }








  /*
   * counting and re-labeling all the valid connected components
   * this is not optimal, but more elegant because it may be used
   * in several further cases.
   *
   * The representative label of a equivalence's classe
   * (i.e. a connected component) is characterized by
   * ( cc[ i ].label == i ).
   * We have to check if this connected component is valid i.e.
   * - if it contains some points with (value >= _hig_value_),
   *   => (cc[ i ].pointsAboveHighThreshold == minNumberOfPtsAboveHigh )
   * - if its size is large enough
   *   => (cc[ i ].pointsAboveLowThreshold >= minNumberOfPtsAboveLow) )
   * if yes, we increment the number of valid connected components
   * (++valid_labels) and give this value as a new label for the
   * connected component.
   * if no, we give 0 as a new label and as a new size.
   *
   * If a label is not representative of its equivalence's classe
   * (i.e. cc[ i ].label < i), we give to it the new value
   * of its representative label (which may be 0 if the connected
   * component is not valid). Recall that the representative label
   * of an equivalence's classe is always parsed before the other
   * labels of the same equivalence's classe. Id. for the size.
   *
   * To keep recognizing the valid classes, the representative labels
   * have the numbers of points, while the others not.
   */

  total_labels = 0;
  valid_labels = 0;
  for ( i = 1; i <= used_labels; i++ ) {
    if ( cc[ i ].label == i ) {
      total_labels ++;
      if ( (cc[ i ].pointsAboveHighThreshold >= minNumberOfPtsAboveHigh)
	   && (cc[ i ].pointsAboveLowThreshold >= minNumberOfPtsAboveLow) )
	cc[ i ].label = ++valid_labels;
      else {
	cc[ i ].label = 0;
	cc[ i ].pointsAboveLowThreshold = 0;
	cc[ i ].pointsAboveHighThreshold = 0;
      }
    }
    else {
      j = cc[ i ].label;
      cc[ i ].label = cc[ j ].label;
    }
  }






  /*
   * At this point, all valid components have been renumbered,
   * and have the right size.
   */

  if ( _VERBOSE_ ) {
    fprintf( stderr, "%s: number of valid connected components: %5d (out of %d)\n",
	     proc, valid_labels, total_labels );
  }





  /*
   * output: all the valid connected components if specified
   */
  if ( (maxNumberOfConnectedComponent <= 0) ||
       (maxNumberOfConnectedComponent >= _MAXIMAL_LABEL_VALUE_) ) {
    cc[0].label = valid_labels;
    cc[0].pointsAboveHighThreshold = total_labels;
    cc[0].pointsAboveLowThreshold = used_labels;
    return( 1 );
  }








  /*
   * output: the n largest connected component
   *         maxNumberOfConnectedComponent > 1
   * we will sort the valid connected components (# = valid_labels)
   * with respect to their size
   */
  if ( maxNumberOfConnectedComponent > 1 ) {
    validCc = (typeConnectedComponent *)malloc((valid_labels + 1) * sizeof(typeConnectedComponent));
    if ( validCc == (typeConnectedComponent*)NULL ) {
      if ( _VERBOSE_ )
	fprintf( stderr, "%s: allocation failed for auxiliary array (to sort connected components)\n", proc );
      return( -1 );
    }

    /* store all the valid connected components in an array
       A valid connected component is recognized by
       (cc[ i ].pointsAboveHighThreshold >= minNumberOfPtsAboveHigh)
       or (cc[ i ].pointsAboveLowThreshold >= minNumberOfPtsAboveLow)

       All attributed labels are in [1...valid_labels]
     */
    cc[ 0 ].label = cc[ 0 ].pointsAboveLowThreshold = cc[ 0 ].pointsAboveHighThreshold = 0;
    j = 0;
    for ( i = 1; i <= used_labels; i++ ) {
      if ( cc[ i ].pointsAboveHighThreshold >= minNumberOfPtsAboveHigh )
	validCc[ ++j ] = cc[i];
    }
    /* sort them */
    SortConnectedComponents( validCc, (int)1, valid_labels );



    /* At this point, we want to keep the connected
       components represented by validCc[i].label
       for i=1...min(maxNumberOfConnectedComponent,valid_labels).

       and to reject the connected components represented by
       validCc[i].label
       for i=min(maxNumberOfConnectedComponent,valid_labels)+1 ... valid_labels

       the labels (validCc[i].label) are in the range 1...valid_labels

       If (valid_labels <= maxNumberOfConnectedComponent) all
       connected components are kept, but they are sorted
       with respect to size.
    */

    /* we mark by
       cc[ i ].pointsAboveHighThreshold = 'index after sort'
       the connected components to be kept
       and by
       cc[ j ].pointsAboveHighThreshold = 0
       the connected components to be removed
    */

    for ( i = 1; (i <= maxNumberOfConnectedComponent) && (i <= valid_labels); i ++ )
      validCc[ validCc[i].label ].pointsAboveHighThreshold = i;
    /* if maxNumberOfConnectedComponent >= valid_labels,
       the following does not matter
    */
    for ( i = maxNumberOfConnectedComponent + 1; i <= valid_labels; i++ )
      validCc[ validCc[i].label ].pointsAboveHighThreshold = 0;


    /* here, for i=1...valid_labels
       if ( validCc[ i ].pointsAboveHighThreshold > 0 )
       the connected component is to be kept
       and its index should be
       validCc[ i ].pointsAboveHighThreshold (its index after sort)
       instead of validCc[ i ].label
    */
    for ( i = 1; i <= valid_labels; i++ )
      validCc[ i ].label = validCc[ i ].pointsAboveHighThreshold;


    /* report the change of value in the complete array */
    for ( i = 1; i <= used_labels; i++ ) {
      if ( cc[i].label == 0 ) continue;
      j =  cc[i].label;
      cc[i].label = validCc[ j ].label;
    }

    free( validCc );

    /* nb de composantes valides
     */
    cc[0].label = (valid_labels > maxNumberOfConnectedComponent) ? maxNumberOfConnectedComponent : valid_labels;
    cc[0].pointsAboveHighThreshold = total_labels;
    cc[0].pointsAboveLowThreshold  = used_labels;
    return( 1 );
  }


  /* this case should never occur
   */
  return( 0 );
}
























/* calcule les offsets pour connaitre les voisins du point
   courant selon la connexite.
   Retourne le nombre de voisins.
*/

static int InitNeighborsOffsets( int array_offset[3][3][2],
				 int offsets[13],
				 int dimx,
				 int dimxy,
				 int connectivity )
{
  int nb=0;
  int i,j,k;

  for ( i = 0; i < 3; i ++ )
  for ( j = 0; j < 3; j ++ )
  for ( k = 0; k < 2; k ++ )
    array_offset[i][j][k] = 0;
  for ( i = 0; i < 13; i ++ )
    offsets[i] = 0;

  switch ( connectivity ) {
  case 8 :
    array_offset[0][0][1] = offsets[ nb++ ] =         - dimx - 1; /* point (-1, -1,  0) */
    array_offset[2][0][1] = offsets[ nb++ ] =         - dimx + 1; /* point ( 1, -1,  0) */
  case 4 :
    array_offset[1][0][1] = offsets[ nb++ ] =         - dimx;     /* point ( 0, -1,  0) */
    array_offset[0][1][1] = offsets[ nb++ ] =                - 1; /* point (-1,  0,  0) */
    break;
  case 26 :
    array_offset[0][0][0] = offsets[ nb++ ] = - dimxy - dimx - 1; /* point (-1, -1, -1) */
    array_offset[2][0][0] = offsets[ nb++ ] = - dimxy - dimx + 1; /* point ( 1, -1, -1) */
    array_offset[0][2][0] = offsets[ nb++ ] = - dimxy + dimx - 1; /* point (-1,  1, -1) */
    array_offset[2][2][0] = offsets[ nb++ ] = - dimxy + dimx + 1; /* point ( 1,  1, -1) */
  case 18 :
    array_offset[1][0][0] = offsets[ nb++ ] = - dimxy - dimx;     /* point ( 0, -1, -1) */
    array_offset[0][1][0] = offsets[ nb++ ] = - dimxy        - 1; /* point (-1,  0, -1) */
    array_offset[2][1][0] = offsets[ nb++ ] = - dimxy        + 1; /* point ( 1,  0, -1) */
    array_offset[1][2][0] = offsets[ nb++ ] = - dimxy + dimx;     /* point ( 0, -1, -1) */
  case 10 :
    array_offset[0][0][1] = offsets[ nb++ ] =         - dimx - 1; /* point (-1, -1,  0) */
    array_offset[2][0][1] = offsets[ nb++ ] =         - dimx + 1; /* point ( 1, -1,  0) */
  case 6 :
    array_offset[1][1][0] = offsets[ nb++ ] = - dimxy;            /* point ( 0,  0, -1) */
    array_offset[1][0][1] = offsets[ nb++ ] =         - dimx;     /* point ( 0, -1,  0) */
    array_offset[0][1][1] = offsets[ nb++ ] =                - 1; /* point (-1,  0,  0) */
  }
  return( nb );
}



















/* on verifie, et eventuellement on change (en 2D) la connexite
 */

static int CheckAndEvaluateConnectivity( int connectivity,
					 int dimz )
{
  int c = 26;

  /* seules les connexites 26, 18, 10, 6 (en 3D) et 8 et 4 (en 2D)
     sont permises
  */
  switch ( connectivity ) {
  default : break;
  case 26 :
  case 18 :
  case 10 :
  case 6 :
  case 8 :
  case 4 : c = connectivity;
  }

  /* cas 2D
   */
  if ( dimz <= 1 ) {
    switch ( c ) {
    default :
    case 26 :
    case 18 :
    case 10 : c = 8; break;
    case 6 :
    case 4 : c = 4;
    }
  }
  return( c );
}



















/* Gestion du depassement du label maximal utilise.

   Pour les composantes deja entierement traitees, on
   determine si elles sont valides ou non.
   => on recupere les labels des composantes
   invalidees.

   Pour les composantes valides, et les composantes
   non entierement traitees, on leur attribue a toutes
   un seul label, et on recupere ainsi tous
   les labels intermediaires.
*/
static typeConnectedComponent* LabelsOverflowManagement( unsigned short int *inputBuf,
							 /* input buffer, it is already
							    labeled until the point (x,y,z)
							    not included
							 */
							 int *theDim,
							 /* dimension of the input buffer */
							 typeConnectedComponent *cc,
							 /* equivalence array */
							 int connectivity,
							 /* connectivity */
							 int minNumberOfPtsAboveLow,
							 int minNumberOfPtsAboveHigh,
							 int outputIsBinary,
							 int *used_labels,
							 int x, int y, int z )
{
  char *proc="_LabelsOverflowManagement";
  int local_connectivity = CheckAndEvaluateConnectivity( connectivity, z+1 );
  int xlast = x;
  int ylast = y;
  int zlast = z;
  int xnum, ynum, znum;

  unsigned short int *theBuf = inputBuf;
  int valid_labels = 0;
  int valid_comps = 0;
  int binary_label = -1;
  int i, j;
  typeConnectedComponent *components = (typeConnectedComponent *)NULL;



  /* determination du dernier point traite
     -> (xlast, ylast, zlast)

     (x,y,z) represente le point courant pour lequel
     l'attribution d'un nouveau label a echoue

     => on cherche le dernier point traite avec succes
     (ce peut etre un point du fond)
   */
  if ( xlast < 0 || ylast < 0 || zlast < 0 ) {
    if ( _VERBOSE_ )
      fprintf( stderr, "%s: bad (negative) last point coordinate(s)\n", proc );
    return( (typeConnectedComponent*)NULL );
  }


  if ( xlast > 0 ) {
    xlast --;
  }
  else if ( ylast > 0 ) {
    xlast = theDim[0] - 1;
    ylast --;
  }
  else if ( zlast > 0 ) {
    xlast = theDim[0] - 1;
    ylast = theDim[1] - 1;
    zlast --;
  } else {
    if ( _VERBOSE_ )
      fprintf( stderr, "%s: all last point coordinates are zeros\n", proc );
    return( (typeConnectedComponent*)NULL );
  }


  /* pour certaines composantes (classes d'equivalence)
     on sait deja si il faut les garder ou non,
     ce sont celles qui n'ont plus de voisins
     dans les points qui restent a parcourir.
   *
     Determination du dernier point
     -> (xnum, ynum, znum)
     pouvant appartenir a l'une de ces composantes
   *
   * On ne regarde que les cas non-degeneres,
   * c-a-d ou le dernier point traite n'appartient pas a un bord.
   */
  znum = ynum = xnum = -1;


  switch ( local_connectivity ) {
  case 4 :
    if ( ylast > 0 ) {
      xnum = xlast;
      ynum = ylast - 1;
      znum = zlast;
    }
    break;
  case 8 :
    if ( ylast > 0 && xlast > 0 ) {
      xnum = xlast - 1;
      ynum = ylast - 1;
      znum = zlast;
    }
    break;
  case 6 :
  case 10 :
    if ( zlast > 0 ) {
      xnum = xlast;
      ynum = ylast;
      znum = zlast - 1;
    }
    break;
  case 18 :
    if ( zlast > 0 && ylast > 0 && xlast > 0 ) {
      xnum = xlast - 1;
      ynum = ylast - 1;
      znum = zlast - 1;
    }
    break;
  case 26 :
  default :
    if ( zlast > 0 && ylast > 0 && xlast > 0 ) {
      xnum = xlast - 1;
      ynum = ylast - 1;
      znum = zlast - 1;
    }
  }




  /* on regarde les composantes pour une elimination eventuelle

     1. Si (xnum, ynum, znum) est valide,
        alors les composantes dont tous les points se trouvent
	entre (0,0,0) et (xnum, ynum, znum) inclus sont deja
	entierement traitees et on peut verifier
	leur validite
	Les autres (dont au moins un point se trouve entre
	(xnum, ynum, znum)+1 et (xlast, ylast, zlast) inclus
	sont conservees telles quelles.
     2. sinon toutes les composantes sont conservees telles quelles
        sans test de validation.
  */
  if ( xnum >= 0 && ynum >= 0 && znum >= 0 ) {
    for ( i = 1; i <= (*used_labels); i++ )
      cc[i].completelyProcessed = 1;
    i = znum  * theDim[1] * theDim[0] + ynum  * theDim[0] + xnum;
    j = zlast * theDim[1] * theDim[0] + ylast * theDim[0] + xlast;
    for ( i +=1; i <= j; i++ ) {
      if ( theBuf[i] > 0 ) {
	cc[ cc[ (int)theBuf[i] ].label ].completelyProcessed = 0;
      }
    }
  } else {
    for ( i = 1; i <= (*used_labels); i++ )
      cc[i].completelyProcessed = 0;
  }




  /* allocation d'un nouveau tableau d'equivalence
   */
  components = (typeConnectedComponent *)malloc( _EQUIVALENCE_ARRAY_SIZE_ * sizeof(typeConnectedComponent) );
  if ( components == (typeConnectedComponent *)NULL ) {
    if ( _VERBOSE_ )
      fprintf( stderr, "%s: unable to allocate equivalence array\n", proc );
    return( (typeConnectedComponent*)NULL );
  }
  components[0].label = 0;




  /* on regarde les labels et on cherche
     les representants des classes d'equivalences (labels[i] = i)
     1. si la composante peut encore grandir
        (completelyProcessed = 0) => on la garde
     2. sinon, on teste sa validite
  *
     si l'output est binaire, on garde la meme valeur
     pour toutes les composantes valides
     (on fait comme si c'etait une seule composante)
  */

  valid_labels = valid_comps = 0;
  for ( i = 1; i <= (*used_labels); i++ ) {

    /* c'est le label representant la classe d'equivalence
     */
    if ( cc[ i ].label == i ) {

      /* la composante peut encore grandir => on la garde telle quelle
       */
      if ( cc[ i ].completelyProcessed == 0 ) {
	cc[ i ].label = ++valid_labels;
	components[ valid_labels ] = cc[i];
      }

      /* on a deja tous les points;
	 la composante ne grandira plus
       */
      else {

	/* on verifie que la composante est valide
	 */
	if ( (cc[ i ].pointsAboveHighThreshold >= minNumberOfPtsAboveHigh)
	     && (cc[ i ].pointsAboveLowThreshold >= minNumberOfPtsAboveLow) ) {
	  valid_comps ++;

	  /* mode binaire, on recupere le numero de la premiere composante
	   * complete, et on affectera ce numero a TOUTES les composantes
	   * valides
	   */
	  if ( outputIsBinary != 0 ) {
	    if ( valid_comps == 1 ) {
	      cc[ i ].label = binary_label = ++valid_labels;
	      components[ valid_labels ] = cc[i];
	    } else {
	      cc[ i ].label = binary_label;
	    }
	  }

	  /* mode etiquettes:
	     on garde la composante telle quelle
	     et on incremente les labels attribues
	   */
	  else {
	    cc[ i ].label = ++valid_labels;
	    components[ valid_labels ] = cc[i];
	  }
	}

	/* on peut ne plus considerer la composante
	   elle n'est pas valide
	 */
	else {
	  cc[ i ].label = 0;
	}
      }
    }

    /* ce n'est pas le label representant la classe d'equivalence
       on recupere le nouveau label de celle-ci
     */
    else {
      j = cc[ i ].label;
      cc[ i ].label = cc[ j ].label;
    }
  }

  if ( _VERBOSE_ ) {
    fprintf( stderr, "%s: previous number of used labels: %d\n\
\t new number of valid labels: %d\n\
\t number of already valid components: %d",
	     proc, *used_labels, valid_labels, valid_comps );

    if ( _VERBOSE_ > 1 ) {
      fprintf( stderr, "%s: last point for valid component: %3d %3d %3d\n\
\t last processed point:           %3d %3d %3d\n\
\t current point:                  %3d %3d %3d",
	       proc, xnum, ynum, znum, xlast, ylast, zlast, x, y, z );

      if ( outputIsBinary == 1 ) {
	fprintf( stderr, "%s: binary mode, label for valid components is %d",
		 proc, binary_label );
      }
    }
  }


  /* renumerote jusqu'au dernier point calcule
   */
  j = zlast * theDim[1] * theDim[0] + ylast * theDim[0] + xlast;
  for ( i = 0; i <= j; i ++, theBuf++ ) {
    *theBuf = (u16)( cc[ (int)(*theBuf) ].label );
  }

  /* on remonte le nombre de labels utilises
     et la nouvelle table d'equivalence
   */
  *used_labels = valid_labels;
  return( components );
}





























/* Sorting an array of 'connected component' structures with respect to size
   adaptation of the quicksort
 */
static void SortConnectedComponents( typeConnectedComponent *tab,
				     int left,
				     int right )
{
  int i, last;
  typeConnectedComponent tmp;

  if ( left >= right ) return;

  tmp = tab[left];   tab[left] = tab[(left+right)/2];   tab[(left+right)/2] = tmp;

  last = left;
  for ( i = left+1; i <= right; i++ )
    if ( tab[i].pointsAboveLowThreshold > tab[left].pointsAboveLowThreshold ) {
      tmp = tab[++last];   tab[last] = tab[i];   tab[i] = tmp;
    }

  tmp = tab[left];   tab[left] = tab[last];   tab[last] = tmp;

  SortConnectedComponents( tab, left, last-1 );
  SortConnectedComponents( tab, last+1, right );
}





















/* Relabel a buffer with respect to an array of
 * connected components descriptors
 */
static int RelabelConnectedComponents( void *inputBuf,
				       bufferType typeIn,
				       int *theDim,
				       unsigned short int *labelsBuf,
				       typeConnectedComponent *components,
				       int outputIsBinary,
				       int binaryLabel )
{
  char *proc = "RelabelConnectedComponents";
  int i, label=binaryLabel;
  int v = theDim[0] * theDim[1] * theDim[2];
  unsigned short int * resBuf = labelsBuf;

  /* binary output ?
     if yes,
     we choose as the single output value the maximal
     value, depending on the type, if no value was specified
  */
  if ( outputIsBinary != 0 ) {
    if ( label == 0 ) {
      switch ( typeIn ) {
      case FLOAT :
      case DOUBLE :
	label = 1;
	break;
      case UCHAR :
	label = 255;
	break;
      case USHORT :
	label = 65535;
	break;
      case SSHORT :
	label = 32767;
	break;
      default :
	if ( _VERBOSE_)
	  fprintf( stderr, "%s: output binary value set to 1.\n", proc );
	label = 1;
      }
    }
    for ( i=1; i<=components[0].pointsAboveLowThreshold; i++ )
      if ( components[i].label > 0 ) components[i].label = label;

  } else {

    switch( typeIn ) {
    case SCHAR :
      if ( components[0].label > 127 ) {
	if ( _VERBOSE_ ) {
	  fprintf( stderr, "%s: found more than 127 connected components\n", proc);
	  fprintf( stderr, "\t different components will have the same label\n" );
	}
      }
      break;
    case UCHAR :
      if ( components[0].label > 255 ) {
	if ( _VERBOSE_ ) {
	  fprintf( stderr, "%s: found more than 255 connected components\n", proc);
	  fprintf( stderr, "\t different components will have the same label\n" );
	}
      }
      break;
    case SSHORT :
      if ( components[0].label > 32767 ) {
	if ( _VERBOSE_ ) {
	  fprintf( stderr, "%s: found more than 32767 connected components\n", proc);
	  fprintf( stderr, "\t different components will have the same label\n" );
	}
      }
      break;
    default :
      break;
    }

  }

  /* we want the background points
     to be 0
  */
  components[0].label = 0;

  switch( typeIn ) {
  case UCHAR :
    {
      register u8 *theBuf = (u8 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (u8)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case SCHAR :
    {
      register s8 *theBuf = (s8 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (s8)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case USHORT :
    {
      register u16 *theBuf = (u16 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (u16)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case SSHORT :
    {
      register s16 *theBuf = (s16 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (s16)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case INT :
    {
      register s32 *theBuf = (s32 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (s32)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case FLOAT :
    {
      register r32 *theBuf = (r32 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (r32)components[ (int)*resBuf ] .label;
      }
    }
    break;
  case DOUBLE :
    {
      register r64 *theBuf = (r64 *)inputBuf;
      for ( i=0; i<v; i++, theBuf++, resBuf++ ) {
	*theBuf = (r64)components[ (int)*resBuf ] .label;
      }
    }
    break;
   default :
    if ( _VERBOSE_)
      fprintf( stderr, "%s: can not deal with such input image type.\n", proc );
    return( -1 );
  }

  return( 1 );
}

























typedef struct {
  int label;
  int size;
} typeCC_for_sort;




static void SortCCWithRespectToSize( typeCC_for_sort *tab,
				     int left,
				     int right )
{
  int i, last;
  typeCC_for_sort tmp;

  if ( left >= right ) return;

  tmp = tab[left];   tab[left] = tab[(left+right)/2];   tab[(left+right)/2] = tmp;

  last = left;
  for ( i = left+1; i <= right; i++ )
    if ( tab[i].size > tab[left].size ) {
      tmp = tab[++last];   tab[last] = tab[i];   tab[i] = tmp;
    }

  tmp = tab[left];   tab[left] = tab[last];   tab[last] = tmp;

  SortCCWithRespectToSize( tab, left, last-1 );
  SortCCWithRespectToSize( tab, last+1, right );
}





int RelabelConnectedComponentsByDecreasingSize( void *inputBuf,
						bufferType typeIn,
						int *theDim )
{
  char *proc = "RelabelConnectedComponentsByDecreasingSize";
  int i, v;
  int lmax = 0;
  typeCC_for_sort *theCC = (typeCC_for_sort *)NULL;


  v = theDim[0]*theDim[1]*theDim[2];

  switch ( typeIn ) {
  case UCHAR :
    {
      u8 *theBuf = (u8*)inputBuf;
      for ( i=0; i<v; i++ )
	if ( lmax < theBuf[i] ) lmax = theBuf[i];
    }
    break;
  case USHORT :
    {
      u16 *theBuf = (u16*)inputBuf;
      for ( i=0; i<v; i++ )
	if ( lmax < theBuf[i] ) lmax = theBuf[i];
    }
    break;
  default :
    if ( _VERBOSE_ ) {
      fprintf( stderr, " %s: can not deal with such image type (1).\n", proc );
    }
    return( -1 );
  }



  if ( lmax == 0 ) {
    if ( _VERBOSE_ ) {
      fprintf( stderr, " %s: null image.\n", proc );
    }
    return( -1 );
  }
  if ( lmax == 1 ) return( 1 );



  theCC = (typeCC_for_sort*)malloc( (lmax+1)*sizeof(typeCC_for_sort) );
  if ( theCC == (typeCC_for_sort *)NULL ) {
    if ( _VERBOSE_ ) {
      fprintf( stderr, " %s: can not allocate auxiliary array.\n", proc );
    }
  }

  for ( i=0; i<=lmax; i++ ) {
    theCC[i].label = i;
    theCC[i].size = 0;
  }



  switch ( typeIn ) {
  case UCHAR :
    {
      u8 *theBuf = (u8*)inputBuf;

      for ( i=0; i<v; i++ ) {
	if ( theBuf[i] > 0 )
	  theCC[ (int)theBuf[i] ].size ++;
      }
    }
    break;
  case USHORT :
    {
      u16 *theBuf = (u16*)inputBuf;

      for ( i=0; i<v; i++ ) {
	if ( theBuf[i] > 0 )
	  theCC[ (int)theBuf[i] ].size ++;
      }
    }
    break;
  default :
    if ( _VERBOSE_ ) {
      fprintf( stderr, " %s: can not deal with such image type (2).\n", proc );
    }
    return( -1 );
  }




  SortCCWithRespectToSize( theCC, 1, lmax );
  /* ici, on a theCC[i] qui est la ieme composante
     selon la taille, et qui a le label theCC[i].label
     => on range les labels dans size
     theCC[ theCC[i].label ].size = i
     pour faire le changement
  */

  for ( i=1; i<=lmax; i++ )
    theCC[ theCC[i].label ].size = i;



  switch ( typeIn ) {
  case UCHAR :
    {
      u8 *theBuf = (u8*)inputBuf;

      for ( i=0; i<v; i++ ) {
	if ( theBuf[i] > 0 )
	  theBuf[i] = theCC[ (int)theBuf[i] ].size;
      }
    }
    break;
  case USHORT :
    {
      u16 *theBuf = (u16*)inputBuf;
      for ( i=0; i<v; i++ ) {
	if ( theBuf[i] > 0 )
	  theBuf[i] = theCC[ (int)theBuf[i] ].size;
      }
      free( theCC );
    }
    break;
  default :
    if ( _VERBOSE_ ) {
      fprintf( stderr, " %s: can not deal with such image type (3).\n", proc );
    }
    return( -1 );
  }

  return( 1 );
}

