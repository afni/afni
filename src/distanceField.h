// Error Codes

typedef long ERROR_NUMBER;

#define ERROR_NONE	0
#define ERROR_MEMORY_ALLOCATION	-1
#define ERROR_READING_FILE	-2
#define ERROR_WRITING_FILE	-3
#define ERROR_OPENING_FILE	-4
#define ERROR_DIFFERENT_DIMENSIONS	-5
#define ERROR_PALETTE	-6
#define ERROR_PARAMETERS	-7
#define ERROR_WANT16BITGRAYSCALEINPUT	-8
#define ERROR_OPM_SERVER_DOWN -9
#define ERROR_SERVER_TIMEOUT -10
#define ERROR_OPENING_DATABASE -11
#define ERROR_CLOSING_DATABASE -12
#define ERROR_UPDATING_DATABASE -13
#define ERROR_PAINTING_WINDOW -14
#define ERROR_IMAGE_PARAMETERS -15
#define ERROR_SINGULAR_MATRIX -16
#define ERROR_FILE_TYPE_NOT_HANDLED -17
#define ERROR_READING_AFFYMETRIX_HEADER -18
#define ERROR_SKEW_KURTOSIS_VARIANCE -19
#define ERROR_TIME_OUT -20
#define ERROR_SEARCH -21
#define ERROR_FILENAME  -22
#define ERROR_NULL_PTR	-23
#define ERROR_UNRECOGNIZABLE_SPECIES	-24
#define ERROR_MAKING_BITMAP	-25
#define ERROR_GETTING_FILENAME	-26
#define ERROR_TOO_MANY_BAD_GENES	-27
#define ERROR_OUT_OF_RANGE	-28
#define ERROR_NAME_CONFLICT	-29
#define ERROR_CELLS_ALREADY_MASKED -30
#define ERROR_UNRECOGNIZABLE_CHIP_TYPE -31
#define ERROR_IMAGE_DIMENSIONS -32
#define ERROR_OPENING_CELL_FILE -33
#define ERROR_OPENING_COMPACT_FILE -34
#define ERROR_OPENING_CDF_FILE -35
#define ERROR_NO_MATCH_INFO -36
#define ERROR_NO_MISMATCH_INFO -37
#define ERROR_OBTAINING_MATCHING_PROBE_PAIR -38
#define ERROR_TOO_MANY_PROBE_PAIRS -39
#define ERROR_NO_ALIGNMENT_INFO -40
#define ERROR_INSUFFICIENT_DATA -41
#define ERROR_FILE_FORMAT -42
#define ERROR_GETTING_DIRECTORY -43
#define ERROR_OPENING_RECORDSET -44
#define ERROR_QUERYING_DATABASE -45
#define ERROR_CONNECTING_TO_DATABASE -46
#define ERROR_OPENING_CURSOR -47
#define ERROR_INITIALIZATION -48
#define ERROR_TOOFEWARGS -49
#define ERROR_FILENOTFOUND -50
#define ERROR_SELECTINGPALETTE -51
#define ERROR_REALIZINGPALETTE -52
#define ERROR_NOSTANDARDINPUT -53
#define ERROR_CLOSING_FILE -54
#define ERROR_EOF -55
#define ERROR_COMPLEXDATAREQUIRED -56
#define ERROR_DATATYPENOTHANDLED -57
#define ERROR_DIVIDEBYZERO -58
#define ERROR_LACKING_GEOMETRIC_INFO -59
#define ERROR_CREATING_DIRECTORY -60
#define ERROR_NO_CONTROL_POINTS -61
#define ERROR_ZERO_MATRIX -62
#define ERROR_PREHEADER_NOT_READ	-63
#define ERROR_OPENING_GRC_FILE -64

//  Other generic definitions

// typedef enum {false, true} bool;
typedef unsigned int DWORD;
typedef unsigned short WORD;
typedef unsigned char BYTE;

#ifndef INVALID_FILE_ATTRIBUTES
	#define INVALID_FILE_ATTRIBUTES	0xFFFFFFFF
#endif

#ifndef MAX
#define MAX(a, b) ((a>b)? a:b)
#endif
#ifndef MIN
#define MIN(a, b) ((a<b)? a:b)
#endif
#ifndef MEDIAN
#define MEDIAN(a, b, c) ((a>b)? ((c>a)? a : max(b,c)) : ((c<a)? a : min(b,c)))
#endif

#define MAX2(a,b)	((a>b)? a:b)
#define MIN2(a,b)	((a<b)? a:b)
#define MAX3(a,b,c)	((a>b)? MAX(a,c) : MAX(b,c))
#define MIN3(a,b,c)	((a<b)? MIN(a,c) : MIN(b,c))

#define	BYTE_DATA	0
#define WORD_DATA	1
#define DWORD_DATA	2
#define SHORT_DATA	3
#define LONG_DATA	4
#define FLOAT_DATA	5
#define DOUBLE_DATA	6
#define RGB_DATA	7
#define PALETTE_DATA	8
#define BYTE_SURFACE_DATA	9
#define EDGE		255
#define NODE		200

// Reference colors (0-255)
#define rRED	20
#define rGREEN	21
#define rBLUE	22
#define rYELLOW	23
#define rMAGENTA	24
#define rCYAN	25
#define rWHITE	26
#define rBLACK	27
#define rORANGE	28
#define rVIOLET	29
#define rCANARY	30
#define rCREAM	31
#define rNAVY	32

// RGB frame references
#define fRED	0
#define fGREEN	1
#define fBLUE	2

#define INSIDE 0
#define OUTSIDE 1

#define DESCENDING 0
#define ASCENDING 1
#define UNKNOWN_MONOTONE	2
#define ZERO_MONOTONE	3

#define CLOCKWISE	0
#define ANTICLOCKWISE	1
#define UNKNOWN_ROTATION_DIRECTION	2
#define ZERO_ROTATION	3

#define	CIRCLE	6.28318530718

#define HORIZONTAL	0
#define VERTICAL	1

#ifndef MAYBE
#define MAYBE   2
#endif

#if UNIX
// #define BIG_ENDIAN	TRUE
#else
#ifndef BIG_ENDIAN
#define BIG_ENDIAN	FALSE
#endif
#endif
#define MATH_PI 3.14159265359
#define MATH_PI_2 1.570796326795
#define MATH_PI_4 0.7853981633975
#define MATH_PI_8 0.39269908169875
#define MATH_PI_16 0.196349540849375
#define MATH_PI_32 0.0981747704246875
#define MATH_7SIXTEENTHS 1.374446785945625
#define MATH_15SIXTEENTHS 2.945243112740625
#define MATH_3EIGHTHS_PI 1.17809724509625
#define MATH_5EIGHTHS_PI 1.96349540849375
#define MATH_7EIGHTHS_PI 2.74889357189125
#define M_TWOPI 6.28318530718
#define M_THREEPI_2 4.712388980385
#define M_ROOT_TWOPI	2.5066282746310829883349776381391

#define DEGREES2RADIANS	0.017453292519943295769236907684886
#define TWODEGREES2RADIANS	0.034906585039886591538473815369772

#define ROOT2	1.414213562373
#define ROOT3	1.7320508075688772935274463415059
#define LN2		0.6931471805599

#define SIN30DEGREES	0.5

#define VECTOR_START	0
#define VECTOR_MIDPOINT	1
#define VECTOR_END		2

#define  ADD   0
#define  SUBTRACT   1
#define  MULTIPLY   2
#define  DIVIDE   3
#define	 LOGARITHMIC	4
#define	 EXPONENTIAL	5
#define	 LINEAR		6
#define	 NONLINEAR	7

// Orders
#define LINEAR_ORDER	1
#define QUADRATIC_ORDER	2
#define CUBIC_ORDER		3
#define QUARTIC_ORDER	4

#define NEAREST_NEIGHBOR_INTERPOLATION	0
#define BILINEAR_INTERPOLATION  1
#define BIQUADRATIC_INTERPOLATION  2
#define BICUBIC_INTERPOLATION  3
#define GAUSSIAN_INTERPOLATION  4
#define EDGE_INTERPOLATION  5
#define TRILINEAR_INTERPOLATION  1
#define TRIQUADRATIC_INTERPOLATION  2
#define TRICUBIC_INTERPOLATION  3
#define LINEAR_INTERPOLATION  1
#define QUADRATIC_INTERPOLATION  2
#define CUBIC_INTERPOLATION  3
#define SINC_INTERP	6

// ROIs
#define  ELLIPSE  0
#define  RECTANGLE  1
#define  POLYGON  2

// Directions
#define EAST 0
#define SOUTHEAST 1
#define SOUTH 2
#define SOUTHWEST 3
#define WEST 4
#define NORTHWEST 5
#define NORTH 6
#define NORTHEAST 7
#define STARTING_POINT	8	// Starting point in sequence for which "Directions" apply
#define EAST_UP 9
#define SOUTHEAST_UP 10
#define SOUTH_UP 11
#define SOUTHWEST_UP 12
#define WEST_UP 13
#define NORTHWEST_UP 14
#define NORTH_UP 15
#define NORTHEAST_UP 16
#define EAST_DOWN 17
#define SOUTHEAST_DOWN 18
#define SOUTH_DOWN 19
#define SOUTHWEST_DOWN 20
#define WEST_DOWN 21
#define NORTHWEST_DOWN 22
#define NORTH_DOWN 23
#define NORTHEAST_DOWN 24

// Sides
#define LEFT	0
#define RIGHT	1
#define TOP		2
#define BOTTOM	3

// Quadrants
#define	UPPER_LEFT	0
#define	UPPER_RIGHT	1
#define	LOWER_LEFT	2
#define	LOWER_RIGHT	3

// Axes
#define X_AXIS 0
#define Y_AXIS 1
#define Z_AXIS 2

// Image Formats
#define JFIF	0
#define TIFF	1
#define PNG		2
#define PDL		3
#define RAW		4
#define UNKOWN_IMAGE_FILE_FORMAT	5
#define TEXT_FILE	6
#define AFFYMETRIX_CELL	7
#define DEGRAAF	8

// Noise Types
#define GAUSSIAN	0
#define SALT_AND_PEPPER	1

// Projection Types
#define SUMMATION	0
#define VARIANCE	1
#define LOCAL_VARIANCE	2
#define MINIMUM		3
#define MAXIMUM		4
#define MEAN		5
#define PERCENTILE	6

// Fourier filter types
#define REMOVE_DIAMETER	0
#define REMOVE_FREQUENCIES	1
#define REMOVE_REGION	2
#define GAUSSIAN_WINDOW	3
#define GAUSSIAN_WINDOW_PLANES	4

// Fourier window types
#define IDEAL_WINDOW	0
#define BUTTERWORTH_WINDOW	1
#define CHEBYSHEV_WINDOW	2
#define HAMMING_WINDOW	3
#define HANNING_WINDOW	4
#define BLACKMAN_WINDOW	5
#define KAISER_WINDOW	6
#define TRAPEZOID_WINDOW	7
#define TAYLOR_WEIGHTING	8
#define HALF_CYCLE_SINE_WINDOW	9
#define PARZEN_WINDOW	10

// Arithmetic types
#define ARITHMETIC_MINIMUM	0
#define ARITHMETIC_MAXIMUM	1
#define ARITHMETIC_SUM		2
#define ARITHMETIC_DIFFERENCE	3
#define ARITHMETIC_PRODUCT	4
#define ARITHMETIC_QUOTIENT	5
#define ARITHMETIC_PERCENTILE	6
#define ARITHMETIC_MEDIAN	7
#define ARITHMETIC_MEAN	8
#define ARITHMETIC_ADD_CONSTANT	9
#define ARITHMETIC_MULT_BY_CONSTANT	10
#define ARITHMETIC_LOG_TRANSFORM	11
#define ARITHMETIC_INVERT	12
#define ARITHMETIC_ABSOLUTE	13

// Box filter types
#define LAPLACIAN	0
#define HIGHPASS	1

// General filter types
#define VARIANCE_FILTER 0
#define PERCENTILE_FILTER 1

// Function types
#define SINC_FUNCTION	0

// Alignment algorithms
#define COMMON_LINES	0
#define FERA			1

// Layover measuring methods
#define LMM_CONTROL_POINTS	0
#define LMM_OPTICAL_FLOW	1
#define LMM_TWO_D_OPTICAL_FLOW	2
#define LMM_EDGES			3

// Optical flow methods
#define OFM_PHASE_SLOPE		0
#define OFM_X_CORR			1
#define OFM_CORRELATION		2
#define OFM_COLINEARITY		3

// Reference versus target
#define REFERENCE	0
#define TARGET		1

// 64-bit integer handling
#if defined(_MSC_VER)
    typedef  __int64 longlong;
#else
    typedef   long long longlong;
#endif

// Special characters
#define CHAR_PLUS_MINUS 177

// extern "C" long PowerOf2(int upwards, long input);
extern long PowerOf2(int upwards, long input);

#define UNIX 1
#if UNIX
	typedef long Long64;
#else
	typedef __int64 Long64;
#endif

typedef struct {
    int left, right, top, bottom;
    } RECT;

typedef struct{
    float real;
    float imag;
} COMPLEX;

typedef struct sLineEqn
{
	float	fOffset;
	float	fSlope;
} LineEqn;

typedef struct sQuadraticEqn
{
	double	dOffset;
	double	dLinearTerm;
	double	dQuadraticTerm;
} QuadraticEqn;

typedef struct sCubicEqn
{
	double	dOffset;
	double	dLinearTerm;
	double	dQuadraticTerm;
	double	dCubicTerm;
} CubicEqn;

typedef struct sQuarticEqn
{
	double	dOffset;
	double	dLinearTerm;
	double	dQuadraticTerm;
	double	dCubicTerm;
	double	dQuarticTerm;
} QuarticEqn;

typedef struct{
    int x, y, z;
    } POINT;

typedef struct sTick
{
	POINT	pStart;
	POINT	pFinish;
} Tick;

typedef struct sAxisNumber
{
	char	csString[16];
	POINT	pLocation;
} AxisNumber;

typedef struct sFlaggedIndex
{
	DWORD	dwIndex;
	bool	bFlag;
} FlaggedIndex;

typedef struct sByteRange
{
   long  bMin;
   long  bMax;
} ByteRange;

typedef struct sIntRange
{
   int  iMin;
   int  iMax;
} IntRange;

typedef struct sLongRange
{
   long  lMin;
   long  lMax;
} LongRange;

typedef struct sShortRange
{
   short  sMin;
   short  sMax;
} ShortRange;

typedef struct sWordRange
{
   WORD  sMin;
   WORD  sMax;
} WordRange;

typedef struct sDWordRange
{
   DWORD dwMin;
   DWORD dwMax;
} DWordRange;

typedef struct sDoubleRange
{
   double  dMin;
   double  dMax;
} DoubleRange;

typedef struct sFloatRange
{
   float  fMin;
   float  fMax;
} FloatRange;

typedef struct vector_struct_3DIntRange
{
	IntRange	irXRange;
	IntRange	irYRange;
	IntRange	irZRange;
} ThreeDIntRange;

typedef struct vector_struct_3D_Range
{
	FloatRange	x;
	FloatRange	y;
	FloatRange	z;
} VectorRange;

typedef struct vector_struct_2D_Range
{
	FloatRange	x;
	FloatRange	y;
} TwoDVectorRange;

typedef struct sTwoDLongVectorRange
{
	LongRange	x;
	LongRange	y;
} TwoDLongVectorRange;

typedef struct sTwoDIntVectorRange
{
	IntRange	x;
	IntRange	y;
} TwoDIntVectorRange;

typedef struct CompSummators	{
			double	F1F2;
			double	A1squared;
			double	A2squared;
			double	A1A2DThetaSquared;
			double	A1A2;
			double	A1A2DTheta;
			double	A1plusA2DThetaSquared;
			double	A1plusA2;
			double	sum_product;
			double	SumXminusMuSquared;
			double	SumYminusMuSquared;
			double	mean_x;
			double	mean_y;
			unsigned int	counter;
			bool	mean_found;
			} sCompSummators;

typedef struct FixedPtCompSummatorsStruct	{
			long	sum_product;
			long	SumXminusMuSquared;
			long	SumYminusMuSquared;
			long	mean_x;
			long	mean_y;
			long	counter;
			} FixedPtCompSummators;

#ifndef Complex
typedef struct Complex_Struct
{
	float	fReal;
	float	fImaginary;
} Complex;
#endif

/*
#ifndef vector
typedef struct vector_struct_2D
	{
	float	x, y;
	} vector;
#endif
*/

typedef struct vector_struct_3D
	{
	float	x, y, z;
	} Vector;

typedef struct SurfaceNormal_struct
	{
	float	dZdX, dZdY;
	} SurfaceNormal;

typedef struct double_vector_struct
	{
	double	x, y;
	} DoubleVector;

typedef struct float_vector_struct
	{
	float	x, y;
	} FloatVector;

typedef struct long_vector_struct
	{
	long	x, y;
	} LongVector;

typedef struct long_3Dvector_struct
	{
	long	x, y, z;
	} Long3DVector;

typedef struct float_3Dvector_struct
{
	float	x, y, z;
} Float3DVector;

typedef struct sDouble3DVector
{
	double	x, y, z;
} Double3DVector;

typedef struct short_vector_struct
	{
	short	x, y;
	} ShortVector;

typedef struct word_vector_struct
	{
		WORD	x, y;
	} WordVector;

typedef struct FixedPtVector_struct
	{
	long	x, y;
	} FixedPtVector;

typedef struct sThreeDRotation
{
	float	fYaw;
	float	fPitch;
	float	fRoll;
} ThreeDRotation;

typedef struct sHoughLine
{
	FloatVector	fvStart;
	FloatVector	fvFinish;
} HoughLine;

typedef struct sHoughLineList
{
	HoughLine	hlLine;
	struct sHoughLineList *previous;
	struct sHoughLineList *next;
} HoughLineList;

typedef struct sRibbon
{
	IntRange	irPlaneRange;
	int			iLength;
	float		fLongAxis;
	int			iRibbonSize;		// Ribbon length times number of sections.  (Stored in linear form.)
	float		fLongAxisSize;		// Log10 of (Long axis times number of sections)
	float		fStraightLength;	// Length with empahsis on straightness
	FixedPtVector *fpvpPoints;
	FixedPtVector fpv2DCOG;
	bool	bRibbonized;			// Whether ribbons, from different planes, have been aligned
} Ribbon;

typedef struct sRibbonList
{
	Ribbon	rsRibbon;
	struct sRibbonList	* next;
	struct sRibbonList	* previous;
} RibbonList;

typedef struct sApplicableRibbonList
{
	Ribbon	*rspRibbon;
	bool	bMatched;
	struct sApplicableRibbonList	* next;
	struct sApplicableRibbonList	* previous;
} ApplicableRibbonList;

typedef struct sRibbonVolume
{
	Long3DVector	l3dvDimensions;
	RibbonList		*rlpRibbonList;
} RibbonVolume;

typedef struct sPlate
{
	Vector	v3dvaPoints[4];
} Plate;

typedef struct sByteSurfacePoint
{
	float	fHeight;
	BYTE	bValue;
} ByteSurfacePoint;

typedef struct sRealColour
{
	BYTE	bRed;
	BYTE	bGreen;
	BYTE	bBlue;
} RealColour;

typedef struct sLongRealColour
{
	long	lRed;
	long	lGreen;
	long	lBlue;
} LongRealColour;

typedef struct sFloatRealColour
{
	float	fRed;
	float	fGreen;
	float	fBlue;
} FloatRealColour;

typedef struct sDoubleRealColour
{
	double	dRed;
	double	dGreen;
	double	dBlue;
} DoubleRealColour;

typedef struct sDoubleRealColourRange
{
   DoubleRealColour  dMin;
   DoubleRealColour  dMax;
} DoubleRealColourRange;

typedef struct boolPlane_Struct
{
	int		iRows;
	int		iColumns;
	bool	**Data;
} boolPlane;

typedef struct BytePlane_Struct
{
	int		iRows;
	int		iColumns;
	BYTE	**Data;
} BytePlane;

typedef struct ByteSurface_Struct
{
	int		iRows;
	int		iColumns;
	ByteSurfacePoint	**Data;
} ByteSurface;

typedef struct ShortPlane_Struct
{
	int		iRows;
	int		iColumns;
	short	**Data;
} ShortPlane;

typedef struct WordPlane_Struct
{
	int		iRows;
	int		iColumns;
	WORD	**Data;
} WordPlane;

typedef struct DWordPlane_Struct
{
	int		iRows;
	int		iColumns;
	DWORD	**Data;
} DWordPlane;

typedef struct LongPlane_Struct
{
	int		iRows;
	int		iColumns;
	long	**Data;
} LongPlane;

typedef struct Long64Plane_Struct
{
	int		iRows;
	int		iColumns;
	Long64	**Data;
} Long64Plane;

typedef struct FloatPlane_Struct
{
	int		iRows;
	int		iColumns;
	float	**Data;
} FloatPlane;

typedef struct boolVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	bool	***Data;
} boolVolume;

typedef struct FloatVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	float	***Data;
} FloatVolume;

typedef struct ComplexVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	COMPLEX	***Data;
} ComplexVolume;

typedef struct WordVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	WORD	***Data;
} WordVolume;

typedef struct IntVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	int		***Data;
} IntVolume;

typedef struct ByteVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	BYTE	***Data;
} ByteVolume;

typedef struct DoublePlane_Struct
{
	int		iRows;
	int		iColumns;
	double	**Data;
} DoublePlane;

typedef struct ComplexPlane_Struct
{
	int		iRows;
	int		iColumns;
	COMPLEX	**Data;
} ComplexPlane;

typedef struct RealColourPlane_Struct
{
	int		iRows;
	int		iColumns;
	RealColour	**Data;
} RealColourPlane;

typedef struct FloatRealColourPlane_Struct
{
	int		iRows;
	int		iColumns;
	FloatRealColour	**Data;
} FloatRealColourPlane;


typedef struct FloatRealColourVolume_Struct
{
	int		iSections;
	int		iRows;
	int		iColumns;
	FloatRealColour	***Data;
} FloatRealColourVolume;

typedef struct DoubleRealColourPlane_Struct
{
	int		iRows;
	int		iColumns;
	DoubleRealColour	**Data;
} DoubleRealColourPlane;

typedef struct VectorPlane_Struct
{
	int		iRows;
	int		iColumns;
	vector	**Data;
} VectorPlane;

typedef struct ThreeDVectorPlane_Struct
{
	int		iRows;
	int		iColumns;
	Vector	**Data;
} ThreeDVectorPlane;

typedef struct FixedPtVectorPlane_Struct
{
	int		iRows;
	int		iColumns;
	FixedPtVector	**Data;
} FixedPtVectorPlane;

typedef struct ShortVectorPlane_Struct
{
	int		iRows;
	int		iColumns;
	ShortVector	**Data;
} ShortVectorPlane;

typedef struct WordRangePlane_Struct
{
	int		iRows;
	int		iColumns;
	WordRange	**Data;
} WordRangePlane;

typedef struct FloatRangePlane_Struct
{
	int		iRows;
	int		iColumns;
	FloatRange	**Data;
} FloatRangePlane;

typedef struct DoubleRangePlane_Struct
{
	int		iRows;
	int		iColumns;
	DoubleRange	**Data;
} DoubleRangePlane;

typedef struct SurfaceNormalPlane_Struct
{
	int		iRows;
	int		iColumns;
	SurfaceNormal	**Data;
} SurfaceNormalPlane;

typedef struct sLong1DProjection
{
   long  lNumberOfPoints;
   long  *lp1DProjection;
} Long1DProjection;

typedef struct s1DProjectionPair
{
   Long1DProjection lpHorizontal1DProjection;
   Long1DProjection lpVertical1DProjection;
} OneDProjectionPair;

typedef union
{
	boolPlane fboolPlane;
	BytePlane fBytePlane;
	ShortPlane fShortPlane;
	WordPlane fWordPlane;
	LongPlane fLongPlane;
	FloatPlane fFloatPlane;
	VectorPlane fVectorPlane;
	FixedPtVectorPlane fFixedPtVectorPlane;
} uGenericPlane;

typedef struct sPassWord
{
	char	*csUserName;
	char	*csPassWord;
	struct sPassWord	*next;
} PassWord;

typedef struct sSurfacePoint
{
	FloatVector	fvCoords;
	WORD	wValue;
} SurfacePoint;

typedef struct sSurface
{
	int iXDim, iYDim;
	int iXRange, iYRange;
	SurfacePoint **spppPoints;
} Surface;

typedef struct sTwoWayWordList
{
	WORD wValue;
	struct sTwoWayWordList *previous;
	struct sTwoWayWordList *next;
} TwoWayWordList;

typedef struct sLengthSorter
{
	DWORD	dwIndex;
	float	fLength;
} LengthSorter;

typedef struct sFloatRect
{
	float	left, right, top, bottom;
} FloatRect;

typedef struct sDisplayPoints
{	long	lNumPoints;
	POINT	*ppPoints;
}	DisplayPoints;

typedef struct sPath
{
	double	dX;
	double	dY;
	double	dZ;
} Path;

typedef struct sVectorSimilarity
{
	float	fColinearity;
	float	fCorrelation;
	float	fCovariance;
	float	fEntropy;
	float	fVariance;
	float	fExplainedVariance;
	float	fKappa;
	float	fDCRatio;
	float	fPeakCntDiff;
	float	fPeakAsymUnnorm;
	float	fPeakAsymNorm;
	float	fPeakSepUnnorm;
	float	fPeakSepNorm;
	float	fDistinctiveness;
	float	fKurtosis;
} VectorSimilarity;

typedef struct sMatrix
{
	long	lRows;
	long	lColumns;
	float	*fpData;
} Matrix;

typedef struct sDoubleMatrix
{
	long	lRows;
	long	lColumns;
	double	*dpData;
} DoubleMatrix;

typedef struct sComplexMatrix
{
	long	lRows;
	long	lColumns;
	COMPLEX	*cpData;
} ComplexMatrix;

typedef struct sImageEllipse
{
   float fHorizontalRadius;
   float fVerticalRadius;
   float fCenterX;
   float fCenterY;
} ImageEllipse;

typedef struct sWindowEllipse
{
   float fHorizontalRadius;
   float fVerticalRadius;
   float fCenterX;
   float fCenterY;
} WindowEllipse;

typedef struct sLinearCorrections
{
	float	fRotationInDegrees;
	FloatVector	fvScale;
	FloatVector	fvTranslation;
} LinearCorrections;

typedef struct sTwoDFloatTriangle
{
	FloatVector	fvaVertices[3];
	LineEqn	leaEdges[3];
	int	iaNeighborIndices[3];
	int	iaNeighborEdgeIndices[3];
} TwoDFloatTriangle;

typedef struct sGeneralRectangle
{
	FloatVector	v2DaCornerCoords[4];
} GeneralRectangle;

typedef struct sIntersection
{
	FloatVector	fvCoords;
	FloatVector	fvaLineVectors[2];
} Intersection;

typedef struct sPolyLine
{
	HoughLine	hlLine;
	int			iaaPolycornerIndices[2][2];
	bool		bClosedForConnection;
	float		fElevation;
	int			iAssociatedPolygon;
} PolyLine;

typedef struct sPolyCorner
{
	FloatVector	fvCoordinates;
	int			iaLineIndices[3];
	short		saEndsInvolved[3];
	float		fDotProduct;
	float		fElevation;
	int			iAssociatedPolygon;
} PolyCorner;

typedef struct sPolyLineList
{
	int	iLineIndex, iCornerIndex;
	struct sPolyLineList	*next;
	struct sPolyLineList	*previous;
} PolyLineList;

typedef struct sPolygonalShape
{
	int	iNumVertices;
	int	*ipVertices;
	float		fElevation;
} PolygonalShape;

typedef struct sPolygonalPlane
{
	int		iLineCount, iCornerCount, iPolygonCount;
	PolyLine	*plpLines;
	PolyCorner	*pcpCorners;
	PolygonalShape	*ppPolygons;
} PolygonalPlane;

/* Array of direction vectors from a pixel to each of its 8-connected neighbors. */
static LongVector	Directions[8]={{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}};

ShortVectorPlane MakeShortVectorPlane(int iRows, int iColumns, ShortVector **Data);

VectorPlane MakeVectorPlane(int iRows, int iColumns, vector **Data);

ThreeDVectorPlane MakeThreeDVectorPlane(int iRows, int iColumns, Vector **Data);

boolPlane MakeboolPlane(int iRows, int iColumns, bool **Data);

BytePlane MakeBytePlane(int iRows, int iColumns, BYTE **Data);

BytePlane MakeBytePlaneFrom1DArray(int iRows, int iColumns, BYTE *Data);

BytePlane MakeBytePlaneFromPadded1DArray(int iRows, int iColumns, BYTE *Data);

BytePlane MakeBytePlaneFrom1D24BitColorArray(int iRows, int iColumns, BYTE *Data);

ByteSurface MakeByteSurface(int iRows, int iColumns, ByteSurfacePoint **Data);

WordPlane MakeWordPlane(int iRows, int iColumns, WORD **Data);

ShortPlane MakeShortPlane(int iRows, int iColumns, short **Data);

Long64Plane MakeLong64Plane(int iRows, int iColumns, Long64 **Data);

LongPlane MakeLongPlane(int iRows, int iColumns, long **Data);

DoublePlane MakeDoublePlane(int iRows, int iColumns, double **Data);

RealColourPlane MakeRealColourPlane(int iRows, int iColumns, RealColour **Data);

DoubleRealColourPlane MakeDoubleRealColourPlane(int iRows, int iColumns, DoubleRealColour **Data);

FloatRealColourPlane MakeFloatRealColourPlane(int iRows, int iColumns, FloatRealColour **Data);

ERROR_NUMBER MakeRealColourPlaneFromFloatPlanes(FloatPlane *fppRedPlane, FloatPlane *fppGreenPlane,
				FloatPlane *fppBluePlane, RealColourPlane *rcppRealColourPlane);

DWordPlane MakeDWordPlane(int iRows, int iColumns, DWORD **Data);

void FreeDWordPlane(DWordPlane dwpPlane);

FloatPlane MakeFloatPlane(long Rows, long Columns, float **Data);

FloatVolume MakeFloatVolume(long Sections, long Rows, long Columns, float ***Data);

boolVolume MakeboolVolume(long iSections, long iRows, long iColumns, bool ***Data);

FloatRealColourVolume MakeFloatRealColourVolume(long Sections, long Rows, long Columns, FloatRealColour ***Data);

IntVolume MakeIntVolume(long iSections, long iRows, long iColumns, int ***Data);

WordVolume MakeWordVolume(long Sections, long Rows, long Columns, WORD ***Data);

ComplexPlane MakeComplexPlane(long Rows, long Columns, COMPLEX **Data);

ComplexVolume MakeComplexVolume(long Sections, long Rows, long Columns, COMPLEX ***Data);

ByteVolume MakeByteVolume(int iSections, int iRows, int iColumns, BYTE ***Data);

WordRangePlane MakeWordRangePlane(int iRows, int iColumns, WordRange **Data);

FloatRangePlane MakeFloatRangePlane(int iRows, int iColumns, FloatRange **Data);

SurfaceNormalPlane MakeSurfaceNormalPlane(int iRows, int iColumns, SurfaceNormal **Data);

FloatPlane Byte2Float(BytePlane Input);

FloatPlane Word2Float(WordPlane Input);

FloatVolume	WordVolume2FloatVolume(WordVolume wvWordVolume);

FloatPlane Short2Float(ShortPlane Input);

FloatPlane Long2Float(LongPlane Input);

boolPlane Byte2bool(BytePlane Input);

boolPlane Float2bool(FloatPlane Input, float fThreshold);

BytePlane bool2Byte(boolPlane Input);

BytePlane Float2Byte(FloatPlane Input, bool Normalise);

WordPlane Float2Word(FloatPlane Input, bool Normalise);

WordPlane Double2Word(DoublePlane Input, bool Normalise);

BytePlane Double2Byte(DoublePlane Input, bool Normalise);

ShortPlane Float2Short(FloatPlane Input, bool Normalise);

LongPlane Float2Long(FloatPlane Input, bool Normalise);

BytePlane Long2Byte(LongPlane Input);

BytePlane Word2Byte(WordPlane Input);

BytePlane DWord2Byte(DWordPlane Input, bool Normalise);

WordPlane DWord2Word(DWordPlane Input);

BytePlane Short2Byte(ShortPlane Input, bool Normalise);

WordPlane Short2Word(ShortPlane Input);

WordPlane Long2Word(LongPlane Input);

ERROR_NUMBER NormaliseWordPlaneToBytePlane(WordPlane wordPlane, BytePlane *bytePlane);

ERROR_NUMBER ClipRangeNormaliseWordPlaneToBytePlane(WordPlane wordPlane, BytePlane *bytePlane, WORD wMin, WORD wMax);

ERROR_NUMBER FuncNormaliseWordPlaneToBytePlane(WordPlane wordPlane, BytePlane *bytePlane, WORD wFunction);

ERROR_NUMBER NormaliseFloatPlaneToBytePlane(FloatPlane fpFloatPlane, BytePlane *bytePlane);

ERROR_NUMBER NormaliseFloatRealColourPlaneToRealColourPlane(FloatRealColourPlane frcpFloatRealColourPlane,
															RealColourPlane *rcppRealColourPlane);

void NormaliseFloatPlane(FloatPlane fpFloatPlane);

void NormaliseFloatVolume(FloatVolume fvFloatVolume);

void NormaliseRealColourPlane(RealColourPlane rcpRealColourPlane);

ERROR_NUMBER SupervisedFloatPlaneToBytePlane(FloatPlane fpFloatPlane, BytePlane *bytePlane, RECT rTrainingRect);

ERROR_NUMBER DuplicateboolPlane(boolPlane bpInput, boolPlane *bppOutput);

ERROR_NUMBER DuplicateBytePlane(BytePlane byte_plane, BytePlane *NewBytePlane);

ERROR_NUMBER DuplicateFloatPlane(FloatPlane fpInput, FloatPlane *fppOutput);

ERROR_NUMBER DuplicateComplexPlane(ComplexPlane cpInput, ComplexPlane *cppOutput);

ERROR_NUMBER DuplicateFloatVolume(FloatVolume fvInput, FloatVolume *fvpOutput);

ERROR_NUMBER DuplicateByteVolume(ByteVolume bvInput, ByteVolume *bvpOutput);

ERROR_NUMBER DuplicateRealColourPlane(RealColourPlane rcpInput, RealColourPlane *rcppOutput);

ERROR_NUMBER DuplicateFloatRealColourPlane(FloatRealColourPlane frcpInput, FloatRealColourPlane *frcppOutput);

void FreeboolPlane(boolPlane bool_Plane);

void FreeBytePlane(BytePlane Byte_Plane);

void FreeByteSurface(ByteSurface bsByteSurface);

void FreeWordPlane(WordPlane Word_Plane);

void FreeShortPlane(ShortPlane spShortPlane);

void FreeLong64Plane(Long64Plane Long64lane);

void FreeLongPlane(LongPlane Long_Plane);

void FreeFloatPlane(FloatPlane Float_Plane);

void FreeFloatVolume(FloatVolume Float_Volume);

void FreeboolVolume(boolVolume bvboolVolume);

void FreeIntVolume(IntVolume ivIntVolume);

void FreeWordVolume(WordVolume wvWordVolume);

void FreeComplexPlane(ComplexPlane Complex_Plane);

void FreeComplexVolume(ComplexVolume cvComplexVolume);

void FreeByteVolume(ByteVolume bvByteVolume);

void FreeVectorPlane(VectorPlane Vector_Plane);

void FreeShortVectorPlane(ShortVectorPlane svpShortVectorPlane);

void FreeThreeDVectorPlane(ThreeDVectorPlane Vector_Plane);

void FreeFixedPtVectorPlane(FixedPtVectorPlane Vector_Plane);

void FreeWordRangePlane(WordRangePlane wrpWordRangePlane);

void FreeFloatRangePlane(FloatRangePlane frpFloatRangePlane);

void FreeSurfaceNormalPlane(SurfaceNormalPlane snpSurfaceNormalPlane);

void FreeDoubleRealColourPlane(DoubleRealColourPlane DoubleRealColour_Plane);

void FreeFloatRealColourPlane(FloatRealColourPlane FloatRealColour_Plane);

void FreeFloatRealColourVolume(FloatRealColourVolume frcvFloatRealColourVolume);

void FreeRealColourPlane(RealColourPlane RealColour_Plane);

void FreeTwoWayWordList(TwoWayWordList *twwlList);

ERROR_NUMBER GetMean(BYTE *input, long Points, BYTE *OutPut);

double InterpCubic(float *fpVector, float x_coord);	// 1D cubic interpolation

double InterpQuadratic(float *fpVector, float x_coord);	// 1D quadratic interpolation

double InterpLinear(float *fpVector, float x_coord);	// 1D linear interpolation

double 	Interp_BC(float **image, float x_coord, float y_coord);

double	Interp_BQ(float **image, float x_coord, float y_coord);

double Interp_BL(float **image, float x_coord, float y_coord);

float ByteInterp_BL(BYTE **image, float x_coord, float y_coord);

float ByteInterp_BQ(BYTE **image, float x_coord, float y_coord);

float ByteInterp_BC(BYTE **image, float x_coord, float y_coord);

ERROR_NUMBER MakeVectorMagnitudePlane(FloatPlane *MagnitudePlane,
			VectorPlane Vector_Plane, BYTE Offset);

ERROR_NUMBER MakeOneDFixedPtVectorMagnitudePlane(BytePlane *MagnitudePlane,
			LongPlane Vector_Plane, BYTE Offset);

ERROR_NUMBER MakeFixedPtVectorMagnitudePlane(FloatPlane *MagnitudePlane,
			FixedPtVectorPlane Vector_Plane, BYTE Offset);

int Compare(const void * arg1, const void * arg2);

int IntCompare(const void * arg1, const void * arg2);

int LongCompare(const void * arg1, const void * arg2);

int FloatCompare(const void * arg1, const void * arg2);

ERROR_NUMBER GetMedian(BYTE *input, long Points, BYTE *OutPut);

ERROR_NUMBER GetWordMedian(WORD *input, long Points, WORD *OutPut);

ERROR_NUMBER GetIntMedian(int *input, long Points, int *OutPut);

ERROR_NUMBER GetFloatMedian(float *input, long Points, float *OutPut);

ERROR_NUMBER GetByteMedian(BYTE *input, long Points, BYTE *OutPut);

ERROR_NUMBER GetComplexMedian(COMPLEX *input, long Points, COMPLEX *OutPut);

ERROR_NUMBER GetDoubleMedian(double *input, long Points, double *OutPut);

ERROR_NUMBER GetFloatRealColourMedian(FloatRealColour *frcInput, long Points, FloatRealColour *frcpOutPut);

ERROR_NUMBER GetRealColourMedian(RealColour *rcInput, long Points, RealColour *rcpOutPut);

ERROR_NUMBER GetFloatPlaneMedian(FloatPlane fpInput, float *fpOutPut);

void PostErrorMessage(ERROR_NUMBER errorNumber, char *ProgName);

void swaw(char *from, char *to, size_t bytecount);
	// Reverses byte order of long words.

void swad(char *from, char *to, size_t bytecount);
	// Reverses byte order of double precision (8-byte) variables.

 void GetAscendingOrderOfThreeFloatValues(float *fpValues, int *ipOrder);

 long DRound(double arg);
	// Rounds double value off to the nearest integer

 int Round(float arg);
	// Rounds floating point value off to the nearest integer

 double Distance(DoubleVector vector1, DoubleVector vector2);
	// Determines the Euclidean distance between two vectors

 double FloatDistance(FloatVector vector1, FloatVector vector2);
	// Same function as Distance() but takes float vectors as arguments

 double LongDistance(LongVector vector1, LongVector vector2);
	// Same function as Distance() but takes long vectors as arguments

 double ShortDistance(ShortVector vector1, ShortVector vector2);
	// Same function as Distance() but takes short vectors as arguments

 double FixedPtDistance(FixedPtVector vector1, FixedPtVector vector2);
	// Same function as Distance() but takes fixed (long) point vectors as arguments

 double FixedPtDistanceSquared(FixedPtVector vector1, FixedPtVector vector2);
	// Same function as FixedPtDistance() but returns the square of the distance

 double PointDistance(POINT vector1, POINT vector2);
	// Same function as Distance() but takes POINT vectors as arguments

 long CityBlockDistance(LongVector vector1, LongVector vector2);
	// Determines the City-block distance between two vectors

 bool NormaliseFloatVector(FloatVector *fvFloatVector);
	// Normalises a 2D floating point vector (ie. gets its norm).

 bool NormaliseDoubleVector(DoubleVector *dvDoubleVector);
	// Normalises a 2D double precision vector (ie. gets its norm).

 ERROR_NUMBER NormaliseVector(Vector *vpVector);
	// Normalises a 3D floating point vector (ie. gets its norm).

 ERROR_NUMBER NormaliseDoubleRealColourPlaneToRealColourPlane(DoubleRealColourPlane drcpRealColourPlane,
	 RealColourPlane *rcppOutput);

 float Norm(Vector vVector);

 float FloatVectorNorm(FloatVector fvVector);

 ERROR_NUMBER NormaliseBytePlane(BytePlane *bytePlane);

 ERROR_NUMBER CopyComplexPlane(ComplexPlane cvInput, ComplexPlane cvOutput);

 ERROR_NUMBER CopyFloatPlane(FloatPlane fvInput, FloatPlane fvOutput);

 ERROR_NUMBER CopyLongPlane(LongPlane lpInput, LongPlane lpOutput);

 ERROR_NUMBER CopyFloatVolume(FloatVolume fpInput, FloatVolume fpOutput);

 ERROR_NUMBER CopyWordVolume(WordVolume wvInput, WordVolume wvOutput);

 ERROR_NUMBER CopyWordPlane(WordPlane wpInput, WordPlane wpOutput);

 ERROR_NUMBER CopyBytePlane(BytePlane bpInput, BytePlane bpOutput);

 ERROR_NUMBER CopyByteVolume(ByteVolume bvInput, ByteVolume bvOutput);

 ERROR_NUMBER CopyRealColourPlane(RealColourPlane rcpInput, RealColourPlane rcpOutput);

 ERROR_NUMBER CopyFloatRealColourPlane(FloatRealColourPlane frcpInput, FloatRealColourPlane frcpOutput);

 ERROR_NUMBER CopyboolPlane(boolPlane bpInput, boolPlane bpOutput);

 ERROR_NUMBER CopyboolPlane2FloatPlane(boolPlane bpInput, FloatPlane fpOutput);

 void InvertFloatVolume(FloatVolume fvVolume);

 void InvertFloatPlane(FloatPlane fpPlane);

 void InvertBytePlane(BytePlane bpPlane);

 ERROR_NUMBER power_of_2(bool upwards, int *input);

 int compar(float *arg1, float *arg2);

 BYTE ByteClip(long arg1, long arg2, short sOperation);
   // Clips the output of a byte operation to prevent bit overflow or divide-by-zero

 char * RootName(char * csFullPathName);
	// Returns a pointer to the root name of the full filename pointed to by csFullPathName.
	// NB. This function returns memory that has been allocated internally.  It must, therefore, be subsequently freed.

 ERROR_NUMBER GetRootName(char *csInputFileName, char *csRootName);
	// Fills the pointer to root name with the root name of the full filename pointed to by csFullPathName.

 ERROR_NUMBER GetDirectory(char *csInputFileName, char *csDirectory);
	// Fills the pointer to Directory with the search path of the full filename pointed to by csFullPathName.

char   *date();   // Return the date and time as an ASCII string

ERROR_NUMBER svdcmp(float **a, long m, int n, float w[], float **v);

bool dtw(double *firstloc, double * firsth, long n1, double *secondloc, double *secondh,long n2,
	double *xindout, double *yindout, double *time, double *x, double *y, double *costout, long *nout,
	long searchdepth_ptr, long window_radius_ptr, bool fixed_start_ptr,long max_extension_ptr);

ERROR_NUMBER GetWindowsDisk(char *csWorkingDirectory, char *csDisk);


 ERROR_NUMBER ModifyScaleFactorsForExp2(float *fpXScaleFactor, float *fpYScaleFactor, DWORD dwXDim, DWORD dwYDim, bool bUpward);
	// Modifies *fpXScaleFactor and *fpYScaleFactor s.t. (*fpXScaleFactor * dwXDim) and (*fpYScaleFactor * dwYDim) are
	// exponents of 2.  If bUpward is TRUE, the scale factors are mofied upwards.  Otherwise they are modified downwards.
 ERROR_NUMBER GetImageFileNames(char *sFileNameString, char ***cpppFileNames, long *lpNumFiles, char *csDirectory);
	// Extracts lpNumFiles file-names from the string sFileNameString and returns them in cpppFileNames.
	// The file-names in sFileNameString must be separated by spaces.

ERROR_NUMBER MakeOneDProjectionPair(OneDProjectionPair * buffer, long lHorizontalLength, long lVerticalLength);

void FreeOneDProjectionPair(OneDProjectionPair * buffer);

ERROR_NUMBER ByteSwapWordVolume(WORD ***wWordData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapDWordVolume(DWORD ***dwDWordData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapShortVolume(short ***sShortData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapLongVolume(long ***lLongData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapFloatVolume(float ***fFloatData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapComplexVolume(COMPLEX ***cComplexData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapByteSurfaceVolume(ByteSurfacePoint ***fByteSurfaceData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapDoubleVolume(double ***dDoubleData, DWORD dwSections, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER ByteSwapIntVector(int *ipVector, DWORD dwLength);

void ByteSwapWordVector(WordVector *wvpWordVector);

void ByteSwapInt(int *ipInteger);

WORD GetWordPlaneRectMin(WordPlane wpWordPlane, RECT rSampleRect);

WORD GetWordPlaneRectMax(WordPlane wpWordPlane, RECT rSampleRect);

ERROR_NUMBER GetWordPlaneRectPercentile(WordPlane wpWordPlane, RECT rSampleRect, short sPercentile, WORD *wThreshold);

ERROR_NUMBER AllocateSurface(Surface *spSurface, DWORD dwXDim, DWORD dwYDim, DWORD dwXRange, DWORD dwYRange);

void FreeSurface(Surface sSurface);

ERROR_NUMBER Surface2FloatPlane(FloatPlane *fpFloatPlane, Surface sSurface, BYTE bInterpolation);

void FillShortVectorPlane(ShortVectorPlane svpShortVectorPlane, ShortVector svValue);

void FillLongPlane(LongPlane lpLongPlane, long lValue);

void FillBytePlane(BytePlane bpBytePlane, BYTE bValue);

void FillFloatPlane(FloatPlane fpFloatPlane, float fValue);

ERROR_NUMBER FillFloatPlaneRect(FloatPlane fpFloatPlane, DWORD dwXStart, DWORD dwXFinish, DWORD dwYStart,
								DWORD dwYFinish, float fValue);

ERROR_NUMBER FillComplexPlaneRect(ComplexPlane cpComplexPlane, DWORD dwXStart, DWORD dwXFinish, DWORD dwYStart,
								DWORD dwYFinish, COMPLEX cValue);

ERROR_NUMBER XLinearInterpFloatPlaneRegion(FloatPlane fpFloatPlane, DWORD dwXStart, DWORD dwXFinish, DWORD dwYStart,
										   DWORD dwYStart2, DWORD dwYFinish, DWORD dwYFinish2, float fValue, float fValue2);

ERROR_NUMBER XLinearInterpFloatPlaneRect(FloatPlane fpFloatPlane, RECT rRectangle, float fInitalValue, float fFinalValue);

ERROR_NUMBER YLinearInterpFloatPlaneRect(FloatPlane fpFloatPlane, RECT rRectangle, float fInitalValue, float fFinalValue);

ERROR_NUMBER RectangularSurface2FloatPlane(FloatPlane *fpFloatPlane, Surface sSurface, BYTE bInterpolation);

ERROR_NUMBER MakeWordGridPlane(WordPlane *wppGridPlane, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER MakeFloatGridPlane(FloatPlane *fppGridPlane, DWORD dwRows, DWORD dwColumns);

ERROR_NUMBER MeasureFloatPlaneDifferences(FloatPlane fpFormerPlane, FloatPlane fpLatterPlane,
											ShortVector svSamplingIncrements, float *fpMeanDiff, float *fpMaxDiff);

ERROR_NUMBER CapitalizeString(char *csInputString, char *csOutputString);

void DetermineOutputFilename(char *csOutputFilename, char *csInputFilename, char *csOutputFileDirectory,
	 char *csSuffix, char *csPrefixExtension);

bool	IsNumeric(char cInput);

float FloatNineByOneMedian(float *fpBuffer);

ERROR_NUMBER FloatNineByOneMedianFilter(float **fppBuffer, DWORD dwNumElements);

int	CountRowsInFile(FILE *fpInputFile);

ERROR_NUMBER GetDirectoryFileList(char *csDirectory, char *csFileSpec, char ***cppsFileNames, long *lpNumFiles);
// N.B. Needs to be implemented for UNIX

ERROR_NUMBER FReadLine(FILE *fpInputFile, char *csBuffer, size_t stMaxBytes2Read);

ERROR_NUMBER FLimitedReadLine(FILE *fpInputFile, char *csBuffer, size_t stMaxBytes2Read);

ERROR_NUMBER ILimitedReadLine(int ifpInputFile, char *csBuffer, size_t stMaxBytes2Read);

void UpdateFloatRange(FloatRange *frpFloatRange, float fNewDataPoint);

 int DoubleCompare(const void * arg1, const void * arg2);

 ERROR_NUMBER FindStringInFile(FILE *fpFile, char *csString);

 int ReadIntegerFromBinaryFile(FILE *fpFile);

 ERROR_NUMBER DetermineOutputDirectory(char *csInputFilename, char *csOutputFileDirectory);

 int HexChar2Int(char cHexChar);

 int Hex2Int(char *csHexNumStr);

 ERROR_NUMBER ErrorOpeningFile(char *csFileName);

 ERROR_NUMBER ErrorReadingFile(char *csFileName);

 ERROR_NUMBER ErrorWritingFile(char *csFileName);

 double Pythag(double dArg1, double dArg2);

 float FloatPythag(float fArg1, float fArg2);

 bool IsSystemBigEndian();

 ERROR_NUMBER SquareFloatPlane(FloatPlane *fppPlane, bool bUp, bool bPadWithEdgeMean);

 ERROR_NUMBER SquareComplexPlane(ComplexPlane *cppPlane, bool bUp);

 ERROR_NUMBER	Complex2Float(ComplexPlane cpInputPlane, FloatPlane *fppRealOut, FloatPlane *fppImagOut);

 ERROR_NUMBER	Float2Complex(FloatPlane fpRealIn, FloatPlane fpImagIn, ComplexPlane *cppOutputPlane);

 ERROR_NUMBER	Complex2Amplitude(ComplexPlane cpInputPlane, FloatPlane *fppAmplitudeOut);

 ERROR_NUMBER	Complex2Phase(ComplexPlane cpInputPlane, FloatPlane *fppPhaseOut);

 double	PhaseFromComplex(COMPLEX cInput);

 ERROR_NUMBER	RealColour2Float(RealColourPlane rcpInputPlane, FloatPlane *fppOutputPlane,
	 bool bUseRed, bool bUseGreen, bool bUseBlue, short sOperation);
/*
 ERROR_NUMBER	RealColour2Float(RealColourPlane rcpInputPlane, FloatPlane *fppOutputPlane, short sOperation);
*/
 ERROR_NUMBER SplitFloatRealColour2Float(FloatRealColourPlane frcpFloatRealColourPlane, FloatPlane *fppRedPlane,
	 FloatPlane *fppGreenPlane, FloatPlane *fppBluePlane);

 ERROR_NUMBER MergeFloatVolumes2FloatRealColourVolume(FloatVolume fvRedVolume, FloatVolume fvGreenVolume,
	 FloatVolume fvBlueVolume, FloatRealColourVolume *frcpFloatRealColourVolume);

 ERROR_NUMBER SplitFloatRealColourVolume2FloatVolumes(FloatRealColourVolume frcvFloatRealColourVolume,
	 FloatVolume *fvpRedVolume, FloatVolume *fvpGreenVolume, FloatVolume *fvpBlueVolume);

 double	RandBetweenMinus1And1();

 char *GetSuffix(char *csFileName);

 void SwapFourBytes(char *csBytes);

 char *SwapEightBytes(char *csBytes);

 ERROR_NUMBER	GetComplexPlaneDifference(ComplexPlane cpAddend, ComplexPlane cpSubtraend, ComplexPlane *cppDifference);

 ERROR_NUMBER	GetComplexPlaneQuotient(ComplexPlane cpNumerator, ComplexPlane cpDenominator, ComplexPlane *cppQuotient);

 ERROR_NUMBER	GetNextRecordLength(FILE *fpFile, long *lpLength);

 double ComplexAmplitude(COMPLEX cComplexValue);

 COMPLEX ComplexMin(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexMagnitudeMin(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexIntensityMin(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexIntensityMax(COMPLEX cFormer, COMPLEX cLatter);

 COMPLEX ComplexMax(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexMagnitudeMax(COMPLEX cFormer, COMPLEX cLatter);

 COMPLEX ComplexAddition(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexIntensityAddition(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexMagnitudeAddition(COMPLEX cFormer, COMPLEX cLatter);

 COMPLEX ComplexSubtraction(COMPLEX cFormer, COMPLEX cLatter);

 COMPLEX ComplexDoubleMultiplication(COMPLEX cComplex, double dDouble);

 COMPLEX ComplexMultiplication(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexMagnitudeSubtraction(COMPLEX cFormer, COMPLEX cLatter);

 float ComplexIntensitySubtraction(COMPLEX cFormer, COMPLEX cLatter);

 ERROR_NUMBER FReadAwayRows(FILE *fpInputFile, int iNumRows);

 ERROR_NUMBER GetCurrentWorkingDirectory(char *csBuffer, long lMaxLength);

 void ClipFloatVolumeToRemoveNegativeValues(FloatVolume fvFloatVolume);

 void ClipFloatPlaneToRemoveNegativeValues(FloatPlane fpFloatPlane);

 void RemoveFloatVolumeOffset(FloatVolume fvFloatVolume);

 void RemoveFloatPlaneOffset(FloatPlane fpFloatPlane);

 long XChooseY(long lX, long lY);

 double Factorial(long lArgument);	// Product of natural numbers from 1 to n

 int TriangularNumber(int iArgument);	// Sum of natural numbers from 1 to n

 ERROR_NUMBER GetFloatValueAfterSubstring(char *csString, char *csSubstring, float *fpValue);

 ERROR_NUMBER GetIntValueAfterSubstring(char *csString, char *csSubstring, int *ipValue);

 ERROR_NUMBER MakeNewDirectory(char *csDirName);

 double RealColourBrightness(RealColour rcColour);

 FloatRealColour FloatRealColourSum(FloatRealColour frcAddend1, FloatRealColour frcAddend2);

 LongRealColour RealColourSum(RealColour rcAddend1, RealColour rcAddend2);

 ByteRange	GetBytePlaneRange(BytePlane bpBytePlane);

 FloatRange	GetFloatPlanePositiveRange(FloatPlane fpFloatPlane);

 FloatRange	GetFloatPlaneRange(FloatPlane fpFloatPlane);

 ERROR_NUMBER GetComplexConjugateProductVector(COMPLEX *cpFormerInput, COMPLEX *cpLatterInput, COMPLEX **cppOutput, long lLength);

 void ComplexConjugateMult(float fFormerReal, float fFormerImag, float fLatterReal,
	 float fLatterImag, float *fpOutputReal, float *fpOutputImag);

 ERROR_NUMBER MultRealColourPlaneByFloatPlane(RealColourPlane rcpRealColourPlane, FloatPlane fpFloatPlane);

 ERROR_NUMBER SplitRealColourPlaneBands(RealColourPlane rcpRealColourPlane, BytePlane *bppRedPlane,
	 BytePlane *bppGreenPlane, BytePlane *bppBluePlane);

 COMPLEX ComplexConjugate(COMPLEX cInput);

 double DoubleVector2Radians(DoubleVector dvVector);

 double CubicBSplineBasisFunction(int iIndex, double dArgument);

 double LinearBSplineBasisFunction(int iIndex, double dArgument);

 void ZeroFloatPlane(FloatPlane fpFloatPlane);

 void ZeroBytePlane(BytePlane bpBytePlane);

 void ZeroboolPlane(boolPlane bpboolPlane);

 void ZeroFloatVolume(FloatVolume fvFloatVolume);

 void EnsurePowerOfTwoForIntRangeLimits(IntRange *irpIntRange);

 ERROR_NUMBER GetPeakImageFromFloatPlane(FloatPlane fpFloatPlane, FloatPlane *fppPeakImage);

 ERROR_NUMBER MakeNormalizedByteVolumeFromFloatVolume(FloatVolume fvFloatVolume, ByteVolume *bvpByteVolume);

 void ZeroHoughLineOnLongPlane(HoughLine hlLine, LongPlane lpIndexPlane);

 FloatVector DoubleVectorToFloatVector(DoubleVector dvInputVector);

 double AngularSeparationInRadians(double dFormerAngle, double dLatterAngle);

 ERROR_NUMBER ReadPlaneFromByteVolume(ByteVolume bvByteVolume, int iPlaneIndex, BytePlane *bppBytePlane);

 ERROR_NUMBER ReadPlaneFromboolVolume(boolVolume bvboolVolume, int iPlaneIndex, boolPlane *bppboolPlane);

 ERROR_NUMBER ReadPlaneFromFloatVolume(FloatVolume fvFloatVolume, int iPlaneIndex, FloatPlane *fppFloatPlane);

 void WritePlaneToByteVolume(ByteVolume bvByteVolume, int iPlaneIndex, BytePlane bpBytePlane);

 void WritePlaneToboolVolume(boolVolume bvboolVolume, int iPlaneIndex, boolPlane bpboolPlane);

 void WritePlaneToFloatVolume(FloatVolume fvFloatVolume, int iPlaneIndex, FloatPlane fpFloatPlane);

 ERROR_NUMBER GetMeanOverRangePlaneFromFloatVolume(FloatVolume fvFloatVolume, IntRange irFloatVolumeRange,
	 FloatPlane *fppFloatPlane);

 ERROR_NUMBER GetMedianOverRangePlaneFromFloatVolume(FloatVolume fvFloatVolume, IntRange irFloatVolumeRange,
	 FloatPlane *fppFloatPlane);

 short DirectionalSeparation(short sOldDirection, short sDirection);

void FreeRibbonList(RibbonList *rlList);
void FreeRibbon(Ribbon *rspRibbon);
void ConfinePointsToImage(BytePlane bpBytePlane, FloatVector *fvpStartingPoint, FloatVector *fvpEndingPoint);

void ConnectPointsOnBytePlane(FixedPtVector fpvStartingPt, FixedPtVector fpvEndingPt, BYTE bValue, BytePlane bpInPlane);
double DoubleModulo(double dNumerator, double dDenominator);




