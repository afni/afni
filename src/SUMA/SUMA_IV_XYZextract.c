/*#define TEST*/
/*#define DEBUG_3*/
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif
 
/*!
 
File :Taken from  IV_XYZextract.c
Author : Ziad Saad
Date : Tue Nov 17 19:02:16 CST 1998
 
Purpose : 
 
 	Extracts the XYZ coordinates of the nodes in an IV file so that we can manipulate the 
 	data to our liking. The program looks for a sequence of characters that seem to indicate
 	the start of the XYZ list. Once the sequence is found, the series of triplets is read and
 	written out either to a file or to the screen. The sequence of triplets must be terminated 
 	by an ending sequence.
 	The starting and ending sequences are hard coded into the program.
 	
 	The program outputs an error message if :
 	If the starting or ending sequences are not found
 	If the number of points read is not a multiple of three
 	If the first number of the first XYZ triplet and the last character in the starting sequence 
 		are not spearated by a space (or tab). You can fix this by manually adding a space.
 	 
 	
 
Input paramters : 
 
 	IV_filename (char *) a string specifying the name of the inventor file
 	N_NodeList (int *) will give the number of rows in NodeList
	Include_Index (int) (0/1) controls the output of the function, see ahead for info
 
Usage : 
		NodeList = SUMA_IV_XYZextract (char *IV_filename, int *N_NodeList, int Include_Index)
 
 
Returns : 
 NodeList (float **) an Mx3 (or Mx4) matrix containing the 
 	
 		X	Y	Z coordinates of each node (with Include_Index = 0)
		or
		NodeIndex  X	Y	Z coordinates of each node (with Include_Index = 1)
 		
 	entries on one line are separated by a tab.
 	
 
Support : 
 
 
 
Side effects : 
 
 
 
***/
 
/* Header FILES */
 
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"
 
/* CODE */
float ** SUMA_IV_XYZextract (char *IV_filename, int *N_NodeList, int IncludeIndex)
{/* SUMA_IV_XYZextract */
	char s[500],serr[500];
	char seq_strt[5][30], seq_end[5][30];
	float f, *linv, **NodeList;
	int i, ex, si, si_exit, evl, se, se_exit, cnt ;
	int nospacing, MaxAlloc = 100;
	div_t cnt3;
	FILE*iv_file;
	
	/* intialize the number of points read to 0 */
	*N_NodeList = 0;
	
	linv = (float *)malloc (MaxAlloc*sizeof(float));
	if (!linv)
		{
				SUMA_alloc_problem ("Allocation error in SUMA_IV-XYZExtract");
				return (NULL);
		}
	
	/*This is the sequence to trigger reading the numbers*/
	sprintf (seq_strt[0],"Coordinate3");
	sprintf (seq_strt[1],"{");
	sprintf (seq_strt[2],"point");	
	sprintf (seq_strt[3],"[");

	si_exit = 4; /* set equal to the number of strings to be matched */

	/*This is a sequence to mark the end of the number list*/
	sprintf (seq_end[0],"]");
	sprintf (seq_end[1],"}");

	se_exit = 2; /* set equal to the number of strings to be matched */

	iv_file = fopen (IV_filename,"r");
	if (iv_file == NULL)
		{
			SUMA_error_message ("SUMA_IV_XYZextract","Could not open input file ",1);
			exit (1);
		}


	si = 0;
	ex = 1;
	cnt = 0;

	nospacing = 0; /* this flag is used to when the last number is read and it has the first seq_end*/
							/* character attached to it, ie no space ! in between*/
	while (ex != EOF && si < si_exit)
	{
		ex = fscanf (iv_file,"%s",s);

		/*evl = equal_strings (s,seq_strt[si]);*/

		if (strlen (seq_strt[si]) >= strlen (s)) 
			{
				evl = SUMA_iswordin (seq_strt[si],s);
				if (evl == 1)
					nospacing = 0; /* There is a space between character in starting sequence and first number*/ 
			}
		else
			{
				evl = SUMA_iswordin (s,seq_strt[si]);
				if (evl == 1)
						nospacing = 1;
			}

		switch (evl)
			{
				case 0:
					si = 0;		/* No match, reset the sequence counter */
					break;
				case 1:
					if (nospacing == 1 && si == si_exit-1) /* it has to be the last character in the sequence*/
						{
							sprintf (serr,"Must have a space character between first number and last character in start sequence ( %s )",s);
							SUMA_error_message ("SUMA_IV_XYZextract",serr,1);
							exit (1);

						}
					si = si +1;	/* increment the start sequence counter */
					#ifdef DEBUG_3
						printf ("found %s  ---  si = %d\n",s,si);
					#endif
					break;
				default:
					break;
			}
	}

	/* Now, read the series of numbers until you encounter the first string of the ending sequence*/
	se = 0;
	nospacing = 0; 

	while (ex != EOF && se < se_exit )
		{
			ex = fscanf (iv_file,"%s",s);
			/*evl = equal_strings (s,seq_end[se]);*/

			if (strlen (seq_end[se]) >= strlen (s)) 
				{
					evl = SUMA_iswordin (seq_end[se],s);
					if (evl == 1)
						nospacing = 0; /* There is a space between last number and fisrt character in ending sequence */ 
				}
			else
				{ 
					evl = SUMA_iswordin (s,seq_end[se]);
					if (evl == 1)
						nospacing = 1;
				}

			switch (evl)
				{
					case 0:
						f = atof (s);
						linv[cnt] = f;
							#ifdef DEBUG_3
								printf ("\nNumber (%d): %d is appended to end sequence !\n",cnt,linv[cnt]);	
							#endif
						
						
						++cnt;
						
						if (cnt >= MaxAlloc - 1)
							{
								MaxAlloc = MaxAlloc + 100;
								linv = (float *)realloc ((void*) linv, MaxAlloc * sizeof(float));
								if (!linv)
									{
										SUMA_alloc_problem ("Re-Allocation error in IV-FaceSetExtract");
										return (NULL);
									}
								
							}  
						se = 0;  /* no match for ending sequence, reset the counter */
						break;
					case 1:		/* found the first character in the end sequence */
						if (nospacing == 1 && se == 0) /* it has to be the first character in the sequence*/
						{
							f = atof (s);
							linv[cnt] = f;
							
							++cnt;
							#ifdef DEBUG_3
								printf ("\nLast number (%d): %f is appended to end sequence !\n",cnt,f);	
							#endif
						}
						#ifdef DEBUG_3
							printf ("\nfound %s  ---  se = %d\n",s,se);
						#endif
						se = se + 1;
						break;
					default:
						break;
				}

		}
	if (si < si_exit)
		{
			SUMA_error_message ("SUMA_IV_XYZextract","Could not find specified starting sequence",0);
			for (i=0;i<si_exit;++i)
					printf ("%s \t",seq_strt[i]);
		}
	else
		{ 
			if (se < se_exit)
				{
					SUMA_error_message ("SUMA_IV_XYZextract","Could not find specified ending sequence",0);
					for (i=0;i<se_exit;++i)
							printf ("%s \t",seq_end[i]);
				}
			else
				{
					/* check that the number of points read is multiple of 3 */
					cnt3 = div (cnt,3);
					if (cnt3.rem != 0)
						{
							SUMA_error_message ("SUMA_IV_XYZextract","number of points read is not multiple of 3 !",0);
							#ifdef DEBUG_3
								printf ("%f XYZ sets read !!!\n",(float)cnt/3);
							#endif
						}
				}
		}

	*N_NodeList = cnt3.quot;
	
	/* Now allocate space for IV_FaceSetsextract and fill it up */
	if (!IncludeIndex)
		NodeList = (float **) SUMA_allocate2D (*N_NodeList, 3, sizeof(float));
	else
		NodeList = (float **) SUMA_allocate2D (*N_NodeList, 4, sizeof(float));
	
	if (!NodeList)
		{
			SUMA_alloc_problem("SUMA_IV_XYZextract : Could not allocate");
			return (NULL);
		}
	
	if (!IncludeIndex)
		{	
			for (i=0; i< cnt; ++i)
				{
					cnt3 = div(i,3);
					NodeList[cnt3.quot][cnt3.rem] = linv[i];
				}
		}
	else
		{
			for (i=0; i< cnt; ++i)
				{
					cnt3 = div(i,3);
					if (cnt3.rem == 0)
						NodeList[cnt3.quot][cnt3.rem] = cnt3.quot;
					
					NodeList[cnt3.quot][cnt3.rem+1] = linv[i];
					
				}
			
		}
		
	fclose (iv_file);
	free (linv);
	
	return (NodeList);
	
	
}/* SUMA_IV_XYZextract */

#ifdef TEST 
void usage ()
 
  {/*Usage*/
		printf ("\n\33[1mUsage: \33[0m SUMA_IV_XYZextract <IV_filename> [-o output filename] [-c]\n\n");
		printf ("\t  	Extracts the XYZ coordinates of the nodes in an IV file so that we can manipulate the \n");
		printf ("\t data to our liking. The program looks for a sequence of characters that seem to indicate\n");
		printf ("\t the start of the XYZ list. Once the sequence is found, the series of triplets is read and\n");
		printf ("\t written out either to a file or to the screen. The sequence of triplets must be terminated \n");
		printf ("\t by an ending sequence.\n");
		printf ("\t The starting and ending sequences are hard coded into the program.\n");
		printf ("\t \n");
		printf ("\t The program outputs an error message if :\n");
		printf ("\t If the starting or ending sequences are not found\n");
		printf ("\t If the number of points read is not a multiple of three\n");
		printf ("\t If the first number of the first XYZ triplet and the last character in the starting sequence \n");
		printf ("\t 	are not spearated by a space (or tab). You can fix this by manually adding a space.\n");
		printf ("\t \n");
		printf ("\t IV_filename : Filename of the ascii inventor file \n");
		printf ("\t [-o output-filename] : output file name containing the : NodeNumber X Y Z  \n");
		printf ("\t                   of each node in the iv file. This parameter is optional\n");
		printf ("\t                   if no filename is specified, the output goes to the screen\n");
		printf ("\t [-co] : Count the number of nodes, do not output results\n");
		printf ("\t [-ii] : include the Node Index in the output.\n");
		printf ("\t The format of the output on each line is tab delimited:\n");
		printf ("\t NodeIndex	X	Y	Z  (with -ii)\n\n");
		printf ("\t X	Y	Z  (without -ii)\n\n");
		printf ("\t This is a new version of IV-XYZextract. The old one is left for backward compatibility\n\n");
		printf ("\t\t\t Ziad Saad \tTue Nov 17 20:05:53 CST 1998 \n");
		exit (0);
  }/*Usage*/
 
main (int argc,char *argv[])
{/* Main */
char outfile[300];
float ** NodeList;
int N_NodeList, writeout,CountOnly,paramnum, ncol,IncludeIndex;
 
if (argc < 2)
    {
      usage ();
      exit (1);
     }

writeout = 0;			/* check out second option */
CountOnly = 0;			/* Set to 1 if you want to Count the number of triplets only */
IncludeIndex = 0;
paramnum = 2;
while (paramnum < argc)
	{
		if (equal_strings (argv[paramnum],"-o") == 1)
			{
				if ((paramnum + 1) >= argc) 
					{
						SUMA_error_message ("SUMA_IV_XYZextract","No Output file name specified with -o option",1);
						exit (1);
					}
				if (filexists (argv[paramnum+1]) == 1)
					{
						SUMA_error_message ("SUMA_IV_XYZextract","Output file exists, will not overwrite",1);
						exit (1);
					}
				else
					{
						sprintf(outfile,"%s",argv[paramnum+1]);
						writeout = 1;
					}
				++ paramnum;
			}
		if (equal_strings (argv[paramnum],"-co") == 1)
			{
				CountOnly = 1;
			}
		
		if (equal_strings (argv[paramnum],"-ii") == 1)
			{
				IncludeIndex = 1;
			}
		
	++ paramnum;
	}

if (IncludeIndex)
	ncol = 4;
else
	ncol = 3;
	
NodeList = SUMA_IV_XYZextract (argv[1], &N_NodeList, IncludeIndex)	;

if (CountOnly)
	printf ("%d Nodes XYZ corrdinates read\n",N_NodeList);

if (writeout == 1)
	write_2Dfloat(NodeList, outfile, N_NodeList, ncol, 1);
else if (!CountOnly)
	SUMA_disp_mat(NodeList, N_NodeList, ncol,1);

SUMA_free2D ((char **)NodeList,N_NodeList);

}/* Main */
#endif
