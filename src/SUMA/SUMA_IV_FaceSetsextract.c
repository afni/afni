/*#define TEST*/
/*#define DEBUG_3*/
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif

/* Header FILES */
#include "SUMA_suma.h"
 
/*!
 
File : Taken from IV_FaceSetsextract.c
Author : Ziad Saad
Date : Tue Nov 17 19:02:16 CST 1998
 
Purpose : 
 
 	Extracts the FaceSets coordinates of the nodes in an IV file so that we can manipulate the 
 	data to our liking. The program looks for a sequence of characters that seem to indicate
 	the start of the FaceSets list. Once the sequence is found, the series of triplets is read and
 	written out either to a file or to the screen. The sequence of triplets must be terminated 
 	by an ending sequence.
 	The starting and ending sequences are hard coded into the program.
 	
 	The program outputs an error message if :
 	If the starting or ending sequences are not found
 	If the number of points read is not a multiple of three
 	If the first number of the first FaceSets triplet and the last character in the starting sequence 
 		are not spearated by a space (or tab). You can fix this by manually adding a space.
 	 
 	
 
Input paramters : 
 
 	IV_filename (char *) a string specifying the name of the inventor file
	N_FaceSetList (int *) will give the number of rows in FaceSetList
 
Usage : 
		FaceSetList = SUMA_IV_FaceSetsextract (char *IV_filename, int *N_FaceSetList)
 
 
Returns : 
 
 	FaceSetList (int *) the facesetlist in the inventor file, an  Mx3 integer vector (used to be a matrix before SUMA 1.2)
 		
 
Support : 
 
 
 
Side effects : 
 
 
 
***/
 

int *SUMA_IV_FaceSetsextract (char *IV_filename, int *N_FaceSetList)
{/* SUMA_IV_FaceSetsextract */
	char s[500],serr[500];
	char seq_strt[5][30], seq_end[5][30];
	int i, f, ex, si, si_exit, evl, se, se_exit;
	int ip, NP, cnt, nospacing, MaxAlloc = 100, *linv, *FaceSetList;
	div_t cnt4;
	FILE*iv_file;

	/* intialize the number of points read to 0 */
	*N_FaceSetList = 0;
	
	linv = (int *)SUMA_malloc (MaxAlloc*sizeof(int));
	if (!linv)
		{
				SUMA_alloc_problem ("Allocation error in IV-FaceSetExtract");
				return (NULL);
		}
		
	/*This is the sequence to trigger reading the numbers*/
	sprintf (seq_strt[0],"IndexedFaceSet");
	sprintf (seq_strt[1],"{");
	sprintf (seq_strt[2],"coordIndex");	
	sprintf (seq_strt[3],"[");

	si_exit = 4; /* set equal to the number of strings to be matched */

	/*This is a sequence to mark the end of the number list*/
	sprintf (seq_end[0],"]");
	sprintf (seq_end[1],"}");

	se_exit = 2; /* set equal to the number of strings to be matched */

	iv_file = fopen (IV_filename,"r");
	if (iv_file == NULL)
		{
			SUMA_error_message ("SUMA_IV_FaceSetsextract","Could not open input file ",1);
			return (NULL);
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
							SUMA_error_message ("SUMA_IV_FaceSetsextract",serr,1);
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
						f = atoi (s);
						linv[cnt] = f;
							#ifdef DEBUG_3
								printf ("\nNumber (%d): %d is appended to end sequence !\n",cnt,linv[cnt]);	
							#endif
		
						++cnt;
						if (cnt >= MaxAlloc - 1)
							{
								MaxAlloc = MaxAlloc + 100;
								linv = (int *)SUMA_realloc ((void*) linv, MaxAlloc * sizeof(int));
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
							f = atoi (s);
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
			SUMA_error_message ("IV_FaceSetextract","Could not find specified starting sequence",0);
			for (i=0;i<si_exit;++i)
					printf ("%s \t",seq_strt[i]);
		}
	else
		{ 
			if (se < se_exit)
				{
					SUMA_error_message ("IV_FaceSetextract","Could not find specified ending sequence",0);
					for (i=0;i<se_exit;++i)
							printf ("%s \t",seq_end[i]);
				}
			else
				{
					/* check that the number of points read is multiple of 4 */
					cnt4 = div (cnt,4);
					if (cnt4.rem != 0)
						{
							SUMA_error_message ("IV_FaceSetextract","number of points read is not multiple of 4 !",0);
							#ifdef DEBUG_3
								printf ("%f FaceSets sets read !!!\n",(float)cnt/4);
							#endif
						}
					
				}
		}

	*N_FaceSetList = cnt4.quot ;
	
	/* Now allocate space for SUMA_IV_FaceSetsextract and fill it up */
	NP = 3;
	FaceSetList = (int *) SUMA_calloc (*N_FaceSetList * NP, sizeof(int));
	if (!FaceSetList)
		{
			SUMA_alloc_problem("IV_FaceSetextract : Could not allocate");
			return(NULL);
		}
	
	i = 0;
	ip = 0;
	while (i < cnt) {
		FaceSetList[ip] = linv[i]; ++ip; ++i;
		FaceSetList[ip] = linv[i]; ++ip; ++i;
		FaceSetList[ip] = linv[i]; ++ip; ++i;
		++i; /* skip the 4th value */
	}
	
	fclose (iv_file);
	
	SUMA_free(linv);
	
	return (FaceSetList);

}/* SUMA_IV_FaceSetsextract */

#ifdef TEST 
void usage ()
 
  {/*Usage*/
		printf ("\n\33[1mUsage: \33[0m SUMA_IV_FaceSetsextract <IV_filename> [-o output filename] [-c]\n\n");
		printf ("\t  	Extracts the FaceSets coordinates of the nodes in an IV file so that we can manipulate the \n");
		printf ("\t data to our liking. The program looks for a sequence of characters that seem to indicate\n");
		printf ("\t the start of the FaceSets list. Once the sequence is found, the series of triplets is read and\n");
		printf ("\t written out either to a file or to the screen. The sequence of triplets must be terminated \n");
		printf ("\t by an ending sequence.\n");
		printf ("\t The starting and ending sequences are hard coded into the program.\n");
		printf ("\t \n");
		printf ("\t The program outputs an error message if :\n");
		printf ("\t If the starting or ending sequences are not found\n");
		printf ("\t If the number of points read is not a multiple of three\n");
		printf ("\t If the first number of the first FaceSets triplet and the last character in the starting sequence \n");
		printf ("\t 	are not spearated by a space (or tab). You can fix this by manually adding a space.\n");
		printf ("\t \n");
		printf ("\t IV_filename : Filename of the ascii inventor file \n");
		printf ("\t [-o output-filename] : output file name containing the : NodeNumber X Y Z  \n");
		printf ("\t                   of each node in the iv file. This parameter is optional\n");
		printf ("\t                   if no filename is specified, the output goes to the screen\n");
		printf ("\t [-co] : Count the number of nodes, do not output results\n");
		printf ("\t The format of the output on each line is tab delimited:\n");
		printf ("\t NodeNumber	X	Y	Z\n\n");
		printf ("\t The older version of this code is called IV-FaceSetsextract\n");
		printf ("\t  it is left around for backward compatibilty\n\n");
		printf ("\t\t\t Ziad Saad \t Tue Nov 17 19:00:42 CST 1998\n");
		exit (0);
  }/*Usage*/
 
main (int argc,char *argv[])
{/* Main */
char outfile[300];
int N_FaceSetList, *FaceSetList ,writeout,CountOnly,paramnum;
 
if (argc < 2)
    {
      usage ();
      exit (1);
     }

writeout = 0;			/* check out second option */
CountOnly = 0;			/* Set to 1 if you want to Count the number of triplets only */
paramnum = 2;
while (paramnum < argc)
	{
		if (equal_strings (argv[paramnum],"-o") == 1)
			{
				if ((paramnum + 1) >= argc) 
					{
						SUMA_error_message ("SUMA_IV_FaceSetsextract","No Output file name specified with -o option",1);
						exit (1);
					}
				if (filexists (argv[paramnum+1]) == 1)
					{
						SUMA_error_message ("SUMA_IV_FaceSetsextract","Output file exists, will not overwrite",1);
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
	++ paramnum;
	}

FaceSetList = SUMA_IV_FaceSetsextract (argv[1], &N_FaceSetList);

if (CountOnly)
	printf ("%d FaceSets sets read\n",N_FaceSetList);
	
if (writeout == 1) {
		FILE *outfid; 
	
	outfid = fopen (outfile, "w");
	if (outfid == NULL) {
		fprintf (SUMA_STDERR, "Error %s: Could not open %s for writing.\n", FuncName, outfile);
	}else {
		i = 0;
		cntlim = N_FaceSetList*3;
		while (i < cntlim) {
			j = 0;
			while (j < 3) {
				fprintf (outfid, "%d\t", FaceSetList[i]);
				++i;
				++j;
			}
			fprintf (outfid, "\n");
		}
		fclose (outfid);
	}
} else if (!CountOnly) {
	i = 0;
	cntlim = N_FaceSetList*3;
	while (i < cntlim) {
		j = 0;
		while (j < 3) {
			fprintf (SUMA_STDOUT, "%d\t", FaceSetList[i]);
			++i;
			++j;
		}
		fprintf (SUMA_STDOUT, "\n");
	}
}
	
SUMA_free(FaceSetList);
	
}/* Main */
#endif
