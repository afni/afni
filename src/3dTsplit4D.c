/*
3dTsplit4D.c
This is a quick program to split a 3d+time dataset into multiple
single 3D files.  Mostly to facilitate transferring data into MATLAB toolboxes.
It can also be useful for deal with SPM and FSL programs.  

Author: Peter J. Molfese, Haskins Laboratories/UConn/Yale
Contact: Peter.Molfese@yale.edu

January 5, 2016 - Initial Version
*/



#include "mrilib.h"

int help_3dTsplit4D( )
{
	printf(
		"USAGE: 3dTsplit4D [options] dataset\n\n"
		"This program converts a 3D+time dataset into multiple 3D single-brick files.\n"
		"The main purpose of this is to accelerate the process of export AFNI/NIFTI datasets\n"
		"if you have the unfortunate need to work with Some other PrograM that doesn't like\n"
		"datasets in the pseudo-4D nature that AFNI knows and loves.\n\n"
		" -prefix PREFIX: Prefix of the output dataset \n"
		"\t Numbers will be added after the prefix to denote prior sub-brick.\n"
		"\n\n\n"
		"Authored by: Peter Molfese, UConn"
		);

	PRINT_COMPILE_DATE; 
	return(0);
}

int main( int argc, char *argv[] )
{
	THD_3dim_dataset *iset, *oset;
	int iarg=1, kk, nval;
	char *prefix = "SPLIT";
	char *sub_prefix[128];
	//char *sub_prefix;
	MRI_IMAGE *inImage, *outImage;
	
	if( argc < 2 || strcmp(argv[1], "-help") == 0 )
	{
		help_3dTsplit4D( );
		exit(0);
	}
	
	mainENTRY("3dTsplit4D"); 
	machdep();
	
	while( iarg < argc && argv[iarg][0] == '-' )
	{
		if( strcmp( argv[iarg], "-prefix") == 0 )
		{
			prefix = argv[++iarg];
			if( !THD_filename_ok(prefix) )
				ERROR_exit("bad -prefix value");
				iarg++;
				continue; //not sure that I need this...
		} //if
	} //while
	
	printf("Prefix set to: %s\n", prefix);
	
	/* Begin reading dataset, error checking like a good programmer */
	iset = THD_open_dataset( argv[iarg] );
	CHECK_OPEN_ERROR( iset, argv[iarg] );
	DSET_load(iset);
	CHECK_LOAD_ERROR(iset);
	
	//begin looping through!
	
	nval=DSET_NVALS(iset);
	INFO_message("Dataset read...\n");
	INFO_message("Number of Sub-bricks: %d\n", nval);
	
	for( kk=0 ; kk < nval ; kk++ )
	{
		oset = EDIT_empty_copy( iset ); //Easy to just copy!
	
		/* MODIFY to make single brik output */
		/* ALSO NEED TO CHANGE PREFIX EACH TIME! */
		
		sprintf(sub_prefix, "%s.%d", prefix, kk);
		//sub_prefix = strncat( prefix, itoa(kk), 10 );
		INFO_message("File Saved: %s", sub_prefix);
		
		EDIT_dset_items( oset, 
			ADN_datum_all , MRI_float ,
			ADN_prefix , sub_prefix ,
			ADN_ntt, 1 ,
			ADN_nvals, 1, ADN_none);
		
		
		inImage = THD_extract_float_brick(kk, iset);
		
		/* Make sure data works */
		if( inImage == NULL)
			ERROR_exit("\n\nFailed to open sub-brick %d of input dataset...", kk);
		//DSET_unload_one(iset, kk);

		
		EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(inImage) );
		
		DSET_write( oset );
		WROTE_DSET( oset );
		
		DSET_unload_one(iset, kk);
		mri_clear_and_free( inImage );
		
	}//for
	
	printf("\n...Done\n");
		
	exit(0);
}