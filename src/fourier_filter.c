/* This file is #included by the filtering program 3dFourier and the plugin plug_fourier */
/* By T. Ross and K. Heimerl 8-99 */


static void *My_Malloc( size_t size) {
	void *ptr=NULL;
	
	ptr = (void *)malloc(size);
	if (ptr == NULL) {
#ifdef IN_FOURIER_PLUGIN
		fprintf(stderr, "Fatal error in Fourier Filter Driver, malloc returned NULL");
		exit(1);
#else
		Error_Exit("Fatal: malloc returned NULL"); 
#endif
	}
	return ptr;
}




/**************************************************************************************
This function zero pads a signal to a power of 2 * factor of 3 or 5 then FFT's the data. 
The zero-padded data are FFT'd, and the resultant spectrum is multiplied by an ideal
low or high pass window.  The signal is then inverse FFt'd and the zero padded portion
of the signal is eliminated.
**************************************************************************************/
static char *filter(float *ORIG_SIG, float low_fc, float high_fc, int N, float period, int ignore, int retrend, int transform)
{

	/**************************************************************************************
			DECLARE and INITIALIZE VARIABLES
	**************************************************************************************/
	float sum, meanorig, mean_new, Fs, slope, inter;
	int  dummy, i,j,power,padded_N, M;
	int Ncutlo1,Ncutlo2,Ncuthi1,Ncuthi2;
	FILE *outfile;
	complex *NEW_SIG, *newfft, *win_fft, *win_low, *win_high;  /*typedef'd by B Cox */
	complex *restored_sig, *final_sig, sumcx, meanfinal;




	/*************************************************************************************
		DETREND ORIGINAL DATA 
	**************************************************************************************/

	if (ignore>=N) {
		if (!transform)
			return "You cannot ignore all of the data";
		else
			high_fc = 1000;  /* fine, make it an all-stop */
	} else {
		for (i=0; i<(N-ignore); i++)
			ORIG_SIG[i] = ORIG_SIG[i+ignore];
		N -= ignore;
	}

	/* Detrend the data, setting the end points to zero */
	slope = ((float)ORIG_SIG[N-1] - (float)ORIG_SIG[0] ) / ((float) N - 1.0);
	inter = (float)ORIG_SIG[0];
	for (i=0; i<N ;++i)
		ORIG_SIG[i] -= (inter + slope*(float)i);

	/*************************************************************************************
		CALCULATE LENGTH TO WHICH DATA MUST BE ZERO PADDED for csfft_cox()
	**************************************************************************************/


	padded_N=csfft_nextup_one35(N);


	NEW_SIG=(complex *)My_Malloc(padded_N*sizeof(complex));		/*Tom Ross's malloc*/
	newfft=(complex *)My_Malloc(padded_N*sizeof(complex));  	/*e.g. newfft[7].r or newfft[7].i*/
	win_fft=(complex *)My_Malloc(padded_N*sizeof(complex));
	win_low=(complex *)My_Malloc(padded_N*sizeof(complex));
	win_high=(complex *)My_Malloc(padded_N*sizeof(complex));
	restored_sig=(complex *)My_Malloc(padded_N*sizeof(complex));
	final_sig=(complex *)My_Malloc(N*sizeof(complex));		


	Fs= 1.0/period;							/* sampling frequency*/

	M=padded_N;
	

	/*************************************************************************************
			PERFORM FFT ON NEW DATA	
	**************************************************************************************/

	for(i=0;i<N;++i)
	{
	 	newfft[i].r=ORIG_SIG[i];
		newfft[i].i=0;
	}
	
	for(i=N;i<M;++i)			/*Zero pad to length padded_N*/
	{
	 	newfft[i].r=0;
		newfft[i].i=0;
	}
	/* M must be pwr of 2 OR pwr of 2 times a factor of 3 or 5*/

	csfft_cox(-1, M, newfft);


	/*************************************************************************************
			GENERATE LOW PASS WINDOW
	**************************************************************************************/
	if((low_fc >= Fs) || (low_fc == 0.0)) {
		if ((low_fc >= Fs) && (!transform)) {
			return "Lowpass filter is an all-pass filter since Fc > sampling frequency\n";
		}
		for(i=0;i<M;++i)
		{
			win_low[i].r= 1;
			win_low[i].i= 0;
		}
	} 
	else if ( (low_fc > 0) && (low_fc < (Fs/(float)M) ) )
	{
		if (!transform) {
			return "Lowpass filter is a no-pass filter since Fc is too small.\n";
		}

		for(i=0;i<M;++i)
		{
		   	win_low[i].r=0;  
			win_low[i].i=0;
		}
	} 
	else
	{ 

	
		Ncutlo1=(int)((low_fc*M)/Fs); 	/*index number of lower cutoff frequency*/
		Ncutlo2=(M-Ncutlo1);		/*index number of upper cutoff frequency*/

		win_low[0].r=1;			/*keep DC offset but no phase change*/
		win_low[0].i=0;

		for(i=1;i<=Ncutlo1;++i)
		{
		    	win_low[i].r=1;
			win_low[i].i=0;
		}
		
		for(i=Ncutlo1+1;i<Ncutlo2;++i)
		{
		    	win_low[i].r=0;  
			win_low[i].i=0;
		}

		for(i=Ncutlo2;i<M;++i)
		{
			win_low[i].r=1;
			win_low[i].i=0;
		}
	}


	/*************************************************************************************
			GENERATE HIGH PASS WINDOW
	**************************************************************************************/
	
	if (high_fc < Fs/(float)M) 
	{
		if ((!transform) && (high_fc!=0)){
			return "Highpass filter is an all-pass filter since Fc is too small.\n";
		}
		for(i=0;i<M;++i)
		{
			win_high[i].r= 1;
			win_high[i].i= 0;
		}	
	}
	else if (high_fc >= Fs) 
	{
		if (!transform) {
			return "Highpass filter is an all-stop filter since Fc > sampling frequency.\n";
		}

		for(i=0;i<M;++i)
		{
		   	win_low[i].r=0;  
			win_low[i].i=0;
		}
	} 
	else
	{ 
		 
		Ncuthi1=(int)((high_fc*M)/Fs); 		/*index number of lower cutoff frequency*/
		Ncuthi2=(M-Ncuthi1);			/*index number of upper cutoff frequency*/

		
		for(i=0;i<Ncuthi1;++i)
		{
			win_high[i].r=0;
			win_high[i].i=0;
		}
		
		for(i=Ncuthi1;i<(int)(M/2);++i)
		{
			win_high[i].r=1;
			win_high[i].i=0;
		}


			win_high[M/2].r=1;		
			win_high[M/2].i=0;



		for(i=(int)(M/2)+1;i<=Ncuthi2;++i)
		{
		    	win_high[i].r=1;
			win_high[i].i=0;
		}


		for(i=Ncuthi2+1;i<M;++i)
		{
			win_high[i].r=0;
			win_high[i].i=0;
		}
	}


	/**************************************************************************************
			MULTIPLY FFT'D NEWSIGNAL BY WINDOWS
	**************************************************************************************/

	/* first multiply through by low pass window  */

	for(i=0;i<M;++i)
	{
		win_fft[i].r=win_low[i].r*newfft[i].r-win_low[i].i*newfft[i].i;
		win_fft[i].i=win_low[i].r*newfft[i].i+win_low[i].i*newfft[i].r;
	}



	/* then multiply result by high pass window  */

	for(i=0;i<M;++i)
	{
		restored_sig[i].r=win_high[i].r*win_fft[i].r-win_high[i].i*win_fft[i].i;
		restored_sig[i].i=win_high[i].r*win_fft[i].i+win_high[i].i*win_fft[i].r;
	}


	/**************************************************************************************
			INVERSE FFT WINDOWED NEW SIGNAL	
	**************************************************************************************/

	csfft_cox(1, M, restored_sig);		/* Doesn't scale for inverse FFT  */

	
	for(i=0;i<M;++i)
	{
		restored_sig[i].r=restored_sig[i].r/(float)M;
		restored_sig[i].i=restored_sig[i].i/(float)M;
	}

		

	/**************************************************************************************
			ORIG_SIGNAL IS RETURNED TO AFNI, FREE ALLOCATED MEMORY
	**************************************************************************************/

	/*Note that data points N-M are the zero padded portion - we don't keep those data*/

	for (i=0;i<N; ++i)
	{
		ORIG_SIG[i]=restored_sig[i].r;

	}


	/* Retrend the data */
	if (retrend) 
		for (i=0; i<N ;++i)
			ORIG_SIG[i] += (inter + slope*(float)i);
	
	if (ignore) {
		for (i=(N+ignore-1); i>=ignore; i--)
			ORIG_SIG[i]=ORIG_SIG[i-ignore];
		for (i=0; i<ignore; i++)
			ORIG_SIG[i]=ORIG_SIG[ignore];
	}


	free(NEW_SIG);		
	free(newfft);  	
	free(win_fft);
	free(win_low);
	free(win_high);
	free(restored_sig);
	free(final_sig);		





	/*************************************************************************************
					the end of the filter
	**************************************************************************************/
	return NULL;  /* No errors */
}



#ifdef IN_FOURIER_PLUGIN
static char *Fourier_Filter_Driver(PLUGIN_interface *plint, THD_3dim_dataset *input, float low_fc, float high_fc, int ignore, int autocorr, int retrend, char *output_prefix)  {
#else
static char *Fourier_Filter_Driver(THD_3dim_dataset *input, float low_fc, float high_fc, int ignore, int autocorr, int retrend, char *output_prefix)  {
#endif
	int i, j;
	int ntimes, nvox;
	float **out_data, *out_temp, *scale, *input_data_f, period;
	byte *input_data_b;
	short *input_data_s;
	THD_3dim_dataset *output=NULL;
	char *err;

	/* should be a valid 3d+time input */	
	DSET_load(input);
	
	ntimes = DSET_NUM_TIMES(input);
	nvox = DSET_NVOX(input);
	
	/* Create a float array for the output */
	out_data = (float **)My_Malloc(ntimes * sizeof(float *));
	for (i=0; i<ntimes; out_data[i++] = (float *)My_Malloc(nvox * sizeof(float)));
	
	/* Create the tempory float array */
	out_temp = (float *)My_Malloc(ntimes*sizeof(float));
	
	/* Get the scale factors for later */
	scale = (float *)My_Malloc(ntimes*sizeof(float));
	for (i=0; i<ntimes; i++) {
		if (!DSET_BRICK_FACTOR(input,i))
			scale[i] = 1.0;
		else
			scale[i] = DSET_BRICK_FACTOR(input,i);
	}
	
	/* get the sampling period */
	period = DSET_TIMESTEP(input);
	switch (DSET_TIMEUNITS(input)) {
		case UNITS_MSEC_TYPE: {period/=1000; break; }
		case UNITS_SEC_TYPE:   break;
		case UNITS_HZ_TYPE:   return ("FIlter_Driver Error:\nHmm, you would think I could handle a 3d+time that is already\nfrequency, but Im lame");
		default: return("Filter_Driver Error:\nI dont understand the units of the 3d+time");
	}

#ifdef IN_FOURIER_PLUGIN
	PLUTO_popup_meter(plint);		
#endif			
			
	/* Loop over voxels, pull out the time series and filter */
	for (i=0; i< nvox; i++) {
		for (j=0; j<ntimes; j++) {
			switch(DSET_BRICK_TYPE(input,j)) {
				case MRI_byte: {
					input_data_b = (byte *)DSET_ARRAY(input,j);
					out_temp[j] = scale[j] * (float)input_data_b[i];
					break;
				}

				case MRI_short: {
					input_data_s = (short *)DSET_ARRAY(input,j);
					out_temp[j] = scale[j] * (float)input_data_s[i];
					break;
				}

				case MRI_float: {
					input_data_f = (float *)DSET_ARRAY(input,j);
					out_temp[j] = scale[j] * input_data_f[i];
					break;
				}
				
				default : {
					return("FIlter_Driver Error:\nInvalid data type for one of the sub-bricks");
				}
			}
		}
					
		err = filter(out_temp, low_fc, high_fc, ntimes, period, ignore, retrend, FALSE);
		if (err != NULL)
			return err;
		
		for(j=0; j<ntimes; j++)
			out_data[j][i] = out_temp[j];
#ifdef IN_FOURIER_PLUGIN		
		PLUTO_set_meter(plint, (int)(100.0*((float)i/(float)nvox)));
#endif
			
	}
	
	/* create new dataset and convert, etc. */
	output = EDIT_empty_copy(input);
	
	j=EDIT_dset_items(output,
		ADN_prefix, output_prefix,
		ADN_none);
	
	for (j=0; j<ntimes; j++) {
		switch(DSET_BRICK_TYPE(input,j)) {
			case MRI_byte: {
				input_data_b = (byte *)My_Malloc(nvox*sizeof(byte));
				for (i=0; i<nvox; i++) 
					input_data_b[i] = (byte)(out_data[j][i] / scale[j]);
				EDIT_substitute_brick(output, j, MRI_byte, (byte *)input_data_b); 
				break;
			} 	
			case MRI_short: {
				input_data_s = (short *)My_Malloc(nvox*sizeof(short));
				for (i=0; i<nvox; i++) 
					input_data_s[i] = (short)(out_data[j][i] / scale[j]);
				EDIT_substitute_brick(output, j, MRI_short, (short *)input_data_s); 
				break;
			} 	
			case MRI_float: {
				input_data_f = (float *)My_Malloc(nvox*sizeof(float));
				for (i=0; i<nvox; i++) 
					input_data_f[i] = (float)(out_data[j][i] / scale[j]);
				EDIT_substitute_brick(output, j, MRI_float, (float *)input_data_f); 
				break;
			} 	
		}
#ifdef IN_FOURIER_PLUGIN		
		PLUTO_set_meter(plint, (int)(100.0*((float)j/(float)ntimes)));
#endif
}
	
	/* Write out the new brick at let the memory be free */
#ifdef IN_FOURIER_PLUGIN
	PLUTO_add_dset(plint, output, DSET_ACTION_MAKE_CURRENT);
#else
	DSET_write(output);
#endif
	DSET_unload(input);
	DSET_unload(output);
	for (i=0; i<ntimes; free(out_data[i++]));
	free (out_data);
	free (scale);
	free (out_temp);
#ifdef IN_FOURIER_PLUGIN
	PLUTO_popdown_meter(plint);		
#endif			

	return NULL;
}





