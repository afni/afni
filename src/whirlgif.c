  
/*
 * whirlgif.c
 *
 * Copyright (C) 1995,1996 by Kevin Kadow (kadokev@msg.net)
 * 
 * Based on txtmerge.c
 * Copyright (C) 1990,1991,1992,1993 by Mark Podlipec. 
 * All rights reserved.
 *
 * This software may be freely copied, modified and redistributed
 * without fee provided that this copyright notice is preserved 
 * intact on all copies and modified copies.
 * 
 * There is no warranty or other guarantee of fitness of this software.
 * It is provided solely "as is". The author(s) disclaim(s) all
 * responsibility and liability with respect to this software's usage
 * or its effect upon hardware or computer systems.
 *
 */
 /*
  * Description:
  *
  * This program reads in a sequence of single-image GIF format files and
  * outputs a single multi-image GIF file, suitable for use as an animation.
  *
  * TODO:
  *
  * More options for dealing with the colormap
  *
  * Eventually, I'd like to have this program compare the current image
  * with the previous image and check to see if only a small section of
  * the screen changed from the previous image. Worth a shot.
  */

 /*
  * Rev 2.00	05Feb96 Kevin Kadow
  *	transparency, gif comments,
  * Rev 1.10	29Jan96 Kevin Kadow
  *	first release of whirlgif
  *
  * txtmerge:
  * Rev 1.00	23Jul91	Mark Podlipec
  *	creation
  * Rev 1.01	08Jan92	Mark Podlipec
  *     use all colormaps, not just 1st.
  *
  * 
  */
#define DA_REV 1.00

#include "whirlgif.h"

#define MAXVAL  4100            /* maxval of lzw coding size */
#define MAXVALP 4200

/*
 * Set some defaults, these can be changed on the command line
 */
unsigned int loop=DEFAULT_LOOP,loopcount=0,
	use_colormap=DEFAULT_USE_COLORMAP,
	debug_flag=0,
	verbose=0;

int imagex = 0;
int imagey = 0;
int imagec = 0;

/* global settings for offset, transparency */
Global global;

GIF_Color gif_cmap[256];

ULONG GIF_Get_Code();
void GIF_Decompress();
void GIF_Get_Next_Entry();
void GIF_Add_To_Table();
void GIF_Send_Data();
void GIF_Clear_Table();
void GIF_Screen_Header();
void GIF_Image_Header();
void GIF_Read_File();
void GIF_Comment();
void GIF_Loop();
void GIF_GCL();
void Calc_Trans();
void set_offset();

GIF_Screen_Hdr gifscrn;
GIF_Image_Hdr gifimage;
GIF_Table table[MAXVALP];

ULONG root_code_size,code_size,CLEAR,EOI,INCSIZE;
ULONG nextab;
ULONG gif_mask[16] = {1,1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,0,0};
ULONG gif_ptwo[16] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,0,0};


UBYTE gif_buff[MAXVALP];
ULONG gif_block_size;
int num_bits,bits;

int pic_i;
char gif_file_name[BIGSTRING];
int screen_was_last;


void TheEnd()
{
 exit(0);
}

void TheEnd1(p)
char *p;
{
 fprintf(stderr,"%s",p);
 TheEnd();
}

Usage()
{
  fprintf(stderr,"\nUsage: whirlgif [-o outfile] [-loop [count]] [-time #delay]\n");
  fprintf(stderr,"\t[ -i listfile] file1 [ -time #delay] file2 ...\n");
  fprintf(stderr,"\nTry whirlgif -help for more information\n") ;
  exit(0);
}

main(argc,argv)
int argc;
char *argv[];
{
 FILE * infile, *fout;
 char temp[BIGSTRING];
 int ret,i;
 int count=0;

 fprintf(stderr,"whirlgif Rev %2.2f (C) 1996 by Kevin Kadow\n",DA_REV);
 fprintf(stderr,"                  (C) 1991,1992 by Mark Podlipec\n");
 
 if (argc < 2) Usage();
 
 /* set global values */
 screen_was_last = FALSE;
 global.trans.type=TRANS_NONE;
 global.trans.valid=FALSE;
 global.time=DEFAULT_TIME;
 global.left=0;
 global.top=0;


 fout=stdout;
 i = 1;
 while( i < argc)
 {
  char *p;
  p = argv[i];
  /*fprintf(stderr,"Option: %s\n",p);*/
  if ( (p[0] == '-') || (p[0] == '+') )
  { 
   ++p; /* strip off the - */
   switch(p[0])
   {
    case 'v':	/* Give lots of information */
	        verbose++;
	        i++;
		break;

    case 'd':	/* Debug mode */
		debug_flag++;
		i++;
		fprintf(stderr,"DEBUG: Debug Level %d\n",debug_flag);
		break;

    case 'l':	/* Enable looping */
		loop=TRUE;
		i++;
		if(*argv[i] !='-') {
		  /* a loop count was given */
		  loopcount=atoi(argv[i++]);
		  if(verbose) fprintf(stderr,"Loop %d times\n",loopcount);
		  }
		else {
		  /* default to infinite loop */
		  loopcount=0;
		  if(verbose) fputs("Looping enabled\n",stderr);
		  }
		break;

    case 'u':	/* Use colormap? true or false */
		i++;
		if(atoi(argv[i]) || !strcmp("true",argv[i])) use_colormap=1;
		else use_colormap=0;
		i++;
		break;

    case 't':	/* either time or transparent */
		i++;
		if(!strcmp("time",p)) {
			/* Delay time in 1/100's of a second */
			global.time=atoi(argv[i++]);
			}
		else if(!strncmp("trans",p,4)) Calc_Trans(argv[i++]);
		break;

    case 'o':	/* Output file - send output to a given filename */
		i++;
		if(!strncmp("off",p,3)) set_offset(argv[i]);
		else
		/* It must be 'output, so do that */
		if(NULL==(fout=fopen(argv[i],"w")))
			{
			fprintf(stderr,"Cannot open %s for output\n",argv[i]);
			exit(1);
			}
		i++;
		break;
    case 'i':	/* input file - file with a list of images */
		i++;
		if(NULL != (infile=fopen(argv[i],"r"))) {
			while(fgets(gif_file_name,BIGSTRING,infile)) {
				strtok(gif_file_name,"\n");
  				if(!count) GIF_Read_File(fout,gif_file_name,1);
  				else       GIF_Read_File(fout,gif_file_name,0);
  				count++;
				}
			fclose(infile);
			}
		else fprintf(stderr,"Cannot read list file %s\n",argv[i]);
		i++;
		break;

    case 'h':   /* big help, added by RWCox */
                big_Usage() ;
                exit(0) ;
                break ;

    default: 
		Usage();
		exit(0);
		break;
   }
   continue;
  }
  /* Not an option, must be the name of an input file */
  if(!count) GIF_Read_File(fout,argv[i],1);
  else       GIF_Read_File(fout,argv[i],0);
  count++;
  i++;
 }
 /* We're done with all the options, finish up */
 if(count >0)
  {
  fputc(';',fout); /* image separator */
  sprintf(temp,"whirlgif %2.2f (C) kadokev@msg.net. %d images",DA_REV,count);
  GIF_Comment(fout,temp);
  }

 fclose(fout);
 fprintf(stderr,"Processed %d files.\n",count);
}


/*
 * Read a GIF file, outputting to fname as we go.
 * It would be faster to read and write the individual blocks,
 * but eventually we'd like to optimize based on changes from
 * previous images(ie only a small section of the image changed.
 */
void
GIF_Read_File(fout,fname,first_image)
FILE * fout;
char *fname;
int first_image;
{
 FILE *fp;
 int ret,i,exit_flag;

 if ( (fp=fopen(fname,"r"))==0)
 { 
  fprintf(stderr,"Can't open %s for reading.\n",fname); 
  TheEnd();
 }

 GIF_Screen_Header(fp,fout,first_image);

 /*** read until  ,  separator */
 do
 {
  i=fgetc(fp);
  if ( (i<0) && feof(fp))
  {
   fclose(fp);
   TheEnd1("GIF_Read_Header: Unexpected End of File\n");
  }
 } while(i != ',');

 if(first_image)
  {
   /* stuff we only do once */
   if(loop) GIF_Loop(fout,loopcount);
   }
 if(global.time||(global.trans.type!=TRANS_NONE && global.trans.valid))
	GIF_GCL(fout,global.time);

 fputc(',',fout); /* image separator */

 GIF_Image_Header(fp,fout,first_image);

 /*FOO*/

 /*** Setup ACTION for IMAGE */

 GIF_Decompress(fp,fout,0);
 fputc(0,fout);  /* block count of zero */

 fclose(fp);
}

void GIF_Decompress(fp,fout)
FILE *fp,*fout;
{
 register ULONG code,old;

 pic_i = 0;
 bits=0;
 num_bits=0;
 gif_block_size=0;
    /* starting code size of LZW */
 root_code_size=(fgetc(fp) & 0xff); fputc(root_code_size,fout);
 GIF_Clear_Table();                /* clear decoding symbol table */

 code=GIF_Get_Code(fp,fout);

 if (code==CLEAR) 
 {
  GIF_Clear_Table(); 
  code=GIF_Get_Code(fp,fout);
 }
 /* write code(or what it currently stands for) to file */
 GIF_Send_Data(code);   
 old=code;
 code=GIF_Get_Code(fp,fout);
 do
 {
  if (table[code].valid==1)    /* if known code */
  {
       /* send it's associated string to file */
    GIF_Send_Data(code);
    GIF_Get_Next_Entry(fp);       /* get next table entry (nextab) */
    GIF_Add_To_Table(old,code,nextab);  /* add old+code to table */
    old=code;
  }
  else      /* code doesn't exist */
  {
    GIF_Add_To_Table(old,old,code);   /* add old+old to table */
    GIF_Send_Data(code);
    old=code;
  }
  code=GIF_Get_Code(fp,fout);
  if (code==CLEAR)
  { 
   GIF_Clear_Table();
   code=GIF_Get_Code(fp,fout);
   GIF_Send_Data(code);
   old=code;
   code=GIF_Get_Code(fp,fout);
  }
 } while(code!=EOI);
}

void GIF_Get_Next_Entry(fp)
FILE *fp;
{
   /* table walk to empty spot */
 while(  (table[nextab].valid==1)
       &&(nextab<MAXVAL)
      ) nextab++;
 /* 
  * Ran out of space??!?  Something's roached 
  */
 if (nextab>=MAXVAL)    
 { 
  fprintf(stderr,"Error: GetNext nextab=%ld\n",(long)nextab);
  fclose(fp);
  TheEnd();
 }
 if (nextab==INCSIZE)   /* go to next table size (and LZW code size ) */
 {
   /* fprintf(stderr,"GetNext INCSIZE was %ld ",nextab); */
   code_size++; INCSIZE=(INCSIZE*2)+1;
   if (code_size>=12) code_size=12;
/*   fprintf(stderr,"<%ld>",INCSIZE); */
 }

}
/*  body is associated string
    next is code to add to that string to form associated string for
    index
*/     

void GIF_Add_To_Table(body,next,index)
register ULONG body,next,index;
{
 if (index>MAXVAL)
 { 
  fprintf(stderr,"Error index=%ld\n",(long)index);
 }
 else
 {
  table[index].valid=1;
  table[index].data=table[next].first;
  table[index].first=table[body].first;
  table[index].last=body;
 }
}

void GIF_Send_Data(index)
register int index;
{
 register int i,j;
 i=0;
 do         /* table walk to retrieve string associated with index */
 { 
  gif_buff[i]=table[index].data; 
  i++;
  index=table[index].last;
  if (i>MAXVAL)
  { 
   fprintf(stderr,"Error: Sending i=%ld index=%ld\n",(long)i,(long)index);
   TheEnd();
  }
 } while(index>=0);

 /* now invert that string since we retreived it backwards */
 i--;
 for(j=i;j>=0;j--)
 {
  /*pic[pic_i] = gif_buff[j] | gif_pix_offset;*/
  pic_i++;
 }
}


/* 
 * initialize string table 
 */
void GIF_Init_Table()       
{
 register int maxi,i;

if (debug_flag) fprintf(stderr,"Initing Table...");
 maxi=gif_ptwo[root_code_size];
 for(i=0; i<maxi; i++)
 {
  table[i].data=i;   
  table[i].first=i;
  table[i].valid=1;  
  table[i].last = -1;
 }
 CLEAR=maxi; 
 EOI=maxi+1; 
 nextab=maxi+2;
 INCSIZE = (2*maxi)-1;
 code_size=root_code_size+1;
}


/* 
 * clear table 
 */
void GIF_Clear_Table()   
{
 register int i;
if (debug_flag) fprintf(stderr,"Clearing Table...\n");
 for(i=0;i<MAXVAL;i++) table[i].valid=0;
 GIF_Init_Table();
}

/*CODE*/
ULONG GIF_Get_Code(fp,fout) /* get code depending of current LZW code size */
FILE *fp,*fout;
{
 ULONG code;
 register int tmp;

 while(num_bits < code_size)
 {
  /**** if at end of a block, start new block */
  if (gif_block_size==0) 
  {
   tmp = fgetc(fp);
   if (tmp >= 0 )
   {
    fputc(tmp,fout);
    gif_block_size=(ULONG)(tmp);
   }
   else TheEnd1("EOF in data stream\n");
  }

  tmp = fgetc(fp);   gif_block_size--;
  if (tmp >= 0)
  {
   fputc(tmp,fout);
   bits |= ( ((ULONG)(tmp) & 0xff) << num_bits );
   num_bits+=8;
  }
  else TheEnd1("EOF in data stream\n");
 }
  
 code = bits & gif_mask[code_size];
 bits >>= code_size;
 num_bits -= code_size; 


 if (code>MAXVAL)
 { 
  fprintf(stderr,"\nError! in stream=%lx \n",(unsigned long)code); 
  fprintf(stderr,"CLEAR=%lx INCSIZE=%lx EOI=%lx code_size=%lx \n",
          (unsigned long)CLEAR,(unsigned long)INCSIZE,(unsigned long)EOI,(unsigned long)code_size); 
  code=EOI;
 }

 if (code==INCSIZE)
 {
  if (code_size<12)
  {
   code_size++; INCSIZE=(INCSIZE*2)+1;
  }
  else if (debug_flag) fprintf(stderr,"<13?>"); 
 }

 return(code);
}


/* 
 * read GIF header 
 */
void GIF_Screen_Header(fp,fout,first_time)
FILE *fp,*fout;
int first_time;
{
 int temp,i;

 for(i=0;i<6;i++)
 {
  temp = fgetc(fp);
  if(i==4 && temp == '7') temp='9';
  if (first_time==TRUE) fputc(temp,fout);
 }

 gifscrn.width  = GIF_Get_Short(fp,fout,first_time);
 gifscrn.height = GIF_Get_Short(fp,fout,first_time);
 temp=fgetc(fp);                 if (first_time==TRUE) fputc(temp,fout);
 gifscrn.m       =  temp & 0x80;
 gifscrn.cres    = (temp & 0x70) >> 4;
 gifscrn.pixbits =  temp & 0x07;

 gifscrn.bc  = fgetc(fp);
 if (first_time==TRUE) 
	{
	/* we really should set the background color to the transparent color */
	fputc(gifscrn.bc,fout);
	}

 temp=fgetc(fp);                 if (first_time==TRUE) fputc(temp,fout);
 imagec=gif_ptwo[(1+gifscrn.pixbits)];

 if (verbose)
  fprintf(stderr,"Screen: %ldx%ldx%ld m=%ld cres=%ld bkgnd=%ld pix=%ld\n",
    (long)gifscrn.width,(long)gifscrn.height,(long)imagec,(long)gifscrn.m,(long)gifscrn.cres,
    (long)gifscrn.bc,(long)gifscrn.pixbits);

 if(global.trans.type==TRANS_RGB) global.trans.valid=0;
 if (gifscrn.m)
 {
  for(i=0;i<imagec;i++)
  {
   gif_cmap[i].cmap.red   = temp = fgetc(fp); 
           if (first_time==TRUE) fputc(temp,fout);
   gif_cmap[i].cmap.green = temp = fgetc(fp); 
           if (first_time==TRUE) fputc(temp,fout);
   gif_cmap[i].cmap.blue  = temp = fgetc(fp); 
           if (first_time==TRUE) fputc(temp,fout);

   if(global.trans.type==TRANS_RGB && !global.trans.valid)
	if(global.trans.red==gif_cmap[i].cmap.red &&
	global.trans.green==gif_cmap[i].cmap.green &&
	global.trans.blue==gif_cmap[i].cmap.blue) {
	  if(debug_flag>1) fprintf(stderr," Transparent match at %d\n",i);
	    global.trans.map=i;
	global.trans.valid=TRUE;
	}
  }
 }
 screen_was_last = TRUE;
}

void GIF_Image_Header(fp,fout,first_time)
FILE *fp,*fout;
int first_time;
{
 int temp,tnum,i,r,g,b;

 gifimage.left   = GIF_Get_Short(fp,fout,1);
 if(global.left) gifimage.left+=global.left;

 gifimage.top    = GIF_Get_Short(fp,fout,1);
 if(global.top) gifimage.top+=global.top;

 gifimage.width  = GIF_Get_Short(fp,fout,1);
 gifimage.height = GIF_Get_Short(fp,fout,1);
 temp=fgetc(fp); 


	
 gifimage.i        = temp & 0x40;
 gifimage.pixbits  = temp & 0x07;
 gifimage.m        = temp & 0x80;

 /* this sets the local colormap bit to true */
 if (screen_was_last && (first_time==FALSE)) temp |= 0x80;

 temp &= 0xf8;
 temp |= gifscrn.pixbits;
 fputc(temp,fout);

 imagex=gifimage.width;
 imagey=gifimage.height;
 tnum=gif_ptwo[(1+gifimage.pixbits)];
 if (verbose)
  fprintf(stderr,"Image: %ldx%ldx%ld (%ld,%ld) m=%ld i=%ld pix=%ld \n",
    (long)imagex,(long)imagey,(long)tnum,(long)gifimage.left,(long)gifimage.top,
	(long)gifimage.m,(long)gifimage.i,(long)gifimage.pixbits);

 /* if there is an image cmap, then use it */

 if (gifimage.m)
 {
/*  if(debug_flag) fprintf(stderr,"DEBUG:Transferring colormap of %d colors\n"); */
  for(i=0;i<tnum;i++)
  {
   gif_cmap[i].cmap.red   = r = fgetc(fp);
   gif_cmap[i].cmap.green = g = fgetc(fp);
   gif_cmap[i].cmap.blue  = b = fgetc(fp);
   fputc(r,fout);
   fputc(g,fout);
   fputc(b,fout);
  }
 }  /* else if screen was last not 1st time */
 else if (screen_was_last && (first_time==FALSE))
 {
/*  if(debug_flag>1) fprintf(stderr,"DEBUG:Writing colormap of %d colors\n"); */
  for(i=0;i<imagec;i++)
  {
   fputc(gif_cmap[i].cmap.red  ,fout);
   fputc(gif_cmap[i].cmap.green,fout);
   fputc(gif_cmap[i].cmap.blue ,fout);
  }
 }
 screen_was_last = FALSE; 
}


/*
 *
 */
int GIF_Get_Short(fp,fout,first_time)
FILE *fp,*fout;
int first_time;
{
 register int temp,tmp1;
 temp=fgetc(fp);	 if (first_time==TRUE) fputc(temp,fout);
 tmp1=fgetc(fp);	 if (first_time==TRUE) fputc(tmp1,fout);
 return(temp|( (tmp1) << 8 ));
}

void GIF_Comment(fout,string)
FILE *fout;
char *string;
{
if(!string || !strlen(string))
        {
        /* Bogus string */
        if(debug_flag) fprintf(stderr,"GIF_Comment: invalid argument");
        return;
        }
fputc(0x21,fout);
fputc(0xF9,fout);
fputs(string,fout);
fputc(0,fout);
}

/*
 * Write a Netscape loop marker.
 */
void GIF_Loop(fout,repeats)
FILE *fout;
unsigned int repeats;
{
UBYTE low=0,high=0;
if(repeats) {
	/* non-zero repeat count- Netscape hasn't implemented this yet */
	high=repeats / 256;
	low=repeats % 256;
	}

fputc(0x21,fout);
fputc(0xFF,fout);
fputc(0x0B,fout);
fputs("NETSCAPE2.0",fout);
fputc(0x03,fout);
fputc(0x01,fout);
fputc(0x00,fout);


fputc(low,fout); /* the delay count - 0 for infinite */
fputc(high,fout); /* second byte of delay count */

if(verbose) fprintf(stderr,"Wrote loop extension\n");
}

/*
 * GIF_GCL - add a Control Label to set an inter-frame delay value.
 */
void GIF_GCL(fout,delay)
FILE * fout;
unsigned int delay;
{
UBYTE low=0,high=0,flag=0;
if(delay) {
	/* non-zero delay, figure out low/high bytes */
	high=delay / 256;
	low=delay % 256;
	}

fputc(0x21,fout);
fputc(0xF9,fout);
fputc(0x04,fout);

if(delay) flag |=0x80;
if(global.trans.valid) flag |=0x01;
fputc(flag,fout);

fputc(low,fout); /* the delay speed - 0 is instantaneous */
fputc(high,fout); /* second byte of delay count */

fputc(global.trans.map,fout);
fputc(0,fout);
if(debug_flag>1) {
  fprintf(stderr,"GCL: delay %d",delay);
  if(global.trans.valid) fprintf(stderr," Transparent: %d",global.trans.map);
  fputc('\n',stderr);
  }
}


void Calc_Trans(string)
char * string;
{
if(string[0] != '#') {
  global.trans.type=TRANS_MAP;
  global.trans.map=atoi(string);
  global.trans.valid=1;
  }
else
  {
  /* it's an RGB value */
  int r,g,b;
  string++;
  if(3==sscanf(string,"%2x%2x%2x",&r,&g,&b)) {
	global.trans.red=r;
	global.trans.green=g;
	global.trans.blue=b;
	global.trans.type=TRANS_RGB;
	if(debug_flag) fprintf(stderr,"Transparent RGB=(%x,%x,%x)\n",r,g,b);
	}
  }
if(debug_flag) fprintf(stderr,"DEBUG:Calc_trans is %d\n",global.trans.type);
}

void set_offset(string)
char * string;
{
char *off_x,*off_y;
off_x=(char *) strtok(string,",");
off_y=(char *)strtok((char *)NULL,",");
if(off_x && off_y) {
	/* set the offset */
	global.left=atoi(off_x);
	global.top=atoi(off_y);
	if(debug_flag>1) fprintf(stderr,"Offset changed to %d,%d\n",
		global.left,global.top);
	return;
	}
if(debug_flag>1) fprintf(stderr,"Couldn't parse offset values.\n");
exit(0);
}

big_Usage()
{
   printf("\n"
     "whirlgif is a quick program that reads a series of GIF files, and produces\n"
     "a single gif file composed of those images.\n"
     "\n"
     "Usage: whirlgif [-v] [-trans index ] [-time delay] [-o outfile]\n"
     "                [-loop] [-i incfile] file1 [ -time delay] file2\n"
     "\n"
     "options:\n"
     "   -v              verbose mode\n"
     "   -loop [count]   add the Netscape 'loop' extension.\n"
     "   -time delay     inter-frame timing.\n"
     "   -trans index    set the colormap index 'index' to be transparent\n"
     "   -o outfile      write the results to 'outfile'\n"
     "   -i incfile      read a list of names from 'incfile'\n"
     "\n"
     "TIPS\n"
     "\n"
     "If you don't specify an output file, the GIF will be sent to stdout. This is\n"
     "a good thing if you're using this in a CGI script, a very bad thing if you\n"
     "run this from a terminal and forget to redirect stdout.\n"
     "\n"
     "The output file (if any) and -loop _MUST_ be specified before any gif images.\n"
     "\n"
     "You can specify several delay statements on the command line to change\n"
     "the delay between images in the middle of an animation, e.g.\n"
     "\n"
     "      whirlgif -time 5 a.gif b.gif c.gif -time 100 d.gif -time 5 e.gif f.gif\n"
     "\n"
     "Although it's generally considered to be evil, you can also specify\n"
     "several transparency statements on the command line, to change the transparent\n"
     "color in the middle of an animation. This may cause problems for some programs.\n"
     "\n"
     "\n"
     "BUGS\n"
     "  + The loop 'count' is ineffective because Netspcape always loops infinitely.\n"
     "  + Should be able to specify delay in an 'incfile' list (see next bug).\n"
     "  + Does not handle filenames starting with a - (hypen), except in 'incfile'.\n"
     "\n"
     "This program is available from http://www.msg.net/utility/whirlgif/\n"
     "-------------------------------------------------------------------\n"
     "Kevin Kadow     kadokev@msg.net\n"
     "Based on 'txtmerge' written by:\n"
     "Mark Podlipec   podlipec@wellfleet.com\n"
   ) ;
   exit(0) ;
}
