/* gifsicle.c - gifsicle's main loop.
   Copyright (C) 1997-2001 Eddie Kohler, eddietwo@lcs.mit.edu
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2 or later; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include "config.h"
#include "gifsicle.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>

/* Need _setmode under MS-DOS, to set stdin/stdout to binary mode */
/* Need _fsetmode under OS/2 for the same reason */
#if defined(_MSDOS) || defined(_WIN32) || defined(__EMX__)
# include <fcntl.h>
# include <io.h>
#endif


Gt_Frame def_frame;

Gt_Frameset *frames = 0;
int first_input_frame = 0;
Gt_Frameset *nested_frames = 0;

Gif_Stream *input = 0;
char *input_name = 0;
static int unoptimizing = 0;

int gif_read_flags = 0;
int gif_write_flags = 0;

static int frames_done = 0;
static int files_given = 0;

int warn_local_colormaps = 1;

static Gt_ColorTransform *input_transforms;
static Gt_ColorTransform *output_transforms;

#define BLANK_MODE	0
#define MERGING		1
#define BATCHING	2
#define EXPLODING	3
#define DELETING	4
#define INSERTING	5
static int mode = BLANK_MODE;
static int nested_mode = 0;

static int infoing = 0;
int verbosing = 0;


#define CHANGED(next, flag)	(((next) & 1<<(flag)) != 0)
#define UNCHECKED_MARK_CH(where, what)			\
  next_##where |= 1<<what;
#define MARK_CH(where, what)				\
  if (CHANGED(next_##where, what))			\
    redundant_option_warning(where##_option_types[what]); \
  UNCHECKED_MARK_CH(where, what)

/* frame option types */
static int next_frame = 0;
#define CH_INTERLACE		0
#define CH_DISPOSAL		1
#define CH_DELAY		2
#define CH_TRANSPARENT		3
#define CH_COMMENT		4
#define CH_NAME			5
#define CH_POSITION		6
#define CH_CROP			7
#define CH_EXTENSION		8
#define CH_FLIP			9
#define CH_ROTATE		10
static const char *frame_option_types[] = {
  "interlace", "disposal", "delay", "transparency",
  "comment", "name", "position", "crop",
  "extension", "flip", "rotation"
};

/* input option types */
static int next_input = 0;
#define CH_UNOPTIMIZE		0
#define CH_CHANGE_COLOR		1
static const char *input_option_types[] = {
  "unoptimization", "color change"
};

/* output option types */
static Gt_OutputData def_output_data;
static Gt_OutputData active_output_data;
static int next_output = 0;
static int active_next_output = 0;
static int any_output_successful = 0;
#define CH_LOOPCOUNT		0
#define CH_LOGICAL_SCREEN	1
#define CH_OPTIMIZE		2
#define CH_OUTPUT		3
#define CH_COLORMAP		4
#define CH_DITHER		5
#define CH_USE_COLORMAP		6
#define CH_COLORMAP_METHOD	7
#define CH_BACKGROUND		8
#define CH_COLOR_TRANSFORM	9
#define CH_RESIZE		10
static const char *output_option_types[] = {
  "loopcount", "logical screen", "optimization", "output file",
  "colormap size", "dither", "colormap", "colormap method",
  "background", "color transformation", "resize"
};


#define SAME_INTERLACE_OPT	300
#define INFO_OPT		301
#define DISPOSAL_OPT		302
#define SAME_LOOPCOUNT_OPT	303
#define SAME_DISPOSAL_OPT	304
#define SAME_DELAY_OPT		305
#define SAME_TRANSPARENT_OPT	308
#define LOGICAL_SCREEN_OPT	309
#define COMMENT_OPT		310
#define UNOPTIMIZE_OPT		311
#define CAREFUL_OPT		312
#define OPTIMIZE_OPT		313
#define SAME_LOGICAL_SCREEN_OPT 314
#define DELETE_OPT		315
#define REPLACE_OPT		316
#define INSERT_OPT		317
#define ALTER_DONE_OPT		318
#define APPEND_OPT		319
#define COLOR_INFO_OPT		320
#define VERBOSE_OPT		321
#define NO_COMMENTS_OPT		322
#define SAME_COMMENTS_OPT	323
#define NAME_OPT		324
#define SAME_NAME_OPT		325
#define NO_NAME_OPT		326
#define POSITION_OPT		327
#define SAME_POSITION_OPT	328
#define VERSION_OPT		329
#define HELP_OPT		330
#define OUTPUT_OPT		331
#define CROP_OPT		332
#define SAME_CROP_OPT		333
#define CHANGE_COLOR_OPT	334
#define COLORMAP_OPT		335
#define COLORMAP_ALGORITHM_OPT	336
#define DITHER_OPT		337
#define USE_COLORMAP_OPT	338
#define NO_EXTENSIONS_OPT	339
#define SAME_EXTENSIONS_OPT	340
#define EXTENSION_INFO_OPT	341
#define BACKGROUND_OPT		342
#define SAME_BACKGROUND_OPT	343
#define FLIP_HORIZ_OPT		344
#define FLIP_VERT_OPT		345
#define NO_FLIP_OPT		346
#define ROTATE_90_OPT		347
#define ROTATE_180_OPT		348
#define ROTATE_270_OPT		349
#define NO_ROTATE_OPT		350
#define APP_EXTENSION_OPT	351
#define EXTENSION_OPT		352
#define COLOR_TRANSFORM_OPT	353
#define RESIZE_OPT		354
#define SCALE_OPT		355
#define NO_WARNINGS_OPT		356
#define WARNINGS_OPT		357
#define RESIZE_WIDTH_OPT	358
#define RESIZE_HEIGHT_OPT	359

#define LOOP_TYPE		(Clp_MaxDefaultType + 1)
#define DISPOSAL_TYPE		(Clp_MaxDefaultType + 2)
#define DIMENSIONS_TYPE		(Clp_MaxDefaultType + 3)
#define FRAME_SPEC_TYPE		(Clp_MaxDefaultType + 4)
#define COLOR_TYPE		(Clp_MaxDefaultType + 5)
#define POSITION_TYPE		(Clp_MaxDefaultType + 6)
#define RECTANGLE_TYPE		(Clp_MaxDefaultType + 7)
#define TWO_COLORS_TYPE		(Clp_MaxDefaultType + 8)
#define COLORMAP_ALG_TYPE	(Clp_MaxDefaultType + 9)
#define SCALE_FACTOR_TYPE	(Clp_MaxDefaultType + 10)

Clp_Option options[] = {
  
  { "append", 0, APPEND_OPT, 0, 0 },
  { "app-extension", 'x', APP_EXTENSION_OPT, Clp_ArgString, 0 },
  
  { "background", 'B', BACKGROUND_OPT, COLOR_TYPE, Clp_Negate },
  { "batch", 'b', 'b', 0, 0 },
  { "bg", 0, BACKGROUND_OPT, COLOR_TYPE, Clp_Negate },

  { "careful", 0, CAREFUL_OPT, 0, Clp_Negate },
  { "change-color", 0, CHANGE_COLOR_OPT, TWO_COLORS_TYPE, Clp_Negate },
  { "cinfo", 0, COLOR_INFO_OPT, 0, Clp_Negate },
  { "clip", 0, CROP_OPT, RECTANGLE_TYPE, Clp_Negate },
  { "colors", 'k', COLORMAP_OPT, Clp_ArgInt, Clp_Negate },
  { "color-method", 0, COLORMAP_ALGORITHM_OPT, COLORMAP_ALG_TYPE, 0 },
  { "color-info", 0, COLOR_INFO_OPT, 0, Clp_Negate },
  { "comment", 'c', COMMENT_OPT, Clp_ArgString, 0 },
  { "no-comments", 'c', NO_COMMENTS_OPT, 0, Clp_OnlyNegated },
  { "crop", 0, CROP_OPT, RECTANGLE_TYPE, Clp_Negate },
  
  { "delay", 'd', 'd', Clp_ArgInt, Clp_Negate },
  { "delete", 0, DELETE_OPT, 0, 0 },
  { "disposal", 'D', DISPOSAL_OPT, DISPOSAL_TYPE, Clp_Negate },
  { "dither", 'f', DITHER_OPT, 0, Clp_Negate },
  { "done", 0, ALTER_DONE_OPT, 0, 0 },
  
  { "explode", 'e', 'e', 0, 0 },
  { "explode-by-name", 'E', 'E', 0, 0 },
  { "extension", 0, EXTENSION_OPT, Clp_ArgString, 0 },
  { "no-extensions", 'x', NO_EXTENSIONS_OPT, 0, 0 },
  { "extension-info", 0, EXTENSION_INFO_OPT, 0, Clp_Negate },
  
  { "flip-horizontal", 0, FLIP_HORIZ_OPT, 0, Clp_Negate },
  { "flip-vertical", 0, FLIP_VERT_OPT, 0, Clp_Negate },
  { "no-flip", 0, NO_FLIP_OPT, 0, 0 },
  
  { "help", 'h', HELP_OPT, 0, 0 },
  
  { "info", 'I', INFO_OPT, 0, Clp_Negate },  
  { "insert-before", 0, INSERT_OPT, FRAME_SPEC_TYPE, 0 },
  { "interlace", 'i', 'i', 0, Clp_Negate },
  
  { "logical-screen", 'S', LOGICAL_SCREEN_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "loopcount", 'l', 'l', LOOP_TYPE, Clp_Optional | Clp_Negate },
  
  { "merge", 'm', 'm', 0, 0 },
  { "method", 0, COLORMAP_ALGORITHM_OPT, COLORMAP_ALG_TYPE, 0 },
  
  { "name", 'n', NAME_OPT, Clp_ArgString, 0 },
  { "no-names", 'n', NO_NAME_OPT, 0, Clp_OnlyNegated },
  
  { "optimize", 'O', OPTIMIZE_OPT, Clp_ArgInt, Clp_Negate | Clp_Optional },
  { "output", 'o', OUTPUT_OPT, Clp_ArgStringNotOption, 0 },
  
  { "position", 'p', POSITION_OPT, POSITION_TYPE, Clp_Negate },
  
  { "replace", 0, REPLACE_OPT, FRAME_SPEC_TYPE, 0 },
  { "resize", 0, RESIZE_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "resize-width", 0, RESIZE_WIDTH_OPT, Clp_ArgUnsigned, Clp_Negate },
  { "resize-height", 0, RESIZE_HEIGHT_OPT, Clp_ArgUnsigned, Clp_Negate },
  { "resiz", 0, RESIZE_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "resi", 0, RESIZE_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "res", 0, RESIZE_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "rotate-90", 0, ROTATE_90_OPT, 0, 0 },
  { "rotate-180", 0, ROTATE_180_OPT, 0, 0 },
  { "rotate-270", 0, ROTATE_270_OPT, 0, 0 },
  { "no-rotate", 0, NO_ROTATE_OPT, 0, 0 },

  { "scale", 0, SCALE_OPT, SCALE_FACTOR_TYPE, Clp_Negate },
  { "screen", 0, LOGICAL_SCREEN_OPT, DIMENSIONS_TYPE, Clp_Negate },
  { "same-background", 0, SAME_BACKGROUND_OPT, 0, 0 },
  { "same-bg", 0, SAME_BACKGROUND_OPT, 0, 0 },
  { "same-clip", 0, SAME_CROP_OPT, 0, 0 },
  { "same-comments", 0, SAME_COMMENTS_OPT, 0, 0 },
  { "same-crop", 0, SAME_CROP_OPT, 0, 0 },
  { "same-extensions", 0, SAME_EXTENSIONS_OPT, 0, 0 },
  { "same-interlace", 0, SAME_INTERLACE_OPT, 0, 0 },
  { "same-logical-screen", 0, SAME_LOGICAL_SCREEN_OPT, 0, 0 },
  { "same-loopcount", 0, SAME_LOOPCOUNT_OPT, 0, 0 },
  { "same-disposal", 0, SAME_DISPOSAL_OPT, 0, 0 },
  { "same-delay", 0, SAME_DELAY_OPT, 0, 0 },
  { "same-names", 0, SAME_NAME_OPT, 0, 0 },
  { "same-position", 0, SAME_POSITION_OPT, 0, 0 },
  { "same-screen", 0, SAME_LOGICAL_SCREEN_OPT, 0, 0 },
  { "same-transparent", 0, SAME_TRANSPARENT_OPT, 0, 0 },
  
  { "transform-colormap", 0, COLOR_TRANSFORM_OPT, Clp_ArgStringNotOption,
    Clp_Negate },
  { "transparent", 't', 't', COLOR_TYPE, Clp_Negate },
  
  { "unoptimize", 'U', UNOPTIMIZE_OPT, 0, Clp_Negate },
  { "use-colormap", 0, USE_COLORMAP_OPT, Clp_ArgString, Clp_Negate },
  
  { "verbose", 'v', VERBOSE_OPT, 0, Clp_Negate },
  { "version", 0, VERSION_OPT, 0, 0 },
  
  { 0, 'w', NO_WARNINGS_OPT, 0, Clp_Negate },
  { "warnings", 0, WARNINGS_OPT, 0, Clp_Negate },
  
  { "xinfo", 0, EXTENSION_INFO_OPT, 0, Clp_Negate },
  
};


static void combine_output_options(void);
static void initialize_def_frame(void);
static void redundant_option_warning(const char *);


static void
set_mode(int newmode)
{
  if (mode == BLANK_MODE)
    mode = newmode;
  else if (mode == newmode)
    ;
  else
    fatal_error("too late to change modes");
}


void
set_frame_change(int kind)
{
  int i;
  Gt_Frameset *fset;
  
  if (mode == BLANK_MODE)
    set_mode(MERGING);
  if (mode < DELETING && frames_done) {
    fatal_error("frame selection and frame changes don't mix");
    return;
  }
  assert(!nested_mode);
  nested_mode = mode;
  
  switch (kind) {
    
   case DELETE_OPT:
    mode = DELETING;
    break;
    
   case REPLACE_OPT:
    for (i = frame_spec_1; i < frame_spec_2; i++)
      FRAME(frames, i).use = 0;
    /* We want to use the last frame's delay, but nothing else about it. */
    FRAME(frames, i).use = -1;
    /* FALLTHRU */
    
   case INSERT_OPT:
    /* Define a nested frameset (or use an existing one). */
    fset = FRAME(frames, frame_spec_2).nest;
    if (!fset) fset = new_frameset(8);
    FRAME(frames, frame_spec_2).nest = fset;
    
    /* Later: Merge frames at the end of the nested frameset. */
    mode = INSERTING;
    nested_frames = frames;
    frames = fset;
    break;
    
   case APPEND_OPT:
    /* Just merge frames at the end of this frameset. */
    mode = INSERTING;
    break;
    
  }
}

void
frame_change_done(void)
{
  if (nested_mode)
    mode = nested_mode;
  if (nested_frames)
    frames = nested_frames;
  nested_mode = 0;
  nested_frames = 0;
}


void
show_frame(int imagenumber, int usename)
{
  Gif_Image *gfi;
  Gt_Frame *frame;
  
  if (!input) return;
  gfi = Gif_GetImage(input, imagenumber);
  if (!gfi) return;
  
  switch (mode) {
    
   case MERGING:
   case INSERTING:
   case EXPLODING:
    if (!frames_done) clear_frameset(frames, first_input_frame);
    frame = add_frame(frames, -1, input, gfi);
    if (usename) frame->explode_by_name = 1;
    break;
    
   case BATCHING:
    add_frame(frames, first_input_frame + imagenumber, input, gfi);
    break;
    
   case DELETING:
    frame = &FRAME(frames, first_input_frame + imagenumber);
    frame->use = 0;
    break;
    
  }
  
  next_frame = 0;
  frames_done = 1;
}


/*****
 * input a stream
 **/

static int gifread_error_count;

static void
gifread_error(const char *message, int which_image, void *thunk)
{
  static int last_which_image = 0;
  static char last_message[256];
  static int different_error_count = 0;
  static int same_error_count = 0;
  const char *filename = (const char *)thunk;
  
  if (gifread_error_count == 0) {
    last_which_image = -1;
    last_message[0] = 0;
    different_error_count = 0;
  }
  
  gifread_error_count++;
  if (last_message[0] && different_error_count <= 10
      && (last_which_image != which_image || message == 0
	  || strcmp(message, last_message) != 0)) {
    if (same_error_count == 1)
      error("  %s", last_message);
    else if (same_error_count > 0)
      error("  %s (%d times)", last_message, same_error_count);
    same_error_count = 0;
    last_message[0] = 0;
  }

  if (last_message[0] == 0)
    different_error_count++;
  
  same_error_count++;
  if (message)
    strcpy(last_message, message);
  else
    last_message[0] = 0;
  if (last_which_image != which_image && different_error_count <= 10
      && message) {
    error("Error while reading `%s' frame #%d:", filename, which_image);
    last_which_image = which_image;
  }
  
  if (different_error_count == 11 && message) {
    error("(more errors while reading `%s')", filename);
    different_error_count++;
  }
}

void
input_stream(char *name)
{
  FILE *f;
  Gif_Stream *gfs;
  int i;
  int saved_next_frame = next_frame;
  Gt_Frame old_def_frame;
  
  input = 0;
  input_name = name;
  frames_done = 0;
  next_frame = 0;
  next_input = 0;
  if (next_output) combine_output_options();
  files_given++;
  
  if (name == 0 || strcmp(name, "-") == 0) {
#if defined(_MSDOS) || defined(_WIN32)
    _setmode(_fileno(stdin), _O_BINARY);
#elif defined(__EMX__)
    _fsetmode(stdin, "b");
#endif
    f = stdin;
    name = "<stdin>";
  } else
    f = fopen(name, "rb");
  if (!f) {
    error("%s: %s", name, strerror(errno));
    return;
  }
  
  /* special error message for empty files */
  i = getc(f);
  if (i == EOF) {
    error("%s: empty file", name);
    return;
  }
  ungetc(i, f);
  
  if (verbosing) verbose_open('<', name);
  gifread_error_count = 0;
  gfs = Gif_FullReadFile(f, gif_read_flags | GIF_READ_COMPRESSED,
			 gifread_error, (void *)name);
  fclose(f);
  gifread_error(0, -1, (void *)name); /* print out last error message */
  
  if (!gfs || (Gif_ImageCount(gfs) == 0 && gfs->errors > 0)) {
    error("%s: not a GIF", name);
    Gif_DeleteStream(gfs);
    if (verbosing) verbose_close('>');
    return;
  }
  
  input = gfs;
  
  /* Processing when we've got a new input frame */
  if (mode == BLANK_MODE)
    set_mode(MERGING);
  
  if (active_output_data.output_name == 0) {
    /* Don't override explicit output names.
       This code works 'cause output_name is reset to 0 after each output. */
    if (mode == BATCHING)
      active_output_data.output_name = input_name;
    else if (mode == EXPLODING) {
      /* Explode into current directory. */
      char *explode_name = (input_name ? input_name : "#stdin#");
      char *slash = strrchr(explode_name, PATHNAME_SEPARATOR);
      if (slash)
	active_output_data.output_name = slash + 1;
      else
	active_output_data.output_name = explode_name;
    }
  }
  
  /* This code rather sucks. Here's the problem: Since we consider options
     strictly sequentially, one at a time, we can't tell the difference
     between these:
     
     --name=X g.gif             h.gif   // name on g.gif #0
     --name=X g.gif          #2 h.gif   // name on g.gif #2
              g.gif --name=X #2 h.gif   // name on g.gif #2
              g.gif --name=X    h.gif   // name on h.gif #0 !!!
      
     Here's the solution. Mark when we CHANGE an option. After processing
     an input GIF, mark all the options as `unchanged' -- but leave the
     VALUES as is. Then when we read the next frame, CLEAR the unchanged
     options. So it's like so: (* means changed, . means not.)
     
     [-.] --name=X [X*] g.gif [X.] #2 [-.] h.gif   == name on g.gif #2
     [-.] g.gif [-.] --name=X [X*] #2 [-.] h.gif  == name on g.gif #2
     [-.] --name=X [X*] g.gif [X.|-.] h.gif  == name on g.gif #0
     [-.] g.gif [-.] --name=X [X*] h.gif  == name on h.gif #0 */
  
  /* Clear old options from the last input stream */
  if (!CHANGED(saved_next_frame, CH_NAME))
    def_frame.name = 0;
  if (!CHANGED(saved_next_frame, CH_COMMENT))
    def_frame.comment = 0;
  if (!CHANGED(saved_next_frame, CH_EXTENSION))
    def_frame.extensions = 0;
  def_frame.input_filename = input_name;
  
  old_def_frame = def_frame;
  first_input_frame = frames->count;
  if (gfs->nimages > 1)
    def_frame.position_is_offset = 1;
  for (i = 0; i < gfs->nimages; i++)
    add_frame(frames, -1, gfs, gfs->images[i]);
  def_frame = old_def_frame;
  
  if (unoptimizing)
    if (!Gif_Unoptimize(gfs)) {
      static int context = 0;
      warning("`%s' is too complex to unoptimize", name);
      if (!context) {
	warncontext("(The reason was local color tables or complex transparency.");
	warncontext("Try running the GIF through `gifsicle --colors=255' first.)");
      }
      context = 1;
    }
  
  apply_color_transforms(input_transforms, gfs);
  gfs->refcount++;
}

void
input_done(void)
{
  if (!input) return;
  
  if (verbosing) verbose_close('>');
  /*if (infoing) {
    int i;
    if (input->userflags == 97)
      stream_info(infoing, input, input_name,
		  colormap_infoing, extension_infoing);
    for (i = first_input_frame; i < frames->count; i++)
      if (FRAME(frames, i).stream == input && FRAME(frames, i).use)
	image_info(infoing, input, FRAME(frames, i).image, colormap_infoing);
  }*/
  
  Gif_DeleteStream(input);
  input = 0;
  
  if (mode == DELETING)
    frame_change_done();
  if (mode == BATCHING || mode == EXPLODING)
    output_frames();
}


/*****
 * colormap stuff
 **/

static void
set_new_fixed_colormap(char *name)
{
  int i;
  if (name && strcmp(name, "web") == 0) {
    Gif_Colormap *cm = Gif_NewFullColormap(216, 256);
    Gif_Color *col = cm->col;
    for (i = 0; i < 216; i++) {
      col[i].red = (i / 36) * 0x33;
      col[i].green = ((i / 6) % 6) * 0x33;
      col[i].blue = (i % 6) * 0x33;
    }
    def_output_data.colormap_fixed = cm;
    
  } else if (name && (strcmp(name, "gray") == 0
		      || strcmp(name, "grey") == 0)) {
    Gif_Colormap *cm = Gif_NewFullColormap(256, 256);
    Gif_Color *col = cm->col;
    for (i = 0; i < 256; i++)
      col[i].red = col[i].green = col[i].blue = i;
    def_output_data.colormap_fixed = cm;
    
  } else if (name && strcmp(name, "bw") == 0) {
    Gif_Colormap *cm = Gif_NewFullColormap(2, 256);
    cm->col[0].red = cm->col[0].green = cm->col[0].blue = 0;
    cm->col[1].red = cm->col[1].green = cm->col[1].blue = 255;
    def_output_data.colormap_fixed = cm;
    
  } else
    def_output_data.colormap_fixed = read_colormap_file(name, 0);
}

static void
do_set_colormap(Gif_Stream *gfs, Gif_Colormap *gfcm)
{
  colormap_image_func image_func;
  if (active_output_data.colormap_dither)
    image_func = colormap_image_floyd_steinberg;
  else
    image_func = colormap_image_posterize;
  colormap_stream(gfs, gfcm, image_func);
}

static void
do_colormap_change(Gif_Stream *gfs)
{
  if (active_output_data.colormap_fixed)
    do_set_colormap(gfs, active_output_data.colormap_fixed);
  
  if (active_output_data.colormap_size > 0) {
    int nhist;
    Gif_Color *hist;
    Gif_Colormap *(*adapt_func)(Gif_Color *, int, int);
    Gif_Colormap *new_cm;
    
    /* set up the histogram */
    {
      int i, any_locals = 0;
      for (i = 0; i < gfs->nimages; i++)
	if (gfs->images[i]->local)
	  any_locals = 1;
      hist = histogram(gfs, &nhist);
      if (nhist <= active_output_data.colormap_size && !any_locals) {
	warncontext("trivial adaptive palette (only %d colors in source)", nhist);
	return;
      }
    }
    
    switch (active_output_data.colormap_algorithm) {
      
     case COLORMAP_DIVERSITY:
      adapt_func = &colormap_flat_diversity;
      break;
      
     case COLORMAP_BLEND_DIVERSITY:
      adapt_func = &colormap_blend_diversity;
      break;
      
     case COLORMAP_MEDIAN_CUT:
      adapt_func = &colormap_median_cut;
      break;
      
     default:
      fatal_error("can't happen");
      
    }
    
    new_cm = (*adapt_func)(hist, nhist, active_output_data.colormap_size);
    do_set_colormap(gfs, new_cm);
    
    Gif_DeleteArray(hist);
    Gif_DeleteColormap(new_cm);
  }
}


/*****
 * output GIF images
 **/

static void
write_stream(char *output_name, Gif_Stream *gfs)
{
  FILE *f;
  
  if (output_name)
    f = fopen(output_name, "wb");
  else {
#ifndef OUTPUT_GIF_TO_TERMINAL
    extern int isatty(int);
    if (isatty(fileno(stdout))) {
      error("not writing to <stdout>: it's a terminal");
      return;
    }
#endif
#if defined(_MSDOS) || defined(_WIN32)
    _setmode(_fileno(stdout), _O_BINARY);
#elif defined(__EMX__)
    _fsetmode(stdout, "b");
#endif
    f = stdout;
    output_name = "<stdout>";
  }
  
  if (f) {
    Gif_FullWriteFile(gfs, gif_write_flags, f);
    fclose(f);
    any_output_successful = 1;
  } else
    error("%s: %s", output_name, strerror(errno));
}

static void
merge_and_write_frames(char *outfile, int f1, int f2)
{
  Gif_Stream *out;
  int compress_immediately;
  int colormap_change;
  assert(!nested_mode);
  if (verbosing) verbose_open('[', outfile ? outfile : "#stdout#");
  
  colormap_change = active_output_data.colormap_size > 0
    || active_output_data.colormap_fixed;
  compress_immediately = !colormap_change
    && active_output_data.scaling == 0
    && active_output_data.optimizing <= 0;
  warn_local_colormaps = !colormap_change;
  
  out = merge_frame_interval(frames, f1, f2, &active_output_data,
			     compress_immediately);
  
  if (out) {
    if (active_output_data.scaling == 1)
      resize_stream(out, active_output_data.resize_width,
		    active_output_data.resize_height);
    else if (active_output_data.scaling == 2)
      resize_stream(out, active_output_data.scale_x * out->screen_width,
		    active_output_data.scale_y * out->screen_height);
    if (colormap_change)
      do_colormap_change(out);
    if (output_transforms)
      apply_color_transforms(output_transforms, out);
    if (active_output_data.optimizing > 0)
      optimize_fragments(out, active_output_data.optimizing);
    write_stream(outfile, out);
    Gif_DeleteStream(out);
  }
  
  if (verbosing) verbose_close(']');
}

static void
output_information(const char *outfile)
{
  FILE *f;
  int i, j;
  Gt_Frame *fr;
  Gif_Stream *gfs;
  
  if (infoing == 2)
    f = stderr;
  else if (outfile == 0)
    f = stdout;
  else {
    f = fopen(outfile, "w");
    if (!f) {
      error("%s: %s", outfile, strerror(errno));
      return;
    }
  }
  
  for (i = 0; i < frames->count; i++)
    FRAME(frames, i).stream->userflags = 97;

  for (i = 0; i < frames->count; i++)
    if (FRAME(frames, i).stream->userflags == 97) {
      fr = &FRAME(frames, i);
      gfs = fr->stream;
      gfs->userflags = 0;
      stream_info(f, gfs, fr->input_filename, fr->colormap_info,
		  fr->extensions_info);
      for (j = i; j < frames->count; j++)
	if (FRAME(frames, j).stream == gfs) {
	  fr = &FRAME(frames, j);
	  image_info(f, gfs, fr->image, fr->colormap_info);
	}
    }
  
  if (f != stderr && f != stdout)
    fclose(f);
}

void
output_frames(void)
{
  /* Use the current output name, not the stored output name.
     This supports `gifsicle a.gif -o xxx'.
     It's not like any other option, but seems right: it fits the natural
     order -- input, then output. */
  int i;
  char *outfile = active_output_data.output_name;
  active_output_data.output_name = 0;
  
  /* Output information only now. */
  if (infoing)
    output_information(outfile);
  
  if (infoing != 1 && frames->count > 0)
    switch (mode) {
      
     case MERGING:
     case BATCHING:
      merge_and_write_frames(outfile, 0, -1);
      break;
      
     case EXPLODING: {
       /* Use the current output name for consistency, even though that means
	  we can't explode different frames to different names. Not a big deal
	  anyway; they can always repeat the gif on the cmd line. */
       int max_nimages = 0;
       for (i = 0; i < frames->count; i++) {
	 Gt_Frame *fr = &FRAME(frames, i);
	 if (fr->stream->nimages > max_nimages)
	   max_nimages = fr->stream->nimages;
       }
       
       if (!outfile) /* Watch out! */
	 outfile = "-";
       
       for (i = 0; i < frames->count; i++) {
	 Gt_Frame *fr = &FRAME(frames, i);
	 int imagenumber = Gif_ImageNumber(fr->stream, fr->image);
	 char *explodename;
	 
	 char *imagename = 0;
	 if (fr->explode_by_name)
	   imagename = fr->name ? fr->name : fr->image->identifier;
	 
	 explodename = explode_filename(outfile, imagenumber, imagename,
					max_nimages);
	 merge_and_write_frames(explodename, i, i);
       }
       break;
     }
     
     case INSERTING:
      /* do nothing */
      break;
      
    }
  
  active_next_output = 0;
  clear_frameset(frames, 0);
  
  /* cropping: clear the `crop->ready' information, which depended on the last
     input image. */
  if (def_frame.crop)
    def_frame.crop->ready = 0;
}


/*****
 * parsing arguments
 **/

int
frame_argument(Clp_Parser *clp, char *arg)
{
  /* Returns 0 iff you should try a file named `arg'. */
  int val = parse_frame_spec(clp, arg, -1, 0);
  if (val == -97)
    return 0;
  else if (val > 0) {
    int i;
    for (i = frame_spec_1; i <= frame_spec_2; i++)
      show_frame(i, frame_spec_name != 0);
    if (next_output) combine_output_options();
    return 1;
  } else
    return 1;
}

static int
handle_extension(Clp_Parser *clp, int is_app)
{
  Gif_Extension *gfex;
  char *extension_type = clp->arg;
  char *extension_body = Clp_Shift(clp, 1);
  if (!extension_body) {
    Clp_OptionError(clp, "%O requires two arguments");
    return 0;
  }

  UNCHECKED_MARK_CH(frame, CH_EXTENSION);
  if (is_app)
    gfex = Gif_NewExtension(255, extension_type);
  else if (!isdigit(extension_type[0]) && extension_type[1] == 0)
    gfex = Gif_NewExtension(extension_type[0], 0);
  else {
    long l = strtol(extension_type, &extension_type, 0);
    if (*extension_type != 0 || l < 0 || l >= 256)
      fatal_error("bad extension type: must be a number between 0 and 255");
    gfex = Gif_NewExtension(l, 0);
  }
  
  gfex->data = (byte *)extension_body;
  gfex->length = strlen(extension_body);
  gfex->next = def_frame.extensions;
  def_frame.extensions = gfex;
  
  return 1;
}


/*****
 * option processing
 **/

static void
initialize_def_frame(void)
{
  /* frame defaults */
  def_frame.stream = 0;
  def_frame.image = 0;
  def_frame.use = 1;
  
  def_frame.name = 0;
  def_frame.no_name = 0;
  def_frame.comment = 0;
  def_frame.no_comments = 0;
  
  def_frame.interlacing = -1;
  def_frame.transparent.haspixel = 0;
  def_frame.left = -1;
  def_frame.top = -1;
  def_frame.position_is_offset = 0;
  
  def_frame.crop = 0;
  
  def_frame.delay = -1;
  def_frame.disposal = -1;
  
  def_frame.nest = 0;
  def_frame.explode_by_name = 0;
  
  def_frame.no_extensions = 0;
  def_frame.extensions = 0;
  
  def_frame.flip_horizontal = 0;
  def_frame.flip_vertical = 0;
  
  /* output defaults */
  def_output_data.output_name = 0;
  
  def_output_data.screen_width = -1;
  def_output_data.screen_height = -1;
  def_output_data.background.haspixel = 0;
  def_output_data.loopcount = -2;
  
  def_output_data.colormap_size = 0;
  def_output_data.colormap_fixed = 0;
  def_output_data.colormap_algorithm = COLORMAP_DIVERSITY;
  def_output_data.colormap_dither = 0;
  
  def_output_data.optimizing = 0;
  def_output_data.scaling = 0;
  
  active_output_data = def_output_data;
}

static void
combine_output_options(void)
{
  int recent = next_output;
  next_output = active_next_output;
#define COMBINE_ONE_OUTPUT_OPTION(value, field)		\
  if (CHANGED(recent, value)) {				\
    MARK_CH(output, value);				\
    active_output_data.field = def_output_data.field;	\
  }
  
  COMBINE_ONE_OUTPUT_OPTION(CH_OUTPUT, output_name);
  
  if (CHANGED(recent, CH_LOGICAL_SCREEN)) {
    MARK_CH(output, CH_LOGICAL_SCREEN);
    active_output_data.screen_width = def_output_data.screen_width;
    active_output_data.screen_height = def_output_data.screen_height;
  }
  COMBINE_ONE_OUTPUT_OPTION(CH_BACKGROUND, background);
  COMBINE_ONE_OUTPUT_OPTION(CH_LOOPCOUNT, loopcount);
  
  COMBINE_ONE_OUTPUT_OPTION(CH_OPTIMIZE, optimizing);
  COMBINE_ONE_OUTPUT_OPTION(CH_COLORMAP, colormap_size);
  COMBINE_ONE_OUTPUT_OPTION(CH_COLORMAP_METHOD, colormap_algorithm);
  if (CHANGED(recent, CH_USE_COLORMAP)) {
    MARK_CH(output, CH_USE_COLORMAP);
    if (def_output_data.colormap_fixed)
      def_output_data.colormap_fixed->refcount++;
    Gif_DeleteColormap(active_output_data.colormap_fixed);
    active_output_data.colormap_fixed = def_output_data.colormap_fixed;
  }
  COMBINE_ONE_OUTPUT_OPTION(CH_DITHER, colormap_dither);
  
  if (CHANGED(recent, CH_RESIZE)) {
    MARK_CH(output, CH_RESIZE);
    active_output_data.scaling = def_output_data.scaling;
    active_output_data.resize_width = def_output_data.resize_width;
    active_output_data.resize_height = def_output_data.resize_height;
    active_output_data.scale_x = def_output_data.scale_x;
    active_output_data.scale_y = def_output_data.scale_y;
  }
  
  def_output_data.colormap_fixed = 0;
  def_output_data.output_name = 0;
  
  active_next_output |= next_output;
  next_output = 0;
}

static void
redundant_option_warning(const char *option_type)
{
  static int context = 0;
  warning("redundant %s option", option_type);
  if (!context) {
    warncontext("(The %s option was overridden by another %s option",
		option_type, option_type);
    warncontext("before it had any effect.)");
  }
  context = 1;
}

static void
print_useless_options(const char *type_name, int value, const char *names[])
{
  int explanation_printed = 0;
  int i;
  if (!value) return;
  for (i = 0; i < 32; i++)
    if (CHANGED(value, i)) {
      warning("useless %s-related %s option", names[i], type_name);
      if (!explanation_printed)
	warncontext("(It didn't affect any %s.)", type_name);
      explanation_printed = 1;
    }
}


/*****
 * main
 **/

int
main(int argc, char **argv)
{
  Clp_Parser *clp =
    Clp_NewParser(argc, argv, sizeof(options) / sizeof(options[0]), options);
  
  Clp_AddStringListType
    (clp, LOOP_TYPE, Clp_AllowNumbers,
     "infinite", 0, "forever", 0,
     0);
  Clp_AddStringListType
    (clp, DISPOSAL_TYPE, Clp_AllowNumbers,
     "none", GIF_DISPOSAL_NONE,
     "asis", GIF_DISPOSAL_ASIS,
     "background", GIF_DISPOSAL_BACKGROUND,
     "bg", GIF_DISPOSAL_BACKGROUND,
     "previous", GIF_DISPOSAL_ASIS,
     0);
  Clp_AddStringListType
    (clp, COLORMAP_ALG_TYPE, 0,
     "diversity", COLORMAP_DIVERSITY,
     "blend-diversity", COLORMAP_BLEND_DIVERSITY,
     "median-cut", COLORMAP_MEDIAN_CUT,
     0);
  Clp_AddType(clp, DIMENSIONS_TYPE, 0, parse_dimensions, 0);
  Clp_AddType(clp, POSITION_TYPE, 0, parse_position, 0);
  Clp_AddType(clp, SCALE_FACTOR_TYPE, 0, parse_scale_factor, 0);
  Clp_AddType(clp, FRAME_SPEC_TYPE, 0, parse_frame_spec, 0);
  Clp_AddType(clp, COLOR_TYPE, Clp_DisallowOptions, parse_color, 0);
  Clp_AddType(clp, RECTANGLE_TYPE, 0, parse_rectangle, 0);
  Clp_AddType(clp, TWO_COLORS_TYPE, Clp_DisallowOptions, parse_two_colors, 0);
  Clp_SetOptionChar(clp, '+', Clp_ShortNegated);
  Clp_SetErrorHandler(clp, clp_error_handler);
  
  program_name = Clp_ProgramName(clp);
  
  frames = new_frameset(16);
  initialize_def_frame();
  
#ifdef DMALLOC
  dmalloc_verbose("fudge");
#endif
  
  /* Yep, I'm an idiot.
     GIF dimensions are unsigned 16-bit integers. I assume that these
     numbers will fit in an `int'. This assertion tests that assumption.
     Really I should go through & change everything over, but it doesn't
     seem worth my time. */
  {
    u_int16_t m = 0xFFFFU;
    int i = m;
    assert(i > 0 && "configuration/lameness failure! bug the author!");
  }
  
  while (1) {
    int opt = Clp_Next(clp);
    switch (opt) {
      
      /* MODE OPTIONS */
      
     case 'b':
      set_mode(BATCHING);
      break;
      
     case 'm':
      set_mode(MERGING);
      break;
      
     case 'e':
      set_mode(EXPLODING);
      def_frame.explode_by_name = 0;
      break;
      
     case 'E':
      set_mode(EXPLODING);
      def_frame.explode_by_name = 1;
      break;
      
      /* INFORMATION OPTIONS */
      
     case INFO_OPT:
      if (clp->negated)
	infoing = 0;
      else
	/* switch between infoing == 1 (suppress regular output) and 2 (don't
           suppress) */
	infoing = (infoing == 1 ? 2 : 1);
      break;
      
     case COLOR_INFO_OPT:
      if (clp->negated)
	def_frame.colormap_info = 0;
      else {
	def_frame.colormap_info = 1;
	if (!infoing) infoing = 1;
      }
      break;
      
     case EXTENSION_INFO_OPT:
      if (clp->negated)
	def_frame.extensions_info = 0;
      else {
	def_frame.extensions_info = 1;
	if (!infoing) infoing = 1;
      }
      break;
      
     case VERBOSE_OPT:
      verbosing = clp->negated ? 0 : 1;
      break;
      
      /* FRAME CHANGE OPTIONS */
      
     case DELETE_OPT:
     case REPLACE_OPT:
     case INSERT_OPT:
     case APPEND_OPT:
      frame_change_done();
      set_frame_change(opt);
      break;
      
     case ALTER_DONE_OPT:
      frame_change_done();
      break;
      
      /* IMAGE OPTIONS */
      
     case NAME_OPT:
      if (clp->negated) goto no_names;
      MARK_CH(frame, CH_NAME);
      def_frame.name = clp->arg;
      break;
      
     no_names:
     case NO_NAME_OPT:
      MARK_CH(frame, CH_NAME);
      def_frame.no_name = 1;
      def_frame.name = 0;
      break;
      
     case SAME_NAME_OPT:
      def_frame.no_name = 0;
      def_frame.name = 0;
      break;
      
     case COMMENT_OPT:
      if (clp->negated) goto no_comments;
      MARK_CH(frame, CH_COMMENT);
      if (!def_frame.comment) def_frame.comment = Gif_NewComment();
      Gif_AddComment(def_frame.comment, clp->arg, -1);
      break;
      
     no_comments:
     case NO_COMMENTS_OPT:
      Gif_DeleteComment(def_frame.comment);
      def_frame.comment = 0;
      def_frame.no_comments = 1;
      break;
      
     case SAME_COMMENTS_OPT:
      def_frame.no_comments = 0;
      break;
      
     case 'i':
      MARK_CH(frame, CH_INTERLACE);
      def_frame.interlacing = clp->negated ? 0 : 1;
      break;
      
     case SAME_INTERLACE_OPT:
      def_frame.interlacing = -1;
      break;
      
     case POSITION_OPT:
      MARK_CH(frame, CH_POSITION);
      def_frame.left = clp->negated ? 0 : position_x;
      def_frame.top = clp->negated ? 0 : position_y;
      break;
      
     case SAME_POSITION_OPT:
      def_frame.left = -1;
      def_frame.top = -1;
      break;
      
     case 't':
      MARK_CH(frame, CH_TRANSPARENT);
      if (clp->negated)
	def_frame.transparent.haspixel = 255;
      else {
	def_frame.transparent = parsed_color;
	def_frame.transparent.haspixel = parsed_color.haspixel ? 2 : 1;
      }
      break;
      
     case SAME_TRANSPARENT_OPT:
      def_frame.transparent.haspixel = 0;
      break;
      
     case BACKGROUND_OPT:
      MARK_CH(output, CH_BACKGROUND);
      if (clp->negated) {
	def_output_data.background.haspixel = 2;
	def_output_data.background.pixel = 0;
      } else {
	def_output_data.background = parsed_color;
	def_output_data.background.haspixel = parsed_color.haspixel ? 2 : 1;
      }
      break;
      
     case SAME_BACKGROUND_OPT:
      MARK_CH(output, CH_BACKGROUND);
      def_output_data.background.haspixel = 0;
      break;
      
     case LOGICAL_SCREEN_OPT:
      MARK_CH(output, CH_LOGICAL_SCREEN);
      if (clp->negated)
	def_output_data.screen_width = def_output_data.screen_height = 0;
      else {
	def_output_data.screen_width = dimensions_x;
	def_output_data.screen_height = dimensions_y;
      }
      break;
      
     case SAME_LOGICAL_SCREEN_OPT:
      MARK_CH(output, CH_LOGICAL_SCREEN);
      def_output_data.screen_width = def_output_data.screen_height = -1;
      break;
      
     case CROP_OPT:
      if (clp->negated) goto no_crop;
      MARK_CH(frame, CH_CROP);
      {
	Gt_Crop *crop = Gif_New(Gt_Crop);
	/* Memory leak on crops, but this just is NOT a problem. */
	crop->ready = 0;
	crop->whole_stream = 0;
	crop->spec_x = position_x;
	crop->spec_y = position_y;
	crop->spec_w = dimensions_x;
	crop->spec_h = dimensions_y;
	def_frame.crop = crop;
      }
      break;
      
     no_crop:
     case SAME_CROP_OPT:
      def_frame.crop = 0;
      break;
      
      /* extensions options */
      
     case NO_EXTENSIONS_OPT:
      def_frame.no_extensions = 1;
      break;
      
     case SAME_EXTENSIONS_OPT:
      def_frame.no_extensions = 0;
      break;
      
     case EXTENSION_OPT:
      if (!handle_extension(clp, 0))
	goto bad_option;
      break;
      
     case APP_EXTENSION_OPT:
      if (!handle_extension(clp, 1))
	goto bad_option;
      break;
      
      /* IMAGE DATA OPTIONS */
      
     case FLIP_HORIZ_OPT:
      MARK_CH(frame, CH_FLIP);
      def_frame.flip_horizontal = !clp->negated;
      break;
      
     case FLIP_VERT_OPT:
      MARK_CH(frame, CH_FLIP);
      def_frame.flip_vertical = !clp->negated;
      break;
       
     case NO_FLIP_OPT:
      def_frame.flip_horizontal = def_frame.flip_vertical = 0;
      break;
      
     case NO_ROTATE_OPT:
      def_frame.rotation = 0;
      break;
       
     case ROTATE_90_OPT:
      MARK_CH(frame, CH_ROTATE);
      def_frame.rotation = 1;
      break;
       
     case ROTATE_180_OPT:
      MARK_CH(frame, CH_ROTATE);
      def_frame.rotation = 2;
      break;
      
     case ROTATE_270_OPT:
      MARK_CH(frame, CH_ROTATE);
      def_frame.rotation = 3;
      break;
       
      /* ANIMATION OPTIONS */
      
     case 'd':
      MARK_CH(frame, CH_DELAY);
      def_frame.delay = clp->negated ? 0 : clp->val.i;
      break;
      
     case SAME_DELAY_OPT:
      def_frame.delay = -1;
      break;
      
     case DISPOSAL_OPT:
      MARK_CH(frame, CH_DISPOSAL);
      if (clp->negated)
	def_frame.disposal = GIF_DISPOSAL_NONE;
      else if (clp->val.i < 0 || clp->val.i > 7)
	error("disposal must be between 0 and 7");
      else
	def_frame.disposal = clp->val.i;
      break;
      
     case SAME_DISPOSAL_OPT:
      def_frame.disposal = -1;
      break;
      
     case 'l':
      MARK_CH(output, CH_LOOPCOUNT);
      if (clp->negated)
	def_output_data.loopcount = -1;
      else
	def_output_data.loopcount = (clp->have_arg ? clp->val.i : 0);
      break;
      
     case SAME_LOOPCOUNT_OPT:
      MARK_CH(output, CH_LOOPCOUNT);
      def_output_data.loopcount = -2;
      break;
      
     case OPTIMIZE_OPT:
      MARK_CH(output, CH_OPTIMIZE);
      if (clp->negated)
	def_output_data.optimizing = 0;
      else
	def_output_data.optimizing = (clp->have_arg ? clp->val.i : 1);
      break;
      
     case UNOPTIMIZE_OPT:
      UNCHECKED_MARK_CH(input, CH_UNOPTIMIZE);
      unoptimizing = clp->negated ? 0 : 1;
      break;
      
      /* WHOLE-GIF OPTIONS */

     case CAREFUL_OPT: {
       if (clp->negated)
	 gif_read_flags = gif_write_flags = 0;
       else {
	 gif_read_flags = 0;
	 gif_write_flags = GIF_WRITE_CAREFUL_MIN_CODE_SIZE;
       }
       break;
     }
      
     case CHANGE_COLOR_OPT: {
       next_input |= CH_CHANGE_COLOR;
       if (clp->negated)
	 input_transforms = delete_color_transforms
	   (input_transforms, &color_change_transformer);
       else if (parsed_color2.haspixel)
	 error("COLOR2 must be in RGB format in `--change-color COLOR1 COLOR2'");
       else
	 input_transforms = append_color_change
	   (input_transforms, parsed_color, parsed_color2);
       break;
     }
     
     case COLOR_TRANSFORM_OPT:
      next_output |= CH_COLOR_TRANSFORM;
      if (clp->negated)
	output_transforms = delete_color_transforms
	  (output_transforms, &pipe_color_transformer);
      else
	output_transforms = append_color_transform
	  (output_transforms, &pipe_color_transformer, clp->arg);
      break;
      
     case COLORMAP_OPT:
      MARK_CH(output, CH_COLORMAP);
      if (clp->negated)
	def_output_data.colormap_size = 0;
      else {
	def_output_data.colormap_size = clp->val.i;
	if (def_output_data.colormap_size < 2
	    || def_output_data.colormap_size > 256) {
	  Clp_OptionError(clp, "argument to `%O' must be between 2 and 256");
	  def_output_data.colormap_size = 0;
	}
      }
      break;
      
     case USE_COLORMAP_OPT:
      MARK_CH(output, CH_USE_COLORMAP);
      Gif_DeleteColormap(def_output_data.colormap_fixed);
      if (clp->negated)
	def_output_data.colormap_fixed = 0;
      else
	set_new_fixed_colormap(clp->arg);
      break;
      
     case COLORMAP_ALGORITHM_OPT:
      MARK_CH(output, CH_COLORMAP_METHOD);
      def_output_data.colormap_algorithm = clp->val.i;
      break;
      
     case DITHER_OPT:
      MARK_CH(output, CH_DITHER);
      def_output_data.colormap_dither = !clp->negated;
      break;
      
     case RESIZE_OPT:
      MARK_CH(output, CH_RESIZE);
      if (clp->negated)
	def_output_data.scaling = 0;
      else if (dimensions_x <= 0 && dimensions_y <= 0) {
	error("one of W and H must be positive in `--resize WxH'");
	def_output_data.scaling = 0;
      } else {
	def_output_data.scaling = 1; /* use resize dimensions */
	def_output_data.resize_width = dimensions_x;
	def_output_data.resize_height = dimensions_y;
      }
      break;
      
     case RESIZE_WIDTH_OPT:
      MARK_CH(output, CH_RESIZE);
      if (clp->negated)
	def_output_data.scaling = 0;
      else if (clp->val.u == 0) {
	error("`--resize-width' argument must be positive");
	def_output_data.scaling = 0;
      } else {
	def_output_data.scaling = 1; /* use resize dimensions */
	def_output_data.resize_width = clp->val.u;
	def_output_data.resize_height = 0;
      }
      break;
      
     case RESIZE_HEIGHT_OPT:
      MARK_CH(output, CH_RESIZE);
      if (clp->negated)
	def_output_data.scaling = 0;
      else if (clp->val.u == 0) {
	error("`--resize-height' argument must be positive");
	def_output_data.scaling = 0;
      } else {
	def_output_data.scaling = 1; /* use resize dimensions */
	def_output_data.resize_width = 0;
	def_output_data.resize_height = clp->val.u;
      }
      break;
      
     case SCALE_OPT:
      MARK_CH(output, CH_RESIZE);
      if (clp->negated)
	def_output_data.scaling = 0;
      else if (parsed_scale_factor_x <= 0 || parsed_scale_factor_y <= 0) {
	error("`--scale' X and Y factors must be positive");
	def_output_data.scaling = 0;
      } else {
	def_output_data.scaling = 2; /* use scale factor */
	def_output_data.scale_x = parsed_scale_factor_x;
	def_output_data.scale_y = parsed_scale_factor_y;
      }
      break;
      
      /* RANDOM OPTIONS */
      
     case NO_WARNINGS_OPT:
      no_warnings = !clp->negated;
      break;
      
     case WARNINGS_OPT:
      no_warnings = clp->negated;
      break;
      
     case VERSION_OPT:
#ifdef GIF_UNGIF
      printf("LCDF Gifsicle %s (ungif)\n", VERSION);
#else
      printf("LCDF Gifsicle %s\n", VERSION);
#endif
      printf("Copyright (C) 1997-2001 Eddie Kohler\n\
This is free software; see the source for copying conditions.\n\
There is NO warranty, not even for merchantability or fitness for a\n\
particular purpose.\n");
      exit(EXIT_OK);
      break;
      
     case HELP_OPT:
      usage();
      exit(EXIT_OK);
      break;
      
     case OUTPUT_OPT:
      MARK_CH(output, CH_OUTPUT);
      if (strcmp(clp->arg, "-") == 0)
	def_output_data.output_name = 0;
      else
	def_output_data.output_name = clp->arg;
      break;
      
      /* NONOPTIONS */
      
     case Clp_NotOption:
      if (clp->arg[0] != '#' || !frame_argument(clp, clp->arg)) {
	input_done();
	input_stream(clp->arg);
      }
      break;
      
     case Clp_Done:
      goto done;
      
     bad_option:
     case Clp_BadOption:
      short_usage();
      exit(EXIT_USER_ERR);
      break;
      
     default:
      break;
      
    }
  }
  
 done:
  
  if (next_output)
    combine_output_options();
  if (!files_given)
    input_stream(0);
  
  frame_change_done();
  input_done();
  if (mode == MERGING)
    output_frames();
  
  verbose_endline();
  print_useless_options("frame", next_frame, frame_option_types);
  print_useless_options("input", next_input, input_option_types);
  if (any_output_successful)
    print_useless_options("output", active_next_output, output_option_types);
  blank_frameset(frames, 0, 0, 1);
#ifdef DMALLOC
  dmalloc_report();
#endif
  return (error_count ? EXIT_ERR : EXIT_OK);
}
