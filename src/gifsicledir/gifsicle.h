/* gifsicle.h - Function declarations for gifsicle.
   Copyright (C) 1997 Eddie Kohler, eddietwo@lcs.mit.edu
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2 or later; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include "gif.h"
#include "clp.h"
#ifdef __GNUC__
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN
#endif

typedef struct Gt_Frameset Gt_Frameset;
typedef struct Gt_Crop Gt_Crop;
typedef struct Gt_ColorTransform Gt_ColorTransform;

typedef struct Gt_Frame {
  
  Gif_Stream *stream;
  Gif_Image *image;
  int use;
  
  char *name;
  int no_name;
  Gif_Comment *comment;
  int no_comments;
  
  Gif_Color transparent;
  int interlacing;
  int left;
  int top;
  
  Gt_Crop *crop;
  
  int delay;
  int disposal;
  
  Gt_Frameset *nest;
  int explode_by_name;
  
  int no_extensions;
  Gif_Extension *extensions;
  
  unsigned flip_horizontal: 1;
  unsigned flip_vertical: 1;
  unsigned colormap_info: 1;
  unsigned extensions_info: 1;
  unsigned position_is_offset: 1;
  unsigned rotation;
  
  const char *input_filename;
  
} Gt_Frame;


struct Gt_Frameset {
  int count;
  int cap;
  Gt_Frame *f;
};


struct Gt_Crop {
  int ready;
  int whole_stream;
  int spec_x;
  int spec_y;
  int spec_w;
  int spec_h;
  int x;
  int y;
  int w;
  int h;
  int left_off;
  int right_off;
};


typedef void (*colormap_transform_func)(Gif_Colormap *, void *);

struct Gt_ColorTransform {
  Gt_ColorTransform *prev;
  Gt_ColorTransform *next;
  colormap_transform_func func;
  void *data;
};


typedef struct {
  
  char *output_name;
  
  int screen_width;
  int screen_height;
  
  Gif_Color background;
  int loopcount;
  
  int colormap_size;
  Gif_Colormap *colormap_fixed;
  int colormap_algorithm;
  int colormap_dither;
  
  int optimizing;

  int scaling;
  int resize_width;
  int resize_height;
  double scale_x;
  double scale_y;
  
} Gt_OutputData;


/*****
 * error & verbose
 **/
extern const char *program_name;
extern int verbosing;
extern int error_count;
extern int no_warnings;
extern int gif_read_flags;
extern int gif_write_flags;

void fatal_error(char *message, ...) NORETURN;
void warning(char *message, ...);
void warncontext(char *message, ...);
void error(char *message, ...);
void clp_error_handler(char *clp_message);
void usage(void);
void short_usage(void);

void verbose_open(char, const char *);
void verbose_close(char);
void verbose_endline(void);

#define EXIT_OK		0
#define EXIT_ERR	1
#define EXIT_USER_ERR	1

/*****
 * info &c
 **/
void stream_info(FILE *, Gif_Stream *, const char *,
		 int colormaps, int extensions);
void image_info(FILE *, Gif_Stream *, Gif_Image *, int colormaps);

char *explode_filename(char *filename, int number, char *name, int max_nimg);

/*****
 * merging images
 **/
#define COLORMAP_ENSURE_SLOT_255 1
void	unmark_colors(Gif_Colormap *);
void	unmark_colors_2(Gif_Colormap *);
int	find_color_index(Gif_Color *c, int nc, Gif_Color *);
int	merge_colormap_if_possible(Gif_Colormap *, Gif_Colormap *);

extern int warn_local_colormaps;
void	merge_stream(Gif_Stream *dest, Gif_Stream *src, int no_comments);
void	merge_comments(Gif_Comment *destc, Gif_Comment *srcc);
Gif_Image *merge_image(Gif_Stream *dest, Gif_Stream *src, Gif_Image *srci);

void	optimize_fragments(Gif_Stream *, int optimizeness);

/*****
 * image/colormap transformations
 **/
Gif_Colormap *read_colormap_file(char *, FILE *);
void	apply_color_transforms(Gt_ColorTransform *, Gif_Stream *);

typedef void (*color_transform_func)(Gif_Colormap *, void *);
Gt_ColorTransform *append_color_transform
	(Gt_ColorTransform *list, color_transform_func, void *);
Gt_ColorTransform *delete_color_transforms
	(Gt_ColorTransform *list, color_transform_func);

void	color_change_transformer(Gif_Colormap *, void *);
Gt_ColorTransform *append_color_change
	(Gt_ColorTransform *list, Gif_Color, Gif_Color);

void	pipe_color_transformer(Gif_Colormap *, void *);

int	crop_image(Gif_Image *, Gt_Crop *);

void	flip_image(Gif_Image *, int scr_width, int scr_height, int is_vert);
void	rotate_image(Gif_Image *, int scr_width, int scr_height, int rotation);
void	scale_image(Gif_Stream *, Gif_Image *, double xfactor, double yfactor);
void	resize_stream(Gif_Stream *, int new_width, int new_height);

/*****
 * quantization
 **/
Gif_Color *histogram(Gif_Stream *, int *);

#define COLORMAP_DIVERSITY		0
#define COLORMAP_BLEND_DIVERSITY	1
#define COLORMAP_MEDIAN_CUT		2
Gif_Colormap *colormap_blend_diversity(Gif_Color *, int, int);
Gif_Colormap *colormap_flat_diversity(Gif_Color *, int, int);
Gif_Colormap *colormap_median_cut(Gif_Color *, int, int);

typedef struct color_hash_item color_hash_item;
typedef void (*colormap_image_func)
     (Gif_Image *, byte *, Gif_Colormap *, Gif_Colormap *,
      color_hash_item **, u_int32_t *);

void	colormap_image_posterize
	(Gif_Image *, byte *, Gif_Colormap *, Gif_Colormap *,
	 color_hash_item **, u_int32_t *);
void	colormap_image_floyd_steinberg
	(Gif_Image *, byte *, Gif_Colormap *, Gif_Colormap *,
	 color_hash_item **, u_int32_t *);
void	colormap_stream(Gif_Stream *, Gif_Colormap *, colormap_image_func);

/*****
 * parsing stuff
 **/
extern int	frame_spec_1;
extern int	frame_spec_2;
extern char *	frame_spec_name;
extern int	dimensions_x;
extern int	dimensions_y;
extern int	position_x;
extern int	position_y;
extern Gif_Color parsed_color;
extern Gif_Color parsed_color2;
extern double	parsed_scale_factor_x;
extern double	parsed_scale_factor_y;

int		parse_frame_spec(Clp_Parser *, const char *, int, void *);
int		parse_dimensions(Clp_Parser *, const char *, int, void *);
int		parse_position(Clp_Parser *, const char *, int, void *);
int		parse_scale_factor(Clp_Parser *, const char *, int, void *);
int		parse_color(Clp_Parser *, const char *, int, void *);
int		parse_rectangle(Clp_Parser *, const char *, int, void *);
int		parse_two_colors(Clp_Parser *, const char *, int, void *);

extern Gif_Stream *input;
extern char *input_name;

void		input_stream(char *);
void		input_done(void);
void		output_frames(void);

/*****
 * stuff with frames
 **/
extern Gt_Frame def_frame;
#define		FRAME(fs, i)	((fs)->f[i])

Gt_Frameset *	new_frameset(int initial_cap);
Gt_Frame *	add_frame(Gt_Frameset *, int num, Gif_Stream *, Gif_Image *);
void		clear_def_frame_once_options(void);

Gif_Stream *	merge_frame_interval(Gt_Frameset *, int f1, int f2,
				     Gt_OutputData *, int compress);
void		clear_frameset(Gt_Frameset *, int from);
void		blank_frameset(Gt_Frameset *, int from, int to, int delete_ob);
