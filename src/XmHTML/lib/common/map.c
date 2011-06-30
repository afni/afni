#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* map.c : XmHTML imagemap routines
*
* This file Version	$Revision$
*
* Creation date:		Tue Feb 25 19:14:55 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the Free
* Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.11  1998/04/27 07:00:22  newt
* tka stuff
*
* Revision 1.10  1998/04/04 06:28:13  newt
* XmHTML Beta 1.1.3
*
* Revision 1.9  1998/02/01 16:12:12  newt
* second stage recovery of bad attribute specification for RECT areas:
* getComplexCoordinates.
*
* Revision 1.8  1997/08/30 01:11:52  newt
* my_strdup -> strdup and _XmHTMLWarning proto changes.
*
* Revision 1.7  1997/08/01 13:02:49  newt
* Performance enhancements + comment updating.
*
* Revision 1.6  1997/05/28 01:52:04  newt
* ?
*
* Revision 1.5  1997/04/29 14:28:00  newt
* Header files modifications.
*
* Revision 1.4  1997/03/11 19:56:01  newt
* replaced XmHTMLAddImagemap call by XmHTMLImageAddImageMap
*
* Revision 1.3  1997/03/04 18:48:10  newt
* _XmHTMLDrawImagemapSelection added
*
* Revision 1.2  1997/03/04 01:00:25  newt
* Polygon and default shaped imagemaps are now working
*
* Revision 1.1  1997/03/02 23:02:42  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
typedef enum{
	MAP_DEFAULT = 1,
	MAP_RECT,
	MAP_CIRCLE,
	MAP_POLY
}MapShape;

/*** Private Function Prototype Declarations ****/
static Region createPoly(int npoints, int *points);
static void deleteArea(mapArea *area);
static void freeImageMap(XmHTMLImageMap *map);
static int* getCoordinates(String attributes, int *ncoords);

/*
* A point is in a rectangle if it's x coordinate is larger than the left
* side and smaller than the right side and if it's y coordinate is larger
* than the top side and smaller than the bottom side.
*/
#define PointInRect(X,Y,CRD) \
	(((X) >= CRD[0] && (X) <= CRD[2]) && ((Y) >= CRD[1] && (Y) <= CRD[3])) 
/*
* a point is in a circle if the distance from the circle's origin to the
* point is less than the radius of the circle (plain ol' Pythagoras)
*/
#define PointInCircle(X,Y,XC,YC,R) \
	(((((X)-(XC))*((X)-(XC))) + (((Y)-(YC))*((Y)-(YC))) ) <= (R*R))

/* seamingly easy */
#define PointInPoly(X,Y,REG) \
	(XPointInRegion((REG),(X),(Y)))

/*** Private Variable Declarations ***/
struct _mapArea{
	String url;						/* url to call when clicked */
	String alt;						/* alternative text */
	Boolean nohref;					/* obvious */
	MapShape shape;					/* type of area */
	int ncoords;					/* no of coordinates */
	int *coords;					/* array of coordinates */
	Region region;					/* Region for polygons */
	XmHTMLAnchor *anchor;			/* anchor object */
	struct _mapArea *next;			/* ptr to next area */
};

/*****
* Name: 		createPoly
* Return Type: 	Region
* Description: 	creates a polygon region given the polygon's coordinates
* In: 
*	npoints:	total no of points
*	points:		array of points defining the polygon.
*				The last point automatically connects to the first.
* Returns:
*	a newly created region
*****/
static Region
createPoly(int npoints, int *points)
{
	static Region region;
	XPoint *xpoints;		/* GdkPoint is equivalent to XPoint */
	int i, half;

	/* create array of XPoint's required for region generation */
	half = npoints/2.;
	xpoints = (XPoint*)calloc(half+1, sizeof(XPoint));
	for(i = 0; i < half; i++)
	{
		xpoints[i].x = points[i*2];
		xpoints[i].y = points[i*2+1];
	}
	/* last point is same as first point */
	xpoints[half].x = points[0];
	xpoints[half].y = points[1];
	
	/* create the region */
	region = XPolygonRegion(xpoints, half+1, WindingRule);

	/* no longer needed, free it */
	free(xpoints);

	return(region);
}

/*****
* Name: 		deleteArea
* Return Type: 	void
* Description: 	frees all memory occupied by the given area
* In: 
*	area:		area to free
* Returns:
*	nothing
*****/
static void
deleteArea(mapArea *area)
{
	/* sanity */
	if(area == NULL)
		return;

	if(area->url)
		free(area->url);
	if(area->alt)
		free(area->alt);
	if(area->coords)
		free(area->coords);
	if(area->shape == MAP_POLY && area->region)
		XDestroyRegion(area->region);
	free(area);
	area = NULL;
}

/*****
* Name: 		freeImageMap
* Return Type: 	void
* Description: 	frees the given imagemap and all areas defined for it.
* In: 
*	map:		imagemap to free
* Returns:
*	nothing
*****/
static void
freeImageMap(XmHTMLImageMap *map)
{
	mapArea *area, *area_list;

	area_list = map->areas;

	while(area_list)
	{
		area = area_list->next;
		deleteArea(area_list);
		area_list = area;
	}
	if(map->name)
		free(map->name);
	free(map);
	map = NULL;
}

/*****
* Name: 		getCoordinates
* Return Type: 	int*
* Description: 	returns array of map coordinates
* In: 
*	attributes:	raw area specs
*	*ncoords:	no of coordinates, filled upon return
* Returns:
*	an array of integers representing the coordinates of an area.
*	returns NULL if no coords are found.
*****/
static int*
getCoordinates(String attributes, int *ncoords)
{
	String chPtr;
	int *coords;
	int num;
	register String tmp;

	*ncoords = 0;
	coords = NULL;

	/* get coordinates and count how many there are */
	if(attributes == NULL ||
		(chPtr = _XmHTMLTagGetValue(attributes, "coords")) == NULL)
		return(NULL);

	/*****
	* Count how many coordinates we have. We start the count at one since
	* we are always one short on separators (3 separators == 4 numbers).
	*****/
	for(tmp = chPtr, num = 1; *tmp != '\0'; tmp++)
		if(*tmp == ',') num++;

	if(num == 1)
	{
		free(chPtr);
		return(NULL);
	}

	_XmHTMLDebug(10, ("map.c: getCoordinates, counted %i numbers\n", num));

	/* allocate memory for these coordinates */
	coords = (int*)calloc(num, sizeof(int));
	
	/* now convert to numbers */
	for(num = 0, tmp = strtok(chPtr, ","); tmp != NULL; 
		tmp = strtok(NULL, ","), num++)
		coords[num] = atoi(tmp);

	/* no longer needed */
	free(chPtr);

#ifdef DEBUG
	{
		int i;
		_XmHTMLDebug(10, ("map.c: getCoordinates: "));  
		for(i = 0; i < num; i++)
			_XmHTMLDebug(10, ("%i ", coords[i]));
		_XmHTMLDebug(10, ("\n"));  
	}
#endif

	*ncoords = num;
	return(coords);
}

/*****
* Name:			getComplexCoordinates
* Return Type: 	int
* Description: 	checks the attributes for coordinates but this time
*				any sequence of non-digits is considered as a separator.
* In: 
*	attributes:	raw area specs
*	*ncoords:	no of coordinates, filled upon return
* Returns:
*	an array of integers representing the coordinates of an area.
*	returns NULL if no coords are found.
* Note:
*	this is *not* the default coordinate parser since this routine is
*	a lot more time-consuming.
*****/
static int*
getComplexCoordinates(String attributes, int *ncoords)
{
	String chPtr;
	int *coords;
	int num = 0;
	register String tmp;

	*ncoords = 0;
	coords = NULL;

	/* get coordinates and count how many there are */
	if(attributes == NULL ||
		(chPtr = _XmHTMLTagGetValue(attributes, "coords")) == NULL)
		return(NULL);

	tmp = chPtr;

	/* count how many coordinates we have */
	do
	{
		/* skip the digit */
		while(*tmp != '\0' && isdigit(*tmp))
			tmp++;
		/* skip the spacer */
		while(*tmp != '\0' && !isdigit(*tmp))
			tmp++;
		num++;
	}
	while(*tmp != '\0');

	if(!num)
	{
		free(chPtr);
		return(NULL);
	}

	_XmHTMLDebug(10, ("map.c: getComplexCoordinates, counted %i numbers\n",
		num));

	/* allocate memory for these coordinates */
	coords = (int*)calloc(num, sizeof(int));
	
	/* convert coordinates to numbers */
	tmp = chPtr;
	num = 0;
	do{
		register char *tmpPtr = tmp;
		/* skip to the end of this digit */
		while(*tmpPtr != '\0' && isdigit(*tmpPtr))
			tmpPtr++;
		*tmpPtr = '\0';
		coords[num++] = atoi(tmp);
		tmp = tmpPtr+1; /* next char starts here */
		/* skip the spacer */
		while(*tmp != '\0' && !isdigit(*tmp))
			tmp++;
	}
	while(*tmp != '\0');

	/* no longer needed */
	free(chPtr);

#ifdef DEBUG
	{
		int i;
		_XmHTMLDebug(10, ("map.c: getComplexCoordinates: "));  
		for(i = 0; i < num; i++)
			_XmHTMLDebug(10, ("%i ", coords[i]));
		_XmHTMLDebug(10, ("\n"));  
	}
#endif

	*ncoords = num;
	return(coords);
}

static void
drawSelectionRectangle(XmHTMLWidget html, XmHTMLImage *image, 
	mapArea *area)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	int x = image->owner->x - html->html.scroll_x + area->coords[0];
	int y = image->owner->y - html->html.scroll_y + area->coords[1];
	int width = area->coords[2] - area->coords[0];
	int height = area->coords[3] - area->coords[1];

	tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(imagemap_fg));
	tka->DrawRectangle(tka->dpy, tka->win, HTML_ATTR(gc), x, y, width, height);
}

static void
drawSelectionPolygon(XmHTMLWidget html, XmHTMLImage *image, 
	mapArea *area)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	XPoint *points;
	int i, npoints;
	int x = image->owner->x - html->html.scroll_x;
	int y = image->owner->y - html->html.scroll_y;

	npoints = area->ncoords/2;

	points = (XPoint*)calloc(npoints+1, sizeof(XPoint));

	for(i = 0; i < npoints; i++)
	{
		points[i].x = area->coords[i*2] + x;
		points[i].y = area->coords[i*2+1] + y;
	}
	/* last point is same as first point */
	points[npoints].x = points[0].x;
	points[npoints].y = points[0].y;

	tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(imagemap_fg));
	tka->DrawLines(tka->dpy, tka->win, HTML_ATTR(gc),
		points, npoints+1, tka->coord_mode[GC_COORDMODE_ORIGIN]);
	free(points);
}

static void
drawSelectionArc(XmHTMLWidget html, XmHTMLImage *image,
	mapArea *area)
{
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	int x = image->owner->x - html->html.scroll_x + area->coords[0];
	int y = image->owner->y - html->html.scroll_y + area->coords[1];
	int radius = area->coords[2];

	/* upper-left corner of bounding rectangle */
	x -= radius;
	y -= radius;
	
	tka->SetForeground(tka->dpy, HTML_ATTR(gc), HTML_ATTR(imagemap_fg));
	tka->DrawArc(tka->dpy, tka->win, HTML_ATTR(gc), x, y, 2*radius,
		2*radius, 0, 23040);
}

/********
****** Public Functions
********/

/*****
* Name: 		_XmHTMLAddAreaToMap
* Return Type: 	void
* Description: 	adds the given area specification to the given imagemap
* In: 
*	map:		XmHTMLImageMap
*	object:		raw area data
* Returns:
*	nothing
*****/
void
_XmHTMLAddAreaToMap(XmHTMLWidget html, XmHTMLImageMap *map, 
	XmHTMLObject *object)
{
	static mapArea *area;
	mapArea *tmp;
	String chPtr;

	/* sanity */
	if(map == NULL || object->attributes == NULL)
		return;

	area = (mapArea*)malloc(sizeof(mapArea));

	(void)memset(area, 0, sizeof(mapArea));

	area->url = _XmHTMLTagGetValue(object->attributes, "href");
	area->alt = _XmHTMLTagGetValue(object->attributes, "alt");
	area->nohref = _XmHTMLTagCheck(object->attributes, "nohref");

	chPtr = _XmHTMLTagGetValue(object->attributes, "shape");

	/* get specified coordinates */
	area->coords = getCoordinates(object->attributes, &area->ncoords);

	/*
	* No shape given, try to figure it out using the number of specified
	* coordinates
	*/
	if(chPtr == NULL)
	{
		switch(area->ncoords)
		{
			case 0:
				/* no coords given => default area */
				area->shape = MAP_DEFAULT;
				break;
			case 3:
				/* 3 coords => circle */
				area->shape = MAP_CIRCLE;
				break;
			case 4:
				/* 4 coords => assume rectangle */
				area->shape = MAP_RECT;
				break;
			default:
				/* assume poly */
				area->shape = MAP_POLY;
		}
	}
	else
	{
		switch(tolower(chPtr[0]))
		{
			case 'c':
				area->shape = MAP_CIRCLE;
				break;
			case 'r':
				area->shape = MAP_RECT;
				break;
			case 'p':
				area->shape = MAP_POLY;
				break;
			default:
				area->shape = MAP_DEFAULT;
		}
		free(chPtr);
	}

	/* check if all coordinates specs are valid for the given shape */
	switch(area->shape)
	{
		case MAP_RECT:
			/* too bad if coords are bad */
			if(area->ncoords != 4)
			{
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLAddAreaToImagemap"),
					XMHTML_MSG_81, area->ncoords, object->line);
				/* too many coordinates, drop the excessive ones */
				if(area->ncoords > 4)
					area->ncoords = 4;
				else
				{
					/*****
					* Less than required, do a complex rescan of the
					* attributes (any sequence of non-digits is considered
					* a separator).
					*****/
					free(area->coords);
					area->coords = getComplexCoordinates(object->attributes,
						&area->ncoords);
					/* too many coordinates, drop the excessive ones */
					if(area->ncoords > 4)
						area->ncoords = 4;
					else
					{
						String chPtr = _XmHTMLTagGetValue(object->attributes,
										"coords");
						_XmHTMLWarning(__WFUNC__(html,
							"_XmHTMLAddAreaToImagemap"), XMHTML_MSG_82, chPtr);
						free(chPtr);
						deleteArea(area);
						return;
					}
				}
			}
			break;
		case MAP_CIRCLE:
			/* too bad if coords are bad */
			if(area->ncoords != 3)
			{
				_XmHTMLWarning(__WFUNC__(html,
					"_XmHTMLAddAreaToImagemap"), XMHTML_MSG_83,
					area->ncoords, object->line);
				deleteArea(area);
				return;
			}
			break;
		case MAP_POLY:
			if(!area->coords)
			{
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLAddAreaToImagemap"),
					XMHTML_MSG_84, area->ncoords, object->line);
				deleteArea(area);
				return;
			}
			if(area->ncoords % 2)
			{
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLAddAreaToImagemap"),
					XMHTML_MSG_85, area->ncoords, object->line);
				area->ncoords--;
			}
			area->region = createPoly(area->ncoords, area->coords);
			break;
		default:
			break;
	}

	/* gets automagically added to the list of anchors for this widget */
	if(!area->nohref)
		area->anchor = _XmHTMLNewAnchor(html, object);

	/* add this area to the list of areas for this imagemap */
	if(map->areas == NULL)
	{
		map->nareas = 1;
		map->areas = area;
		return;
	}
	for(tmp = map->areas; tmp != NULL && tmp->next != NULL ; tmp = tmp->next);
	map->nareas++;
	tmp->next = area;

	_XmHTMLDebug(10, ("map.c: _XmHTMLAddAreaToMap, stored href %s, map now "
		"contains %i areas.\n", area->url, map->nareas));
}

/*****
* Name: 		_XmHTMLCreateImagemap
* Return Type: 	XmHTMLImageMap
* Description: 	initializes a new imagemap
* In: 
*	name:		name for this map
* Returns:
*	the newly created imagemap
*****/
XmHTMLImageMap*
_XmHTMLCreateImagemap(String name)
{
	static XmHTMLImageMap *map;

	map = (XmHTMLImageMap*)malloc(sizeof(XmHTMLImageMap));

	(void)memset(map, 0, sizeof(XmHTMLImageMap));

	map->name = strdup(name);

	return(map);
}

/*****
* Name: 		_XmHTMLStoreImagemap
* Return Type: 	void
* Description: 	stores the given imagemap in the given html widget
* In: 
*	html:		XmHTMLWidget
*	map:		map to store
* Returns:
*	nothing, but appends the given map to the list of imagemaps of the
*	given HTML widget.
*****/
void
_XmHTMLStoreImagemap(XmHTMLWidget html, XmHTMLImageMap *map)
{
	XmHTMLImageMap *tmp;

	/* head of the list */
	if(html->html.image_maps == NULL)
	{
		html->html.image_maps = map;
		return;
	}

	/* walk to the one but last map in the list and insert it */
	for(tmp = html->html.image_maps; tmp != NULL && tmp->next != NULL; 
		tmp = tmp->next);
	tmp->next = map;
}

/*****
* Name: 		_XmHTMLGetImagemap
* Return Type: 	XmHTMLImageMap*
* Description: 	retrieves the imagemap with the given name.
* In: 
*	html:		XmHTMLWidget
*	name:		name of map to retrieve
* Returns:
*	named map if found, NULL otherwise.
*****/
XmHTMLImageMap*
_XmHTMLGetImagemap(XmHTMLWidget html, String name)
{
	XmHTMLImageMap *tmp;

	if(!name || *name == '\0')
		return(NULL);

	for(tmp = html->html.image_maps; tmp != NULL && 
		strcasecmp(tmp->name, &name[1]); tmp = tmp->next);

	_XmHTMLFullDebug(10, ("map.c: _XmHTMLGetImageMap, found %s match for "
		"named imagemap %s\n", (tmp ? "a" : "no"), name));

	return(tmp);
}

/*****
* Name: 		_XmHTMLDrawImagemapSelection
* Return Type: 	void
* Description: 	draws a bounding box around each area in an imagemap
* In: 
*	html:		XmHTMLWidget id
*	image:		image for which to paint bounding boxes
* Returns:
*	nothing
*****/
void
_XmHTMLDrawImagemapSelection(XmHTMLWidget html, XmHTMLImage *image)
{
	XmHTMLImageMap *map;
	int xs, ys;
	mapArea *area;

	if((map = _XmHTMLGetImagemap(html, image->map_url)) == NULL)
		return;

	/* map coordinates to upperleft corner of image */
	xs = html->html.scroll_x - image->owner->x;
	ys = html->html.scroll_y - image->owner->y;

	area = map->areas;

	while(area)
	{
		switch(area->shape)
		{
			case MAP_RECT:
				drawSelectionRectangle(html, image, area);
				break;
			case MAP_CIRCLE:
				drawSelectionArc(html, image, area);
				break;
			case MAP_POLY:
				drawSelectionPolygon(html, image, area);
				break;
			default:
				break;
		}
		area = area->next;
	}
}

/*****
* Name: 		_XmHTMLGetImagemapAnchor
* Return Type: 	XmHTMLAnchor*
* Description:  checks whether the given coordinates lie somewhere within
*				the given imagemap.
* In: 
*	html:		XmHTMLWidget
*	x,y:		point coordinates, relative to upper-left corner of the
*				html widget
*	image:		current image data, required to make x and y coordinates
*				relative to upper-left corner of the image.
*	map:		imagemap to check
* Returns:
*	anchor data if successfull, NULL otherwise
*****/
XmHTMLAnchor*
_XmHTMLGetAnchorFromMap(XmHTMLWidget html, int x, int y,
	XmHTMLImage *image, XmHTMLImageMap *map)
{
	int xs, ys;
	mapArea *area, *def_area;
	XmHTMLAnchor *anchor = NULL;
	Boolean found = False;

	/* map coordinates to upperleft corner of image */
	xs = x + html->html.scroll_x - image->owner->x;
	ys = y + html->html.scroll_y - image->owner->y;

	_XmHTMLFullDebug(10, ("map.c: _XmHTMLGetAnchorFromMap, x = %i, y = %i, "
		"relative x = %i, relative y = %i\n", x, y, xs, ys));

	area = map->areas;
	def_area = NULL;

	/*
	* We test against found instead of anchor becoming non-NULL:
	* areas with the NOHREF attribute set don't have an anchor but
	* should be taken into account as well.
	*/
	while(area && !found)
	{
		switch(area->shape)
		{
			case MAP_RECT:
				if(PointInRect(xs, ys, area->coords))
				{
					anchor = area->anchor;
					found = True;
				}
				break;
			case MAP_CIRCLE:
				if(PointInCircle(xs, ys, area->coords[0], area->coords[1],
					area->coords[2]))
				{
					anchor = area->anchor;
					found = True;
				}
				break;
			case MAP_POLY:
				if(PointInPoly(xs, ys, area->region))
				{
					anchor = area->anchor;
					found = True;
				}
				break;
			/*
			* just save default area info; it's only needed if nothing
			* else matches.
			*/
			case MAP_DEFAULT:
				def_area = area;
				break;
		}
		area = area->next;
	}
	if(!found && def_area)
		anchor = def_area->anchor;

	_XmHTMLFullDebug(10, ("map.c: _XmHTMLGetAnchorFromMap, %s anchor found\n",
		(anchor ? "an" : "no")));

	return(anchor);
}

/*****
* Name: 		_XmHTMLFreeImageMaps
* Return Type: 	void
* Description: 	frees all imagemaps for the given widget
* In: 
*	html:		XmHTMLWidget
* Returns:
*	nothing
*****/
void
_XmHTMLFreeImageMaps(XmHTMLWidget html)
{
	XmHTMLImageMap *map, *map_list;

 	map_list = html->html.image_maps;

	while(map_list != NULL)
	{
		map = map_list->next;
		freeImageMap(map_list);
		map_list = NULL;
		map_list = map;
	}
	html->html.image_maps = NULL;
}

/*****
* Name: 		_XmHTMLCheckImagemaps
* Return Type: 	void
* Description: 	checks whether an image requires an external imagemap
* In: 
*	html:		XmHTMLWidget containing images to check
* Returns:
*	nothing
* Note:
*	this routine is only effective when a XmNimagemapCallback callback
*	is installed. When an external imagemap is required, this routine
*	triggers this callback and will load an imagemap when the map_contents
*	field is non-null after the callback returns. We make a copy of this
*	map and use it to parse and load the imagemap.
*****/
void
_XmHTMLCheckImagemaps(XmHTMLWidget html)
{
	XmHTMLImage *image;
	XmHTMLImageMap *imagemap;

	_XmHTMLDebug(10, ("map.c: _XmHTMLCheckImagemaps Start\n"));

	if(html->html.images == NULL || html->html.imagemap_callback == NULL)
	{
		_XmHTMLDebug(10, ("map.c: _XmHTMLCheckImagemaps End: %s.\n",
			(html->html.images ? "no imagemap_callback" : 
			"no images in document")));
		return;
	}

	for(image = html->html.images; image != NULL; image = image->next)
	{
		if(image->map_url != NULL)
		{
			if((imagemap = _XmHTMLGetImagemap(html, image->map_url)) == NULL)
			{
				static XmHTMLImagemapCallbackStruct cbs;

				_XmHTMLImagemapCallback(html, image, &cbs);

				_XmHTMLDebug(10, ("map.c: _XmHTMLCheckImagemaps, return from "
					"imagemap_callback, %s imagemap.\n",
					cbs.map_contents ? "loading" : "not loading"));

				/* parse and add this imagemap */
				if(cbs.map_contents != NULL)
				{
					String map;
					map = strdup(cbs.map_contents);
					XmHTMLImageAddImageMap((Widget)html, map);
					free(map);
				}
			}
		}
	}
	_XmHTMLDebug(10, ("map.c: _XmHTMLCheckImagemaps End\n"));
}
