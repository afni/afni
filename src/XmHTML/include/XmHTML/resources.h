/*****
* resources.h : XmHTML default resource list
*
* This file Version	$Revision$
*
* Creation date:		Thu Nov 21 17:56:18 GMT+0100 1996
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
* (C)Copyright 1995-1996 Ripley Software Development
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
* $Source$
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:08:41  rwcox
* Cadd
*
* Revision 1.19  1998/04/27 07:03:44  newt
* Replaced a few resources by new defines in XmHTMLfuncs.h
*
* Revision 1.18  1998/04/04 06:28:37  newt
* XmHTML Beta 1.1.3
*
* Revision 1.17  1997/10/26 23:50:43  newt
* Added the XmNenableFormColors resource
*
* Revision 1.16  1997/10/23 00:25:27  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.15  1997/08/31 17:42:19  newt
* log edit
*
* Revision 1.14  1997/08/30 01:37:59  newt
* Changed XtOffset to the Offset macro.
* Added XmNhiglightOnEnter, XmNimageMapToPalette, XmNimageFSDither,
* XmNimagePalette, XmNalphaChannelProcessing, XmNimageRGBConversion and
* XmNhighlightColor resources.
*
* Revision 1.13  1997/08/01 13:12:50  newt
* Progressive image loading changes.
*
* Revision 1.12  1997/05/28 01:56:27  newt
* Added the XmNdecodeGIFProc and XmNuncompressCommand resources.
*
* Revision 1.11  1997/04/29 14:31:32  newt
* Removed obsoleted resources.
*
* Revision 1.10  1997/03/28 07:27:24  newt
* XmNmimeType, XmNframeCallback, (ImageProc*) cast. Changed fontSizeList value
* for h3 from 16 to 14.
*
* Revision 1.9  1997/03/20 08:16:28  newt
* added XmNrepeatDelay and removed XmNimageDefaultProc resources
*
* Revision 1.8  1997/03/11 19:59:33  newt
* Added the XmNfreezeAnimations resource
*
* Revision 1.7  1997/03/04 18:50:18  newt
* XmNimagemapBoundingBoxForeground and XmNimagemapDrawBoundingBoxes
*
* Revision 1.6  1997/03/04 01:02:04  newt
* Obsolete entries removed
*
* Revision 1.5  1997/03/02 23:23:56  newt
* Changes to XmNfontFamily resources; added XmNcharset and XmNimagemapCallback
*
* Revision 1.4  1997/02/11 02:03:58  newt
* added XmNhandleShortTags stuff. Changed XmNscrolledWindowMargin to XmNmargin
*
* Revision 1.3  1997/01/09 06:56:08  newt
* expanded copyright marker
*
* Revision 1.2  1997/01/09 06:48:57  newt
* new resource: XmNparserCallback
*
* Revision 1.1  1996/12/19 02:17:22  newt
* Initial Revision
*
*****/ 

#ifndef _resources_h_
#define _resources_h_

#define Offset(field) XtOffsetOf(XmHTMLRec, html.field)
#define Stringify(VAL)	#VAL

static XtResource resources [] =
{
	{
		XmNalignment,
		XmCAlignment, XmRAlignment,
		sizeof(XtEnum), Offset(alignment),
		XmRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
	},
	{
		XmNanchorCursor,
		XmCCursor, XmRCursor,
		sizeof(Cursor), Offset(anchor_cursor),
		XmRImmediate, (XtPointer)NULL 
	},
	{
		XmNanchorDisplayCursor,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(anchor_display_cursor),
		XmRString, "True"
	},
	{
		XmNanchorButtons,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(anchor_buttons),
		XmRString, "True"
	},
	{
		XmNanchorForeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(anchor_fg),
		XmRString, "blue1"
	},
	{
		XmNanchorVisitedForeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(anchor_visited_fg),
		XmRString, "red"
	},
	{
		XmNanchorTargetForeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(anchor_target_fg),
		XmRString, "blue1"
	},
	{
		XmNanchorActivatedForeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(anchor_activated_fg),
		XmRString, "red"
	},
	{
		XmNanchorActivatedBackground,
		XmCBackground, XmRPixel,
		sizeof(Pixel), Offset(anchor_activated_bg),
		XmRString, "white"
	},
	{
		XmNhighlightOnEnter,
		XmCHighlightOnEnter, XmRBoolean,
		sizeof(Boolean), Offset(highlight_on_enter),
		XmRString, "True",
	},
	{
		XmNanchorUnderlineType,
		XmCAnchorUnderlineType, 
		XmRAnchorUnderlineType,
		sizeof(XtEnum), Offset(anchor_underline_type),
		XmRImmediate, (XtPointer)XmSINGLE_LINE
	},
	{
		XmNanchorVisitedUnderlineType,
		XmCAnchorUnderlineType,
		XmRAnchorUnderlineType,
		sizeof(XtEnum), Offset(anchor_visited_underline_type),
		XmRImmediate, (XtPointer)XmSINGLE_LINE
	},
	{
		XmNanchorTargetUnderlineType,
		XmCAnchorUnderlineType, 
		XmRAnchorUnderlineType,
		sizeof(XtEnum), Offset(anchor_target_underline_type),
		XmRImmediate, (XtPointer)XmSINGLE_DASHED_LINE
	},
	{
		XmNanchorVisitedProc,
		XmCAnchorVisitedProc, XmRPointer,
		sizeof(XmHTMLAnchorProc), Offset(anchor_visited_proc),
		XmRImmediate, (XmHTMLAnchorProc*)NULL
	},
	{
		XmNanchorTrackCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(anchor_track_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNactivateCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(activate_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNarmCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(arm_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNframeCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(frame_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNformCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(form_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNfocusCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(focus_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNlosingFocusCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(losing_focus_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNlinkCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(link_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNinputCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(input_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNmotionTrackCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(motion_track_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNimagemapCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(imagemap_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNdocumentCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(document_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNeventCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(event_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNobjectCallback,
		XmCCallback, XmRCallback,
		sizeof(XtCallbackList), Offset(object_callback),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNcharset,
		XmCString, XmRString,
		sizeof(String), Offset(charset),
		XmRString, XmHTML_DEFAULT_CHARSET
	},
	{
		XmNfontFamily,
		XmCString, XmRString,
		sizeof(String), Offset(font_family),
		XmRString, XmHTML_DEFAULT_PROPORTIONAL_FONT
	},
	{
		XmNfontFamilyFixed,
		XmCString, XmRString,
		sizeof(String), Offset(font_family_fixed),
		XmRString, XmHTML_DEFAULT_FIXED_FONT
	},
	{
		XmNfontSizeList,
		XmCString, XmRString,
		sizeof(String), Offset(font_sizes),
		XmRString, XmHTML_DEFAULT_FONT_SCALABLE_SIZES
	},
	{
		XmNfontSizeFixedList,
		XmCString, XmRString,
		sizeof(String), Offset(font_sizes_fixed),
		XmRString, XmHTML_DEFAULT_FONT_FIXED_SIZES
	},
	{
		XmNhorizontalScrollBar,
		XmCHorizontalScrollBar, XmRWidget,
		sizeof(Widget), Offset(hsb),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNimageEnable,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(images_enabled),
		XmRString, "True"
	},
	{
		XmNmaxImageColors,
		XmCMaxImageColors, XmRInt,
		sizeof(int), Offset(max_image_colors),
		XmRString, "0"
	},
	{
		XmNscreenGamma,
		XmCScreenGamma, XmRFloat,
		sizeof(float), Offset(screen_gamma),
		XmRString, Stringify(XmHTML_DEFAULT_GAMMA)
	},
	{
		XmNimageProc,
		XmCImageProc, XmRPointer,
		sizeof(XmImageProc), Offset(image_proc),
		XmRImmediate, (XmImageProc*)NULL
	},
	{
		XmNdecodeGIFProc,
		XmCDecodeGIFProc, XmRPointer,
		sizeof(XmImageGifProc), Offset(gif_proc),
		XmRImmediate, (XmImageGifProc*)NULL
	},
	{
		XmNprogressiveReadProc,
		XmCProgressiveReadProc, XmRPointer,
		sizeof(XmHTMLGetDataProc), Offset(get_data),
		XmRImmediate, (XmHTMLGetDataProc*)NULL
	},
	{
		XmNprogressiveEndProc,
		XmCProgressiveEndProc, XmRPointer,
		sizeof(XmHTMLEndDataProc), Offset(end_data),
		XmRImmediate, (XmHTMLEndDataProc*)NULL
	},
	{
		XmNeventProc,
		XmCEventProc, XmRPointer,
		sizeof(XmHTMLEventProc), Offset(event_proc),
		XmRImmediate, (XmHTMLEventProc*)NULL
	},
	{
		XmNscriptProc,
		XmCScriptProc, XmRPointer,
		sizeof(XmHTMLScriptProc), Offset(script_proc),
		XmRImmediate, (XmHTMLScriptProc*)NULL
	},
	{
		XmNprogressiveInitialDelay,
		XmCProgressiveInitialDelay, XmRInt,
		sizeof(int), Offset(plc_delay),
		XmRImmediate, (XtPointer)PLC_DEFAULT_DELAY
	},
	{
		XmNprogressiveMinimumDelay,
		XmCProgressiveMinimumDelay, XmRInt,
		sizeof(int), Offset(plc_min_delay),
		XmRImmediate, (XtPointer)PLC_MIN_DELAY
	},
	{
		XmNprogressiveMaximumDelay,
		XmCProgressiveMaximumDelay, XmRInt,
		sizeof(int), Offset(plc_max_delay),
		XmRImmediate, (XtPointer)PLC_MAX_DELAY
	},
	{
		XmNperfectColors,
		XmCEnableMode,
		XmREnableMode,
		sizeof(XtEnum), Offset(perfect_colors),
		XmRImmediate, (XtPointer)XmAUTOMATIC
	},
	{
		XmNuncompressCommand,
		XmCString, XmRString,
		sizeof(String), Offset(zCmd),
		XmRString, "uncompress"
	},
	{
		XmNresizeHeight,
		XmCResizeHeight, XmRBoolean,
		sizeof(Boolean), Offset(resize_height),
		XmRString, "False"
	},
	{
		XmNresizeWidth,
		XmCResizeWidth, XmRBoolean,
		sizeof(Boolean), Offset(resize_width),
		XmRString, "False"
	},
	{
		XmNscrollBarDisplayPolicy,
		XmCScrollBarDisplayPolicy, 
		XmRScrollBarDisplayPolicy,
		sizeof(XtEnum), Offset(sb_policy),
		XmRImmediate, (XtPointer)XmAS_NEEDED
	},
	{
		XmNscrollBarPlacement,
		XmCScrollBarPlacement, 
		XmRScrollBarPlacement,
		sizeof(XtEnum), Offset(sb_placement),
		XmRImmediate, (XtPointer)XmBOTTOM_RIGHT
	},
	{
		XmNmarginHeight,
		XmCMarginHeight, XmRDimension,
		sizeof(Dimension), Offset(margin_height),
		XmRImmediate, (XtPointer)XmHTML_DEFAULT_MARGIN 
	},
	{
		XmNmarginWidth,
		XmCMarginWidth, XmRDimension,
		sizeof(Dimension), Offset(margin_width),
		XmRImmediate, (XtPointer)XmHTML_DEFAULT_MARGIN
	},
	{
		XmNstringDirection,
		XmCStringDirection, 
		XmRStringDirection,
		sizeof(XtEnum), Offset(string_direction),
		XmRImmediate, (XtPointer)XmSTRING_DIRECTION_L_TO_R
	},
	{
		XmNstrictHTMLChecking,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(strict_checking),
		XmRString, "False"
	},
	{
		XmNenableOutlining,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(enable_outlining),
		XmRString, "False"
	},
	{
		XmNtopLine,
		XmCTopLine, XmRCardinal,
		sizeof(Cardinal), Offset(top_line),
		XmRString, "0"
	},
	{
		XmNvalue,
		XmCValue, XmRString,
		sizeof(String), Offset(value),
		XmRString, (String)NULL
	},
	{
		XmNverticalScrollBar,
		XmCVerticalScrollBar, XmRWidget,
		sizeof(Widget), Offset(vsb),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNworkWindow,
		XmCWorkWindow, XmRWidget,
		sizeof(Widget), Offset(work_area),
		XmRImmediate, (XtPointer)NULL
	},	
	{
		XmNenableBadHTMLWarnings,
		XmCHTMLWarningMode, XmRHTMLWarningMode,
		sizeof(Byte), Offset(bad_html_warnings),
		XmRImmediate, (XtPointer)XmHTML_ALL
	},
	{
		XmNenableBodyColors,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(body_colors_enabled),
		XmRString, "True"
	},
	{
		XmNenableBodyImages,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(body_images_enabled),
		XmRString, "True"
	},
	{
		XmNenableDocumentColors,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(allow_color_switching),
		XmRString, "True"
	},
	{
		XmNenableDocumentFonts,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(allow_font_switching),
		XmRString, "True"
	},
	{
		XmNenableFormColors,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(allow_form_coloring),
		XmRString, "True"
	},
	{
		XmNimagemapBoundingBoxForeground,
		XmCForeground, XmRPixel,
		sizeof(Pixel), Offset(imagemap_fg),
		XmRString, "White"
	},
	{
		XmNimagemapDrawBoundingBoxes,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(imagemap_draw),
		XmRString, "False"
	},
	{
		XmNrepeatDelay,
		XmCRepeatDelay, XmRInt,
		sizeof(int), Offset(repeat_delay),
		XmRImmediate, (XtPointer)XmHTML_DEFAULT_REPEAT_DELAY
	},
	{
		XmNsmoothScrolling,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(smooth_scroll),
		XmRString, "True"
	},
	{
		XmNfreezeAnimations,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(freeze_animations),
		XmRString, "False"
	},
	{
		XmNmimeType,
		XmCString, XmRString,
		sizeof(String), Offset(mime_type),
		XmRString, "text/html"
	},
	{
		XmNimageMapToPalette,
		XmCConversionMode,
		XmRConversionMode,
		sizeof(XtEnum), Offset(map_to_palette),
		XmRImmediate, (XtPointer)XmDISABLED
	},
	{
		XmNimagePalette,
		XmCString, XmRString,
		sizeof(String), Offset(palette),
		XmRString, (String)NULL
	},
	{
		XmNbodyImage,
		XmCString, XmRString,
		sizeof(String), Offset(def_body_image_url),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNalphaChannelProcessing,
		XmCEnableMode,
		XmREnableMode,
		sizeof(XtEnum), Offset(alpha_processing),
		XmRImmediate, (XtPointer)XmALWAYS
	},
	{
		XmNimageRGBConversion,
		XmCConversionMode,
		XmRConversionMode,
		sizeof(XtEnum), Offset(rgb_conv_mode),
		XmRImmediate, (XtPointer)XmBEST
	},
	{
		XmNclientData,
		XmCClientData, XmRPointer,
		sizeof(XtPointer), Offset(client_data),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNloadType,
		XmCLoadType, XmRLoadType,
		sizeof(XtEnum), Offset(load_type),
		XmRImmediate, (XtPointer)XmLOAD_NORMAL
	},
	{
		XmNenableIconEntities,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(icon_entities_enabled),
		XmRString, "False"
	},
	{
		XmNiconAlignment,
		XmCVerticalAlignment, XmRVerticalAlignment,
		sizeof(XtEnum), Offset(icon_valign),
		XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
	},
	/*****
	* Resources redefined from parent classes.
	*****/
	{
		XmNhighlightColor,
		XmCHighlightColor, XmRPixel,
		sizeof(Pixel), XtOffset(XmHTMLWidget, manager.highlight_color),
		XmRString, "SteelBlue1"
	},
	{
		XmNtabWidth,
		XmCTabWidth, XmRInt,
		sizeof(int), Offset(tabwidth),
		XmRImmediate, (XtPointer)XmHTML_DEFAULT_TABWIDTH
	},
	{
		XmNxResolution,
		XmCYResolution, XmRInt,
		sizeof(int), Offset(res_x),
		XmRImmediate, (XtPointer)0
	},
	{
		XmNyResolution,
		XmCYResolution, XmRInt,
		sizeof(int), Offset(res_y),
		XmRImmediate, (XtPointer)0
	}

	/*****
	* Debugging resources.
	*****/
#ifdef DEBUG
	,{
		XmNdebugDisableWarnings,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(debug_disable_warnings),
		XmRString, "False"
	},
	{
		XmNdebugEnableFullOutput,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(debug_full_output),
		XmRString, "False"
	},
	{
		XmNdebugSaveClipmasks,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(debug_save_clipmasks),
		XmRString, "False"
	},
	{
		XmNdebugFilePrefix,
		XmCString, XmRString,
		sizeof(String), Offset(debug_prefix),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNdebugLevels,
		XmCString, XmRString,
		sizeof(String), Offset(debug_levels),
		XmRImmediate, (XtPointer)NULL
	},
	{
		XmNdebugNoAnimationLoopCount,
		XmCBoolean, XmRBoolean,
		sizeof(Boolean), Offset(debug_no_loopcount),
		XmRString, (XtPointer)False
	}
#endif
};

/* Don't add anything after this endif! */
#endif /* _resources_h_ */
