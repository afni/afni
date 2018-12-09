/*****
* escapes.h : HTML iso escape code table
*
* This file Version	$Revision$
*
* Creation date:		Tue Nov 19 05:13:58 GMT+0100 1996
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
* Revision 1.1  2011/06/30 16:08:57  rwcox
* Cadd
*
* Revision 1.2  1997/01/09 06:56:05  newt
* expanded copyright marker
*
* Revision 1.1  1996/12/19 02:17:20  newt
* Initial Revision
*
*****/

#ifndef _escapes_h_
#define _escapes_h_

typedef struct{
	char *escape;	/* escape character sequence */
	char token;		/* corresponding iso-char */
	int len;		/* length of escape sequence */
}escape_data;

/*
* List of all possible HTML escape codes. This list has been alphabetically
* sorted to speed up the search process.
* This list contains 198 elements. The last element is the NULL element.
* The first part of this table contains the hash escapes, the second part
* contains the named entity escapes.
*/
#define NUM_ESCAPES	198
static escape_data escapes[NUM_ESCAPES] = {
	{"#160;",		'\240', 5},
	{"#161;",		'\241', 5},
	{"#162;",		'\242', 5},
	{"#163;",		'\243', 5},
	{"#164;",		'\244', 5},
	{"#165;",		'\245', 5},
	{"#166;",		'\246', 5},
	{"#167;",		'\247', 5},
	{"#168;",		'\250', 5},
	{"#169;",		'\251', 5},
	{"#170;",		'\252', 5},
	{"#171;",		'\253', 5},
	{"#172;",		'\254', 5},
	{"#173;",		'\255', 5},
	{"#174;",		'\256', 5},
	{"#175;",		'\257', 5},
	{"#176;",		'\260', 5},
	{"#177;",		'\261', 5},
	{"#178;",		'\262', 5},
	{"#179;",		'\263', 5},
	{"#180;",		'\264', 5},
	{"#181;",		'\265', 5},
	{"#182;",		'\266', 5},
	{"#183;",		'\267', 5},
	{"#184;",		'\270', 5},
	{"#185;",		'\271', 5},
	{"#186;",		'\272', 5},
	{"#187;",		'\273', 5},
	{"#188;",		'\274', 5},
	{"#189;",		'\275', 5},
	{"#190;",		'\276', 5},
	{"#191;",		'\277', 5},
	{"#192;",		'\300', 5},
	{"#193;",		'\301', 5},
	{"#194;",		'\302', 5},
	{"#195;",		'\303', 5},
	{"#196;",		'\304', 5},
	{"#197;",		'\305', 5},
	{"#198;",		'\306', 5},
	{"#199;",		'\307', 5},
	{"#200;",		'\310', 5},
	{"#201;",		'\311', 5},
	{"#202;",		'\312', 5},
	{"#203;",		'\313', 5},
	{"#204;",		'\314', 5},
	{"#205;",		'\315', 5},
	{"#206;",		'\316', 5},
	{"#207;",		'\317', 5},
	{"#208;",		'\320', 5},
	{"#209;",		'\321', 5},
	{"#210;",		'\322', 5},
	{"#211;",		'\323', 5},
	{"#212;",		'\324', 5},
	{"#213;",		'\325', 5},
	{"#214;",		'\326', 5},
	{"#215;",		'\327', 5},
	{"#216;",		'\330', 5},
	{"#217;",		'\331', 5},
	{"#218;",		'\332', 5},
	{"#219;",		'\333', 5},
	{"#220;",		'\334', 5},
	{"#221;",		'\335', 5},
	{"#222;",		'\336', 5},
	{"#223;",		'\337', 5},
	{"#224;",		'\340', 5},
	{"#225;",		'\341', 5},
	{"#226;",		'\342', 5},
	{"#227;",		'\343', 5},
	{"#228;",		'\344', 5},
	{"#229;",		'\345', 5},
	{"#230;",		'\346', 5},
	{"#231;",		'\347', 5},
	{"#232;",		'\350', 5},
	{"#233;",		'\351', 5},
	{"#234;",		'\352', 5},
	{"#235;",		'\353', 5},
	{"#236;",		'\354', 5},
	{"#237;",		'\355', 5},
	{"#238;",		'\356', 5},
	{"#239;",		'\357', 5},
	{"#240;",		'\360', 5},
	{"#241;",		'\361', 5},
	{"#242;",		'\362', 5},
	{"#243;",		'\363', 5},
	{"#244;",		'\364', 5},
	{"#245;",		'\365', 5},
	{"#246;",		'\366', 5},
	{"#247;",		'\367', 5},
	{"#248;",		'\370', 5},
	{"#249;",		'\371', 5},
	{"#250;",		'\372', 5},
	{"#251;",		'\373', 5},
	{"#252;",		'\374', 5},
	{"#253;",		'\375', 5},
	{"#254;",		'\376', 5},
	{"#255;",		'\377', 5},
	{"AElig;",		'\306', 6},
	{"Aacute;",		'\301', 7},
	{"Acirc;",		'\302', 6},
	{"Agrave;",		'\300', 7},
	{"Aring;",		'\305', 6},
	{"Atilde;",		'\303', 7},
	{"Auml;",		'\304', 5},
	{"Ccedil;",		'\307', 7},
	{"ETH;",		'\320', 4},
	{"Eacute;",		'\311', 7},
	{"Ecirc;",		'\312', 6},
	{"Egrave;",		'\310', 7},
	{"Euml;",		'\313', 5},
	{"Iacute;",		'\315', 7},
	{"Icirc;",		'\316', 6},
	{"Igrave;",		'\314', 7},
	{"Iuml;",		'\317', 5},
	{"Ntilde;",		'\321', 7},
	{"Oacute;",		'\323', 7},
	{"Ocirc;",		'\324', 6},
	{"Ograve;",		'\322', 7},
	{"Oslash;",		'\330', 7},
	{"Otilde;",		'\325', 7},
	{"Ouml;",		'\326', 5},
	{"THORN;",		'\336', 6},
	{"Uacute;",		'\332', 7},
	{"Ucirc;",		'\333', 6},
	{"Ugrave;",		'\331', 7},
	{"Uuml;",		'\334', 5},
	{"Yacute;",		'\335', 7},
	{"aacute;",		'\341', 7},
	{"acirc;",		'\342', 6},
	{"acute;",		'\264', 6},
	{"aelig;",		'\346', 6},
	{"agrave;",		'\340', 7},
	{"amp;",		'&',	4},
	{"aring;",		'\345', 6},
	{"atilde;",		'\343', 7},
	{"auml;",		'\344', 5},
	{"brvbar;",		'\246', 7},
	{"ccedil;",		'\347', 7},
	{"cedil;",		'\270', 6},
	{"cent;",		'\242', 5},
	{"copy;",		'\251', 5},
	{"curren;",		'\244', 7},
	{"deg;",		'\260', 4},
	{"divide;",		'\367', 7},
	{"eacute;",		'\351', 7},
	{"ecirc;",		'\352', 6},
	{"egrave;",		'\350', 7},
	{"eth;",		'\360', 4},
	{"euml;",		'\353', 5},
	{"frac12;",		'\275', 7},
	{"frac14;",		'\274', 7},
	{"frac34;",		'\276', 7},
	{"gt;",			'>',	3},
	{"hibar;",		'\257', 6},
	{"iacute;",		'\355', 7},
	{"icirc;",		'\356', 6},
	{"iexcl;",		'\241', 6},
	{"igrave;",		'\354', 7},
	{"iquest;",		'\277', 7},
	{"iuml;",		'\357', 5},
	{"laquo;",		'\253', 6},
	{"lt;",			'<',	3},
	{"macr;",		'\257', 5},
	{"micro;",		'\265', 6},
	{"middot;",		'\267', 7},
	{"nbsp;",		'\240', 5},
	{"not;",		'\254', 4},
	{"ntilde;",		'\361', 7},
	{"oacute;",		'\363', 7},
	{"ocirc;",		'\364', 6},
	{"ograve;",		'\362', 7},
	{"ordf;",		'\252', 5},
	{"ordm;",		'\272', 5},
	{"oslash;",		'\370', 7},
	{"otilde;",		'\365', 7},
	{"ouml;",		'\366', 5},
	{"para;",		'\266', 5},
	{"plusmn;",		'\261', 7},
	{"pound;",		'\243', 6},
	{"quot;",		'\"',	5},
	{"raquo;",		'\273', 6},
	{"reg;",		'\256', 4},
	{"sect;",		'\247', 5},
	{"shy;",		'\255', 4},
	{"sup1;",		'\271', 5},
	{"sup2;",		'\262', 5},
	{"sup3;",		'\263', 5},
	{"szlig;",		'\337', 6},
	{"thorn;",		'\376', 6},
	{"times;",		'\327', 6},
	{"uacute;",		'\372', 7},
	{"ucirc;",		'\373', 6},
	{"ugrave;",		'\371', 7},
	{"uml;",		'\250', 4},
	{"uuml;",		'\374', 5},
	{"yacute;",		'\375', 7},
	{"yen;",		'\245', 4},
	{"yuml;",		'\377', 5},
	{NULL,			0,		0}
};

/* Don't add anything after this endif! */
#endif /* _escapes_h_ */
