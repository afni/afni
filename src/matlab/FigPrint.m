function [err] = FigPrint (Opt, colopt)
%
%   mode 1:
%   [err] = FigPrint ([Opt])
%   mode 2:
%   [err] = FigPrint (h, [col])
%Purpose:
%   Prints the figure
%   
%   
%Input Parameters:
%   Usage mode 1:
%   Opt is an optional structure with the following optional fields
%      .Handle : Handle to the current figure to print. 
%              Default is the current figure
%      .Color : Choose 'c' for color  printer
%                      'b' for bw (default)  printer
%      .Layout : Layout of the figure, choose from
%               'p' for portrait 
%               'l' for landscape
%               't' for tall (print to fit)(default)
%      .GUI : flag (0/1) uses some special recommended settings to print GUIs
%             Default is 0
%      .Prompt : flag (0/1/2). Asks if you want to print the figure. Default is 0
%                 1 is command line prompt, 2 is GUI prompt 
%      .PrintOpt : a string containing additional printing options such as 
%                '-zbuffer -r200' for example, default is ''
%      .PaperPosition : a four element vector specifying the position
%               of the figure when printed on the page like
%               [0.25 0.25 4 6] (for [left bottom width height])
%               the units are inches
%      .as: 'jpg', or 'pdf' , prints electronic instead of paper version
%      .prefix: prefix for electronic printout.
%    Usage mode 2:
%       h is the figure handle (h can be a vector of handles)
%       col is either 'b' for bw or 'c' for color
%   
%Output Parameters:
%   err : 0 No Problem
%       : 1 Mucho Problems
%   
%   
%      
%More Info :
%   print, printopt
%   
%   
%
%     Author : Ziad Saad
%     Date : Fri Jun 05 13:49:26 CDT 1998 


%Define the function name for easy referencing
FuncName = 'FigPrint';

%initailize return variables
err = 1;

%Set up the default Options
OptDef.Handle = gcf;
OptDef.Color = 'b';
OptDef.Layout = 't';
OptDef.GUI = 0;
OptDef.Prompt = 0;
OptDef.PrintOpt = '';
OptDef.PaperPosition = [];
OptDef.as = [];
OptDef.prefix = 'FigPrint';

%if nothing was passed
if (nargin == 0),
	Opt = OptDef;
else
   if (~isstruct(Opt)),
      h = Opt; Opt = []; Opt.Handle = h;
      if (nargin == 2),
         Opt.Color = colopt;
      end
   end
	if (~isfield(Opt,'Handle') | isempty (Opt.Handle)),	Opt.Handle = OptDef.Handle;	end
	if (~isfield(Opt,'Color') | isempty (Opt.Color)),	Opt.Color = OptDef.Color;	end
	if (~isfield(Opt,'Layout') | isempty (Opt.Layout)),	Opt.Layout = OptDef.Layout;	end
	if (~isfield(Opt,'GUI') | isempty (Opt.GUI)),	Opt.GUI = OptDef.GUI;	end
	if (~isfield(Opt,'Prompt') | isempty (Opt.Prompt)),	Opt.Prompt = OptDef.Prompt;	end
	if (~isfield(Opt,'PrintOpt') | isempty (Opt.PrintOpt)),	Opt.PrintOpt = OptDef.PrintOpt;	end
	if (~isfield(Opt,'PaperPosition') | isempty (Opt.PaperPosition)),	Opt.PaperPosition = []; end
	if (~isfield(Opt,'as') | isempty (Opt.as)),	Opt.as = []; end
	if (~isfield(Opt,'prefix') | isempty (Opt.prefix)),	Opt.prefix = 'FigPrint'; end
end
	

for (kk=1:1:length(Opt.Handle)),
   figh = Opt.Handle(kk);
   
   if (Opt.Prompt == 1),
	   figure (figh);
	   s = input ('Print figure (y/n) ?','s');
	   if (isempty(s) | s ~= 'y'), err = 0; return;	end
   elseif (Opt.Prompt == 2),
	   figure (figh);
	   Butt = questdlg(sprintf('Print figure %d?', figh), 'Print Figure', 'Yes', 'No', 'No');
	   switch Butt,
		   case 'No',
			   err = 0; 
			   return;
	   end
   end

   %Now form the command line
	   %The handle
	   Hndl = sprintf('-f%g',figh);

	   %The layout
	   if (Opt.Layout == 'p'),
	     orient(figh, 'portrait');
	   elseif (Opt.Layout == 'l'),
	     orient(figh, 'landscape');
	   elseif (Opt.Layout == 't'),
	     orient(figh, 'tall');
	   else 
	     fprintf (2,'%s: Wrong layout specification\n\a', mfilename);
	     err = 1;
	     return;
	   end

	   %The colors
	   [e,mc] = unix('hostname -s'); ilt = find(isletter(mc)); mc = mc(ilt);
	   switch mc,
	   case 'rhodium',
		   if (Opt.Color == 'b') 
		     prntype = '-Plp';
		     prndevice = '-dps2';
		   elseif (Opt.Color == 'c')
		     prntype = '-Plpc';
		     prndevice = '-dpsc2';
		   else 
		     fprintf (2,'%s: Wrong printer (Color) specification\n\a', mfilename);
		     err = 1;
		     return;
		   end
	   case 'eomer',
		   if (Opt.Color == 'b') 
		     prntype = '-P_128.231.212.205';
		     prndevice = '-dps2';
		   elseif (Opt.Color == 'c')
		     prntype = '-PPhaser_8550DT__00_00_aa_9b_d2_6b_';
		     prndevice = '-dpsc2';
		   else 
		     fprintf (2,'%s: Wrong printer (Color) specification\n\a', mfilename);
		     err = 1;
		     return;
		   end
	   case 'iodine',
		   if (Opt.Color == 'b') 
		     prntype = '-PHP8100DN';
		     prndevice = '-dps2';
		   elseif (Opt.Color == 'c')
		     prntype = '-PTek';
		     prndevice = '-dpsc2';
		   else 
		     fprintf (2,'%s: Wrong printer (Color) specification\n\a', mfilename);
		     err = 1;
		     return;
		   end	
	   otherwise,
		   fprintf (2,'%s: Machine %s not recognized.\n\a',mfilename , mc);
		   err = 1;
		   return
	   end
	   %The GUI settings
	   if (Opt.GUI),
		   set(figh,'PaperPositionMode','auto'); 
		   set(figh,'InvertHardcopy','off');
	   end

	   %the size settings
	   if (~isempty(Opt.PaperPosition)),
		   set(figh,'PaperUnits','inches');
		   set(figh,'PaperPosition', Opt.PaperPosition);
	   end

	   if (isempty(Opt.as)),
            CmndLine = sprintf ('print %s %s %s %s',prndevice, Opt.PrintOpt, prntype, Hndl);

       else
           CmndLine = sprintf ('saveas(%d, ''%s'' ,''%s'')', figh, sprintf('%s.%02d.%s', Opt.prefix, kk, Opt.as), Opt.as);
       end
                   fprintf (1,'%s verbose : Executing %s ...\n',FuncName,CmndLine);

            eval([CmndLine]);
	end
   
	err = 0;
	return;

