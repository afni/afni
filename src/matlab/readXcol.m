%
% a script to show and sample default colors on the X windows system
% default colors are read from myrgb.txt
% see also ROIcmap
%
ColFile = fopen ('myrgb.txt', 'r');
N = 752;

name(N).s = '';
name(N).r = 0;
name(N).g = 0;
name(N).b = 0;
for (i=1:1:N),
name(i).r = fscanf(ColFile, '%g ', 1);
name(i).g = fscanf(ColFile, '%g ', 1);
name(i).b = fscanf(ColFile, '%g ', 1);
name(i).s = fgets(ColFile);
end

fclose (ColFile);

Mall = zeros (N, 3);
Mall(:,1) = [name(:).r]';
Mall(:,2) = [name(:).g]';
Mall(:,3) = [name(:).b]';
Mall = Mall./255;

if (0),
for (i=6:1:N-5),
   M = zeros (11,3);
   for (j=-5:1:5)
      M(j+6,1) = name(i+j).r./255;
      M(j+6,2) = name(i+j).g./255;
      M(j+6,3) = name(i+j).b./255;
   end
   colormap(M);
   subplot 211;
	image ([1:1:length(M(:,1))]);   
   input ('Hit Enter:', 's');
end

end
figure(1), clf;
subplot (211); 
str = sprintf('Pick colors (background then foreground) with mouse\nHit "enter" to quit');
colormap(Mall); image ([1:1:N]); title (str, 'fontsize',14);
drawnow;
i = 0;
subplot (269);cla

x1 = 1;
while (~isempty(x1)),
   [x1,y] = ginput (1);
   if (~isempty(x1)),
      x1 = floor(x1(1)-0.5)+1;
      subplot (269);
      addsquare([0 i], [2.5 1+i], Mall(x1,:)); hold on
      plot (-0.2, i+0.5, 'k*');
      axis ([-1 3 -1 11]);
      str = sprintf ('back: %s', name(x1).s);
      title (str);
      [x2,y] = ginput (1);
      x2 = floor(x2(1)-0.5)+1;
      subplot (269);
      str = sprintf ('SUMA (%g %g)', x1, x2 );
      ht = text (0.3, 0.3+i, 0, str, 'fontsize',14, 'color', Mall(x2,:));
      str = sprintf ('back: %s (%g), fore: %s (%g)', name(x1).s, x1, name(x2).s, x2);
      title (str);
      i = rem(i +1, 10);
   end
end


