%A script for testing function New_HEAD
   iopt = input('Enter test number [1 or 2]: ');
   if (iopt ~= 1 & iopt ~= 2),
      fprintf(1,'Must choose either 1 or 2. Not %g\n', iopt);
      return;
   end
   opt = sprintf('test%d', iopt);
   FuncName = sprintf('new_head_%s', opt);
   if (strcmp(opt,'test1')),
      echo on
      %say you have a matrix of a particular size
      M = flow(100);
      %now to put it in a dataset
      optt.prefix = sprintf('test1_%s', FuncName);
      unix(sprintf('rm -f %s*.????  >& /dev/null', optt.prefix));
      optt.dimen  = size(M);
      optt.orient = 'RAI';
      [err,Info1, optt] = New_HEAD(optt); %note that we're also returning optt , some fields get added in New_HEAD for convenience
      if (err),
         fprintf(1,'Error %s:\nFailed in New_HEAD\n', FuncName);
         return;
      end
      [e,m,i] = WriteBrik(M,Info1,optt);
      
      %now write the output as floats
      optt.prefix = sprintf('test1_float_%s', FuncName);
      unix(sprintf('rm -f %s*.????  >& /dev/null', optt.prefix));
      optt.datum = 'float';
      [err,Info2, optt] = New_HEAD(optt);
      if (err),
         fprintf(1,'Error %s:\nFailed in New_HEAD\n', FuncName);
         return;
      end

      [e,m,i] = WriteBrik(M,Info2,optt);
      
      echo off
      fprintf(1,'\nTo view results, try:\n\n');
      fprintf(1,'  unix(''afni -yesplugouts %s.HEAD %s.HEAD &'');\n', Info1.RootName, Info2.RootName);
      fprintf(1,'  i = 1; cs(i) = NewCs(''Set_Function'', '''', %s); i = i + 1;\n', Info2.RootName);
      fprintf(1,'  cs(i) = NewCs(''See_Overlay'', '''', ''+''); i = i + 1;\n');
      fprintf(1,'  cs(i) = NewCs(''open_window'', '''', ''axialimage'', ''mont=2x2:8 keypress=v geom=500x500+800+50''); i = i+1;\n');
      fprintf(1,'  err = TellAfni(cs); clear cs\n');
      fprintf(1,'\n\n');
      fprintf(1,'  Open all three views and click on "See Overlay"\n');
      return;
   elseif (strcmp(opt,'test2')),
      echo on
      %Now let us say we have a timeseries of matrices
      n=30; N = 40;
      M = zeros(n,2*n,n,N);
      for (i=1:1:N),
         M(:,:,:,i) = flow(n).*cos(i./N*2.0.*pi)+randn(n,2*n,n)./2.0;
      end
      %Create a multi-brick dataset
      optt.prefix = sprintf('test2_bucket%s', FuncName);
      unix(sprintf('rm -f %s*.????  >& /dev/null', optt.prefix));
      optt.dimen  = size(M);
      optt.orient = 'RAI';
      [err,Info1, optt] = New_HEAD(optt); %note that we're also returning optt , some fields get added in New_HEAD for convenience
      if (err),
         fprintf(1,'Error %s:\nFailed in New_HEAD\n', FuncName);
         return;
      end
      [e,m,i] = WriteBrik(M,Info1,optt);
      %make output a time series of TR 2.0s
      optt.tr = 2.0;
      optt.prefix = sprintf('test2_TS%s', FuncName);
      unix(sprintf('rm -f %s*.????  >& /dev/null', optt.prefix));
      optt.dimen  = size(M);
      optt.orient = 'RAI';
      [err,Info2, optt] = New_HEAD(optt); %note that we're also returning optt , some fields get added in New_HEAD for convenience
      if (err),
         fprintf(1,'Error %s:\nFailed in New_HEAD\n', FuncName);
         return;
      end
      [e,m,i] = WriteBrik(M,Info2,optt);
      %write it in TLRC
      optt.view = '+tlrc';       %note that nothing special is done to the coords here
                                 %You will have to set the origin by hand. Better off using
                                 %a tlrc master assuming you have a resolution match
      [err,Info2, optt] = New_HEAD(optt);
      [e,m,i] = WriteBrik(M,Info2,optt);
      
      echo off
      fprintf(1,'\nTo view results, try:\n');
      fprintf(1,'  unix(''afni -yesplugouts %s.HEAD %s.HEAD &'');\n', Info1.RootName, Info2.RootName);
      
      
   else
      fprintf(1,'Error %s:\nOpt = %s is invalid\n', FuncName, opt);
      return;
   end
return;
