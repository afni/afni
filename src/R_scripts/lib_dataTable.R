## top ##########################
## data table checker library
## Justin Rajendra ~March 2023
## all functions to add to dataTable_checker.R 
## can add to all Gang stats programs

## load libraries ########
suppressPackageStartupMessages(library(data.table))

## global log text #############
log.out <- c()  ## not the greatest method...
log.name <- "temp_log.txt"

## some declared variables #######
respVar <- c('InputFile','Inputfile','inputFile','inputfile',
             'Ausgang_val','ausgang_val')   ## for misspellings 

## mode ########
mode_func <- function(x) {
    uniqx <- unique(x) ; return(uniqx[which.max(tabulate(match(x,uniqx)))])
}

## log file prefix name #################
dtCheck_log_name <- function(name.in=log.name){
    
    ## split file and path
    temp.name <- basename(name.in)
    temp.dir <- dirname(name.in)   ## returns "." if there is no path
    
    ## strip off the extension if there is one (must be a better way to do this)
    if( endsWith(temp.name,".nii") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar(".nii"))
    } else if( endsWith(temp.name,".nii.gz") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar(".nii.gz"))
    } else if( endsWith(temp.name,"+orig") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar("+orig"))
    } else if( endsWith(temp.name,"+tlrc") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar("+tlrc"))
    } else if( endsWith(temp.name,"+orig.gz") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar("+orig.gz"))
    } else if( endsWith(temp.name,"+tlrc.gz") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar("+tlrc.gz"))
    } else if( endsWith(temp.name,".niml") ){
        temp.name <- substr(temp.name,1,nchar(temp.name)-nchar(".niml"))
    }
    
    ## combine and save out to global variable...
    log.name <<- paste0(temp.dir,"/",temp.name,"_log.txt")

}   ## end dtCheck_log_name

## add to log text and print the line ##############
## put as many \n as you want for the output
dtCheck_log_print <- function(line.in,cat.out=TRUE){
    log.out <<- rbind(log.out,line.in)  ## log.out is global
    if( cat.out ){ cat(line.in) }
}

## takes a data frame and adds line by line to log.out. Does not print to screen
dtCheck_log_df <- function(df.in){
    for( i in 1:nrow(df.in) ){
        ## combine all in row then with \n
        line.out <- paste(as.character(df.in[i,]),collapse="")
        line.out <- paste0(line.out,"\n")
        log.out <<- rbind(log.out,line.out)   ## add to global
    }
    log.out <<- rbind(log.out,"\n")
}

## create output of bad files with index and new lines
## must have some bad files
## takes data frame, output of some 3dinfo
## a number to compare against and if it should equal or not equal (T/F)
## returns nothing, prints to screen and adds to log.out
dtCheck_bad_files <- function(data.in,cmd.num,bad.num=0,equals=TRUE){
    
    ## get the bad indices
    if( equals ){
        bad.ind <- which(cmd.num == bad.num)
    } else {
        bad.ind <- which(cmd.num != bad.num)
    }
    
    ## get the longest number of digits and pad the index
    max.dig <- max(nchar(bad.ind))
    ind.out <- paste0("[",bad.ind,"] ")
    ind.out <- formatC(ind.out,width=max.dig+1,flag="-")

    ## get the file names at the bad indices
    bad.dsets <- data.in$InputFile[bad.ind]

    ## create lines
    tab.out <- c()
    for( i in 1:length(bad.ind) ){
        temp.line <- paste0(ind.out[i],data.in$InputFile[bad.ind[i]])
        tab.out <- rbind(tab.out,temp.line)
    }
    
    ## output
    dtCheck_err("Datasets not found",1)
    dtCheck_log_df(tab.out)  ## save to log
    cat(tab.out,sep="\n")    ## print to screen
    cat('\n')
}   ## end dtCheck_bad_files

## write log.out to file
dtCheck_write_log <- function(file.out=log.name){
    cat(log.out,file=file.out,sep="")  ## log.out is global
}

## cat messages #################
## nl is new line feeds defaults to 2 otherwise 1
dtCheck_err <- function(msg,nl=2){ 
    if( nl == 2 ){ end.line <- "!!!\n\n" } else { end.line <- "!!!\n" }
    dtCheck_log_print(paste0("**ERROR: ",msg,end.line)) 
}
dtCheck_warn <- function(msg,nl=2){ 
    if( nl == 2 ){ end.line <- "!\n\n" } else { end.line <- "!\n" }
    dtCheck_log_print(paste0("+*Warning: ",msg,end.line)) 
}
dtCheck_good <- function(msg,nl=2){   ## need your own ending punctuation
    if( nl == 2 ){ end.line <- "\n\n" } else { end.line <- "\n" }
    dtCheck_log_print(paste0("++Good: ",msg,end.line)) 
}

## add leading spaces to the data frame to aid in aligning text #########
## takes a valid data frame and outputs the same NOT FRAME with padded spaces
dtCheck_lead_space <- function(data.in){
    
    ## empty to fill
    data.temp <- c() ; name.temp <- c()
    
    for( i in 1:length(data.in) ){
        
        ## stuff to change
        t.col <- as.character(data.in[[i]])
        t.name <- names(data.in)[i]
        
        ## get max number of characters
        t.max <- max(c(nchar(t.col),nchar(t.name)))
        
        ## pad with spaces (left justified)
        t.col <- formatC(t.col,width=t.max+1,flag="-")
        t.name <- formatC(t.name,width=t.max+1,flag="-")
        
        ## add to output
        data.temp <- cbind(data.temp,t.col)
        name.temp <- cbind(name.temp,t.name)
    }
    
    data.temp <- rbind(name.temp,data.temp)
    return(data.temp)
    
}   ## end dtCheck_lead_space function

## convert command line flat string to data frame ##############
## taken from 3dMVM (not sure this is the best way to do it...)
dtCheck_str2frame <- function(flat.in){
    wd <- which(flat.in %in% respVar) ; len <- length(flat.in)
    if(len %% wd != 0){
        dtCheck_err("dataTable is NOT regular and rectangular")
        dtCheck_write_log(log.name) ## write out to file
        q()
    }
    
    data.out <- NULL
    for(i in 1:wd){
        data.out <- data.frame(cbind(data.out,flat.in[seq(wd+i, len, wd)]))
    }
    names(data.out) <- flat.in[1:wd]
    return(data.out)
}   ## end dtCheck_str2frame

## try to read a dataTable FILE ######################
## needs the dataTable file name (with path or relative)
## does not save the imported data. Just cats info to the screen and log
dtCheck_tryRead <- function(file.in){
    
    ## check to see if the file is really there
    if( !file.exists(file.in) ){ 
        cat(paste("**ERROR:",file.in,"not found!!!")) ; cat("\n\n") ; return(1)
    }
    
    ## which file
    dtCheck_log_print("\nChecking dataTable file:\n")
    dtCheck_log_print(paste0(basename(file.in),"\n\n"))
    
    ## read all by line
    data.str <- readLines(file.in)
    
    ## get info from the first line assuming it is a header
    hdr.line <- strsplit(data.str[1], "[, ]|[[:space:]]+")
    hdr.len <- length(hdr.line[[1]])
    
    ## empty to fill
    miss.row <- miss.num <- miss.line <- na.line <- c()
    
    ## go through each line
    for( i in 2:length(data.str) ){
        
        ## split line by comma or some spaces
        tmp.line <- strsplit(data.str[i], "[, ]|[[:space:]]+")
        
        ## check to see if there is a missing element
        ## first without delimiter for csv or either with tsv
        if( length(tmp.line[[1]]) != hdr.len ){
            if( hdr.len - length(tmp.line[[1]]) == 1 ){
                dtCheck_log_print(paste("Missing",
                                        hdr.len - length(tmp.line[[1]]),
                                        "element from line",i,"\n"))
            } else {
                dtCheck_log_print(paste("Missing",
                                        hdr.len - length(tmp.line[[1]]),
                                        "elements from line",i,"\n"))
            }
            miss.row <- c(miss.row,i)
            miss.num <- c(miss.num,hdr.len - length(tmp.line[[1]]))
            miss.line <- c(miss.line,tmp.line)
        } else {
            ## finds missing elements with delimiter (empty cells) and NAs
            miss.ele <- found.na <- c()
            for( j in 1:hdr.len ){
                if( tmp.line[[1]][j] == "" ){ miss.ele <- c(miss.ele,j) }
                if( tmp.line[[1]][j] == "NA" ){ found.na <- c(found.na,j) }
            }
            
            ## cat info for missing elements
            if( length(miss.ele) > 0 ){
                if( length(miss.ele) == 1 ){
                    dtCheck_log_print(paste("Missing",length(miss.ele),
                                            "element from line",i,"\n"))
                } else {
                    dtCheck_log_print(paste("Missing",length(miss.ele),
                                            "elements from line",i,"\n"))
                }
                miss.row <- c(miss.row,i)
                miss.num <- c(miss.num,length(miss.ele))
                miss.line <- c(miss.line,tmp.line)
            }
            
            ## cat info for NAs (may need to change this)
            if( length(found.na) > 0 ){
                dtCheck_log_print(paste("NA's found on line",i,"\n"))
                na.line <- c(na.line,i)
            }
        }
    }   ## end for each row
    
    ## check output checks
    if( length(na.line) > 0 ){
        dtCheck_log_print("\n") ; return(length(na.line))
    } else if( length(miss.row) > 0 ){ 
        dtCheck_log_print("\n") ; return(length(miss.row))
    } else {
        dtCheck_log_print("++Good: Table is regular and rectangular.\n")
        return(0)
    }
}   ## end dtCheck_tryRead

## numeric and integer summary text ################
## takes valid int or numeric vector and outputs text to screen
num_int_sum <- function(data.in){
    
    ## check for outliers
    col.outlier <- boxplot.stats(data.in)$out
    if( length(col.outlier) > 0 ){ 
        col.out <- paste0(" | Num outliers=",length(col.outlier))
    } else { col.out <- "" }
    
    ## get range, mean, median, stdev (should no have NAs)
    col.range <- range(data.in,na.rm=TRUE)
    # col.mean <- prettyNum(mean(data.in,na.rm=TRUE))
    # col.med <- prettyNum(median(data.in,na.rm=TRUE))
    # col.sd <- prettyNum(sd(data.in,na.rm=TRUE))
    
    ## see if it could be a factor
    pos.lev <- length(levels(as.factor(as.character(data.in))))
    if( pos.lev < 5 ){
        lev.out <- paste0(" | ? categorical ? Num levels=",pos.lev)
    } else { lev.out <- "" }
    
    ## put it all together and return a string
    col.detail <- paste0("Min=",prettyNum(col.range[1]),
                         " | Max=",prettyNum(col.range[2]),
                         # " | Mean=",col.mean,
                         # " | Median=",col.med,
                         # " | StDev=",col.sd,
                         col.out,lev.out)
    return(col.detail)
}   ## end num_int_sum

## check image datasets for various things #################

## afni check if InputFile exists returns 0 for good
dtCheck_img_exists <- function(data.in){
    
    # afni.path <- dirname(system('which afni',intern=TRUE))
    
    ## get all of the input files in one long string
    in.dsets <- paste0(data.in$InputFile,collapse=" ")
    
    ## make afni command and save out result
    afni.cmd <- paste0("3dinfo -exists ",in.dsets)
    cmd.num <- system(afni.cmd,intern=TRUE)
    
    if( sum(as.numeric(cmd.num)) == nrow(data.in) ){
        dtCheck_good('All InputFiles exist.')
        return(0)
    } else {
        dtCheck_bad_files(data.in,cmd.num,0,TRUE)
        return(1)
    }
}   ## end dtCheck_img_exists

## afni check if all InputFiles have only 1 volume
dtCheck_1_vol <- function(data.in){
    
    # afni.path <- dirname(system('which afni',intern=TRUE))
    
    ## get all of the input files in one long string
    in.dsets <- paste0(data.in$InputFile,collapse=" ")
    
    ## make afni command and save out result
    afni.cmd <- paste0("3dinfo -nv ",in.dsets)
    cmd.num <- system(afni.cmd,intern=TRUE)
    
    if( sum(as.numeric(cmd.num)) == nrow(data.in) ){
        dtCheck_good('All InputFiles have exactly 1 volume.')
        return(0)
    } else {
        bad.dsets <- data.in$InputFile[which(cmd.num != 1)]
        dtCheck_err("Datasets have more than 1 volume",1)
        dtCheck_log_print(paste0(paste0(as.character(bad.dsets),
                                        collapse="\n"),"\n\n"))
        return(1)
    }
}   ## end dtCheck_1_vol

## afni check if all InputFiles are in the same grid
dtCheck_same_grid <- function(data.in){
    
    # afni.path <- dirname(system('which afni',intern=TRUE))
    
    ## get all of the input files in one long string
    in.dsets <- paste0(data.in$InputFile,collapse=" ")
    
    ## make afni command and save out result
    afni.cmd <- paste0("3dinfo -same_grid  ",in.dsets)
    cmd.num <- system(afni.cmd,intern=TRUE)
    
    if( sum(as.numeric(cmd.num)) == nrow(data.in) ){
        dtCheck_good('All InputFiles are on the same grid.')
        return(0)
    } else {
        bad.dsets <- data.in$InputFile[which(cmd.num == 0)]
        dtCheck_err("Datasets not on the same grid",1)
        dtCheck_log_print(paste0(paste0(as.character(bad.dsets),
                                        collapse="\n"),"\n\n"))
        return(1)
    }
}   ## end dtCheck_same_grid

## a little summary #########################
## takes a valid data frame
## prints a bunch of stuff to screen and returns nothing
dtCheck_printSummary <- function(data.in){
    
    dtCheck_log_print("Data summary: \n")
    
    data.detail <- c()
    ## collect some info on each variable
    for( i in 1:length(data.in) ){
        
        ## get the variable class and get a little summary
        col.class <- class(data.in[[i]])
        
        if( col.class == "factor" ){
            lev.col <- levels(data.in[[i]])
            pos.num <- !is.na(suppressWarnings(as.numeric(lev.col)))
            pos.num <- levels(data.in[[i]])[pos.num]
            if( length(pos.num) > 0 ){
                pos.num <- paste("| ? Numeric levels:",
                                 paste(pos.num,collapse=" "))
            }
            ## if there are not too many levels, print them out
            if( length(levels(data.in[[i]])) < 4 ){
                lev.var <- tapply(data.in[[i]],data.in[[i]],length)
                lev.out <- paste0(names(lev.var),"=",lev.var,collapse=" | ")
                col.detail <- paste0(lev.out," ",pos.num) 
                col.detail <- paste("Counts:",col.detail)
            } else {
                col.detail <- paste0("Unique levels=",
                                     length(levels(data.in[[i]]))," ",pos.num)
            }
        } else if( col.class == "integer" ){
            col.detail <- num_int_sum(data.in[[i]])
        } else if( col.class == "numeric" ){
            col.detail <- num_int_sum(data.in[[i]])
        } else if( col.class == "logical" ){
            t.count <- length(data.in[[i]][data.in[[i]] == TRUE])
            f.count <- length(data.in[[i]][data.in[[i]] == FALSE])
            col.detail <- paste0("TRUE=",t.count," | ","FALSE=",f.count)
        } else { col.detail <- "NA" }
        
        ## add to vector
        data.detail <- c(data.detail,col.detail)
    }
    
    ## rename classes
    class.in <- sapply(data.in,class) ; class.out <- c()
    for( i in class.in ){
        if( i %in% c("factor","logical") ){
            class.out <- c(class.out,"Categorical")
        } else if( i %in% c("integer","numeric") ){
            class.out <- c(class.out,"Quantitative")
        }
    }
    
    ## combine all stuff
    data.summary <- data.frame(Variable=names(data.in),
                               Detected_Type=class.out,Details=data.detail)
    
    ## set a long width so it prints the longer strings regardless of width
    oldoptions <- options(width=10000)
    
    ## print the data frame to screen
    print.data.frame(data.summary,row.names=FALSE,right=FALSE)
    
    ## save output to log.out global
    df.print <- dtCheck_lead_space(data.summary)
    dtCheck_log_df(df.print)
    
    # options(oldoptions) ## change the options back (not sure if this is necessary)
    
    cat("\n") ; return(0)
}   ## end dtCheck_printSummary

## check for common user errors ####################
subj_first <- function(data.in){
    if( names(data.in)[1] != "Subj" ){
        cat(paste0('\n**ERROR: First column header is "',
                   names(data.in)[1],'"'))
        cat('\n         The first column must be "Subj" !!!\n')
        return(1)
    } else { return(0) }
}   ## end subj_first

InFile_last <- function(data.in){
    if( ! (names(data.in)[length(data.in)] %in% respVar) ){
        cat(paste0('\n**ERROR: Last column header is "',
                   names(data.in)[length(data.in)],'"'))
        cat('\n         The last column must be "InputFile" or "Ausgang_val" !!!\n')
        return(1)
    } else { return(0) }
}   ## end InFile_last

InFile_dups <- function(data.in){
    
    ## make sure we have InputFiles
    if( names(data.in)[length(data.in)] != "InputFile" ){
        cat(paste0('\n**ERROR: Last column header is "',
                   names(data.in)[length(data.in)],'"'))
        cat('\n         The last column must be "InputFile" for this check !!!\n')
        return(1)
    }
    ## get the list of dup files and the line number of the files
    dup.i.file <- data.in$InputFile[duplicated(data.in$InputFile)]
    dup.index <- duplicated(data.in$InputFile)
    
    ## combine line number with names
    dup.index <- paste0("[",which(dup.index),"]")
    dup.out <- paste(dup.index,dup.i.file)
    dup.out <- paste(dup.out,collapse="\n         ")
    
    if( length(dup.i.file) > 0 ){
        cat('\n**ERROR: Duplicates found in "InputFile" column:\n')
        cat("         ") ;cat(dup.out) ; cat("\n\n")
        return(1)
    } else { return(0) }
}   ## end InFile_dups

file_subj_check <- function(data.in){
    
    ## make sure we have InputFiles and Subj
    if( names(data.in)[length(data.in)] != "InputFile" ){
        cat(paste0('\n**ERROR: Last column header is "',
                   names(data.in)[length(data.in)],'"'))
        cat('\n         The last column must be "InputFile" for this check !!!\n')
        return(1)
    }
    if( subj_first(data.in) != 0 ){
        cat(paste0('\n**ERROR: First column header is "',
                   names(data.in)[1],'"'))
        cat('\n         The first column must be "Subj" for this check !!!\n')
        return(1)
    }
    
    ## need to fix this here ################
    ## check if InputFiles are divisible by subjects
    file.subj <- length(levels(data.in$InputFile)) / length(levels(data.in$Subj))
    
    ## if not an integer
    if( file.subj%%1 != 0 ){
        ## get the counts, mode and list of differing subj
        file.counts <- table(data.in$Subj)
        file.mode <- mode_func(file.counts)
        nonmode.df <- data.frame(
            Subj=levels(data.in$Subj)[which(file.counts != file.mode)],
            NumInputFiles=as.numeric(file.counts[which(file.counts != file.mode)]))
        
        cat('\n+*Warning: Each "Subj" does not have the same ')
        cat('number of "InputFiles" !\n')
        cat(paste0("Most Subj have ",file.mode," InputFiles. \n"))
        cat("The following Subj differ:\n")
        print.data.frame(nonmode.df,row.names=FALSE,right=FALSE)
        cat("\n")
        return(1)
    } 
    return(0) 
}

## takes a valid data frame
rule_error <- function(data.in){
    
    err.check <- 0
    ## check mandatory table names
    if( subj_first(data.in) != 0 ){ err.check <- err.check + 1 }
    if( InFile_last(data.in) != 0 ){
        err.check <- err.check + 1
    } else if( names(data.in)[length(data.in)] == "InputFile" ){
        ## check dups in InputFile
        if( InFile_dups(data.in) != 0 ){ err.check <- err.check + 1 }
    }
    
    ## give warning about input files per subject
    if( err.check == 0 ){ file_subj_check(data.in) }
    
    if( err.check > 0 ){ cat("\n") } ; return(err.check)
}   ## end rule_error function

## read in a regular, rectangular table file and send to summary #########
## not used
dtCheck_test_read_Table <- function(data.in){
    
    ## get data and split it out
    data.df <- fread(data.in,stringsAsFactors=TRUE)
    
    ## print out dimensions
    dim.tab <- dim(data.df)
    cat("rows: ") ; cat(dim.tab[1]) ; cat(" ")
    cat("columns: ") ; cat(dim.tab[2]) ; cat("\n")
    
    ## check for errors
    r.e <- rule_error(data.df)
    if( r.e != 0 ){ return(1) } # else { cat("\n") }
    
    ## print out the brief summary
    dtCheck_printSummary(data.df)
    
    return(data.df)
    
}   ## end dtCheck_test_read_Table

## test for after conversion to data frame ##############

## takes a good data frame, checks it and returns the same exact frame
dtCheck_testDF <- function(data.in){
    
    ## convert characters to factors 
    data.in <- as.data.frame(unclass(data.in),stringsAsFactors = TRUE)
    
    ## grep the +/- numeric variables and convert to numeric
    num.cols <- sapply(data.in, function(x) !any(grepl("[^0-9.-]", x)))
    data.in[ , num.cols] <- apply(data.in[,num.cols],2,
                                  function(x) as.numeric(as.character(x)))
    
    ## print out dimensions
    dim.tab <- dim(data.in)
    dtCheck_log_print("\nDimensions: \n")
    dtCheck_log_print(paste0("rows: ",dim.tab[1]," | columns: ",
                             dim.tab[2],"\n\n"))
    
    ## check for errors
    r.e <- rule_error(data.in)
    if( r.e != 0 ){ return(1) } #else { cat("\n") }
    
    ## print out the brief summary
    dtCheck_printSummary(data.in)
    
    return(data.in)
    
}   ## end dtCheck_testDF

## overall check after you get a good data frame ############
dtCheck_overall <- function(data.in){
    
    ## get a print out a summary and get a data frame
    tableTest.df <- dtCheck_testDF(data.in)
    
    if( length(tableTest.df) == 1 ){ return(1) }
    
    ## make sure all InputFiles exist on disk
    exists.val <- dtCheck_img_exists(tableTest.df)
    
    if( exists.val == 0 ){
        test.vols <- dtCheck_1_vol(tableTest.df)
        test.grid <- dtCheck_same_grid(tableTest.df)
        
        ## exit on either failure
        if( test.vols + test.grid != 0 ){
            dtCheck_err("One or more tests failed. See above")
            return(1)
        }
    } else {
        dtCheck_err("One or more tests failed. See above")
        return(1)
    }
    
    return(0)
    
}   ## end dtCheck_overall

# dtCheck_overall <- function(file.in){
#     
#     try.read <- dtCheck_tryRead(file.in)
#     if( try.read != 0 ){
#         dtCheck_err("dataTable is NOT regular and rectangular")
#         return(1)
#     } else {
#         ## get a print out a summary and get a data frame
#         tableTest.df <- dtCheck_testTable(file.in)
#         
#         if( length(tableTest.df) == 1 ){ return(1) }
#         
#         ## make sure all InputFiles exist on disk
#         exists.val <- dtCheck_img_exists(tableTest.df)
#         
#         if( exists.val == 0 ){
#             test.vols <- dtCheck_1_vol(tableTest.df)
#             test.grid <- dtCheck_same_grid(tableTest.df)
#             
#             ## exit on either failure
#             if( test.vols + test.grid != 0 ){
#                 dtCheck_err("One or more tests failed. See above")
#                 return(1)
#             }
#         }
#     }
#     return(0)
#     
# }   ## end dtCheck_overall

