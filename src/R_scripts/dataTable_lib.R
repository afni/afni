## top ##########################
## data table checker library
## Justin Rajendra ~ March 2023
## all functions to add to dataTable_checker.R 
## can add to all Gang stats programs

## mode ########
mode_func <- function(x) {
    uniqx <- unique(x)
    return(uniqx[which.max(tabulate(match(x,uniqx)))])
}

## add leading spaces to the data frame to aid in aligning text #########
## takes a valid data frame and outputs the same frame with padded spaces
lead_space <- function(data.in){
    
    ## empty to fill
    data.temp <- c() ; name.temp <- c()
    
    for( i in 1:length(data.in) ){
        
        ## stuff to change
        t.col <- as.character(data.in[[i]])
        t.name <- names(data.in)[i]
        
        ## get max number of characters
        t.max <- max(c(nchar(t.col),nchar(t.name)))
        
        ## pad with spaces (name is left justified)
        t.col <- formatC(t.col,width=t.max)
        t.name <- formatC(t.name,width=t.max,flag="-")
        
        ## add to output
        data.temp <- cbind(data.temp,t.col)
        name.temp <- cbind(name.temp,t.name)
    }
    
    ## save as data frame and return
    data.temp <- as.data.frame(data.temp)
    names(data.temp) <- name.temp
    return(data.temp)
    
}   ## end lead_space function

## try to read a data table FILE ######################
## needs the data table file name (with path or relative)
## does not save the imported data. Just cats info to the screen
tryRead <- function(file.in){
    
    ## check to see if the file is really there
    if( !file.exists(file.in) ){ 
        cat(paste("**ERROR:",file.in,"not found!!!"))
        cat("\n\n") ; return(1)
    }
    
    ## which file
    cat(basename(file.in)) ; cat(":\n")
    
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
                cat(paste("Missing",hdr.len - length(tmp.line[[1]]),
                          "element from line",i,"\n"))
            } else {
                cat(paste("Missing",hdr.len - length(tmp.line[[1]]),
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
                    cat(paste("Missing",length(miss.ele),
                              "element from line",i,"\n"))
                } else {
                    cat(paste("Missing",length(miss.ele),
                              "elements from line",i,"\n"))
                }
                miss.row <- c(miss.row,i)
                miss.num <- c(miss.num,length(miss.ele))
                miss.line <- c(miss.line,tmp.line)
            }
            
            ## cat info for NAs (may need to change this)
            if( length(found.na) > 0 ){
                cat(paste("NA's found on line",i,"\n"))
                na.line <- c(na.line,i)
            }
        }
    }   ## end for each row
    
    ## check output checks
    if( length(na.line) > 0 ){
        return(length(na.line))
    } else if( length(miss.row) > 0 ){ 
        return(length(miss.row))
    } else {
        cat("++Good: Table is regular and rectangular.\n")
        return(0)
    }
}   ## end tryRead

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

## a little summary #########################
## takes a valid data frame
## prints a bunch of stuff to screen and returns nothing
printSummary <- function(data.in){
    
    cat("Data summary: \n")
    
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
    
    # options(oldoptions) ## change the options back (not sure if this is necessary)
    
    cat("\n") ; return(0)
}   ## end printSummary

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
    if( ! (names(data.in)[length(data.in)] %in% c("InputFile","Ausgang_val")) ){
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
    file_subj_check(data.in)
    
    if( err.check > 0 ){ cat("\n") } ; return(err.check)
}   ## end rule_error function

