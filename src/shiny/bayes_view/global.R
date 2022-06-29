## top ##################################
## 04/2022 Justin Rajendra
## bayes view
## global

# ## clean up
# rm(list=ls())

## check for packages and load them #################
pkgs <- c('shiny','data.table','shinydashboard','plotly','RColorBrewer',
          'tools','ggplot2','ggridges','dplyr','tidyr','scales','DT')
for(pkg in pkgs){
    if(!require(pkg,character.only=TRUE)){
        print(paste0("ERROR: Missing ",pkg,"! please install it with: ",
                     "@afni_R_package_install -custom ",pkg))
        quit(save="no")
    } else {
        require(pkg,character.only=TRUE)
    }
}

## get arguments and check if folder is there #################
args <- commandArgs(TRUE)
data.path <- args[1]
if(!dir.exists(data.path)){
    print(paste0("ERROR: ",data.path," does not exist!"))
    quit(save="no")
}

## get file list ####################################
file.temp <- list_files_with_exts(data.path,ext=c('RData'))
file.list <- as.list(file.temp)
names(file.list) <- basename(file_path_sans_ext(file.temp))
rm(file.temp)

## lists for choices ##################################
order.list <- list('P-plus','Original')
f.face.list <- list('Plain'='plain','Bold'='bold','Italic'='italic',
                    'Bold Italic'='bold.italic')

### plot parameters ##################################
trans <- 0.25
line.wd <- 2
point.size <- 2
markers <- c(21,23)
marker.size <- 3
jit <- 0.1

## colors #########################################
dark.col <- brewer.pal(6,"Dark2")
set1.col <-  brewer.pal(6,"Set1")
acc.col <- brewer.pal(6,"Accent")
rain.col <- rainbow(6)[c(1,3,4,5,6)]
gang.col <- c("blue","cyan","gray","gray","yellow","#C9182B")

col.list <- list("Blue - Red","Dark2","Set1","Rainbow","Accent")

### stat functions ##################################

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
