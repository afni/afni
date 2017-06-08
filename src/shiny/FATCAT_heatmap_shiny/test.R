
rotate <- function(x) t(apply(x, 2, rev))

col.ramp <- colorRampPalette(c('blue','white','red'))


net_file <- '/Users/discoraj/Dropbox/nih/shiny/FORJUSTIN/REST_corr_rz_000.netcc'



num.rois <- as.numeric(strsplit(readLines(net_file,n=1),split=' ')
                       [[1]][2])
num.mat <- as.numeric(strsplit(readLines(net_file,n=2)[2],split=' ')
                      [[1]][2])
roi.lab <- as.matrix(read.table(net_file,nrows=1,header=FALSE))
if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }

## read in data
net.mat <- as.matrix(fread(net_file,skip="CC",
                           nrows=num.rois,header=FALSE))
if(is.na(sum(net.mat[,ncol(net.mat)]))){
  net.mat <- net.mat[,1:ncol(net.mat)-1]
}
rownames(net.mat) <- colnames(net.mat) <- roi.lab


hm2 <- heatmap.2(net.mat,col=col.ramp(100),Rowv=TRUE,Colv='Rowv',
          breaks=seq(0.4287,1,length.out=101),
          trace='none',symm=TRUE,revC=TRUE,
          distfun=function(c) dist(c,method='euclidean'),
          hclustfun=function(c) hclust(c,method='complete'))

hm2$carpet <- hm2$carpet[,rev(seq.int(ncol(hm2$carpet)))]

hm2$carpet[(upper.tri(hm2$carpet,diag=TRUE))] <- NA
hm2$carpet <- hm2$carpet[,rev(seq_len(ncol(hm2$carpet)))]

revRowInd <- match(c(1:length(hm2$rowInd)), hm2$rowInd)
revColInd <- match(c(1:length(hm2$colInd)), hm2$colInd)
heatmap.2(t(hm2$carpet[revRowInd, revColInd]),
          col=col.ramp(100),breaks=seq(0.4287,1,length.out=101),
          trace='none',symm=TRUE,revC=TRUE,dendrogram='column',
          Rowv=hm2$rowDendrogram, Colv=hm2$colDendrogram)




#
# heatmap.2(net.mat,col=col.ramp(100),Rowv=FALSE,
#           trace='none',symm=TRUE,revC=FALSE,
#           breaks=seq(0.4287,1,length.out=101))


