#####################################
## 11/2017 Justin Rajendra
## FATCAT matrix plot
## UI

#########################################################################
header <- dashboardHeader(title='FATCAT Matrix Plot',titleWidth=300)

#########################################################################
sidebar <- dashboardSidebar(width=300,sidebarMenu(
  
  ## style the text a bit to the right
  tags$style(type="text/css",".tab-content { overflow: visible !important; }"),
  tags$head(tags$style(".learn_link{margin-left:15px;}")),
  tags$head(tags$style(".badness{margin-left:15px;} .badness{color:red;}")),
  
  #################################
  menuItem(
    "Main",tabName="stuff",icon=icon("cogs"),selected=TRUE,startExpanded=TRUE,
    
    ## file and matrix selectors
    selectInput('net_file','File:',file.list),
    selectInput('stat_sel','Stat:',NULL),
    
    ## thresholding options
    radioButtons('tri_sel','Matrix:',inline=TRUE,c('Full','Upper','Lower')),
    fluidRow(
      column(width=4,checkboxInput('thresh_yn','Threshold')),
      column(width=8,checkboxInput('zero_yn','Remove zeros'))
    ),
    conditionalPanel('input.thresh_yn',
                     fluidRow(
                       column(width=6,
                              numericInput('thresh_min','Low Pass:',
                                           -1,step=0.1)),
                       column(width=6,
                              numericInput('thresh_max','High Pass:',
                                           1,step=0.1))
                     ) ),
    ## clustering
    selectInput('h_clust','Cluster linkage (before thresholding):',
                c('none','complete','single','average','median',
                  'centroid','mcquitty')),
    conditionalPanel('input.h_clust != "none"',
                     htmlOutput('hclust_link',class='learn_link'),
                     selectInput('dist_meth','Distance Method:',
                                 c("euclidean","maximum","manhattan",
                                   "canberra","binary","minkowski")),
                     htmlOutput('dist_link',class='learn_link')
    ),
    
    ## histogram
    checkboxInput('hist_yn','Show histogram below',value=TRUE),
    conditionalPanel('input.hist_yn',
                     h5('Histogram is after all thresholding.',
                        class='learn_link')),br(),
    
    ## download and warnings
    downloadButton('downloadLog','Download Log',class='d_button'),
    tags$head(tags$style(".d_button#downloadLog{margin-left: 12px;}
                         .d_button#downloadLog{color:black;}")),br(),
    conditionalPanel(
      "input.col_thresh == 'Yes'",br(),
      h5("Data min/max was set manually!",class='badness'),
      h5("Colors and values may be misleading!",class='badness')
    ),br()
    
  ), ## end files tab
  
  #################################
  menuItem("ROIs",tabName="ROIs",icon=icon("code-fork"),
           selectInput('rois','Select ROIs:',multiple=TRUE,choices=NULL,
                       selected=NULL,selectize=TRUE),br()
  ),  ## end ROIs tab
  
  #################################
  menuItem("Colors",tabName="colors",icon=icon("edit"),
           
           ## opt-in for min max colors
           radioButtons('col_thresh','Edit data min/max:',c("Yes","No"),
                        selected="No",inline=TRUE),
           conditionalPanel(
             "input.col_thresh == 'Yes'",
             h5("For coloring purposes ONLY!!",class='badness'),
             h5("Color min/max also thresholds the data!",class='badness'),
             h5("Any data beyond min/max",class='badness'),
             h5("will be set to min/max!!",class='badness'),
             h5("As shown in the histogram!!",class='badness'),
             fluidRow(
               column(width=6,numericInput('range_min','Set data min:',-1,
                                           step=0.1)),
               column(width=6,numericInput('range_max','Set data max:',1,
                                           step=0.1)) )
           ),
           
           ## color choices
           radioButtons('col_2_3','Number of colors:',c(2,3),selected=3,
                        inline=TRUE),
           colourInput('col1','Lower',allowTransparent=TRUE,
                       value=rgb(0,0,1,alpha=0.4)),
           conditionalPanel(
             "input.col_2_3 == 3",
             colourInput('col2','Middle',allowTransparent=TRUE,
                         value=rgb(1,1,1,alpha=0.7))
           ),
           colourInput('col3','Upper',allowTransparent=TRUE,
                       value=rgb(1,0,0,alpha=0.4)),
           h5("Transparency only applies to the Circos.",
              class='learn_link'),
           h5("Circos only takes the lower and upper colors",
              class='learn_link'),br()
  ),   ## end colors
  
  #################################
  menuItem("Heatmap PNG",tabName="png",icon=icon("file-image-o"),
           conditionalPanel(
             "input.col_thresh == 'Yes'",br(),
             h5("Data min/max was set manually!",class='badness'),
             h5("Colors and values may be misleading!",class='badness')
           ),
           numericInput('static_dpi','DPI (min 72):',value=200,min=72),
           downloadButton('downloadPlot', 'Download Heatmap',
                          class='d_button'),
           tags$head(tags$style(".d_button#downloadPlot{margin-left: 12px;}
                                .d_button#downloadPlot{color:black;}")),br(),
           conditionalPanel('input.h_clust != "none"',
                            checkboxInput('dendro','Show dendrogram on png?',
                                          value=TRUE)
           ),br()
  ),  ## end heatmap png tab
  
  #################################
  menuItem("Circos PNG",tabName="circos",icon=icon("times-circle-o"),
           conditionalPanel(
             "input.col_thresh == 'Yes'",br(),
             h5("Data min/max was set manually!",class='badness'),
             h5("Colors and values may be misleading!",class='badness')
           ),br(),
           h5("Longer labels may need larger image size.",
              class='learn_link'),
           sliderInput('circos_dim','Image size (inches):',value=8,
                       min=4,step=4,max=32),
           numericInput('circos_dpi','DPI (min 72):',value=200,min=72),
           conditionalPanel(
             "input.col_2_3 == 3",h5("Circos may look better with 2 colors.",
                                     class='badness')
           ),
           downloadButton('downloadCircos','Download Circos',class='d_button'),
           tags$head(tags$style(".d_button#downloadCircos{margin-left: 12px;}
                                .d_button#downloadCircos{color:black;}")),
           br(),br()
  ), ## end circos
  
  h6("If you can't scroll down, resize the browser window.",
     style="margin-left: 12px; margin-bottom: 0px;")
)   ## end sidebar menu
)   ## end sidbar

#########################################################################
body <-  dashboardBody(
  fluidRow(
    plotlyOutput('cor_heatmap_plot',height=700,width=700),
    conditionalPanel('input.hist_yn',br(),br(),
                     plotlyOutput('cor_hist_plot',height=700,width=700)
    )
  )
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)
