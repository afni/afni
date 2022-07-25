## top ##################################
## 01/2022 Justin Rajendra
## Gang RBA 
## UI

header <- dashboardHeader(title=paste("Bayes View"),titleWidth=290)

## sidebar top #####################
sidebar <- dashboardSidebar(
  width=290,
  tags$head(tags$script('
        var dimension = [0, 0, 0, 0];
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            dimension[2] = screen.width;
            dimension[3] = screen.height;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            dimension[2] = screen.width;
            dimension[3] = screen.height;
            Shiny.onInputChange("dimension", dimension);
        });
    ')),
  
  sidebarMenu(
    id="tabs",
    
    ## main #############################################
    menuItem(
      "Data",icon=icon('database'),selected=TRUE,startExpanded=TRUE,
      
      selectInput('fileSel',"Data File",file.list,multiple=FALSE),
      
      selectInput('orderSel','Order By:',order.list),
      
      br()
    ),   ## end main
    
    ## Rois #############################################
    menuItem(
      "ROIs",icon=icon('map-pin'),selected=FALSE,startExpanded=FALSE,
      
      selectInput('roiSel','Select ROIs',NULL,NULL,multiple=TRUE),
      
      br()
    ),   ## end ROIs
    
    ## dimensions ##################
    menuItem(
      "Current View",icon=icon('arrows-alt'),selected=FALSE,startExpanded=FALSE,
      
      sliderInput('axesHeight','Axes Height (pixels per ROI)',
                  min=10,max=100,value=25),
      checkboxInput('autoWidth','Auto Resize Plot Width',TRUE),
      conditionalPanel(
        '!input.autoWidth',
        sliderInput('plotWidth','Plot Width (pixels)',
                    min=400,max=2000,value=400)
      ),
      sliderInput('plotRes','Plot Resolution',min=5,max=2000,value=72),
      
      ## display current dimensions
      h5(paste("Visible Plot Width x Height (pixels)"),
         style="margin-left: 12px;"),
      h4(textOutput("cur_plot_dim"),style="margin-left: 12px;"),
      
      ## output size
      h5(paste("Output Plot Width x Height (inches)"),
         style="margin-left: 12px;"),
      h4(textOutput("out_plot_dim"),style="margin-left: 12px;"),
      
      checkboxInput('x_range_custom','Customize X Axis Range?'),
      conditionalPanel(
        'input.x_range_custom',
        sliderInput('plotRange','Range:',
                    min=-0.5,max=0.5,value=c(-0.05,0.05),step=0.01)
      ),
      
      ## reset to defaults
      actionButton('resetDim','Reset Dimensions'),
      
      br()
    ),   ## end dimensions
    
    ## decorations menu item #######################
    menuItem(
      "Decorations",icon=icon('palette'),selected=FALSE,startExpanded=FALSE,
      
      ## choose what to edit here
      selectInput('plot_pars','Edit Item:',
                  c('Colors','Color Bar','Title','X Axis','Y Axes')),
      hr(),
      conditionalPanel(
        'input.plot_pars == "Colors"',
        selectInput('colPal','Color Palette',col.list),
        conditionalPanel(
          'input.colPal != "Default"',
          sliderInput('numCols','Number of Colors (except for Default)',
                      min=3,max=11,value=6)
        ),
        checkboxInput('revCols','Reverse Color Order',FALSE)
      ),
      conditionalPanel(
        'input.plot_pars == "Color Bar"',
        sliderInput('colBarHeight','Color Bar Height',min=5,max=300,value=15),
        sliderInput('colBar_size','Color Bar Label Size',min=3,max=50,value=10),
        textInput('colBar_title','Color Bar Title','P+'),
        sliderInput('colBar_title_size','Color Bar Title Size',
                    min=1,max=100,value=24),
        selectInput('colBar_face','Color Bar Title Font Face',
                    f.face.list,selected='plain')
      ),
      conditionalPanel(
        'input.plot_pars == "Title"',
        textInput('plotTitle','Title','Posterior Distribution'),
        sliderInput('title_size','Title Size',min=3,max=50,value=24),
        selectInput('title_face','Title Font Face',f.face.list,selected='bold')
      ),
      conditionalPanel(
        'input.plot_pars == "X Axis"',
        textInput('x_label','X Axis Label','Var'),
        sliderInput('x_label_size','X Label Size',min=3,max=50,value=20),
        selectInput('xlab_face','X Label Font Face',f.face.list),
        sliderInput('x_axis_size','X Ticks Size',min=3,max=50,value=15),
        selectInput('x_axis_face','X Ticks Font Face',f.face.list)
      ),
      conditionalPanel(
        'input.plot_pars == "Y Axes"',
        sliderInput('ROI_label_size','ROI Label Size',min=3,max=50,value=10),
        # selectInput('ylab_face','Y Ticks Font Face',f.face.list)
        
        sliderInput('P_label_size','P Label Size',min=3,max=50,value=10)
      ),
      
      ## reset to defaults
      hr(),
      actionButton('resetDecor','Reset to Defaults'),
      br()
    ),   ## end decorations
    
    ## output menu item #######################
    menuItem(
      "Download",icon=icon('image'),selected=FALSE,startExpanded=FALSE,
      
      selectInput('outputFormat','File Format',
                  c('eps','jpeg','pdf','png','svg','tiff'),selected='pdf'),
      sliderInput('outputWidth','Output Width (pix)',
                  min=400,max=2000,value=400,step=5),
      sliderInput('outputHeight','Output Height (pix)',
                  min=400,max=2000,value=400,step=5),
      sliderInput('outputDPI','Output DPI',min=5,max=1200,value=72),
      
      # h5(paste("Output Plot Width x Height (inches)"),
      #    style="margin-left: 12px;"),
      # h4(textOutput("out_plot_dim"),style="margin-left: 12px;"),
      # 
      downloadButton('downloadPlot','Download Plot',class='d_button'),
      tags$head(tags$style(".d_button#downloadPlot{margin-left: 12px;}
                                .d_button#downloadPlot{color:black;}")),
      br(),br()
    ),
    
    br()
  )   ## end sidebar menu                  
)   ## end sidebar

## body top #######################################################################
body <-  dashboardBody(

  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Plot",br(),
      div(style='width:auto;overflow-x:scroll;height:800px;overflow-y:auto;',
          plotOutput('bayesPlot'))),
    
    tabPanel("Table",br(),
             dataTableOutput("statsTable"))
  ),
  
  br()
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)

