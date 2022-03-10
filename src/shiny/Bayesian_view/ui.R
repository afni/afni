## top ##################################
## 01/2022 Justin Rajendra
## Gang RBA 
## UI

header <- dashboardHeader(title=paste("Bayes View"),titleWidth=290)

## sidebar top #####################
sidebar <- dashboardSidebar(
  width=290,
  sidebarMenu(
    id="tabs",
    
    ## main #############################################
    menuItem(
      "Data",icon=icon('database'),selected=TRUE,startExpanded=TRUE,
      
      selectInput('fileSel',"Data File",file.list,multiple=FALSE),
      
      selectInput('roiSel','Select ROIs',NULL,NULL,multiple=TRUE),
      
      selectInput('orderSel','Order By:',order.list),
      
      checkboxInput('x_range_custom','Customize X Axis Range?'),
      
      conditionalPanel('input.x_range_custom',
                       sliderInput(
                         'plotRange','Range:',
                         min=-0.5,max=0.5,value=c(-0.05,0.05),step=0.01)
      ),
      
      br()
    ),   ## end subject and date
    
    ## decorations menu item #######################
    menuItem(
      "Decorations",icon=icon('palette'),selected=FALSE,startExpanded=FALSE,
      
      ## choose what to edit here
      selectInput('plot_pars','Edit Item:',
                  c('Colors','Title','X Axis','Y Axes')),
      
      conditionalPanel(
        'input.plot_pars == "Colors"',
        selectInput('colPal','Color Palette',col.list)
      ),
      conditionalPanel(
        'input.plot_pars == "Title"',
        textInput('plotTitle','Title','Bayesian Plot'),
        sliderInput('title_size','Title Size',min=3,max=50,value=20),
        selectInput('title_face','Title Font Face',f.face.list,selected='bold')
      ),
      conditionalPanel(
        'input.plot_pars == "X Axis"',
        sliderInput('x_axis_size','X Ticks Size',min=3,max=50,value=10),
        sliderInput('x_label_size','X Label Size',min=3,max=50,value=15),
        selectInput('xlab_face','X Label Font Face',f.face.list)
      ),
      conditionalPanel(
        'input.plot_pars == "Y Axes"',
        sliderInput('ROI_label_size','ROI Label Size',min=3,max=50,value=10),
        sliderInput('P_label_size','P Label Size',min=3,max=50,value=10)
        # selectInput('ylab_face','Y Ticks Font Face',f.face.list)
      ),
      
      br()
    ),
    
    ## output menu item #######################
    menuItem(
      "Output",icon=icon('image'),selected=FALSE,startExpanded=FALSE,
      
      selectInput('outputFormat','File Format',c('png','pdf')),
      sliderInput('outputHeight','Height (in)',min=3,max=20,value=5),
      sliderInput('outputWeight','Weight (in)',min=3,max=20,value=5),
      
      br()
    ),
    
    br()
  )   ## end sidebar menu                  
)   ## end sidebar

## body top #######################################################################
body <-  dashboardBody(
  
  fluidRow( plotOutput('gangPlot') ),
  br()
  
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)

