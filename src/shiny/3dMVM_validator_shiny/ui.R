#####################################
## 11/2017 Justin Rajendra
## 3dMVM validator
## UI

#########################################################################
header <- dashboardHeader(title=paste("3dMVM Validator"),titleWidth=275)

#########################################################################
sidebar <- dashboardSidebar(width=275,sidebarMenu(

  tags$style(HTML(".box.box-solid.box-primary>.box-header {
                  color:#DCDCDC; background:#DCDCDC }
                  .box.box-solid.box-primary{ background:#DCDCDC}")),

  menuItem("Data",tabName="model",icon=icon("cloud-download"),
           selected=TRUE,startExpanded=TRUE,

           br(),
           actionButton('get_coord','Get Coordinate',icon=icon('map-marker')),
           tags$style(type='text/css',
                      "button#get_coord { margin-left: 13px; }"),

           radioButtons('vox_roi','Extract Data From:',c('Voxel','ROI'),
                        inline=TRUE),

           conditionalPanel('input.vox_roi == "ROI"',
                            numericInput('srad_num','Seed Radius:',
                                         value=5,min=2,step=1,width="50%"),
                            actionButton('make_roi','Make ROI',
                                         icon=icon('circle-thin')),
                            tags$style(type='text/css',
                                       "button#make_roi { margin-left: 13px; }"),
                            br()
           ),

           actionButton('extract_data','Extract Data',icon=icon('refresh')),
           tags$style(type='text/css',
                      "button#extract_data { margin-left: 13px;
                                             margin-bottom:13px; }"),

           actionButton('load_data','Load Data',icon=icon('download')),
           tags$style(type='text/css',
                      "button#load_data { margin-left: 13px; }"),

           radioButtons('SS_type','Sum of Squares Type:',c('2','3'),
                        selected='3',inline=TRUE),br()

  )   ## end model tab
)   ## end side bar menu
)   ## end sidbar

#########################################################################
body <-  dashboardBody(
  tabsetPanel(
    id='tabs',

    ##########################
    tabPanel('Model',icon=icon("edit"),br(),
             fluidRow(
               box(title='Categorical Variables:',width=6,
                   tableOutput('cat_var_tab')),
               box(title='Numerical Variables:',width=6,
                   tableOutput('qnt_var_tab'))
             ),br(),
             fluidRow(
               box(title='Number of subjects:',width=2,h3(n.subj)),
               box(title='Coordinates:',width=4,
                   tableOutput('coord_tab')),
               box(title='Specification errors:',width=6,
                   tableOutput('bad_vars'),
                   tags$head(tags$style("#bad_vars{color: red;}")))
             ),br(),
             fluidRow(column(
               width=12,
               textInput('model_in',
                         'Specify between subjects model: -bsVars',
                         value=bsVars[1],width="100%"))
             ),
             fluidRow(column(
               width=12,
               textInput('ws_vars_in',
                         'Specify within subjects model: -wsVars ',
                         value='',width="100%",
                         placeholder=paste("Best guess:",
                                           paste(wsVars,collapse=" ")) ))
             ),
             fluidRow(column(
               width=12,
               textInput('qnt_vars_in',value="",width="100%",
                         'Specify quantitative variables (comma separated):
                         -qVars',
                         placeholder=paste("Possible:",qntVar_str) ))
             ),
             fluidRow(column(
               width=12,
               textInput('qnt_vars_center',
                         'Specify -qVars centering (comma separated numbers,
                          matching above order): -qVarCenters',
                         value="",width="100%"))
             ),br(),
             fluidRow(box(title='Model result:',
                          verbatimTextOutput('model_summary'),width=12)
             ),br()
    ),   ## end model spec panel

    #################################
    tabPanel('Script',icon=icon("newspaper-o"),br(),
             fluidRow(
               box(width=9,title='Enter script file name:',
                      textInput('script_name',NULL,width='100%',
                                value=paste0(cur.dir,"/MyMVM.txt")) ),
               box(width=3,
                   actionButton('downloadScript','Save Script'),
                   checkboxInput('OverwriteScript','Overwrite script?') )
             ),
             fluidRow(
               box(width=12,title='Execute script at terminal with:',
                      verbatimTextOutput('exec_script') )
             ),

             br(),
             fluidRow(
               box(width=12,status="primary",solidHeader=TRUE,
                   textOutput('mvm_script'),
                   tags$style(type="text/css",
                              "#mvm_script {white-space: pre-wrap;
                                     font-family:courier;}") )
             )
    ),

    #################################
    tabPanel(
      'Script Extras',icon=icon("th-list"),br(),
      fluidRow(column(width=12,
                      textInput('mvm.prefix','-prefix (no spaces)',
                                value=paste0(cur.dir,"/My_MVM.nii.gz"),
                                width="100%") )
      ),
      fluidRow(column(width=12,
                      textInput('mvm.mask','-mask (full path)',
                                value="",width="100%"))
      ),
      fluidRow(box(column(width=4,
                          numericInput('n.jobs','-jobs',value=1,
                                       min=1,step=1)),
                   column(width=4,
                          checkboxInput('GES','-GES'),
                          checkboxInput('SC','-SC'),
                          checkboxInput('robust','-robust')
                   ),
                   column(width=4,
                          checkboxInput('wsE2','-wsE2'),
                          checkboxInput('wsMVT','-wsMVT'),
                          checkboxInput('dbgArgs','-dbgArgs')
                   )
      ) )
    ),   ## end model extras panel

    #################################
    tabPanel(
      'GLTs',icon=icon("snowflake-o"),br(),
      fluidRow(
        box(width=12,title='Current model: (if you change the model, all GLTs will be erased!)',
            textOutput('glt_cur_mod') )
      ),
      fluidRow(
        box(title='Do not push buttons if there are errors!!!',width=6,
            tableOutput('bad_glt'),
            tags$head(tags$style("#bad_glt{color: red;}")) )
      ),
      fluidRow(
        column(width=12,
               textInput('glt_label','GLT label: -gltLabel', value="",
                         width="100%"))
      ),
      fluidRow(
        column(width=12,
               selectInput('glt_vars','Variable:',choices=NULL,
                           width="100%") )
      ),
      fluidRow(
        column(width=12,
               selectInput('glt_lvl','Multi-select levels in order:',
                           NULL,width="100%",multiple=TRUE) )
      ),
      fluidRow(
        column(width=8,
               textInput('glt_weights',width="100%",
                         paste('Comma separated weights matching',
                               'above level order:') )),
        column(width=4,
               actionButton('add_glt_var','Add variable',
                            icon=icon('plus-square-o')) )
      ),
      fluidRow(
        column(width=12,textInput('glt_code','GLT code: -gltCode',
                                  value="",width="100%"))
      ),br(),
      fluidRow(
        box(width=2,
            actionButton('test_glt','Test GLT',
                         icon=icon('check-square-o')),
            br(),br(),
            actionButton('add_glt','Add GLT',
                         icon=icon('plus-square-o')),
            br(),br(),
            actionButton('clear_glt','Clear GLT',icon=icon('remove')),
            br(),br(),
            actionButton('clear_all_glt','Clear All',icon=icon('remove'))
        ),
        box(width=10,tableOutput('glt_test_out'))
      ),
      br(),
      fluidRow(
        textAreaInput('glt_list',NULL,value="",width='800px',
                      height='300px'),
        tags$style(type='text/css',"textarea#glt_list {
                                           margin-left: 16px;
                                           font-family:courier;}")
      ),br()
    ),   ## end GLT panel

    #################################
    tabPanel('Data Table',icon=icon("table"),br(),
             fluidRow(column(width=12,br(),
                             dataTableOutput('mvm_table'),br()) )
    ),   ## end full data table panel

    #################################
    tabPanel('Data Summary',icon=icon("calculator"),br(),
             fluidRow(column(width=12,br(),
                             verbatimTextOutput('summary_table'),br()) )
    ),   ## end data table summary panel

    #################################
    tabPanel('3dMVM Help',icon=icon("question-circle-o"),
             fluidRow( htmlOutput("mvm_help") )
    )   ## end 3dMVM help web page

  )
)  ## end dashboard body
## run it
dashboardPage(header, sidebar, body)
