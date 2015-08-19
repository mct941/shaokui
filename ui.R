ui <- fluidPage(
      fluidRow(
        column(1,img(src="PMX.jpg",height=50,width=75),align="left"),       
        column(10,titlePanel("Pharmacometric Web Application:  Data Exploration and Simple Sampling Strategy")),
        column(1,img(src="Takeda.jpg",height=50,width=125),align="right")
      ),
      fluidRow(column(12, tabsetPanel(
        tabPanel("Information",     
          h3(HTML(paste(tags$b("Objective:"),("This web application provides an interactive interface to explore pharmacokinetic data and enable a simple sampling strategy.")))),
          br(),
          #h3(HTML(paste(tags$b("Background:"),h4("During drug development, there are many instances, particularly in late development, when it is not possible to have a robust pharmacokinetic sampling scheme to characterize the full PK profile that is often performed in early development, considering the significant patient burden, large time investment to collect samples, and the associated costs of measuring plasma concentrations.  So, it is important to identify the appropriate times for collecting a limited number of blood samples in patients such that the exposure can be determined.  Different approaches for assessing this include:")))),
          #tags$ul(
          #  tags$li(h4(HTML(paste(tags$b("Optimal Sampling Strategy"),(": C and D-optimality strategies minimize various scalar functions of the covariance matrix of the parameter estimates.  But this approach requires model development, which can be time-consuming, such that sampling schedule can be optimized for a given value of model parameters"))))),
          #  tags$li(h4(HTML(paste(tags$b("Limited Sampling Strategy"),(": Multiple linear regression is used to describe Cmax or AUC as a linear function of several concentration-timepoints, choosing the best subset of time points that describe the pharmacokinetic parameter of interest.  This approach assumes a linear combination of timepoints."))))),            
          #  tags$li(h4(HTML(paste(tags$b("Simple Sampling Strategy"),(": This brute force approach calculates Cmax or AUC  for every permutation of timepoints, choosing the best subset of time points that describe the pharmacokinetic parameter of interest.  No assumptions are required, but this approach can be time-consuming.")))))         
          #),  
          #br(),
          h3(HTML(paste(tags$b("Tabs:")))),
          tags$ul(
            tags$li(h4(HTML(paste(tags$b("Data Load:"),("select data file for evaluation; a brief data summary is provided"))))),
            tags$li(h4(HTML(paste(tags$b("Data Process:"),("map key data variables; a modified dataset can be generated, based on filter and sort conditions"))))),
            tags$li(h4(HTML(paste(tags$b("Data Explore:"),("summarize variables; pairs plots of correlations"))))),
            tags$li(h4(HTML(paste(tags$b("Data Table:"),("data is presented in a tabular format with sort, filter, and search functions"))))),
            tags$li(h4(HTML(paste(tags$b("Data Plot:"),("data is presented in a graph with highlight, zoom, and paneling functions for data point identification"))))),
            tags$li(h4(HTML(paste(tags$b("Data Evaluation:"),("data is analyzed to present user with permuatation of timepoints that meet user criteria which includes number of timepoints, %error in Cmax, and %error in AUC")))))
          ),
          br(),
          h3(HTML(paste(tags$b("Version History:")))),
          tags$ul(
            tags$li(h4(HTML(paste(tags$b("v1.0:"),("web application is launched"))))),
            tags$li(h4(HTML(paste(tags$b("v1.1:"),("added a mean line to plot and allow user to specify specific timepoints for evaluation"))))),
            tags$li(h4(HTML(paste(tags$b("v1.2:"),("allow axis to vary, based on data range during stratification"))))),
            tags$li(h4(HTML(paste(tags$b("v1.5:"),("major plotting additions:  1) added histograms (plus binwidth specifications), barplots, boxplots; 2) enable conversion of variables to categorical or continuous; 3) added line options of linear regression (plus r2 value), loess smoother, or reference lines; 4) added axis options for log scale, reverse scale, and axis switching; 5) added point option to enable jitter"))))),
            tags$li(h4(HTML(paste(tags$b("v1.7"),("added tab for data exploration"))))),
            tags$li(h4(HTML(paste(tags$b("v1.8"),("calculated NCA parameters"))))),
            tags$li(h4(HTML(paste(tags$b("v1.9"),("added more summary stats"))))),
            tags$li(h4(HTML(paste(tags$b("v2.0"),("added NONMEM compatability and download capabilities"))))),
            tags$li(h4(HTML(paste(tags$b("v2.1"),("added tooltips")))))
          ),
          br(),
          h3(HTML(paste(tags$b("Author:"),("Max Tsai")))),
          h4(HTML(paste(tags$b("Acknowledgements:"),("Shaokui Ge for his contributions to this application")))),
          br(),
          h4(HTML(paste(em("Disclaimer:  This application has not been validated and should be used at the discretion of the user"))))
          
          #h5(HTML(paste(tags$b("References:")))),
          #tags$ul(
          #  tags$li(h6("Population pharmacokinetics and limited sampling strategy for first-line tuberculosis drugs and moxifloxacin. Magis-Escurra C, et al. Int J Antimicrob Agents. 2014 Sep;44(3):229-34")),
          #  tags$li(h6("Limited sampling strategy for predicting area under the concentration-time curve for mycophenolic Acid in chinese adults receiving mycophenolate mofetil and tacrolimus early after renal transplantation. Cai W, et al. Ther Drug Monit. 2015 Jun;37(3):304-10")),
          #  tags$li(h6("A limited sampling strategy for estimation of the area under the plasma concentration-time curve of gefitinib. Miura M, et al Ther Drug Monit. 2014 Feb;36(1):24-9.")),
          #  tags$li(h6("Clinical usefulness of limited sampling strategies for estimating AUC of proton pump inhibitors. Niioka T. Yakugaku Zasshi. 2011 Mar;131(3):407-13."))  
          #)  
        ),  
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Load",
          fluidRow(
            column(3, wellPanel(
              fileInput("file", p('Data File'), multiple=TRUE,
                        accept=c(".csv","text/comma-separated-values,text/plain",".xls",".xlsx",".tab",".tbl",".dat")),
              bsTooltip('file',"comma delimited file is default",placement="right",options=list(container="body")),
              hr(),
              tags$b("File Import Options:"),
              checkboxInput("header", "Header (Y/N)", TRUE), 
              bsTooltip('header',"Is there a row for column names?","right",options=list(container="body")),
              checkboxInput("rownames", "Row names (Y/N)", FALSE), 
              bsTooltip('rownames',"Is there a column for row names?","right",options=list(container="body")),
              checkboxInput("strings", "Strings as Factors (Y/N)", FALSE), 
              bsTooltip('strings',"Should character data be treated as categorical variables?","right",options=list(container="body")),
              checkboxInput("nonmem", "NONMEM Output Table (Y/N)", FALSE), 
              bsTooltip('nonmem',"Is this a NONMEM output file?","right",options=list(container="body")),
              conditionalPanel("input.nonmem==true",checkboxInput("sim", "NONMEM Simulations with Replicates (Y/N)", FALSE)),
              conditionalPanel("input.nonmem==true",bsTooltip('sim',"Is this a simulation output with SUBPROB>1?",
                                                              "right",options=list(container="body"))),                       
              hr(),
              fluidRow(column(6,
                selectInput("sep","Delimiter:", c(Comma=',', Semicolon=';', Tab='\t', Space=' ', Pipe='|'), ','), 
                bsTooltip('sep',"What character is used to separate data values?","right",options=list(container="body"))
              ),
              column(6,
                textInput("na", "Missing Values:", value="."),  
                bsTooltip('na',"What character is used to designate missing values?","right",options=list(container="body"))
              )),
              checkboxGroupInput('quote', 'Quote Style:',c(None='','Single Quote'="'",'Double Quote'='"'),selected='',inline=T), 
              bsTooltip('quote',"What type of quote are used for data values?","right",options=list(container="body")),
              hr(),
              bsTooltip('loadq',"see help file for data load tab","right",options=list(container="body")),
              actionButton('loadq',label="",icon=icon("question-circle")),
              bsModal('loadpage',"Data Load",'loadq',size="large",
                strong("User options:"),
                p("Data files with following extensions are automatically displayed and listed: *.csv,*.txt,*.tab,*.tbl,*.dat;",
                  "for all other file extensions, choose all files in popup file selection window;",
                  "currently sas or excel files can not imported in their native formats and should be converted to *.txt or *.csv."),
                p("File options include importing first row as the column names, first column as row names,",
                  "character data treated as categorical variables."),
                p("The character for a missing value can be specified; '.' is the default character."),
                p("The character for a delimiter can be specified; comma is the default delimiter;",
                  "other options include semicolon, tab, space, and pipe-delimited."),
                p("The character for a quote style can be specified; none is the default."),
                p("NONMEM formatted output tables including NONMEM simulations with multiple replicates can be imported."),
                br(),
                strong("Display output:"),
                p("The dataset structure is displayed listing the dimensions of the dataset",
                   "and the data type for each variable along with some initial values.",
                  "A variable with all numeric values is considered to be of numeric or integer type.",
                  "A variable with numeric and character values is considered to be of character type.",
                  "unless character value is treated as missing, in which case it would revert back to numeric or integer type.",
                  "If character strings are treated as factor, in which case it is considered to be of factor type."),
                p("Summary statistics of the dataset are displayed, listing the following for each variable type:"),
                tags$ul(
                  tags$li(HTML(paste(tags$b("character:"),("length of the variable")))),
                  tags$li(HTML(paste(tags$b("numeric or integer:"),("minimum, 25%, median, mean, 75%, and maximum value")))),
                  tags$li(HTML(paste(tags$b("factor:"),("count tabulation of each category"))))
                )
              )
              )),
            column(9,
              #verbatimTextOutput('dim'), bsTooltip('dim',"Data dimensions","left"),
              verbatimTextOutput('structure'), bsTooltip('structure',"Data structure: variable type and initial values",
                                                         "left",,options=list(container="body")),
              verbatimTextOutput('summary'), bsTooltip('summary',"Data summary statistics: mean, range, and percentiles for continuous and count frequency for categorical variables",
                                                       "left",options=list(container="body"))            
            )
          )
        ),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Process",fluidRow(
          column(4,wellPanel(
            strong(textOutput('MAP')),bsTooltip('MAP',"Appropriate variables need to be mapped to these keys"),
            hr(),uiOutput('ID'),uiOutput('TIME'),uiOutput('DV'),conditionalPanel("input.sim==true",uiOutput('REP')),
            uiOutput('UNIQUE'),p(HTML(paste(tags$b("Example levels of unique identifier:")))),textOutput('UNIQUEID'),hr(),
            bsTooltip('processq',"see help file for data process tab",placement="right",options=list(container="body")),
            actionButton('processq',label="",icon=icon("question-circle")),
            bsModal('processpage',"Data Load",'processq',size="large",
              strong("User options:"),
              p(HTML(paste(tags$b("Data Mapping:"),"Certain key variables such as subject, time, and concentration",
                "need to be properly mapped to the appropriate variables in the dataset.  The default variable for subject is ID;",
                "the default variable for time is TIME; the default variable for concentration is DV;",
                "if applicable, the default variable for replicate is REP.",
                "If these named variables are located in the dataset, then mapping is automatic and no action is needed by user."))),
              p("A unique identifer is required.  By default, the unique identifier is ID.",
                "If there are multiple DV values (excluding TIME) for a given ID value, then a unique identifer needs to be defined.",
                "For example, for a dataset containing parent and metabolite concentrations defined by different CMT values",
                "a unique identifer may be comprised of ID and CMT.  For a dataset containing data on Days 1 and 8, a unique",
                "identifier comprised of ID and DAY may be needed."
                )
            )
          )),
          column(3,wellPanel(
            p(HTML(paste(tags$b("Data Converting:"),("Variables can be converted to categorical/continuous")))),
            code("Caution: dataset will be modified"),
            hr(),
            uiOutput("CONVERTCON"),
            uiOutput("CONVERTCAT"), 
            uiOutput("TRANSFORMCON")
          )),
          column(3,wellPanel(
            p(HTML(paste(tags$b("Data Subsetting:"),("Observations can be filtered and variables can be removed")))),
            code("Caution: dataset will be modified"),
            hr(),
            selectInput('subset_type',label="Filter by:",choices=c("NONE","COLUMN","ROW"),selected="NONE"),
            conditionalPanel("input.subset_type=='ROW'", textInput("rows","Rows","Enter filter condition (e.g., TIME<=24)...")),
            conditionalPanel("input.subset_type=='ROW'", 
            bsPopover("rows","Common R Logic Syntax",trigger="focus",options=list(container="body"),
              content = paste0("<p>Less Than: < ; Greater Than: > ;</p>","<p>Less Than or Equal To: <= ;</p>",
              "<p>Greater Than or Equal To: >= ;</p>","<p>Equal To: = ; Not Equal To: != ;</p>",
              "<p>Is Missing: is.na(); </p>","<p>Is Not Missing: !is.na() ;</p>",
              "<p>Group Membership: %in% ; </p>","<p>Boolean operators: &, |, !, xor, any, all</p>")
            )),
            #checkboxInput('nafilt','Remove variables with all missing values',value=TRUE),
            conditionalPanel("input.subset_type=='ROW'", checkboxInput('nafilt','Remove variables with all missing values',value=TRUE)),
            conditionalPanel("input.nafilt==true", 
              bsTooltip('nafilt',"Automatically remove variables that only have missing values",options=list(container="body"))),     
            conditionalPanel("input.subset_type=='COLUMN'",uiOutput("COLUMNS"))
          )),
          column(3,wellPanel(
            p(HTML(paste(tags$b("Data Sorting:"),("Order of observations can be re-ordered, based on selected variables")))),
            code("Caution: dataset will be modified"),
            hr(),
            uiOutput("SORTS")
          ))
        )),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Calculate",fluidRow(
          
          )
        ),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Explore",fluidRow(
          column(3,wellPanel(selectInput('data_type_ex',"Dataset:",c("RAW DATA","NCA PK"),selected="RAW DATA"),
            bsTooltip('data_type_ex',placement="right",options=list(container="body"),
              "Choose one dataset to explore:  raw concentration data or PK parameters derived from noncompartmental analysis"),                      
            uiOutput("convars"), uiOutput("catvars"),
            conditionalPanel("data_type_ex=='RAW DATA'",checkboxInput('exid',"Group by unique identifier")),
            bsTooltip('exid',"Do you want subject-level data instead?",placement="right",options=list(container="body")),
            conditionalPanel("input.explore_type=='GRAPH'",uiOutput("colorvars"))
          ),
          wellPanel(
            selectInput('explore_type',"Results:",c("TABLE","GRAPH"),selected="TABLE"),
            bsTooltip('explore_type',"Select format of exploratory results",placement="right",options=list(container="body")),
            conditionalPanel("input.explore_type=='TABLE'",
              selectInput('stat_type',"Statistic",c("N","MEAN","MEAN 95% CI","MEDIAN","PERCENT.CV","RANGE","PERCENTILES"),selected="N")),
            conditionalPanel("input.explore_type=='TABLE'",bsTooltip('stat_type',"Select summary statistic",options=list(container="body"))),
            conditionalPanel("input.explore_type=='TABLE'",
              sliderInput('exslid',"Percentile(s):",min=0,max=100,value=c(25,75),step=5,round=T)),
            conditionalPanel("input.explore_type=='TABLE'",bsTooltip('exslid',"Select percentile range",options=list(container="body"))),
            conditionalPanel("input.explore_type=='TABLE'",downloadButton('exportstats',"Download summary statistics")),
            conditionalPanel("input.explore_type=='TABLE'",
              bsTooltip('exportstats',"file name is output-stats.csv",placement="right",options=list(container="body")))               
          )),
          column(9,
            conditionalPanel("input.explore_type=='GRAPH'",plotOutput('explot',height="600px")),
            conditionalPanel("input.explore_type=='GRAPH'", 
              bsPopover("explot","Scatterplot Matrix Graphs",options=list(container="body"),placement="left",
                content=paste0("<p>Diagonal:</p><p>* continuous=histogram</p><p>* categorical=barplot</p>",
                               "<p>Upper Triangle:</p><p>* continuous=correlation coefficient</p>",
                               "<p>* categorical=ratioplot</p><p>* combo=boxplot</p>",
                               "<p>Lower Triangle:</p><p>* continuous=loess smoother</p>",
                               "<p>* categorical=grouped barplot</p><p>* combo=grouped histogram</p>")
              )
            ),
            conditionalPanel("input.explore_type=='TABLE'",dataTableOutput('extable'))            
          )
        )),
        ############################################################################################################################
        ####################################################################,########################################################
        tabPanel("Data Table",fluidRow(column(12,
          wellPanel(fluidRow(
            column(8,selectInput('data_type_tbl',"Dataset:",c("RAW DATA","NCA PK"),selected="RAW DATA"),
              bsTooltip('data_type_tbl',placement="right",options=list(container="body"),
                "Choose one dataset:  raw concentration data or PK parameters derived from noncompartmental analysis")
              ),
            column(4,p("Export table"),downloadButton('exporttable', 'Download data table'),
              bsTooltip('exporttable',"file name is output-table.csv",options=list(container="body"))
            )
          )),  
          dataTableOutput('contents'),
          bsPopover("contents","Data Table Features",options=list(container="body"),placement="top",trigger="hover",
            content=paste0("<p>Number of viewed rows can be selected in upper left corner</p>",
              "<p>Global search in upper right corner</p>",
              "<p>Table can be filtered by entering values under columns and sorted in ascending or descending order by selecting column header</p>")
          )
        ))),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Plot",fluidRow(
            column(4,
              fluidRow(
                column(7,wellPanel(
                  selectInput('data_type_plot',"Dataset:",c("RAW DATA","NCA PK"),selected="RAW DATA"),
                  bsTooltip('data_type_plot',"Choose one dataset:  raw concentration data or PK parameters derived from noncompartmental analysis",
                            placement="right",options=list(container="body")),
                  selectInput('plot_type',"Plot Type:",c("SCATTERPLOT","HISTOGRAM","BARPLOT","BOXPLOT"),selected="SCATTERPLOT"),
                  bsTooltip('plot_type',"Choose a plot type: scatterplot, histogram, barplot, boxplot",
                            placement="right",options=list(container="body")),
                  uiOutput('xvars'), radioButtons('xconvert',"Convert to:",c("Categorical","Continuous"),inline=T),
                  bsTooltip('xconvert',"Select data type for X variable",placement="right",options=list(container="body")),
                  conditionalPanel("input.plot_type!='HISTOGRAM'",uiOutput('yvars')),
                  conditionalPanel("input.plot_type!='HISTOGRAM'",
                  radioButtons('yconvert',"Convert to:",c("Categorical","Continuous"),selected="Continuous",inline=T)),
                  conditionalPanel("input.plot_type!='HISTOGRAM'", 
                    bsTooltip('yconvert',"Select data type for Y variable",placement="right",options=list(container="body")))
                ),
                  wellPanel(uiOutput("colors"),conditionalPanel("input.plot_type=='SCATTERPLOT'",uiOutput("shapes"))
                )
              ),    
                column(5,wellPanel(
                  selectInput('strata_type',"Stratification:",choices=c("NONE","PANEL","GRID"),selected="NONE"),
                  bsPopover('strata_type',placement="right",title="Stratification Type:",options=list(container="body"),trigger="focus",
                    paste0("<p>Panel: strings together plots in different frames based in a single variable</p>",
                           "<p>Grid: lays out plots in a 2-dimensional (rows and columns) in a grid format</p>")
                  ),
                  conditionalPanel("input.strata_type=='PANEL'",uiOutput("stratas")),
                  conditionalPanel("input.strata_type=='GRID'",uiOutput("rowstratas")),
                  conditionalPanel("input.strata_type=='GRID'",uiOutput("colstratas")),
                  conditionalPanel("input.strata_type!='NONE'",selectInput('scale_type',"Axis Scale:",c("FIXED","FREE_X","FREE_Y","FREE"),selected="fixed")),
                  conditionalPanel("input.strata_type!='NONE'",bsTooltip('scale_type',placement="right",options=list(container="body"),
                    "Axis scales in panels can either be fixed and dynamic in the x or y direction"))
                ),
                  wellPanel(
                    conditionalPanel("input.plot_type=='HISTOGRAM'", uiOutput('bins')),
                    conditionalPanel("input.plot_type=='SCATTERPLOT'",
                      radioButtons("jitter","Jitter Points",list("Yes"="jitter","No"="identity"),selected="identity",inline=T)),
                    conditionalPanel("input.plot_type=='SCATTERPLOT'",bsTooltip('jitter',placement="right",options=list(container="body"),
                      "Adds some noise to datapoints for visualization")),
                    conditionalPanel("input.plot_type=='BOXPLOT'",
                      radioButtons("jitterb","Jitter Points",list("Yes"="jitter","No"="identity"),selected="identity",inline=T)),
                    conditionalPanel("input.plot_type=='BOXPLOT'",bsTooltip('jitterb',placement="right",options=list(container="body"),
                      "Displays datapoints and ddds some noise for visualization")),
                    selectInput('line_type',"Add line:",c("NONE","MEAN","REGRESSION","TRENDLINE","REFERENCE")),
                    bsPopover('line_type',placement="right",options=list(container="body"),title="Line Type",trigger="focus",
                      paste0("<p>none: no line is plotted</p>",
                             "<p>Mean: mean line and 5th and 95th percentiles for each group (if applicable) are plotted</p>",
                             "<p>Regression: linear regression line and 95% CI for each group (if applicable) are plotted</p>",
                             "<p>Trendline: loess smoother line and 95% CI for each group (if applicable) are plotted</p>",
                             "<p>Reference: horizontal or vertical or both lines are plotted</p>")
                    ),
                    conditionalPanel("input.line_type=='REFERENCE'",textInput('yref',"Horizontal Lines:")),                  
                    conditionalPanel("input.line_type=='REFERENCE'",bsTooltip('yref',placement="right",options=list(container="body"),
                      "Enter x values (group number for categorical variables)  for reference lines")),
                    conditionalPanel("input.line_type=='REFERENCE'",textInput('xref',"Vertical Lines:")),
                    conditionalPanel("input.line_type=='REFERENCE'",bsTooltip('xref',placement="right",options=list(container="body"),
                      "Enter y values (group number for categorical variables) for reference lines")),
                    conditionalPanel("input.line_type=='REGRESSION'",verbatimTextOutput('r2')) ,
                    conditionalPanel("input.line_type=='REGRESSION'",bsTooltip('r2',placement="right",options=list(container="body"),
                      "provides overall coefficient of determination"))
                  ) 
                )            
              )  
            ),
            column(8,
              fluidRow(column(12,
                column(2,checkboxGroupInput("log","Log Scale",c("X","Y"),inline=T)),    
                bsTooltip('log',"Change axis to log scale?",options=list(container="body")),
                column(2,checkboxGroupInput("rev","Reverse Scale",c("X","Y"),inline=T)),
                bsTooltip('rev',"Change axis to reverse scale?",options=list(container="body")),
                column(2,checkboxInput("flip","Axis X<->Y",FALSE)),
                bsTooltip('flip',"Switch X & Y axes?",options=list(container="body")),
                column(2,
                  conditionalPanel("input.plot_type=='SCATTERPLOT'",actionButton('plotdata',"Get Data",icon=icon("search"))),
                  conditionalPanel("input.plot_type=='BOXPLOT'",actionButton('plotdata',"Get Data",icon=icon("search"))),
                  bsTooltip('plotdata',"Identify selected datapoints",options=list(container="body")),
                  bsModal('plotdatatable',"Selected datapoints (only 1st 12 variables are shown)",
                          'plot',size="large",dataTableOutput('brush_data'))
                  #bsTooltip('plotdatatable',placement="left",options=list(container="body"),trigger="click",
                  #          "Only 1st 12 variables are shown; use Data Process to remove unwanted variables")
                ),
                column(2,downloadButton("exportplot","Download plot")),
                bsTooltip('exportplot',"file name is output-plot.csv",options=list(container="body"))
              )),
              fluidRow(column(12,  
              plotOutput('plot',click="plot_click",dblclick="plot_dblclick",hover="plot_hover",height="600px",
                brush=brushOpts(id="plot_brush",resetOnNew=TRUE)),
              bsTooltip('plot',placement="left",options=list(container="body"),
                "Highlight section to select datapoint(s); Double-click to zoom in and out")
              #verbatimTextOutput('brush_info')
              ))
            )
          )
        ),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Evaluation",
          fluidRow(
            column(3, 
              wellPanel(
                #actionButton("run","Run",icon=icon("calculator")),
                selectInput('output_type',label="Results:",choices=c("TABLE","GRAPH"),selected="TABLE"),
                bsTooltip('output_type',placement="right",options=list(container="body"),"Select format of results"),
                conditionalPanel("input.output_type=='GRAPH'",checkboxInput("pk_number","Group by Number of Timepoints",value=FALSE)),            
                conditionalPanel("input.output_type=='GRAPH'",
                  bsTooltip('pk_number',placement="right",options=list(container="body"),
                    "Results grouped by number of timepoints are plotted"))
              ),
              wellPanel(
                strong("Specify criteria:"),uiOutput('TIMEPTS'),uiOutput('timeptslider'),uiOutput('cmaxslider'),uiOutput('aucslider'),
                hr(),checkboxInput('pk_outlier', "Evaluate individual values", value = FALSE),
                bsTooltip('pk_outlier',placement="right",options=list(container="body"),
                  "More conservative approach such that individual values have acceptable bias"),
                conditionalPanel("input.pk_outlier==true", uiOutput('indcmaxslider')),
                conditionalPanel("input.pk_outlier==true", uiOutput('indaucslider'))
              ),
              wellPanel(
                downloadButton('export',"Download results"),
                bsTooltip('export',placement="right",options=list(container="body"),"file name is output-results.csv")             
              )  
            ),
            column(9,  
              conditionalPanel("input.output_type=='TABLE'",dataTableOutput('pktable')),
              bsPopover('pktable',placement="left",options=list(container="body"),title="Performance",
                        content=paste0("<p>Bias: mean percentage prediction error (MPPE=1/n*(full-reduced)/reduced*100)</p>",
                                       "<p>Accuracy: root mean square prediction error (RMSE=sqrt(1/n*((full-reduced)/reduced)^2)*100)")),
              conditionalPanel("input.output_type=='GRAPH'",
                plotOutput('pkplot',click="pkplot_click",dblclick="pkplot_dblclick",hover="pkplot_hover",height="600px",
                brush=brushOpts(id="pkplot_brush",resetOnNew=TRUE))
              ),
              bsTooltip('pkplot',placement="left",options=list(container="body"),
                        "Highlight section to select datapoint(s); Double-click to zoom in and out"),
              bsModal('plotresultstable',"Selected datapoints",'pkplot',size="large",dataTableOutput('pk_data'))
            )
          )
        ),
        tabPanel("Data TEST",fluidRow(verbatimTextOutput('test'),dataTableOutput('test2')))
      )))        
    )
