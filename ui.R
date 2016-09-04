ui <- fluidPage(
      fluidRow(
        #column(1,img(src="PMX.jpg",height=50,width=75),align="left"),       
        column(12,titlePanel("Pharmacometric Web Application:  PK Data Exploration and Simple Sampling Strategy"))
        #column(1,img(src="Takeda.jpg",height=50,width=125),align="right")
      ),
      fluidRow(column(12, tabsetPanel(
        tabPanel("Information",     
          h3(HTML(paste(tags$b("Objective:"),("This web application provides a simple visualization tool for looking at simulations.")))),
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
            tags$li(h4(HTML(paste(tags$b("Data Calculate:"),("perform non-compartmental analysis"))))),
            tags$li(h4(HTML(paste(tags$b("Data Explore:"),("summarize variables; pairs plots of correlations"))))),
            tags$li(h4(HTML(paste(tags$b("Data Table:"),("data is presented in a tabular format with sort, filter, and search functions"))))),
            tags$li(h4(HTML(paste(tags$b("Data Plot:"),("data is presented in a graph with highlight, zoom, and paneling functions for data point identification"))))),
            tags$li(h4(HTML(paste(tags$b("Data Evaluation:"),("data is analyzed to present user with permuatation of timepoints that meet user criteria which includes number of timepoints, %error in Cmax, and %error in AUC")))))
          ),
          br(),
          h3(HTML(paste(tags$b("Author:"),("Max Tsai")))),
          h4(HTML(paste(tags$b("Acknowledgements:"),("Shaokui Ge for his contributions to this application")))),
          h4(HTML(paste(em("Disclaimer:  This application has not been validated and should be used at the discretion of the user")))),
          br(),
          actionButton('introq',label="Version History"),
          bsModal('intropage',"Version History",'introq',size="large",
            tags$ul(
              tags$li(h4(HTML(paste(tags$b("v1.0:"),("web application is launched"))))),
              tags$li(h4(HTML(paste(tags$b("v1.1:"),("added a mean line to plot and allow user to specify specific timepoints for evaluation"))))),
              tags$li(h4(HTML(paste(tags$b("v1.2:"),("allow axis to vary, based on data range during stratification"))))),
              tags$li(h4(HTML(paste(tags$b("v1.5:"),("major plotting additions:  1) added histograms (plus binwidth specifications), barplots, boxplots; 2) enable conversion of variables to categorical or continuous; 3) added line options of linear regression (plus r2 value), loess smoother, or reference lines; 4) added axis options for log scale, reverse scale, and axis switching; 5) added point option to enable jitter"))))),
              tags$li(h4(HTML(paste(tags$b("v1.7"),("added tab for data exploration"))))),
              tags$li(h4(HTML(paste(tags$b("v1.8"),("calculated NCA parameters"))))),
              tags$li(h4(HTML(paste(tags$b("v1.9"),("added more summary stats"))))),
              tags$li(h4(HTML(paste(tags$b("v2.0"),("added NONMEM compatability and download capabilities"))))),
              tags$li(h4(HTML(paste(tags$b("v2.1"),("added tooltips"))))),
              tags$li(h4(HTML(paste(tags$b("v2.2"),("added help windows")))))
            )
          )
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
              fileInput("file", p('Data File:'), multiple=TRUE,
                        accept=c(".csv","text/comma-separated-values,trext/plain",".tab",".tbl",".dat",".xls",".xlsx")),
              bsTooltip('file',"comma delimited file is default",placement="right",options=list(container="body")),
              hr(),
              tags$b("File Import Options:"),
              checkboxInput("header", "Header (Y/N)", TRUE), 
              bsTooltip('header',"Is there a row for column names?","right",options=list(container="body")),
              checkboxInput("rownames", "Row names (Y/N)", FALSE), 
              bsTooltip('rownames',"Is there a column for row names?","right",options=list(container="body")),
              checkboxInput("strings", "Strings as Factors (Y/N)", FALSE), 
              bsTooltip('strings',"Should character data be treated as categorical data?","right",options=list(container="body")),
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
              bsTooltip('loadq',"see help file for Data Load Panel","right",options=list(container="body")),
              actionButton('loadq',label="",icon=icon("question-circle")),
              bsModal('loadpage',"Data Load",'loadq',size="large",
                p(strong("User options:")),
                p("Data files with following extensions are automatically displayed and listed: *.csv,*.txt,*.tab,*.tbl,*.dat;",
                  "for all other file extensions, choose all files in popup file selection window;",
                  "currently sas & excel files can not imported in their native formats and should be converted to *.txt or *.csv."),
                p("File options include importing first row as the column names, first column as row names,",
                  "character data treated as categorical variables."),
                p("The character for a missing value can be specified; '.' is the default character."),
                p("The character for a delimiter can be specified; comma is the default delimiter;",
                  "other options include semicolon, tab, space, and pipe-delimited."),
                p("The character for a quote style can be specified; none is the default."),
                p("NONMEM formatted output tables including NONMEM simulations with multiple replicates can be imported."),
                br(),
                p(strong("Display output:")),
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
              verbatimTextOutput('structure'), verbatimTextOutput('summary'), 
              bsTooltip('structure',"Data structure: variable type and initial values","left",options=list(container="body")),
              bsTooltip('summary',placement="left",options=list(container="body"),
                paste("Data summary statistics: mean, range, and percentiles for continuous variables",
                      "and count frequency for categorical variables")
              )            
            )
          )
        ),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Process",fluidRow(
          column(4,wellPanel(
            strong(textOutput('MAP')),bsTooltip('MAP',"Appropriate variables need to be mapped to these keys"),
            code("Caution: dataset will be modified"),hr(),uiOutput('ID'),uiOutput('TIME'),uiOutput('DV'),
            conditionalPanel("input.sim==true",uiOutput('REP')),uiOutput('UNIQUE'),
            p(HTML(paste(tags$b("Example levels of unique identifier:")))),textOutput('UNIQUEID'),hr(),
            bsTooltip('processq',"see help file for Data Process Panel",placement="right",options=list(container="body")),
            actionButton('processq',label="",icon=icon("question-circle")),
            bsModal('processpage',"Data Process",'processq',size="large",
              p(strong("User options:")),
              p(HTML(paste(tags$em("Data Mapping:"),"Certain key variables such as subject, time, and concentration",
                "need to be properly mapped to the appropriate variables in the dataset.  The default variable for subject is ID;",
                "the default variable for time is TIME; the default variable for concentration is DV;",
                "if applicable, the default variable for replicate is REP.",
                "If these named variables are located in the dataset, then mapping is automatic and no action is needed by user."))),
              p("A unique identifer is required.  By default, ID is assigned to the unique identifier UNIQUE.",
                "If there are multiple DV values (excluding TIME) for a given ID value, then a unique identifer needs to be defined.",
                "For example, for a dataset containing parent and metabolite concentrations defined by different CMT values",
                "a unique identifer may be comprised of ID and CMT.  For a dataset containing data on Days 1 and 8, a unique",
                "identifier comprised of ID and DAY may be needed."
                ),
              p(HTML(paste(tags$em("Data Transformation:"),
                "One or more variables can be converted from categorical to continuous and vice versa.",
                "If variable is not found, please see Data Load panel to see how the variable was imported",
                "Continuous variables can also be transformed to form a new variable",
                "such as absolute value, exponential, log, natural log, or square root",
                "Please note that only 1 new variable can be created at this time. The user needs to name this new variable."))
                ),
              p(HTML(paste(tags$em("Data Subsetting:"),
                "Observations can be filtered and variables can be removed.",
                "For columns, select variable(s) that should be retained in the dataset",
                "For rows, enter criteria to be used to retain observations.  Standard operators include:",
                tags$ul(
                  tags$li(HTML(paste(tags$b("Less Than:"),(" <   ;"),tags$em("Greater Than:"),(" >")))),
                  tags$li(HTML(paste(tags$b("Less Than or Equal to:"),("<=   ;"),tags$em("Greater Than or Equal to:"),("<=")))),
                  tags$li(HTML(paste(tags$b("Equal to:"),(" ==   ;"),tags$em("Not Equal to:"),(" !=")))),
                  tags$li(HTML(paste(tags$b("Is Missing:"),(" is.na()   ;"),tags$em("Is Not Missing:"),(" !is.na()")))),
                  tags$li(HTML(paste(tags$b("Group Membership:"),(" %in%")))),
                  tags$li(HTML(paste(tags$b("Boolean operators:"),(" &, |, !, xor, any, all"))))
                )
              ))),
              p(HTML(paste(tags$em("Data Sorting:"),
                "One or more variables can be selected to sort the dataset in ascending order",
                "The hierarchy of the sorting variables is in the order that was selected"
              ))),
              br(),
              p(strong("Display output:")),
              p("See Data Table panel to view new dataset")
            )
          )),
          column(4,wellPanel(
            strong(textOutput('TRANS')),bsTooltip('TRANS',"Variables can be converted to categorical/continuous or transformed to form a new variable"),
            code("Caution: dataset will be modified"),hr(),uiOutput("CONVERTCON"),uiOutput("CONVERTCAT"), 
            selectInput('transform',"Transformation",c("NONE","ABS(X)","EXP(X)","LN(X)","LOG10(X)","SQRT(X)"),"NONE"),
            bsTooltip('transform',"create a new variable and add to dataset",placement="right",options=list(container="body")),
            conditionalPanel("input.transform!='NONE'",
              fluidRow(column(6,uiOutput('TRANSFORM')),
                       column(6,
                         textInput('newvar',"New Variable"),
                         bsTooltip('newvar',"name the new variable",placement="right",options=list(container="body"))
                         )
                       )
            )  
          )),
          column(4,wellPanel(
            strong(textOutput('SUB')),bsTooltip('SUB',"Observations can be filtered and variables can be removed"),
            code("Caution: dataset will be modified"),hr(),
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
          ),
          wellPanel(
            strong(textOutput('SORT')),bsTooltip('SORT',"Order of observations can be re-ordered, based on selected variables"),
            code("Caution: dataset will be modified"),
            hr(),
            uiOutput("SORTS")
          ))
        )),
        ############################################################################################################################
        ############################################################################################################################
        #tabPanel("Data Calculate",fluidRow(
        #  dataTableOutput('test')
        #)
        #),
        ############################################################################################################################
        ############################################################################################################################
        tabPanel("Data Explore",fluidRow(
          column(2,wellPanel(
            selectInput('data_type_ex',"Dataset:",c("RAW DATA","NCA PK"),selected="RAW DATA"),
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
            bsTooltip('exploreq',"see help file for Data Explore Panel",placement="right",options=list(container="body")),
            actionButton('exploreq',label="",icon=icon("question-circle")),
            bsModal('explorepage',"Data Explore",'exploreq',size="large",
              p(strong("User options:")),
              p(HTML(paste(tags$em("Dataset:"),"User has two options:",
               "the raw concentration dataset or a dataset containing PK parameters."))),
              tags$ul(
                tags$li(HTML(paste(tags$b("Raw concentration dataset:"),(" the original dataset containing ID,TIME,DV")))),
                tags$li(HTML(paste(tags$b("PK dataset:"),(" Cmax, Tmax, and AUC derived using noncompartmental methods"))))
              ),
              p(HTML(paste(tags$em("Results:"),"User has two output options: graphical or tabular."))),
              tags$ul(
                tags$li(HTML(paste(tags$b("Graph:"),(" shows distribution and correlation between variables")))),
                tags$li(HTML(paste(tags$b("Table:"),(" User can select from the following summary statistics:")))),
                tags$ul(
                  tags$li(HTML(paste(tags$em("N:"),(" length of variable")))),
                  tags$li(HTML(paste(tags$em("Mean:"),(" mean value for the variable")))),
                  tags$li(HTML(paste(tags$em("Mean 95% CI:"),(" 95% CI of the mean value (+/-1.96*standard error)")))),
                  tags$li(HTML(paste(tags$em("Median:"),(" median value for the variable")))),
                  tags$li(HTML(paste(tags$em("Percent CV"),
                    (" percent coefficient of variation (standard deviation/mean*100%)")))),
                  tags$li(HTML(paste(tags$em("Range:"),(" range of values for the variable")))),
                  tags$li(HTML(paste(tags$em("Percentiles:"),(" percentiles for the variable defined by slider"))))                    
                )  
              ),
              p(HTML(paste(tags$em("Download:"),(" User can download summary statistics to file called output-stats.csv.")))),
              br(),
              p(strong("Display output:")),
              p("Graph default display settings:"),
              tags$ul(
                tags$li(HTML(paste(tags$b("Diagonal:")," * continuous=histogram; * categorical=barplot"))),
                tags$li(HTML(paste(tags$b("Upper:")," * continuous=correlation coefficient;",
                                                     " * categorical=ratioplot; * combo=boxplot"))),
                tags$li(HTML(paste(tags$b("Lower:")," * continuous=loess smoother;",
                                                     " * categorical=grouped barplot; * combo=grouped histogram")))
              )
            )
          )),
          column(2, wellPanel(
            conditionalPanel("input.explore_type=='TABLE'",
            bsTooltip('stat_type',"Select summary statistic",options=list(container="body"))),
            conditionalPanel("input.explore_type=='GRAPH'",strong("Diagonal:")),
            fluidRow(
              column(7,
                conditionalPanel("input.explore_type=='GRAPH'",
                  selectInput('diagcon',"Continuous",c("DENSITY","BAR","BLANK"),selected="DENSITY"))
              ),
              column(5,
                conditionalPanel("input.explore_type=='GRAPH'",
                  selectInput('diagcat',"Categorical",c("BAR","BLANK"),selected="BAR"))
              )
              ),  
            conditionalPanel("input.explore_type=='TABLE'",
              sliderInput('exslid',"Percentile(s):",min=0,max=100,value=c(25,75),step=5,round=T)),
            conditionalPanel("input.explore_type=='TABLE'",
              bsTooltip('exslid',"Select percentile range",options=list(container="body"))),
            conditionalPanel("input.explore_type=='TABLE'",
              selectInput('stat_type',"Statistic",c("N","MEAN","MEAN 95% CI","MEDIAN","PERCENT.CV","RANGE","PERCENTILES"),
                          selected="N")),
            conditionalPanel("input.explore_type=='TABLE'",hr()),
            conditionalPanel("input.explore_type=='TABLE'",downloadButton('exportstats',"Download statistics")),
            conditionalPanel("input.explore_type=='TABLE'",
            bsTooltip('exportstats',"file name is output-stats.csv",placement="right",options=list(container="body"))),
            conditionalPanel("input.explore_type=='GRAPH'",strong("Upper Triangle:")),
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('uppercon',"Continuous",c("POINTS","SMOOTH","DENSITY","COR","BLANK"),selected="COR")),
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('uppercat',"Categorical",c("FACETBAR","RATIO","BLANK"),selected="RATIO")),
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('uppercombo',"Combination",c("BOX","DOT","FACETHIST","FACETDENSITY","DENSTRIP","BLANK"),
                          selected="BOX")),
            conditionalPanel("input.explore_type=='GRAPH'",strong("Lower Triangle:")),
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('lowercon',"Continuous",c("POINTS","SMOOTH","DENSITY","COR","BLANK"),selected="SMOOTH")),        
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('lowercat',"Categorical",c("FACETBAR","RATIO","BLANK"),selected="FACETBAR")),
            conditionalPanel("input.explore_type=='GRAPH'",
              selectInput('lowercombo',"Combination",c("BOX","DOT","FACETHIST","FACETDENSITY","DENSTRIP","BLANK"),
                            selected="FACETHIST"))
            )
          ),            
          column(8,
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
            column(2,selectInput('data_type_tbl',"Dataset:",c("RAW DATA","NCA PK"),selected="RAW DATA"),
              bsTooltip('data_type_tbl',placement="right",options=list(container="body"),
                "Choose one dataset:  raw concentration data or PK parameters derived from noncompartmental analysis")
              ),
            column(2,br(),
              downloadButton('exporttable','Download data table'),
              bsTooltip('exporttable',"file name is output-table.csv",options=list(container="body"))
            ),
            column(2,
              br(),
              bsTooltip('tableq',"see help file for Data Table Panel",placement="right",options=list(container="body")),
              actionButton('tableq',label="",icon=icon("question-circle")),
              bsModal('tablepage',"Data Table",'tableq',size="large",
                p(strong("User options:")),
                p(HTML(paste(tags$em("Dataset:"),"User has two options:",
                "the raw concentration dataset or a dataset containing PK parameters."))),
                tags$ul(
                  tags$li(HTML(paste(tags$b("Raw concentration dataset:"),(" the original dataset containing ID,TIME,DV")))),
                  tags$li(HTML(paste(tags$b("PK dataset:"),(" Cmax, Tmax, and AUC derived using noncompartmental methods"))))
                ),
                p("Number of viewed rows can be selected in upper left corner.  Global search in upper right corner",
                  "Table can be filtered by entering values under columns and",
                  "sorted in ascending or descending order by selecting column header."),
                br(),
                p(HTML(paste(tags$em("Download:"),(" User can download data table to file called output-table.csv.")))),
                br(),
                p(strong("Display output:")),
                p("The default table display contains all data.",
                  "Subsets of data may be displayed, based on selected variables and filter criteria.",
                  "New variables will also be displayed.")
              )
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
                      "Displays datapoints and adds some noise for visualization")),
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
                column(2,actionButton('show',"Get Data",icon=icon("search"))),
                  #conditionalPanel("input.plot_type=='SCATTERPLOT'",actionButton('plotdata',"Get Data",icon=icon("search"))),
                  #conditionalPanel("input.plot_type=='BOXPLOT'",actionButton('plotdata',"Get Data",icon=icon("search"))),
                #),
                bsTooltip('show',"Show selected data",options=list(container="body")),
                column(2,downloadButton("exportplot","Download plot")),
                bsTooltip('exportplot',"file name is output-figure.png",options=list(container="body")),
                column(2,actionButton('plotq',label="",icon=icon("question-circle"))),
                bsModal('plotpage',"Data Plot",'plotq',size="large",
                  p(strong("User options:")),
                  p(HTML(paste(tags$em("Dataset:"),"User has two options:",
                  "the raw concentration dataset or a dataset containing PK parameters."))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Raw concentration dataset:"),(" the original dataset containing ID,TIME,DV")))),
                    tags$li(HTML(paste(tags$b("NCA PK dataset:"),(" Cmax, Tmax, and AUC derived using noncompartmental methods"))))
                  ),
                  p(HTML(paste(tags$em("Plot Types:"),"User can select plot type:"))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Scatterplot:"),(" a graph used to display values for typically two variables using Cartesian coordinates for a set of data.")))),
                    tags$li(HTML(paste(tags$b("Histogram:"),(" a graph of the distribution of numerical data. It is an estimate of the probability distribution of a continuous variable.")))),
                    tags$li(HTML(paste(tags$b("Barplot:"),(" a graph that presents Grouped data with rectangular bars with lengths proportional to the values that they represent.")))),
                    tags$li(HTML(paste(tags$b("Boxplot:"),(" a graph displaying the distribution of data based on the five number summary: minimum, first quartile, median, third quartile, and maximum."))))
                  ),
                  p(HTML(paste(tags$em("X & Y Variables:"),"User can select variables for X and Y (if necessary) variables.",
                    "Variables can be toggled between continuous and categorical classification",
                    "which affects how the plot is displayed."))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Raw concentration dataset:"),(" TIME and DV are the default X and Y variables.")))),
                    tags$li(HTML(paste(tags$b("NCA PK dataset:"),(" CMAX and AUC are the default X and Y variables."))))
                  ),
                  p(HTML(paste(tags$em("Grouping Options:"),"User can various visual aids to discriminate different groups:"))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Categorical groupings:"),(" a discrete color and/or shape is assigned to each categorical level")))),
                    tags$li(HTML(paste(tags$b("Continuous groupings:"),(" a spectrum of colors are assigned, based on the range of values; shape grouping is not applicable"))))
                  ),
                  p(HTML(paste(tags$em("Plot Options:"),"User can add a little noise to datapoints to enhance visualization or add a line:"))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Mean line:"),(" adds a mean line plus shaded region covering 5-95th percentiles")))),
                    tags$li(HTML(paste(tags$b("Regression line:"),(" adds a linear regression plus shaded region covering 95th percent confidence intervals")))),
                    tags$li(HTML(paste(tags$b("Trendline:"),(" adds a loess smoother plus shaded region covering 95th percent confidence intervals")))),
                    tags$li(HTML(paste(tags$b("Reference line(s):"),(" adds either vertical or horizonal lines or both; shaded regions are added if multiple lines are added"))))
                  ),
                  p(HTML(paste(tags$em("Axis Options:"),"User can manipulate how axes are displayed:"))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Log scale:"),(" uses the logarithm of a physical quantity instead of the quantity itself and useful for wide data ranges")))),
                    tags$li(HTML(paste(tags$b("Reverse scale:"),(" uses the reverse order of the scale and may be useful for data with negative values")))),
                    tags$li(HTML(paste(tags$b("Axis X<->Y:"),(" switches x and y axes"))))
                  ),
                br(),
                p(HTML(paste(tags$em("Download:"),(" User can download plot to file called output-figure.png.")))),
                br(),
                p(strong("Display output:")),
                p("The plot will be displayed, based on user selected options"),
                p("Highlight section to select datapoint(s); Double-click to zoom in and out"),
                p("Only first 12 variables are presented; use Data Process to remove unnecessary variables")
                )
              )),
              fluidRow(column(12,  
              plotOutput('plot',click="plot_click",dblclick="plot_dblclick",hover="plot_hover",height="600px",
                brush=brushOpts(id="plot_brush",resetOnNew=TRUE)),
              bsTooltip('plot',placement="left",options=list(container="body"),
                "Highlight section to select datapoint(s); Double-click to zoom in and out"),
              #bsTooltip('plotdata',"Identify selected datapoints",options=list(container="body")),
              bsModal('plotdatatable',"Selected datapoints (only 1st 12 variables are shown)",
                      'show',size="large",dataTableOutput('brush_data'))
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
              ),
                actionButton('evaluationq',label="",icon=icon("question-circle")),
                bsModal('evaluationpage',"Data Evaluation",'evaluationq',size="large",
                p(strong("User options:")),
                  p(HTML(paste(tags$em("Results:"),"User has two options for presenting results:"))),
                  tags$ul(
                    tags$li(HTML(paste(tags$b("Table:"),(" tabulated results of bias (MPPE) & accuracy (RMSE)")))),
                    tags$li(HTML(paste(tags$b("Graph:"),(" graphical results of bias (MPPE); stratification by number of timepoints is an option"))))
                  ),
                  p(HTML(paste(tags$em("Criteria:"),"Threshold for desired level of mean bias in CMAX and AUC values can be achieved by adjusting slider.",
                    "Threshold for desired level of individual bias can be adjusted; this is a more conservative criteria"))),
                  br(),
                  p(HTML(paste(tags$em("Download:"),(" User can download result table to file called output-results.csv.")))),
                  br(),
                  p(strong("Display output:")),
                  p("The plot will be displayed, based on user selected options"),
                  p("Highlight section to select datapoint(s); Double-click to zoom in and out")
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
        )
        #tabPanel("Data TEST",fluidRow(verbatimTextOutput('test'),dataTableOutput('test2')))
      )))        
    )
