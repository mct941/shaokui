#shinyServer
#setwd("C:/users/mtsai/desktop")
library(gtools)
library(magrittr)
library(shiny)
library(ggplot2)
library(dplyr)
library(GGally)
library(readxl)
library(shinyBS)
library(lazyeval)
library(rdrop2)



decode <- function(x, search, replace, default = NULL) {
 
    # build a nested ifelse function by recursion
    decode.fun <- function(search, replace, default = NULL)
        if (length(search) == 0L) {
            function(x) if (is.null(default)) x else rep(default, length(x))
        } else {
            function(x) ifelse(x == search[1L], replace[1L],
                                                decode.fun(tail(search,  -1L),
                                                           tail(replace, -1L),
                                                           default)(x))
        }
 
    return(decode.fun(search, replace, default)(x))
}

read.nonmem <- function(file, n=-1) {
    ## auxiliary function to split text lines at blanks
    my.split <- function(line, numeric=FALSE) {
        pieces <- unlist(strsplit(line, split=" +"))[-1]

        if( numeric )
            return(as.numeric(pieces))
        else
            return(pieces)
    }

    cat(sprintf("Reading NONMEM data from '%s'\n", file))

    lines <- readLines(file, n) # read file as text
    cat(sprintf("- %d lines\n", length(lines)))

    idx <- substring(lines,1,1)!="T"
    cat(sprintf("- %d tables\n", sum(!idx)))

    lines <- lines[idx] # strip lines starting with T (TABLE NO ...)

    ## do we have header lines??
    if( length(grep("^ +[A-Za-z]", lines[1]))>0 ) { # yes!
        data <- sapply(lines[lines!= lines[1]], my.split, numeric=TRUE)
        header <-  my.split(lines[1])
        cat(sprintf("- file has column names (%s)\n", paste(header,collapse=", ")))
    } else {                                        # no
        data <- sapply(lines, my.split, numeric=TRUE)
        header <- sprintf("Column%02d", 1:nrow(data)) # make fake column names
        cat("- file has NO header names - creating names Column01, Column02, ...\n")
    }
    cat(sprintf("- %d columns\n", nrow(data)))

    ## transpose data and make a data.frame
    df <- data.frame(data[1,])
    for( i in 2:nrow(data))
        df <- cbind(df, data[i,])

    ## set column and row names
    rownames(df) <- NULL
    colnames(df) <- header

    cat("ok.\n")

    return(df)
}

stat.func <-function(x) c(min(x,na.rm=T),mean(x,na.rm=T),max(x,na.rm=T))
tmax.calc <- function(TIME,DV) TIME[which.max(DV)]
auc.calc <- function(TIME,DV) {n <- length(unique(TIME[!is.na(DV)])); sum(sapply(1:(n-1), FUN=function(x) {0.5*(TIME[x+1]-TIME[x])*(DV[x+1]+DV[x])}))}
factorize <- function(x) if(length(levels(as.factor(as.character(x))))<=8) x <- factor(x) else as.numeric(x)
pctcv.calc <- function(x) round(sd(x,na.rm=T)/mean(x,na.rm=T)*100)
range.calc <- function(x) paste(round(range(x,na.rm=T),2),collapse="~")
pct.calc <- function(x,p) paste(round(quantile(x,prob=p/100,na.rm=T),2),collapse="~")
se.calc <- function(x) sd(x,na.rm=T)/sqrt(length(x))
uci95.calc <- function(x) round(mean(x,na.rm=T) + 1.96*se.calc(x),digits=2)
lci95.calc <- function(x) round(mean(x,na.rm=T) - 1.96*se.calc(x),digits=2)
ci95.calc <- function(x) paste(lci95.calc(x),uci95.calc(x),sep="~")

server <- (function(input, output, session) { 
  inputdta <- reactive({inFile<-input$file;           
                        if(input$nonmem) dta <- read.nonmem(inFile$datapath) else
                        dta <- read.csv(inFile$datapath, header=input$header, sep=input$sep,quote=input$quote,strings=input$strings,na.strings=input$na)
                        return(dta)})
  output$dim <- renderText({if(is.null(input$file)) return(NULL)
                  c("File Name:", input$file$name, "(", "Observations:", dim(inputdta())[1], ";","Variables:", as.numeric(dim(inputdta())[2]),")")
                })
  
  output$structure <- renderPrint({if(is.null(input$file)) return(NULL); str(inputdta())})
  output$summary <- renderPrint({if(is.null(input$file)) return(NULL); summary(inputdta())})
    
  ############################################################################################################################
  ############################################################################################################################
    ### mapping variables
  output$MAP <- renderText("Data Mapping:")
  output$TRANS <- renderText("Data Transformation:")
  output$SUB <- renderText("Data Subsetting:")
  output$SORT <- renderText("Data Sorting:")
  
  
  output$ID <-  renderUI({if(is.null(input$file)) return(NULL)
                          tipify(selectInput("id","Subject",sort(names(inputdta())),selected="ID"),
                                 "ID is default variable name",placement="right",options=list(container="body"))
                          }) 
  output$TIME <-  renderUI({if(is.null(input$file)) return(NULL)
                            tipify(selectInput("time","Time",sort(names(inputdta())),selected="TIME"),
                                   "TIME is default variable name",placement="right",options=list(container="body"))
                            })   
  output$DV <-  renderUI({if(is.null(input$file)) return(NULL)
                          tipify(selectInput("dv","Concentration",sort(names(inputdta())),selected="DV"),
                                 "DV is default variable name",placement="right",options=list(container="body"))
                          }) 
  output$REP <-  renderUI({if(is.null(input$file)) return(NULL)
                           tipify(selectInput("rep","Simulation Replicate Number",sort(names(inputdta())),selected="REP"),
                                  "REP is default variable name",placement="right",options=list(container="body"))
                           }) 
  output$UNIQUE <-  renderUI({if(is.null(input$file)) return(NULL)
                          if(input$sim) tipify(selectizeInput("unique","Unique Identifier",sort(names(dta_map())),
                                               multiple=T,selected=list("ID","REP")),placement="right",options=list(container="body"),
                                               "Select variable(s) to create unique ID")
                          else tipify(selectizeInput("unique","Unique Identifier",sort(names(dta_map())),
                                      multiple=T,selected="ID"),placement="right",options=list(container="body"),
                                      "Select variable(s) to create unique ID")
                          }) 
  output$UNIQUEID <- renderPrint({if(is.null(input$file)) return(NULL)
                                  head(levels(dta_unique()$UNIQUE),20)
                                  })
  
  output$CONVERTCON <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('convertcon',"Convert from Continuous to Categorical Variable:",multiple=T,
                                          sort(names(dta_map()[sapply(dta_map(),is.numeric)]))),
                                     "Select one or more continuous variable(s)",placement="right",options=list(container="body"))
                            })
  output$CONVERTCAT <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('convertcat',"Convert from Categorical to Continuous Variable:",multiple=T,
                                          sort(names(dta_map()[sapply(dta_map(),is.factor)]))),
                                     "Select one or more categorical variable(s)",placement="right",options=list(container="body"))
                            })
  output$TRANSFORM <- renderUI({if(is.null(input$file)) return(NULL)
                                tipify(selectInput('transvar',"Variable:",sort(names(dta_convert()[sapply(dta_convert(),is.numeric)]))),
                                       "Select one continuous variable",placement="right",options=list(container="body"))
                                })
  
  output$COLUMNS <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('columns','Columns',sort(names(dta_transform())),multiple=TRUE), 
                                     "Select one or more variable(s) below in desired order to keep; use backspace to undo selection(s)",
                                     placement="left",options=list(container="body"))
                              }) 

  output$SORTS <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('sorts','Columns',sort(names(dta_filt())),multiple=TRUE),
                                     "Select one or more variable(s) below in desired order to keep; use backspace to undo selection(s)",
                                     placement="left",options=list(container="body"))
                            })
    
  dta_map <-  reactive({dta <- inputdta()
                        if(input$nonmem=="TRUE") dta %<>% filter(EVID==0|is.na(as.numeric(EVID)))
                        dta$V1 <- dta[,sprintf(input$id)] 
                        dta$V2 <- dta[,sprintf(input$time)] 
                        dta$V3 <- dta[,sprintf(input$dv)] 
                        if(input$sim=="TRUE") dta$V4 <- dta[,sprintf(input$rep)] 
                                                                  
                        if(input$id=="ID") dta %<>% select(-V1) else dta %<>% rename(ID=V1)
                        if(input$time=="TIME") dta %<>% select(-V2) else dta %<>% rename(TIME=V2)             
                        if(input$dv=="DV") dta %<>% select(-V3) else dta %<>% rename(DV=V3)
                        if(input$sim=="TRUE") {if(input$rep=="REP") dta %<>% select(-V4) else dta %<>% rename(REP=V4)}
                        return(dta)
                        })
  dta_unique <- reactive({dta <- dta_map()
                          dta %<>% mutate(ID=str_pad(ID,width=str_length(max(ID)),pad="0"))
                          dta %<>% unite_("UNIQUE",input$unique,remove=F)
                          dta %<>% arrange(UNIQUE,ID,TIME) %>% 
                                   mutate(UNIQUE=as.factor(UNIQUE),ID=as.numeric(ID),TIME=as.numeric(TIME),DV=as.numeric(as.character(DV)))                      
                          return(dta)
                          })
  
  dta_filtna <- reactive({if(input$nafilt) dta <- dta_unique()[,!unlist(lapply(dta_unique(), function(x) all(is.na(x))))]
                          else dta <- dta_unique()
                          return(dta)
                          })
  dta_convert <- reactive({ dta <- dta_filtna()
                            if(!is.null(input$convertcat)) dta[,input$convertcat] <- as.numeric(as.matrix(dta[,c(input$convertcat)]))
                            if(!is.null(input$convertcon)) dta %<>% mutate_each_(funs(factor),input$convertcon)
                            return(dta)
                            })
  dta_transform <- reactive({ dta <- dta_convert()
                            if(input$transform=="ABS(X)") dta %<>% mutate_(.dots=setNames(list(interp(~abs(x),
                                                                           x=as.name(input$transvar))),input$newvar))
                            if(input$transform=="EXP(X)") dta %<>% mutate_(.dots=setNames(list(interp(~round(exp(x),3),
                                                                           x=as.name(input$transvar))),input$newvar))
                            if(input$transform=="LN(X)") dta %<>% mutate_(.dots=setNames(list(interp(~round(log(x),3),
                                                                          x=as.name(input$transvar))),input$newvar))
                            if(input$transform=="LOG10(X)") dta %<>% mutate_(.dots=setNames(list(interp(~round(log10(x),3),
                                                                             x=as.name(input$transvar))),input$newvar))
                            if(input$transform=="SQRT(X)") dta %<>% mutate_(.dots=setNames(list(interp(~round(sqrt(x),3),
                                                                            x=as.name(input$transvar))),input$newvar))
                            return(dta)
                          })
  dta_filt <- reactive({dta <- dta_transform()
                        if(input$subset_type!="NONE") {
                          if(!is.null(input$columns)) dta %<>% select(one_of(paste("UNIQUE",input$columns)))
                          if(input$rows!="Enter filter condition (e.g., TIME<=24)...") dta %<>% filter_(input$rows) 
                          }    
                        return(dta)                    
                        })
  
  dta_sort <- reactive({ dta <- dta_filt()
                         if(!is.null(input$sorts)) dta %<>% arrange_(input$sorts)                  
                         return(dta)  
                        })
  dta <- reactive ({dta <- dta_sort()
                    #dta.factorize <- data.frame(lapply(dta.factorize,factorize)) %>% filter(!is.na(DV))
                    return(dta)
                    })
  ############################################################################################################################
  ############################################################################################################################
    
  dta_pk <- reactive({demo <- dta() %>% filter(!is.na(DV)) %>% group_by(UNIQUE) %>% distinct() 
                      pk <- dta() %>% filter(!is.na(DV)) %>% group_by(UNIQUE) %>% 
                                      summarise(CMAX=max(DV,na.rm=T),TMAX=tmax.calc(TIME,DV),AUC=auc.calc(TIME,DV)) 
                      dta <- left_join(pk,demo) %>% select(-TIME,-DV) %>% data.frame()
                      return(dta)
                      })
  ############################################################################################################################
  ############################################################################################################################
  output$convars <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('convar',"Continuous Variable:",multiple=T,
                                          sort(names(dta_exdata()[sapply(dta_exdata(),is.numeric)]))),
                              "Select one or more variable(s); use backspace to undo selection(s)","right",options=list(container="body"))
                            })
  output$catvars <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectizeInput('catvar',"Categorical Variable:",multiple=T,
                                          sort(names(dta_exdata()[sapply(dta_exdata(),is.factor)]))),
                              "Select one or more variable(s); use backspace to undo selection(s)","right",options=list(container="body"))
                            })
  output$colorvars <- renderUI({if(is.null(input$file)) return(NULL)
                              tipify(selectInput('colorvar',"Color by:",selected="NONE",
                                          sort(names(dta_explot()[sapply(dta_explot(),is.factor)]))),
                                     "Select one variable",placement="right",options=list(container="body"))
                            })

  output$explot <- renderPlot({if(is.null(input$file)) return(NULL)
                               if(is.null(input$catvar) & is.null(input$convar)) return(NULL)
                                                              
                               if(length(input$catvar)==1 & is.null(input$convar)) 
                                 p <- ggplot(data=dta_explot(),aes_string(x=input$catvar)) + geom_bar() + ylab("COUNT") 
                                
                               if(is.null(input$catvar) & length(input$convar)==1) 
                                 p <- ggplot(data=dta_explot(),aes_string(x=input$convar)) + geom_histogram()  + ylab("COUNT")
                               
                               if(length(input$catvar)+length(input$convar)!=1) 
                                 p <- dta_explot() %>% ggpairs(columns=which(colnames(dta_explot()) %in% c(input$catvar,input$convar)),
                                                               color=input$colorvar,
                                                               diag=list(continuous=tolower(input$diagcon),
                                                                         discrete=tolower(input$diagcat)),
                                                               upper=list(continuous=tolower(input$uppercon),
                                                                          discrete=tolower(input$uppercat),
                                                                          combo=tolower(input$uppercombo)),
                                                               lower=list(continuous=tolower(input$lowercon),
                                                                          discrete=tolower(input$lowercat),
                                                                          combo=tolower(input$lowercombo)))
                               return(p)
                               })
  output$extable <- renderDataTable({if(is.null(input$file)) return(NULL)
                                     if(is.null(input$catvar) & is.null(input$convar)) return(NULL)
                                     #if(is.null(input$catvar) & length(input$convar)==1) dta_extable()                                       
                                     #if(length(input$catvar)+length(input$convar)!=1) dta_extable()
                                     dta_extable()
                                     })  
  
  dta_exdata <- reactive({if(input$data_type_ex=="RAW DATA") dta <- dta()
                         if(input$data_type_ex=="NCA PK") dta <- dta_pk()
                         return(dta)
                        })
  dta_exid <- reactive({dta <- dta_exdata()  
                        if(input$exid & !is.null(input$catvar)) dta %<>% group_by(UNIQUE) %>% summarise_each(funs(first))
                        if(input$exid & is.null(input$catvar)) dta %<>% group_by(UNIQUE) %>% summarise_each(funs(mean(.,na.rm=T)))
                        return(dta)
                        })  
  dta_exslid <- reactive({var <- paste(as.character(input$exslid),collapse=",") })
  dta_excond <- reactive({if(input$stat_type!='PERCENTILES') var <- NULL else var <- 1; return(var)})
  dta_explot <- reactive({dta <- dta_exid() %>% select(one_of(c(input$convar,input$catvar))) 
                          if(input$explore_type=="GRAPH") dta %<>% mutate(NONE=factor(0))
                          return(dta)})
  dta_extable <- reactive({dta <- dta_exid()
                           #if(input$exid) dtatbl <- dta_exdata() %>% group_by(ID) %>% summarise_each(funs(mean))  
                           #Categorical only
                           if(!is.null(input$catvar) & is.null(input$convar)) {
                              #if(length(input$catvar)==1) dta.ex <- dtatbl %>% select(one_of(input$catvar)) %>% 
                              #                                                sapply(levels(dta()[,sprintf(input$catvar)]),
                              #                                                       function(x) length(dta()[,sprintf(input$catvar)]==x))
                              if(length(input$catvar)>=1) 
                                dta2 <- dta %>% select(one_of(input$catvar)) %>% summarise_each(funs(N=length(.))) %>%
                                                mutate(STATISTIC="N") %>% select(one_of(c("STATISTIC",input$catvar)))
                           }  
                           #Continuous with or no levels
                           if(is.null(input$catvar) & !is.null(input$convar)) {
                              dta2 <- dta %>% select(one_of(input$convar)) %>% 
                                              summarise_each(funs(N=length(.),MEAN=round(mean(.,na.rm=T),2),
                                                                  MEAN.95CI=ci95.calc(.), MEDIAN=round(median(.,na.rm=T),2),
                                                                  PERCENT.CV=round(pctcv.calc(.),2),RANGE=range.calc(.),
                                                                  PERCENTILES=pct.calc(.,p=c(as.numeric(unlist(str_split(dta_exslid(),",")))))))
                              if(length(input$convar)==1) dta2 %<>% gather_("STATISTIC",input$convar,names(dta2))
                              if(length(input$convar)>1) dta2 %<>% t %>% as.data.frame %>% add_rownames %>% 
                                                                separate(rowname,into=c("VARIABLE","STATISTIC"),sep="_",extra="drop") %>% 
                                                                spread(VARIABLE,V1) %>% 
                                                                mutate(STATISTIC=ordered(STATISTIC,levels=c("N","MEAN","MEAN.95CI","MEDIAN",
                                                                                         "PERCENT.CV","RANGE","PERCENTILES"))) %>%
                                                                arrange(STATISTIC)
                           }
                          #Categorical + Continuous
                           if(!is.null(input$catvar) & !is.null(input$convar)) {
                             dta2 <- dta %>% group_by_(.dots=input$catvar) %>% select(one_of(input$convar))                                                 
                             if(input$stat_type=="N") dta2 %<>% summarise_each(funs(length))
                             if(input$stat_type=="MEAN") dta2 %<>% summarise_each(funs(round(mean(.,na.rm=T),2)))
                             if(input$stat_type=="MEAN 95% CI") dta2 %<>% summarise_each(funs(ci95.calc(.)))
                             if(input$stat_type=="MEDIAN") dta2 %<>% summarise_each(funs(median(.,na.rm=T)))
                             if(input$stat_type=="PERCENT CV") dta2 %<>% summarise_each(funs(pctcv.calc(.)))
                             if(input$stat_type=="RANGE") dta2 %<>% summarise_each(funs(range.calc(.)))
                             if(input$stat_type=="PERCENTILES") 
                               dta2 %<>% summarise_each(funs(pct.calc(.,p=c(as.numeric(unlist(str_split(dta_exslid(),",")))))))
                             #if(input$stat_type=="MIN") dta.ex %<>% summarise_each(funs(min))
                             #if(input$stat_type=="MAX") dta.ex %<>% summarise_each(funs(max))
                             dta2 %<>% mutate(STATISTIC=as.character(input$stat_type)) %>% 
                                       select(one_of(c("STATISTIC",input$catvar,input$convar)))
                           }   
                           #dta.ex %<>% summarise_each(funs(round,digits=2))
                           return(dta2)
                          })
  output$exportstats <-  downloadHandler(
                      filename = function() {'output-stats.csv'},
                      content = function(file) {write.csv(dta_extable(), file, row.names=F)}
                    )
  
  ############################################################################################################################
  ############################################################################################################################
  dta_print <- reactive ({if(is.null(input$file)) return(NULL)
                          if(input$data_type_tbl=="RAW DATA") dta <- dta()
                          if(input$data_type_tbl=="NCA PK") dta <- dta_pk()
                          return(dta)
                          })
  
  output$contents <-  renderDataTable({if(is.null(input$file)) return(NULL);                                                                                                                                           
                                       dta_print()},options=list(lengthMenu=c(10,25,50,100),pageLength=10)
                                       )
  
  output$exporttable <-  downloadHandler(
                      filename = function() {'output-table.csv'},
                      content = function(file) {write.csv(dta_print(), file, row.names=F)}
                    )
  ############################################################################################################################
  ############################################################################################################################
  output$xvars <- renderUI({if(is.null(input$file)) return(NULL)
                            tipify(selectInput('xvar',"X Variable:",selected="TIME",
                                        sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                        names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                                   "Select X variable",placement="right",options=list(container="body"))
                            })
  output$yvars <- renderUI({if(is.null(input$file)) return(NULL)
                            tipify(selectInput('yvar',"Y Variable:",selected="DV",
                                         sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                         names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                                   "Select Y variable",placement="right",options=list(container="body"))
                            })
              
  output$stratas <- renderUI({if(is.null(input$file)) return(NULL)
                      tipify(selectInput("strata","Stratify by:",selected="NONE",
                                  sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                                         names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                             "Select stratification variable",placement="right",options=list(container="body"))
                    })  
  output$rowstratas <-  renderUI({if(is.null(input$file)) return(NULL) 
                          tipify(selectInput("rowstrata","Stratify by Row:",selected="NONE",
                                      sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                                 "Select row stratification variable",placement="right",options=list(container="body"))
                        })  
  output$colstratas <-  renderUI({if(is.null(input$file)) return(NULL) 
                          tipify(selectInput("colstrata","Stratify by Column:",selected="NONE",
                                      sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                                 "Select column stratification variable",placement="right",options=list(container="body"))
                        })
  output$colors <-  renderUI({if(is.null(input$file)) return(NULL) 
                      tipify(selectInput("color","Group by Color:",selected="NONE",
                        sort(c(names(dta_strat()[sapply(dta_strat(),is.numeric)]),
                               names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                        "Select variable to group data by color",placement="right",options=list(container="body"))
                    })  
  output$shapes <-  renderUI({if(is.null(input$file)) return(NULL) 
                      tipify(selectInput("shape","Group by Shape:",selected="NONE",
                        sort(c(names(dta_strat()[sapply(dta_strat(),is.factor)])))),
                        "Select variable to group datapoint by shape",placement="right",options=list(container="body"))
                    })  
  output$bins <-  renderUI({if(is.null(input$file)) return(NULL) 
                    tipify(sliderInput('bin',"Binwidth:",min=1,max=round(max(dta_plot()$X)/6),value=round(max(dta_plot()$X)/30)),
                           "Define bin size for histogram",placement="right",options=list(container="body"))
                  })
  #output$sizes <- renderUI({if(is.null(input$file)) return(NULL) 
  #                  selectInput("size","Group by Size:",choices=c("NONE",names(dta())),selected="NONE")
  #                })  
  
    
  dta_strat <- reactive({#dta <- left_join(dta(),dta_pk()) %>% mutate(NONE=factor(0))
                         #strat.dta <- data.frame(lapply(strat.dta,factorize)) %>% filter(!is.na(DV))
                        if(input$data_type_plot=="RAW DATA") dta <- dta() %>% mutate(NONE=factor(0)) 
                        else 
                        if(input$data_type_plot=="NCA PK") dta <- dta_pk() %>% mutate(NONE=factor(0))# %>% select(-TIME,-DV)                                                               
                        return(dta)
                        })
 
  dta_plot <- reactive({
    dta <- dta_strat()
    if(input$xconvert=="Categorical") dta[,sprintf(input$xvar)] <- factor(dta[,sprintf(input$xvar)])
    if(input$xconvert=="Continuous") dta[,sprintf(input$xvar)] <- as.numeric(as.character(dta[,sprintf(input$xvar)]))
    if(input$yconvert=="Categorical") dta[,sprintf(input$yvar)] <- factor(dta[,sprintf(input$yvar)])
    if(input$yconvert=="Continuous") dta[,sprintf(input$yvar)] <- as.numeric(as.character(dta[,sprintf(input$yvar)]))
    
    if(input$strata_type=="PANEL") dta$STRATVAR <- dta[,sprintf(input$strata)]  else dta$STRATVAR <- 0
    if(!is.null(input$color))  dta$COLORGRP <- dta[,sprintf(input$color)] else dta$COLORGRP <- 0
    if(!is.null(input$shape))  dta$SHAPEGRP <- dta[,sprintf(input$shape)] else dta$SHAPEGRP <- 0               
    #if(input$group_type=="SIZE")  plot.dta$SIZEGRP <- plot.dta[,sprintf(input$size)] else plot.dta$SIZEGRP <- rep(0,nrow(plot.dta))               
      
    #if(length(levels(as.factor(as.character(plot.dta$COLORGRP))))<=8) plot.dta$COLORGRP<- as.factor(plot.dta$COLORGRP)
    #if(length(levels(as.factor(as.character(plot.dta$SHAPEGRP))))<=6) plot.dta$SHAPEGRP<- as.factor(plot.dta$SHAPEGRP)
    #if(length(levels(as.factor(as.character(plot.dta$SIZEGRP))))<=6) plot.dta$SIZEGRP<- as.factor(plot.dta$SIZEGRP)
    #if(input$data_type_plot=="NCA PK") dta %<>% distinct(ID)
    return(dta)  
  })
  
  #dta_outlier <- reactive({
  #   dta <- dta_plot()
  #  dta %<>% group_by_(input$xvar) %>% mutate_(Q1=quantile(input$yvar,1/4),Q3=quantile(input$yvar,3/4) %>% 
  #                                             IQR=Q3-Q1,upper.limit=Q3+1.5*IQR,lower.limit=Q1-1.5*IQR) %>%
  #                                     filter_(input$yvar>upper.limit|input$yvar<lower.limit) 
  #  #boxplot.out <- data.frame(lapply(boxplot.out,factorize)) %>% filter(!is.na(DV)) %>% mutate(TIME=is.numeric(is.character(TIME)))
  #  return(dta)
  #})
  
  observe({
    if(input$data_type_plot=="RAW DATA") updateSelectInput(session,"xvar",selected="TIME")
    if(input$data_type_plot=="RAW DATA") updateSelectInput(session,"yvar",selected="DV")
    if(input$data_type_plot=="NCA PK") updateSelectInput(session,"xvar",selected="CMAX")
    if(input$data_type_plot=="NCA PK") updateSelectInput(session,"yvar",selected="AUC")
        
    if(input$plot_type=="SCATTERPLOT") updateRadioButtons(session,"xconvert",selected="Continuous")
    if(input$plot_type=="HISTOGRAM") updateRadioButtons(session,"xconvert",selected="Continuous")
    if(input$plot_type=="BARPLOT") updateRadioButtons(session,"xconvert",selected="Categorical")
    if(input$plot_type=="BOXPLOT") updateRadioButtons(session,"xconvert",selected="Categorical")
    
    if(input$plot_type=="SCATTERPLOT") updateSelectInput(session,"line_type",choices=c("NONE","MEAN","LINEAR REGRESSION","LOESS SMOOTHER","REFERENCE"))
    if(input$plot_type=="HISTOGRAM") updateSelectInput(session,"line_type",choices=c("NONE","DENSITY","REFERENCE"))
    if(input$plot_type=="BARPLOT") updateSelectInput(session,"line_type",choices=c("NONE","REFERENCE"))
    if(input$plot_type=="BOXPLOT") updateSelectInput(session,"line_type",choices=c("NONE","MEAN","REFERENCE"))
  })
  
  dta_reg <- reactive ({dta <- dta_plot() %>% mutate_(X=input$xvar,Y=input$yvar)})  
  output$r2 <- renderText({
    linear.mod <- lm(data=dta_reg(),Y~X)
    r2 <- ifelse(summary(linear.mod)$r.squared<0.01,"r-squared < 0.01",paste("r-squared =",round(summary(linear.mod)$r.squared,digits=2)))
    return(r2)
  })
  
  ncol <- reactive({ dta <- ncol(plot_dta())
  })
  
  plot <- reactive({if(is.null(input$file)) return(NULL)
    ## Scatterplot options    
    if(input$plot_type=="SCATTERPLOT" & !is.null(input$xvar) & !is.null(input$yvar)) 
      p <- ggplot(data=dta_plot(),aes_string(x=input$xvar,y=input$yvar,color=input$color,fill=input$color,shape=input$shape)) +
                  xlab(input$xvar) + ylab(input$yvar) + coord_cartesian(xlim=ranges$x,ylim=ranges$y) 
    if(input$plot_type=="SCATTERPLOT" & is.numeric(dta_plot()$COLORGRP)) 
      p <- p + geom_point(aes(color=as.numeric(COLORGRP)),position=input$jitter,size=rel(3)) + 
               scale_color_continuous(name=input$color) + scale_shape(name=input$shape)
    
    if(input$plot_type=="SCATTERPLOT" & length(levels(dta_plot()$COLORGRP))>=2 & length(levels(dta_plot()$SHAPEGRP))==1) 
      p <- p + geom_point(position=input$jitter,size=rel(3),shape=16) + 
               scale_color_brewer(name=input$color,palette="Set1") + scale_shape(guide=FALSE)
    
    if(input$plot_type=="SCATTERPLOT" & length(levels(dta_plot()$COLORGRP))==1 & length(levels(dta_plot()$SHAPEGRP))==1) 
      p <- p + geom_point(position=input$jitter,size=rel(3),color="black",shape=16) + theme(legend.position="none")
    
    if(input$plot_type=="SCATTERPLOT" & length(levels(dta_plot()$COLORGRP))>=2 & length(levels(dta_plot()$SHAPEGRP))>=2) 
      p <- p + geom_point(position=input$jitter,size=rel(3)) +
               scale_color_brewer(name=input$color,palette="Set1") + scale_shape(name=input$shape)
    
    if(input$plot_type=="SCATTERPLOT" & length(levels(dta_plot()$COLORGRP))==1 & length(levels(dta_plot()$SHAPEGRP))>=2) 
      p <- p + geom_point(position=input$jitter,size=rel(3),color="black")  +
               scale_color_discrete(guide=FALSE) + scale_shape(name=input$shape)
    
    #if(input$plot_type=="SCATTERPLOT" & input$group_type=="NONE") 
    #  p <- p + geom_point(position=input$jitter,size=rel(3),color="black") + theme(legend.position="none")
        
    if(input$plot_type=="SCATTERPLOT" & input$line_type=="MEAN" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_summary(geom="line",alpha=0.6,size=rel(2),fun.y=mean) +
               stat_summary(geom="ribbon",aes(color=NULL),alpha=0.1,
                            fun.ymin=function(x) quantile(x, 0.05),fun.ymax=function(x) quantile(x, 0.95)) + 
               scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    }
    if(input$plot_type=="SCATTERPLOT" & input$line_type=="MEAN" & length(levels(dta_plot()$COLORGRP))<2) { 
      p <- p + stat_summary(geom="line",color="black",alpha=0.6,size=rel(2),fun.y=mean)
               stat_summary(geom="ribbon",aes(color=NULL),fill="black",alpha=0.1,
                            fun.ymin=function(x) quantile(x, 0.05),fun.ymax=function(x) quantile(x, 0.95))                                  
    }
    ##  Histogram options
    if(input$plot_type=="HISTOGRAM")
      p <- ggplot(data=dta_plot(),aes_string(x=input$xvar,color=input$color,fill=input$color)) + 
                  geom_histogram(stat="bin",binwidth=input$bin,position="dodge") + 
                  xlab(input$xvar) + ylab("COUNT") + coord_cartesian(xlim=ranges$x) + 
                  scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    if(input$plot_type=="HISTOGRAM" & length(levels(dta_plot()$COLORGRP))<=1) 
      p <- p + geom_histogram(binwidth=input$bin,color="black",fill="black") + theme(legend.position="none")
    if(input$plot_type=="HISTOGRAM" & input$line_type=="DENSITY") 
      p <- ggplot(data=dta_plot(),aes_string(x=input$xvar,fill=input$color)) + geom_density(alpha=0.3,colour=NA) + 
                  geom_line(stat="density") + xlab(input$xvar) + ylab("DENSITY") + coord_cartesian(xlim=ranges$x) +
                  scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    if(input$plot_type=="HISTOGRAM" & input$line_type=="DENSITY" & length(levels(dta_plot()$COLORGRP))<=1) 
      p <- p + geom_density(fill="black",alpha=0.3,colour=NA) + geom_line(stat="density")    
      
    ## Barplot options
    if(input$plot_type=="BARPLOT")
       p <- ggplot(data=dta_plot(),aes_string(x=input$xvar,y=input$yvar,color=input$color,fill=input$color)) + 
                   geom_bar(stat="identity") + xlab(input$xvar) + ylab(input$yvar) + coord_cartesian(xlim=ranges$x,ylim=ranges$y) +
                   scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    if(input$plot_type=="BARPLOT" & length(levels(dta_plot()$COLORGRP))<=1) 
       p <- p + geom_bar(stat="identity",color="black",fill="black")
    
    ## Boxplot options
    if(input$plot_type=="BOXPLOT") 
      p <- ggplot(data=dta_plot(),aes_string(x=input$xvar,y=input$yvar,color=input$color)) + 
                  xlab(input$xvar) + ylab(input$yvar) + coord_cartesian(xlim=ranges$x,ylim=ranges$y) 
    
    if(input$plot_type=="BOXPLOT" & is.null(input$color) & input$jitterb=="identity") 
      p <- p + geom_boxplot(color="black",outlier.shape=NA)
    if(input$plot_type=="BOXPLOT" & is.null(input$color) & input$jitterb=="jitter") 
      p <- p + geom_boxplot(color="black",outlier.shape=NA) + geom_point(color="black",position=input$jitterb,alpha=0.5)
    
    if(input$plot_type=="BOXPLOT" & !is.null(input$color)) {
      if(length(levels(dta_plot()$COLORGRP))>=2 & input$jitterb=="jitter") 
        p <- p + geom_boxplot(outlier.shape=NA) + geom_point(position=input$jitterb,alpha=0.5) + scale_color_brewer(palette="Set1")
      if(length(levels(dta_plot()$COLORGRP))<2 & input$jitterb=="jitter") 
        p <- p + geom_boxplot(color="black",outlier.shape=NA) + geom_point(color="black",position=input$jitterb,alpha=0.5)
      if(length(levels(dta_plot()$COLORGRP))<2 & input$jitterb=="identity") 
        p <- p + geom_boxplot(color="black",outlier.shape=NA) 
      if(length(levels(dta_plot()$COLORGRP))>=2 & input$jitterb=="identity") 
        p <- p + geom_boxplot(outlier.shape=NA) + scale_color_brewer(palette="Set1")
    }    
    if(input$plot_type=="BOXPLOT" & input$line_type=="MEAN") { 
      p <- p + stat_summary(aes(group=1),geom="line",color="black",alpha=0.4,size=rel(2),fun.y=mean) +
               stat_summary(aes(group=1),geom="point",shape="*",color="black",alpha=0.8,size=rel(5),fun.y=mean)    
    }
          
#      if(input$xvar!=input$color) p <- p + geom_boxplot(outlier.shape=NA) + geom_point(position=input$jitter,alpha=0.5)
#      if(input$xvar==input$color) p <- p + geom_boxplot(outlier.shape=NA) + 
#                                           geom_point(data=dta_outlier(),position=input$jitter,aes(x=X,y=Y,color=COLORGRP),alpha=0.5)     
##      if(length(levels(dta_plot()$COLORGRP))<2) 
#         p <- p + geom_boxplot(color="black",outlier.shape=NA) + geom_point(color="black",position=input$jitter,alpha=0.5)   
#      if(length(levels(dta_plot()$COLORGRP))>=2) 
#         p <- p + geom_boxplot() + geom_point(position=input$jitter,alpha=0.5)   

  
    if(input$line_type=="REGRESSION" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_smooth(method="lm",alpha=0.2,size=rel(2)) + 
               scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    }
    if(input$line_type=="REGRESSION" & length(levels(dta_plot()$COLORGRP))<2) {
      p <- p + stat_smooth(color="black",fill="black",method="lm",alpha=0.2,size=rel(2))
    }
    if(input$line_type=="TRENDLINE" & length(levels(dta_plot()$COLORGRP))>=2) {
      p <- p + stat_smooth(method="loess",alpha=0.2,size=rel(2)) + 
               scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
    }
    if(input$line_type=="TRENDLINE" & length(levels(dta_plot()$COLORGRP))<2) {
      p <- p + stat_smooth(color="black",fill="black",method="loess",alpha=0.2,size=rel(2))
    }
    if(input$line_type=="REFERENCE" & !is.null(input$xref)) {
      p <- p + geom_vline(x=as.numeric(unlist(str_split(input$xref,","))),size=rel(2),linetype="dashed",color="darkblue",alpha=0.6)
      if(length(unlist(str_split(input$xref,",")))>1) {
        p <- p + annotate("rect",xmin=min(as.numeric(unlist(str_split(input$xref,","))),na.rm=T),
                          xmax=max(as.numeric(unlist(str_split(input$xref,","))),na.rm=T),
                          ymin=min(as.numeric(dta_plot()[,input$yvar]),na.rm=T),
                          ymax=max(as.numeric(dta_plot()[,input$yvar]),na.rm=T),
                          fill="blue",alpha=0.1)
      }
    }
    if(input$line_type=="REFERENCE" & !is.null(input$yref)) {
      p <- p + geom_hline(y=as.numeric(unlist(str_split(input$yref,","))),size=rel(2),linetype="dashed",color="darkred",alpha=0.6)
      if(length(unlist(str_split(input$yref,",")))>1) {
        p <- p + annotate("rect",ymin=min(as.numeric(unlist(str_split(input$yref,","))),na.rm=T),
                          ymax=max(as.numeric(unlist(str_split(input$yref,","))),na.rm=T),
                          xmin=min(as.numeric((dta_plot()[,input$xvar])),na.rm=T),
                          xmax=max(as.numeric((dta_plot()[,input$xvar])),na.rm=T),
                          fill="red",alpha=0.1)
      }
    }
    
    if("X" %in% input$log) p <- p + scale_x_log10(); if("Y" %in% input$log) p <- p + scale_y_log10()
    if("X" %in% input$rev) p <- p + scale_x_reverse(); if("Y" %in% input$rev) p <- p + scale_y_reverse()
    if(input$flip) p <- p + coord_flip()

    facets <- paste(input$rowstrata,'~',input$colstrata)  
    if(input$strata_type=="PANEL") {p <- p + facet_wrap(~STRATVAR,scales=tolower(input$scale_type))}
    if(input$strata_type=="GRID") {p <- p + facet_grid(facets,scales=tolower(input$scale_type))}

    #if(!is.null(input$color)) p <- p + aes_string(color=input$color)
    #if(!is.null(input$shape)) p <- p + aes_string(shape=input$shape)
    #if(!is.null(input$size)) p <- p + aes_string(size=input$size)
    #if(!is.null(input$color)) p <- p + geom_point(aes(color=COLORGRP))
    #if(!is.null(input$shape)) p <- p + geom_point(aes(shape=factor(SHAPEGRP)))
    #if(!is.null(input$size)) p <- p + geom_point(aes(size=SIZEGRP))
    #if (!is.null(input$plot_click)) p <- p + geom_point(data=clickid(),aes(x=TIME,y=DV),size=rel(5),col="red")
    return(p)
  })
  output$plot <- renderPlot(plot())
    
  
  clickid <-  reactive({
                click.id <- nearPoints(dta(),input$plot_click,maxpoints=1)
                dta() %>% filter(ID==click.id$ID)
              })
   
  output$brush_info <- renderPrint({if(is.null(input$file)) return(NULL)
    cat("Selected datapoints:\n")
    dta <- brushedPoints(dta_plot(), input$plot_brush)
    dta %<>% filter_(!is.na(input$xvar)) %>% select(-NONE,-STRATVAR,-COLORGRP,-SHAPEGRP) 
    return(dta)
  })
    
  output$brush_data <- renderDataTable({if(is.null(input$file)) return(NULL)
    dta <- brushedPoints(dta_plot(), input$plot_brush)
    dta %<>% filter_(!is.na(input$xvar)) %>% select(1:12) 
    return(dta)
  })

  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  output$exportplot <- downloadHandler(
      filename='output-figure.png',content=function(file) {
        device <- function(...,width,height) grDevices::png(...,width=width,height=height,res=300,units="in")
        ggsave(file,plot=plot(),device=device)
      })
  ########################################################################################################################
  ############################################################################################################################
  output$timeptslider <-  renderUI({if(is.null(input$file)) return(NULL)
                            tipify(sliderInput("timeptrange","Number of timepoints", 
                                               min=3, max=length(unique(dta()$TIME)), step=1, round=T,
                                               value=c(3,length(unique(dta()$TIME))-1)),
                                   "Use slider to specify how many timepoints are desired",
                                   placement="right",options=list(container="body"))                                   
                          })
  
  output$cmaxslider <-  renderUI({if(is.null(input$file)) return(NULL)
                          tipify(sliderInput("cmaxrange","Mean Cmax Percent Error Range", round=T, value=min(pk()$MPPE_Cmax),
                                             min=floor(min(pk()$MPPE_Cmax)), max=ceiling(max(pk()$MPPE_Cmax))),
                                 "Use slider to specify acceptable bias in mean Cmax values",
                                 placement="right",options=list(container="body"))
                        })
  output$aucslider <- renderUI({if(is.null(input$file)) return(NULL)
                        tipify(sliderInput("aucrange","Mean AUC Percent Error Range", round=T,
                                           min=floor(min(pk()$MPPE_AUC)), max=ceiling(max(pk()$MPPE_AUC)), 
                                           value=c(min(pk()$MPPE_AUC),max(pk()$MPPE_AUC))),
                               "Use sliders to specify acceptable bias in mean AUC values",
                               placement="right",options=list(container="body"))
                      })

  output$indcmaxslider <- renderUI({if(is.null(input$file)) return(NULL)
                            tipify(sliderInput("indcmaxrange","Cmax Percent Error Range", round=T, value=min(pk()$MinPPE_Cmax),
                                               min=floor(min(pk()$MinPPE_Cmax)), max=ceiling(max(pk()$MaxPPE_Cmax))),
                                   "Use slider to specify acceptable bias in individual Cmax values",
                                   placement="right",options=list(container="body"))
                            })
  output$indaucslider <-  renderUI({if(is.null(input$file)) return(NULL)
                            tipify(sliderInput("indaucrange","AUC Percent Error Range", round=T,
                                               min=floor(min(pk()$MinPPE_AUC)), max=ceiling(max(pk()$MaxPPE_AUC)), 
                                               value=c(min(pk()$MinPPE_AUC),max(pk()$MaxPPE_AUC))),
                                   "Use sliders to specify acceptable bias in individual AUC values",
                                   placement="right",options=list(container="body"))
                          })
  output$TIMEPTS <- renderUI({if(is.null(input$file)) return(NULL)
                      tipify(selectizeInput("timepts","Timepoint(s):",sort(unique(dta()$TIME)),multiple=T),
                             "Use dropdown to select timepoint(s) if there is/are specific one(s) to include in sampling schedule",
                             placement="right",options=list(container="body"))
                    })
  
  
  pos <-  reactive({time.list<-unique(dta()$TIME); timept.min <- 3; timept.max <- length(time.list)-1
                    time.pos <-lapply(timept.min:timept.max, function(x) combinations(length(time.list),x))
                    return(time.pos)
                    })    
  combo <-  reactive({time.list<-unique(dta()$TIME)
                      timept.min <- 3; timept.max <- length(time.list)-1
                      all.combos <- lapply(timept.min:timept.max, function(x) combinations(length(time.list),x,time.list)) %>%
                                    llply(function(x) {scenario <- c(data.frame(x),sep=",");do.call(paste,scenario)}) %>% unlist() %>% 
                                    data.frame(combo=paste("c",1:length(.),sep=""),total=sapply(str_split(.,","),length),timept=.) 
                      return(all.combos)
                      }) 
  ### NCA for every combination
  temp1 <-  reactive({if(is.null(input$file)) return(NULL)
                      dta <- dta() %>% mutate(ID=str_pad(ID,width=str_length(max(ID)),pad="0"))
                      return(dta)
                      })
  temp2 <-  reactive({if(is.null(input$file)) return(NULL)
                      dta <- lapply(id.list, function(x) {dta()[which(dta()$UNIQUE==(x)),]})
                      return(dta)
                      })



  ### NCA for every combination
  pk <-  reactive({if(is.null(input$file)) return(NULL)
                   dta <- dta() %>% mutate(ID=str_pad(ID,width=str_length(max(ID)),pad="0"))
                   id.list<-unique(dta$UNIQUE); time.list<-unique(dta$TIME)
                   timept.min <- 3; timept.max <- length(time.list)-1         
          
                  #PK.full <- dta() %>% group_by(ID) %>% summarise(CMAX=max(DV),TMAX=tmax.calc(TIME,DV),AUC=auc.calc(TIME,DV))
                   full <- lapply(id.list, function(x) {dta[which(dta$UNIQUE==(x)),]})
                  
                  ###AUC for each combination
                   AUC_error <- withProgress(message = 'Calculating AUC', value=0, {
                                  unlist(lapply(1:(timept.max-timept.min+1), function(timept,...){
                                  unlist(lapply(1:nrow(pos()[[timept]]), function(scenario,...){
                                   incProgress(1/nrow(combo()), detail=paste(timept+timept.min-1,"timepoints: #",scenario))
                                   c(stat.func(as.vector(unlist(lapply(1:length(id.list), function(id){
                                      (auc.calc(full[[id]]$TIME[c(pos()[[timept]][scenario,])],
                                                full[[id]]$DV[c(pos()[[timept]][scenario,])])-dta_pk()$AUC[id])/dta_pk()$AUC[id]
                                      })))),
                                    mean(as.vector(unlist(lapply(1:length(id.list), function(id){
                                      abs(auc.calc(full[[id]]$TIME[c(pos()[[timept]][scenario,])],
                                                   full[[id]]$DV[c(pos()[[timept]][scenario,])])-dta_pk()$AUC[id])
                                      })))),
                                    sqrt(mean(as.vector(unlist(lapply(1:length(id.list), function(id){
                                      ((auc.calc(full[[id]]$TIME[c(pos()[[timept]][scenario,])],
                                                 full[[id]]$DV[c(pos()[[timept]][scenario,])])-dta_pk()$AUC[id])/dta_pk()$AUC[id])**2
                                      })))))
                                    )  
                                    }))
                                  }))  
                                }) 
                  ###Cmax for each combination
                  Cmax_error <- withProgress(message = 'Calculating Cmax', value=0, {
                                  unlist(lapply(1:(timept.max-timept.min+1), function(timept,...){
                                    unlist(lapply(1:nrow(pos()[[timept]]), function(scenario,...){
                                      incProgress(1/nrow(combo()), detail=paste(timept+timept.min-1,"timepoints: #",scenario))
                                      c(stat.func(as.vector(unlist(lapply(1:length(id.list), function(id){
                                          (max(full[[id]]$DV[c(pos()[[timept]][scenario,])])-dta_pk()$CMAX[id])/dta_pk()$CMAX[id]
                                        })))),
                                        mean(as.vector(unlist(lapply(1:length(id.list), function(id){
                                          abs(max(full[[id]]$DV[c(pos()[[timept]][scenario,])])-dta_pk()$CMAX[id])
                                        })))),
                                        sqrt(mean(as.vector(unlist(lapply(1:length(id.list), function(id){
                                          ((max(full[[id]]$DV[c(pos()[[timept]][scenario,])])-
                                             dta_pk()$CMAX[id])/dta_pk()$CMAX[id])**2
                                        })))))
                                      )  
                                    }))
                                  }))  
                                })

                  AUCerror<-data.frame(matrix(round(as.vector(AUC_error)*100,2), ncol=5, byrow=T)) 
                  Cmaxerror<-data.frame(matrix(round(as.vector(Cmax_error)*100,2), ncol=5, byrow=T)) 
                  all.results <- bind_cols(combo(),Cmaxerror,AUCerror)
                  names(all.results) <- c("Combination","Number","Timepoints",
                                           "MinPPE_Cmax","MPPE_Cmax","MaxPPE_Cmax","MAPE_Cmax","RMSE_Cmax",
                                           "MinPPE_AUC","MPPE_AUC","MaxPPE_AUC","MAPE_AUC","RMSE_AUC")
                  return(all.results)          
                  
                })
  
  pk_filter <-  reactive({if(is.null(input$file)) return(NULL)
                          if(!is.null(input$timepts)) {
                            combomtrx <- sapply(1:nrow(combo()),function(x) as.vector(str_split(combo()$timept[x],",")))
                            selection <- data.frame(apply(sapply(1:length(input$timepts), function(x) sapply(1:length(combomtrx), function(y) input$timepts[x] %in% combomtrx[[y]])),1,all))
                            names(selection) <- "selection"
                            results <- bind_cols(pk(),selection) %>% filter(selection==TRUE)
                          } else {results <- pk()}

                          results %<>%  filter(Number>=unlist(input$timeptrange)[1] & 
                                               Number<=unlist(input$timeptrange)[2]) %>%
                                        filter(MPPE_Cmax>=unlist(input$cmaxrange)[1]) %>%
                                        filter(MPPE_AUC>=unlist(input$aucrange)[1] & 
                                               MPPE_AUC<=unlist(input$aucrange)[2]) 

                          if(input$pk_outlier) results %<>% filter(MinPPE_Cmax>=unlist(input$indcmaxrange)[1]) %>%
                                                            filter(MinPPE_AUC>=unlist(input$indaucrange)[1] & 
                                                                   MaxPPE_AUC<=unlist(input$indaucrange)[2])                         
                          
                          
                          return(results)
                          
                })
  
  
  ## Display filtered results
  output$pktable <- renderDataTable({if(is.null(input$file)) return(NULL); if(input$output_type=="Graph") return(NULL)
                      results <- pk_filter() %>%  select(Combination,Number,Timepoints,MPPE_Cmax,MPPE_AUC,RMSE_Cmax,RMSE_AUC)
                      names(results) <-  c("Combination","Number of Timepoints","Selected Timepoints",
                                           "Mean % Cmax","Mean % AUC","RMSE % Cmax","RMSE % AUC")
                      return(results)
                    })
  
  ### Export functions
  output$export <-  downloadHandler(
                      filename = function() {'output-timepoints.csv'},
                      content = function(file) {write.csv(pk_filter(), file, row.names=F)}
                    )
  
  output$pkplot <- renderPlot({if(is.null(input$file)) return(NULL);if(input$output_type=="Table") return(NULL)
    p <- ggplot(data=pk_filter(),aes(x=MPPE_AUC,y=MPPE_Cmax)) + scale_x_reverse() + scale_y_reverse() + 
          xlab("Mean AUC (% Prediction Error)") + ylab("Mean Cmax (% Prediction Error)") 
    p <- p + scale_color_continuous(name="Total Error",low="blue",high="red") + scale_shape_discrete(name="Number of Timepoints")
    p <- p + coord_cartesian(xlim = pkranges$x, ylim = pkranges$y)
    if(input$pk_number==FALSE) p <- p + geom_point(aes(color=abs(MPPE_AUC)+abs(MPPE_Cmax),shape=factor(Number)), size=rel(5))
    if(input$pk_number) p <- p + geom_point(aes(shape=NULL,color=abs(MPPE_AUC)+abs(MPPE_Cmax)),size=rel(5)) + facet_wrap(~Number)
    return(p)
  })
                           
  output$pk_data <- renderDataTable({if(is.null(input$file)) return(NULL);if(input$output_type=="Table") return(NULL); 
                      if(is.null(pkranges)) return(NULL)
                      else {cat("Selected Scenarios:\n")
                            brushedPoints(pk_filter() %>% select(Combination,Number,Timepoints,MPPE_Cmax,MPPE_AUC,RMSE_Cmax,RMSE_AUC) %>% 
                                          data.frame(), input$pkplot_brush)
                      }      
                    })
  
  
  pkranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$pkplot_dblclick, {
    brush <- input$pkplot_brush
    if (!is.null(brush)) {
      pkranges$x <- c(brush$xmin, brush$xmax)
      pkranges$y <- c(brush$ymin, brush$ymax)
    } else {
      pkranges$x <- NULL
      pkranges$y <- NULL
    }
  })

 output$test <- renderPrint({sapply(dta_plot(),class)})
 #output$test2 <- renderDataTable({})
})
