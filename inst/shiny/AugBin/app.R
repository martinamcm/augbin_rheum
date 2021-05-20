library('shinydashboard')
library('shiny')
library('DT')
library('ggplot2')
library('shinycssloaders')
library('data.table')
library('dplyr')
library('formattable')
library('tidyr')
library('ggpubr')
library('caTools')
library('knitr')
library('stats')
library('MASS')
library('shinyjs')
library('xml2')
library('rvest')
library('rmarkdown')

source('LatVarAnalysis_21.R',local=TRUE)
source('LatVarAnalysis_11.R',local=TRUE)
source('LatVarAnalysis_20.R',local=TRUE)
source('LatVarAnalysis_10.R',local=TRUE)

rmarkdown::render(
  input = "HomePage.Rmd",
  output_format = "html_document",
  output_file = file.path(getwd(), "HomePageRend.html")
)
xml2::write_html(rvest::html_node(xml2::read_html("HomePageRend.html"), "body"), file = "LandPage.html")

#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}


ui <- dashboardPage(
 
  dashboardHeader(title = "AugBin"),
  
 dashboardSidebar(
     sidebarMenu(id="tab",
     menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Source code", icon = icon("file-code"),href="https://github.com/martinamcm/AugBin")
    )
  ),
 
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                includeHTML("LandPage.html")
              )),
      
     tabItem(tabName = "analysis",
     fluidRow(
      box(title="Upload Files",width=3,solidHeader = TRUE,status="primary",
          
          # Input: Select a file ----
          fileInput("file1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          # Horizontal line ----
          tags$hr(),
          
          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          
          # Input: Select quotes ----
          radioButtons("quote", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          
          # Horizontal line ----
          tags$hr(),
          
          # Input: Select number of rows to display ----
          radioButtons("disp", "Display",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head")
          
      ),
      
      
      box(title="Raw Data Table", width=8,status="primary",solidHeader = TRUE,
          
          DT::dataTableOutput("mytable")
          
      ),
      
      
      box(title="Raw Data Plots", width=11, solidHeader = TRUE,status="primary",
          
          column(width=4,
                 uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
                 uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
                 selectInput("plot.type","Plot Type:", 
                             list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar", violin = "violin")
                 ),
                 checkboxInput("show.points", "show points", TRUE),
                 actionButton("GoPlot","Get Plot")
          ),
          column(width=7,
                 h3(textOutput("caption")),
                 uiOutput("plot")
          )
      ),
      
      box(title="Composite Structure",width=3,status = "primary", solidHeader = TRUE,
          
          numericInput("Ctsno", "Number of continuous components", value=1, min=1, max=2),
          numericInput("Binno", "Number of binary components", value=1, min=0, max=1),
          actionButton("GetModel", "Generate Model")
          
      ),
      
      box(title="Model Summary",width=8,status = "primary", solidHeader = TRUE,
          
          uiOutput("markdown")
          
      ),
      
      box(title="Analysis",width=11,status="primary",solidHeader = TRUE,
          
          #Enter dicotomisation thresholds for continuous measures 
          column(width=3,
                 uiOutput("dichY1"),
                 uiOutput("transformY1"),
                 
         conditionalPanel("input.Ctsno==2",
                 uiOutput("dichY2"),
                 uiOutput("transformY2")),
         
         actionButton("GoAnalysis", "Run Analysis")),
          
          column(width=8,
                 uiOutput("ResultsTable"),
                 uiOutput("ResultsTable2"),
                 uiOutput("ResultsTable3"),
                 uiOutput("plot2")
          )
      
      ),
      
      box(title="Goodness-of-fit",width=11,status="primary",solidHeader = TRUE,
          
          column(width=6,
                 uiOutput("plot4")),
          
          column(width=5,
                 uiOutput("plot3")
           )
        
        )
      ),
     
     tabItem(tabName = "code",
             verbatimTextOutput("Hi!")
             )
    )
  )
))



server <- shinyServer(function(input, output, session) {
  
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
 InputData <- eventReactive(input$file1,{
   read.csv(input$file1$datapath)
  })
  
 rawData <- reactive(
    rawData <- InputData()
 )
 
 
  newData<-reactive({
   
    Y1 <- rawData()[,3]
    newY1 <- Y1+abs(min(Y1))+0.001
   
    Box = boxcox(newY1 ~ 1, lambda = seq(-6,6,0.1))
    Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
    Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
    lambda1 = Cox2[1, "Box.x"]                 # Extract that lambda
    
    newY1 = (newY1 ^ lambda1 - 1)/lambda1   # Transform the original data
    dichot.Y1<-((input$dichY1+abs(min(Y1))+0.001)^lambda1-1)/lambda1 #Transform cutpoint
    
    if(input$Ctsno==1){
      newDat<-newY1
      newData<-list(newDat,dichot.Y1)
      return(newData)
    }
    
    if(input$Ctsno==2){
      
      Y2 <- rawData()[,4]
      newY2 <- Y2+abs(min(Y2))+0.001
      
      Box = boxcox(newY2 ~ 1, lambda = seq(-6,6,0.1))
      Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
      Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
      lambda2 = Cox2[1, "Box.x"]                 # Extract that lambda
      
      newY2 = (newY1 ^ lambda2 - 1)/lambda2   # Transform the original data
      dichot.Y2<-((input$dichY2+abs(min(Y2))+0.001)^lambda2-1)/lambda2 #Transform cutpoint
      
      newData<-list(newY1,newY2,dichot.Y1,dichot.Y2)
      return(newData)
    }
    
    return(newData)
    
  })
  
  Dichot <- reactive({
   
     if(input$Ctsno==1){
     
        if(input$transformY1==TRUE){
        Dichot<-newData()[[2]]
        return(Dichot)
      }
      else{
        Dichot<-input$dichY1
         return(Dichot)
        }
    }
    
    if(input$Ctsno==2){
      
      if(input$transformY1==TRUE){
       Dichot <- c(newData()[[3]],input$dichY2)
      }
      else if(input$transformY2==TRUE){
        Dichot <- c(input$dichY1,newData()[[4]])
      }
      else if(input$transformY1 && input$transformY2==TRUE){
       Dichot <- c(newData()[[3]],newData()[[4]])
      }
      else{
        Dichot <- c(input$dichY1,input$dichY2)
    }
    }
    return(Dichot)
  })
  
  DataInf <- reactive({
    
    DataInf<-rawData()
    
    if(input$Ctsno==2){
    
      if(input$transformY1==TRUE){
        DataInf[,3]<-newData()[[1]]
         return(DataInf)
        }
      else if(input$transformY2==TRUE){
        DataInf[,4]<-newData()[[2]]
        return(DataInf)
        }
      else if(input$transformY1 && input$transformY2==TRUE){
        DataInf[,3:4] <- cbind(newData()[[1]],newData()[[2]])
        return(DataInf)
        }
      else{
        DataInf <- rawData()}
      }
    
    else if(input$Ctsno==1){
      
      if(input$transformY1==TRUE){
        DataInf[,3] <- newData()[[1]]
        return(DataInf)
      }
      else{
        DataInf <- rawData()
      }
    }
    return(DataInf)
    })
  
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(rawData(),options = list(scrollX = TRUE))
    
  })
  
  
  ##Add reactive plots
  output$variable <- renderUI({ 
    var.opts<-namel(colnames(rawData()))
    selectInput("variable","Variable:", var.opts) # update UI 				 
  }) 
  
  output$group <- renderUI({ 
    var.opts<-namel(colnames(rawData()))
    var.opts<-var.opts[-1]
    selectInput("group","Groups:", var.opts) # uddate UI 				 
  }) 
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph",
           "violin" = "Violin plot")
  })
  
  
  output$plot <- renderUI({
    plotOutput("p")%>% withSpinner(color="#0dc5c1")
  })
  
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    input$GoPlot
    
    isolate({
      plot.obj<<-list()
      plot.obj$variable<<-req(input$variable)
      plot.obj$group<<-req(input$group)
      
      plot.type<-switch(input$plot.type,
                        "boxplot" 	= 	geom_boxplot(),
                        "histogram" =	geom_histogram(alpha=0.5,position="identity",bins=12),
                        "density" 	=	geom_density(alpha=.75),
                        "bar" 		=	geom_bar(position="dodge"),
                        "violin" = geom_violin()
      )
      
      require(ggplot2)
      #plotting theme
      
      .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
      )
      
      if(input$plot.type=="violin")	{		#control for 1D or 2D graphs 
        p<-ggplot(rawData(), 
                  aes_string(
                    x 		= sprintf("factor(%s)",plot.obj$group), 
                    y 		= plot.obj$variable,
                    fill 	= sprintf("factor(%s)",plot.obj$group) 
                  )
        ) + plot.type
        
        if(input$show.points==TRUE)
        { 
          p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
        }
        
      } 
      
      else if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
        p<-ggplot(rawData(), 
                  aes_string(
                    x 		= sprintf("factor(%s)",plot.obj$group), 
                    y 		= plot.obj$variable,
                    fill 	= sprintf("factor(%s)",plot.obj$group) 
                  )
        ) + plot.type 
        
        if(input$show.points==TRUE)
        { 
          p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
        }
        
      }
      
      else {
        
        p<-ggplot(rawData(), 
                  aes_string(
                    x 		= plot.obj$variable,
                    fill 	= sprintf("factor(%s)",plot.obj$group),
                    group 	= sprintf("factor(%s)",plot.obj$group) ##Can edit this to include NULL
                    #color 	= as.factor(plot.obj$group)
                  )
        ) + plot.type
      }
      
      p<-p+labs(
        fill 	= input$group,
        x 		= "",
        y 		= input$variable
      )  +
        .theme
      print(p)
    })	
    
  })
  
  
  output$ResultsTable <- renderUI({
    formattableOutput("Analysis")#%>% withSpinner(color="#0dc5c1")
  })
  
  output$Analysis <- renderFormattable({
    
    Method <- c("Latent Variable", "Standard Binary")
    ResponseT <- GenAnalysis()[c(10,21)]
    ResponseC <- GenAnalysis()[c(11,22)]
    dataresultstable <- data.frame(Method,ResponseT,ResponseC)
    formattable(dataresultstable,col.names=(c("Method","Treatment Response","Control Response")),digits=3)
    
  })
  
  output$Analysis2 <- renderFormattable({
    
    TreatEffect<-c("Log-Odds Ratio","Log-Risk Ratio","Risk Difference")
    LatentVariable<-GenAnalysis()[c(2,5,8)]
    LatVarCIlow<-GenAnalysis()[c(1,4,7)]
    LatVarCIhigh<-GenAnalysis()[c(3,6,9)]
    dataresultstable <- data.frame(TreatEffect,LatentVariable,LatVarCIlow,LatVarCIhigh)
    formattable(dataresultstable,col.names=(c("Latent Variable ","Point Estimate","95% Lower CI","95% Upper CI")),digits=3)
    
  })
  
  output$ResultsTable2 <- renderUI({
    formattableOutput("Analysis2")
  })
  
  output$Analysis3 <- renderFormattable({
    
    TreatEffect<-c("Log-Odds Ratio","Log-Risk Ratio","Risk Difference")
    Bin<-GenAnalysis()[c(13,16,19)]
    BinCIlow<-GenAnalysis()[c(12,15,18)]
    BinCIhigh<-GenAnalysis()[c(14,17,20)]
    dataresultstable <- data.frame(TreatEffect,Bin,BinCIlow,BinCIhigh)
    formattable(dataresultstable,col.names=(c("Standard Binary","Point Estimate","95% Lower CI","95% Upper CI")),digits=3)
    
  })
  
  output$ResultsTable3 <- renderUI({
    formattableOutput("Analysis3")
  })
  
   output$dichY1 <- renderUI(
    numericInput("dichY1", label=HTML("Y<sub>1</sub> responder threshold"), 0)
    )
    
  output$transformY1 <- renderUI(
    checkboxInput("transformY1", label=HTML("Transform Y<sub>1</sub>"), FALSE)
    )

    output$dichY2 <- renderUI({
      numericInput("dichY2", label=HTML("Y<sub>2</sub> responder threshold"), 0)
    })
    
    output$transformY2 <- renderUI({
      checkboxInput("transformY2", label=HTML("Transform Y<sub>2</sub>"), FALSE)
    })

 

  GenAnalysis <- eventReactive(input$GoAnalysis,{
    
    if(input$Ctsno==2 && input$Binno==1){
      validate(
        need(dim(rawData())[2] == 7, "Please ensure the correct composite structure is selected")
      )
      source('LatVarAnalysis_21b.R', local=TRUE)
      
      Analysis<-LatVarfunc(DataInf(),Dichot())
      
    }
    
    else if(input$Ctsno==2 && input$Binno==0){
      validate(
        need(dim(rawData())[2] == 6, "Please ensure the correct composite structure is selected")
      )
      source('LatVarAnalysis_20.R', local=TRUE)
      Analysis<-LatVarfunc(DataInf(),Dichot())
    }
    
    else if(input$Ctsno==1 && input$Binno==1){
      validate(
        need(dim(rawData())[2] == 5, "Please ensure the correct composite structure is selected")
      )
      source('LatVarAnalysis_11.R', local=TRUE)
      Analysis<-LatVarfunc(DataInf(),Dichot())
    }
    
    else{
      validate(
        need(dim(rawData())[2] == 4, "Please ensure the correct composite structure is selected")
      )
      source('LatVarAnalysis_10.R', local=TRUE)
      
      Analysis<-LatVarfunc(DataInf(),Dichot())
    }
  }
  )
  
  library('knitr')
  
   getPage <- function() {
    
      if(input$Ctsno==2 && input$Binno==1){
         getPage <- withMathJax(includeMarkdown("twoctsonebin.Rmd")) }
     else if(input$Ctsno==1 && input$Binno==1){
        getPage <- withMathJax(includeHTML("onectsonebin.html"))}
      else if(input$Ctsno==2 && input$Binno==0){
        getPage <- withMathJax(includeHTML("twocts.html"))}
      else { getPage <- withMathJax(includeHTML("onects.html"))}
      
      return(getPage)
  }
  
  ModelPage <- eventReactive(input$GetModel,getPage())
  
  output$markdown <- renderUI({
    ModelPage()
  })
  
  output$plot2 <- renderUI({
    plotOutput("p2",width="100%",height="150px")%>% withSpinner(color="#0dc5c1")
  })
  
  
  output$p2<-renderPlot({
    
    Method <- c("Latent Variable", "Standard Binary")
    ResponseT <- GenAnalysis()[c(10,21)]
    ResponseC <- GenAnalysis()[c(11,22)]
    OddsRatio <- GenAnalysis()[c(2,13)]
    ClowOR <- GenAnalysis()[c(1,12)]
    CuppOR <- GenAnalysis()[c(3,14)]
    RiskRatio <- GenAnalysis()[c(5,16)]
    ClowRR <- GenAnalysis()[c(4,15)]
    CuppRR <- GenAnalysis()[c(6,17)]
    RiskDiff <- GenAnalysis()[c(8,19)]
    ClowRD <- GenAnalysis()[c(7,18)]
    CuppRD <- GenAnalysis()[c(9,20)]
    
    dataplots <- data.frame(Method, ResponseT, ResponseC, OddsRatio, ClowOR, CuppOR, RiskRatio, ClowRR, CuppRR,
                            RiskDiff,ClowRD,CuppRD)
    
    ggOR<-ggplot(dataplots,aes(x=as.factor(Method), y=OddsRatio, ymin=ClowOR, ymax=CuppOR, group=Method, 
                               color=Method))+
      geom_pointrange()+
      #geom_hline(yintercept = 0, linetype=3)+
      coord_flip()+
      ylab("Log-odds ratio")+
      xlab("")+
      scale_x_discrete(breaks = NULL)+
      scale_color_brewer(palette="Dark2")+ theme(legend.title = element_text(size = 14,face="bold"),legend.text = element_text(size = 12),
      axis.text=element_text(size=11), axis.title=element_text(size=12))
    
    ggRR<-ggplot(dataplots,aes(x=as.factor(Method), y=RiskRatio, ymin=ClowRR, ymax=CuppRR, group=Method, 
                               color=Method))+
      geom_pointrange()+
      #geom_hline(yintercept = 0, linetype=3)+
      coord_flip()+
      ylab("Log-risk ratio")+
      xlab("")+
      scale_x_discrete(breaks = NULL)+
      scale_color_brewer(palette="Dark2")+ theme(legend.title = element_text(size = 14,face="bold"),legend.text = element_text(size = 12),
                                                 axis.text=element_text(size=11), axis.title=element_text(size=12))
    
    ggRD<-ggplot(dataplots,aes(x=as.factor(Method), y=RiskDiff, ymin=ClowRD, ymax=CuppRD, group=Method, 
                               color=Method))+
      geom_pointrange()+
      #geom_hline(yintercept = 0, linetype=3)+
      coord_flip()+
      ylab("Risk Difference")+
      xlab("")+
      scale_x_discrete(breaks = NULL)+
      scale_color_brewer(palette="Dark2")+ theme(legend.title = element_text(size = 14,face="bold"),legend.text = element_text(size = 12),
                                                 axis.text=element_text(size=11), axis.title=element_text(size=12))
    
    ggarrange(ggOR,ggRR,ggRD,common.legend = TRUE,ncol=3)
    
    #print(ggresults)
    #vals$gg<-ggresults
    
  })
  
  output$plot3 <- renderUI({
    plotOutput("p3",width="120%",height="250px")
  })
  
  output$p3 <- renderPlot({
    
    n<-dim(rawData())[1]
    restot <- GenAnalysis()[23:(23+n)]
    df<-data.frame(restot)
    x <- seq(0, max(restot), 0.01)
    df2 <- with(df, data.frame(x = x, y = dchisq(x, df=3)))
    
    ggplot(df, aes(x=restot))+geom_histogram(bins=11,fill=I("white"),col=I("black"),aes(y=..density..))+
      theme_minimal()+ geom_line(data = df2, aes(x = x,y=y), color = "red")+xlab("Modified Pearson residual")+ylab("Density")+ 
      theme(legend.title = element_text(size = 14,face="bold"),legend.text = element_text(size = 12),axis.text=element_text(size=11), 
            axis.title=element_text(size=12))
  })
  
  output$plot4 <- renderUI({
    plotOutput("p4",width="90%",height="250px")
  })
  
  output$p4 <- renderPlot({
    
    n<-dim(rawData())[1]
    restot <- GenAnalysis()[23:(23+(n-1))]
    w=seq(1,n,1)
    z=restot
    dfres<-data.frame(w,z)
    ggplot(dfres,aes(x=restot))+geom_point(aes(x=w,y=z),shape=18,size=2,colour="DarkBlue")+
      ylab("Modified Pearson residual")+xlab("Patient number")+theme_bw()+ 
      theme(legend.title = element_text(size = 14,face="bold"),legend.text = element_text(size = 12),
       axis.text=element_text(size=11), axis.title=element_text(size=12))
    
    
  })
  
 # vals <- reactiveValues()
  
#  output$resultplot <- downloadHandler(
 #   filename = function() { paste(results, '.png', sep='') },
  #  content = function(file) {
   #   ggsave(file, plot = vals$gg, device = "png",width=10,height = 10,units="in")
    #}
  #)
  
  
})

shinyApp(ui, server)

