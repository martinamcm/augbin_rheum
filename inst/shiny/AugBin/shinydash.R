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

source('LatVarAnalysis.R', local=TRUE)
source('LatVarAnalysis11.R', local=TRUE)
source('LatVarAnalysis20.R', local=TRUE)
source('LatVarAnalysis10.R', local=TRUE)

#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}


ui <- dashboardPage(
  dashboardHeader(title = "AugBin"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
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
      conditionalPanel("input.Ctsno==2",        
       numericInput("dichY1", label=HTML("Y<sub>1</sub> responder threshold"), 0),
       checkboxInput("transformY1", label=HTML("Transform Y<sub>1</sub>"), FALSE),
       numericInput("dichY2", label=HTML("Y<sub>2</sub> responder threshold"), 0),
       checkboxInput("transformY2", label=HTML("Transform Y<sub>2</sub>"), FALSE)),
      
      conditionalPanel("input.Ctsno==1",
                       numericInput("dichY1", label=HTML("Y<sub>1</sub> responder threshold"), 0),
                       checkboxInput("transformY1", label=HTML("Transform Y<sub>1</sub>"), FALSE)),
      actionButton("GoAnalysis", "Run Analysis")
        ),
   
   column(width=8,
          uiOutput("ResultsTable"),
          uiOutput("plot2")
   )
        ),
   
   box(title="Goodness-of-fit",width=11,status="primary",solidHeader = TRUE,
       
       column(width=5,
       uiOutput("plot4")),
       
       column(width=5,
       uiOutput("plot3")
       )
       )
   
   )
  )
)


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
  
# rawData <- eventReactive(input$file1, {
#   read.csv(input$file1$datapath)
#  })
 
 rawData <- eventReactive(input$file1,{
   read.csv(input$file1$datapath)
   })
 
 newData<-reactive({
   
   rawData <- rawData()
   
   newDat1 <- rawData[,1]
   newDat2 <- rawData[,2]
   newDat3 <- rawData[,3]+200
   newDat4 <- rawData[,4]
   
   newData<-data.frame(newDat1,newDat2,newDat3,newDat4)
   
 })
  
 DataInf<- reactive({
 input$transformY1
   
   if(input$transformY1==TRUE){
     DataInf<-newData()
   }
  else{
    DataInf<-rawData()
  }
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
 
 
 GenAnalysis <- eventReactive(input$GoAnalysis,{

    
   if(input$Ctsno==2 && input$Binno==1){
     source('LatVarAnalysis.R', local=TRUE)
    
       Analysis<-LatVarfunc(rawData(),c(input$dichY1,input$dichY2))
      
     }
   
   else if(input$Ctsno==2 && input$Binno==0){
     source('LatVarAnalysis20.R', local=TRUE)
     Analysis<-LatVarfunc(rawData(),c(input$dichY1,input$dichY2))
   }
  
    else if(input$Ctsno==1 && input$Binno==1){
     source('LatVarAnalysis11.R', local=TRUE)
      Analysis<-LatVarfunc(rawData(),input$dichY1)
     }
   
   else{
     source('LatVarAnalysis10.R', local=TRUE)
     
      Analysis<-LatVarfunc(rawData(),input$dichY1)
   }
 }
 )
 
 library('knitr')
 
# getPage <- function() {
 #  return(includeHTML("twoctsonebin.html"))
 #}
 
 getPage <- eventReactive(input$GetModel,
                          if(input$Ctsno==2 && input$Binno==1){
                            includeHTML("twoctsonebin.html") }
                          else if(input$Ctsno==1 && input$Binno==1){
                            includeHTML("onectsonebin.html")}
                          else if(input$Ctsno==2 && input$Binno==0){
                            includeHTML("twocts.html")}
                          else {includeHTML("onects.html")}
          )
                            
 
 output$markdown <- renderUI({
   getPage()
   })
 
 
 #output$markdown <- renderUI({
#  HTML(markdown::markdownToHTML(knit('twoctsonebin.rmd', quiet = TRUE)))
# })
 
 
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
     geom_hline(yintercept = 0, linetype=3)+
     coord_flip()+
     ylab("Log-odds ratio")+
      xlab("")+
     scale_x_discrete(breaks = NULL)+
     scale_color_brewer(palette="Dark2")
   
    ggRR<-ggplot(dataplots,aes(x=as.factor(Method), y=RiskRatio, ymin=ClowRR, ymax=CuppRR, group=Method, 
                           color=Method))+
     geom_pointrange()+
     geom_hline(yintercept = 0, linetype=3)+
     coord_flip()+
     ylab("Log-risk ratio")+
      xlab("")+
     scale_x_discrete(breaks = NULL)+
     scale_color_brewer(palette="Dark2")
    
    ggRD<-ggplot(dataplots,aes(x=as.factor(Method), y=RiskDiff, ymin=ClowRD, ymax=CuppRD, group=Method, 
                                  color=Method))+
      geom_pointrange()+
      geom_hline(yintercept = 0, linetype=3)+
      coord_flip()+
      ylab("Risk Difference")+
      xlab("")+
      scale_x_discrete(breaks = NULL)+
      scale_color_brewer(palette="Dark2")
    
    ggarrange(ggOR,ggRR,ggRD,common.legend = TRUE,ncol=3)
 })
 
 output$plot3 <- renderUI({
   plotOutput("p3")
 })
   
 output$p3 <- renderPlot({
  
 n<-dim(rawData())[1]
 restot <- GenAnalysis()[23:(23+n)]
 df<-data.frame(restot)
 x <- seq(0, max(restot), 0.01)
 df2 <- with(df, data.frame(x = x, y = dchisq(x, df=3)))
 
 ggplot(df, aes(x=restot))+geom_histogram(bins=11,fill=I("white"),col=I("black"),aes(y=..density..))+
   theme_minimal()+ geom_line(data = df2, aes(x = x,y=y), color = "red")+xlab("Modified Pearson residual")
})
 
 output$plot4 <- renderUI({
   plotOutput("p4")
 })
 
 output$p4 <- renderPlot({
 
 n<-dim(rawData())[1]
 restot <- GenAnalysis()[23:(23+(n-1))]
 w=seq(1,n,1)
 z=restot
 dfres<-data.frame(w,z)
 ggplot(dfres,aes(x=restot))+geom_point(aes(x=w,y=z),shape=18,size=2,colour="DarkBlue")+ylab("Modified Pearson residual")+xlab("Patient number")+theme_bw()
 
 
 })
 
 
})

shinyApp(ui, server)

