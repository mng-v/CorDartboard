#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("shinythemes")
# install.packages("shinyalert")
# install.packages("plotrix")
# install.packages("RColorBrewer")
# install.packages("dplyr")
# install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyalert)
library(plotrix)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
#
box_size <- 2

# Define UI for application that draws a histogram
ui <- fluidPage(
   #useShinyjs(),  # Set up shinyjs
    #Everyone can choose their own style of application
    theme = shinytheme("yeti"),
    navbarPage(title = "Virumaa college",
               
                tabPanel("Nav1: File",
                
                        # Application title
                        titlePanel("File Uploading"),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(fluid = TRUE,
                            sidebarPanel(style = "background-color: #FEF8DF",
                                
                                    h5("Upload the file from computer"),   
                                    fileInput("file",
                                              "Upload the file:"),
                                    h5("The default maximum file size is 5 MB"),
                                    radioButtons("sep","Separator",
                                                 choices = c(Comma=',',Semicolon=';',Tab='\t',Space='')
                                    ),br(),
                                    checkboxInput("header","Header",value = FALSE),
                                    checkboxInput("stringAsFactor","stringAsFactor",FALSE),
                                    radioButtons("encoding","Encoding",
                                                 choices = c(UTF8='UTF-8',ANSI='ANSI',CyrillicW ='Windows-1251')
                                    )
                                )
                                
                            ,
                            
                            
                            # Show a file and file info
                            mainPanel(uiOutput("tb"))
                        )   
               ),
               tabPanel("Nav2: Variables",
                        titlePanel("Working with dataset variables"),
                        tags$head(tags$style(type = 'text/css',".shiny-input-panel{background-color: darkgreen}")),
                          sidebarLayout( position = c("left"),fluid = FALSE,
                            sidebarPanel(style = "background-color: #FEF8DF",
                              uiOutput("Pre"),
                              br()
                              
                              ),
                                mainPanel(uiOutput("num"))
                                      )
                       ),
                        
               tabPanel("Nav3: Correlation",
                        titlePanel("Working with correlation"), 
                        sidebarLayout( position = c("left"),fluid = TRUE,
                        sidebarPanel(style = "background-color: #FEF8DF",
                          uiOutput("sidebar3"),
                          br()),
                        mainPanel(uiOutput("main3"))
                        
                        )),
               tabPanel("Nav4: About project",
                        titlePanel(
                          h2("About project", style = "text-align:center; color: #067BA8; ")
                          ),
                        
                          dashboardBody(
                          
                          flipBox(
                            id = 1,
                            main_img = "Maksimova.jpg",
                            header_img = "blueBG.jpg",
                            front_title = "Natalja Maksimova",
                            back_title = "About Natalja",
                            back_content="
                            I'm a second year MSc student in Business Information Technology at TalTech Virumaa College.
I'm interested in data analysis, machine learning and R language. I like to work with RStudio.
Besides, I like to create 3D models, animations and web games.
I believe that successful learning happens through projects, 
you cannot know everything and it is difficult for one to create something on a large scale.
                            "
                          ),
                             flipBox(
                          id = 2,
                          main_img = "Erika.jpg",
                          header_img = "orangeBG.jpg",
                          front_title = "Erika Maksimova",
                          back_title = "About Erika",
                          back_content="
                            I am a first year Computer Science BSc student at Tallinn University of Technology. I am interested in artificial intelligence, machine learning and Python,
                            but I always look for new projects and new opportunities to widen my horizons.
                            I have chosen the field of IT because of my passion for innovation and progress 
                            — I believe that you only truly learn something when you find a way to apply your
                            knowledge for the betterment of others. In my free time I like to learn different 
                            languages, seeing as I believe that it is important to continue the process of
                            learning even out of school and work environment.
                            "
                        ),
                          flipBox(
                            id = 3,
                            main_img = "projectLogo.JPG",
                            header_img = "blueBG.jpg",
                            front_title = "Correlation project",
                            back_title = "About project",
                            back_content= "This project was created to determine and 
                            visualize the strength of the linear relationship between 
                            numerical variables. The strength of the relationship between a selected target variable and the other variables in a data set is 
                            determined through the linear correlation coefficient. 
                            Linear correlation refers to straight-line relationships between two variables.
This project explores correlation using a chart that resembles a dartboard with radius 1, which is divided into 5 concentric rings of equal width. The position of a point on this dartboard is defined by the correlation coefficient, and the closer to the center, the greater feature influence on the target variable. The correlation sign is shown using color: green for positive and red for negative correlation coefficients.
It works very simply. The main panel consists of four tabs Nav1: File, Nav2: Variables, Nav3: Correlation, Nav4: About project.
1. On the Nav1: File tab upload the data file from your computer. You can specify the columns separator and the encoding and mark header and stringAsFactor options. You can also get a descriptive overview of the dataset here by clicking on the subtab Summary.
2. On the Nav2: Variables tab you see the list of all dataset variables. Remove unnecessary variables before calculating correlations by unchecking their check boxes. You can also see here an overview of types of variables included in the dataset.
3. On the Nav3: Correlation tab visualization of the strength of linear dependence is provided using only numerical data features. You should select your target variable from the list. In addition to visualization, the total
                            numbers of variables with very weak, weak, medium, strong, very strong correlation with target variable are calculated.
                            Since in the future medium, strong, very strong relationships are more interesting, the names of 
                            these features are displayed below the chart.",
                            fluidRow(
                              boxPad(
                               # color = "green",
                                descriptionBlock(
                                  header = "Version 1.0",
                                  text = "11.01.2021",
                                  rightBorder = FALSE,
                                  marginBottom = TRUE
                                )
                            )
                          )
                      
                      
                        )
                        )
        
        
    )
   
))

# 
server <- function(input, output) {
# NAV 1
    #read the data from the file
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){
            return()
        }
        read.table(file1$datapath,sep = input$sep,header=input$header,
                   stringsAsFactors = input$stringAsFactor,
                   encoding=input$encoding)  
    }
        
    )
    
    #this reactive output contains the dataset
    output$filedf <- renderTable({
        file_to_read=input$file
        if(is.null(data())){
            return()
        }
        
        input$file
    }
        
    )
    #this reactive output displays the dataset in table format
    output$table <- DT::renderDataTable({
        if(is.null(data())){return ()}
        DT::datatable(data(),
                      options = list(pageLength = 8)
                      )
    })
    
    # this reactive output displays dataset dimensions
    output$dim <- renderText({
        
        HTML(paste0("Rows number is ",nrow(data())," and columns number is ", ncol(data())))
        
        })
    
    #this reactive output contains the summary of the dataset and displays the summary in table format
    output$sum <- renderPrint({
        if(is.null(data())){return()}
        summary(data())
           
    })
    
    # the following renderUI is used to dynamically generate the tabsets when the file is loaded
    output$tb <- renderUI({
      
        if(is.null(data()))
            h5("Works in",tags$img(src='RStudio.png',heigth=170,width=170),
               tags$img(src='Virumaa_kolledz_gradient_EST_veeb.jpg',heigth=170,width=170))
        else
            tabsetPanel(id = 'dataset', 
                        
                tabPanel("File",
                         h4("File options:", style = "color: #067BA8"),
                         tableOutput("filedf"),
                        tabPanel("Dataset dimensions",
                                 h4("Dataset dimensions:", style = "color: #067BA8"),
                                 textOutput("dim"),
                                 br()),
                        tabPanel("Data",
                                 h4("File dataset:", style = "color: #067BA8"),
                                 DT::dataTableOutput("table"))
                        ),
                tabPanel("Summary",
                         h4("Summary of the dataset:", style = "color: #067BA8"),
                         verbatimTextOutput("sum"))
                )
    })
    
   
   
   
    
    # NAV2
   # NAV 2: SidebarLayout
    output$Pre <- renderUI({
        newData=data()
        if(is.null(newData))
            p("Opens if some file is selected")
        else
        conditionalPanel(
            #Input
          br(),
            h5("Select Variables:"),
            checkboxGroupInput(inputId = "select_vars",
                                               label="",
                                               names(newData),choices = names(newData)),
            # Button
            h5("Dowload dataset as csv file to your computer:"),
            downloadButton("downloadData", "Download")
        )
    })
    
    #NAV2 renders
    # shows data after feature selection
    output$table1 <- DT::renderDataTable({
        newData=data()
        if(is.null(newData)){return ()}
        DT::datatable(newData[,input$select_vars, drop = FALSE],
                      options = list(pageLength = 3)
        )
    })
    
    
    # outputs how many numeric or factor variables are in total
    output$num_type <- renderValueBox({
      
         nData=selectData()
         valueBox(sum(sapply(nData, is.numeric)),
                  "Numeric variables", icon = icon("sort-numeric-up"),color = "yellow")
           })
    
    
    output$fac_type <- renderValueBox({
      nData=selectData()
      valueBox(sum(sapply(nData, is.factor)),
               "Factor variables", icon = icon("list-ol"),color = "light-blue")
      
    })
    output$chr_type <- renderValueBox({
      nData=selectData()
      valueBox(sum(sapply(nData, is.character)),
               "Character variables", icon = icon("paragraph"),color = "navy")
      
    })
    
    # outputs the types of the selected features
    output$every_type <- renderTable({
      nData=selectData()
      t=sapply(nData,class)
      t <- as.data.frame(t)
      r=row.names(t)
      cbind(Var=r,Type=t)
    })
    
    #outputs box 2 with the types of the selected variables
    output$all_types <- renderValueBox({
      nData=selectData()
      valueBox(
                      checkboxGroupInput("class_var", label=h4("2. Select Variables"),
                                          names(nData)),
                               
                                      
       icon = icon("edit"), subtitle = "then choose another type")
    })
    
     
   
    
    # NAV2 mainpanel
    output$num <- renderUI({
        
       if(is.null(data()))
               {
                h5("TEXT")
                h5("Works in",tags$img(src='RStudio.png',heigth=170,width=170),
               tags$img(src='Virumaa_kolledz_gradient_EST_veeb.jpg',heigth=170,width=170))
              }
       else
        tabsetPanel(id="variables",
        tabPanel("Work with variables",
                tabPanel("Data rep",
                         h4("Dataset after feature selection:", style = "color: #067BA8"),
                         DT::dataTableOutput("table1"))       
                 ),
       
        tabPanel("Variables' types",value = "Type",
                 h4("Dataset variables' types", style = "color: #067BA8"),
                 valueBoxOutput("num_type"),
                 valueBoxOutput("fac_type"),
                 valueBoxOutput("chr_type"),
                
                 #1
                 box(
                   tableOutput("every_type"),
                       status = "info", title = h4("The types of all variables"),
                   width = 4
                      )
           ))
    })
    #NAV2 reactived
    # 1. Download  csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selectData(), file, row.names = FALSE)
      }
    )
    #write the newdata with the selected variables
    selectData <- reactive({
      newData=data()
      if(is.null(newData)){return ()}
      newData[,input$select_vars, drop = FALSE]
    })
    
      
    
    #NAV3 reactived 
    
    numData <- reactive({
      newData=selectData()
      if(is.null(newData)){return ()}
        Filter(is.numeric,newData)
    })
    y <- reactive({
      newData= numData()
      if(is.null(newData)){return ()}
      newData[,input$select_y]
    })
    
    
      
      y_nr <- reactive({
       # newData= numData()
       if(is.null(numData())){return ()}
        which(colnames(numData())==input$select_y )
        
        }) 
      
  #X <- dbC[,-y_nr]
      X <- reactive({
        newData= numData()
        nr <- y_nr()
        if(is.null(newData)){return ()}
        newData[,-nr]
      })
   #r <- cor(x=X,y=Y), X <- dbC[,-y_nr],Y <- dbC[,y_nr]
      
      r <- reactive({
        X= X()
        newData= numData()
        nr <- y_nr()
        Y <- newData[,nr]
        #cor(x=newData,y=Y)
        cor(newData)[nr,-nr]
      })
   #corDB <- data.frame(value = abs(r),variable=colnames(num_db[,-1]),sign=ifelse(r>0,"+","-")) 
      corDB <- reactive({
        nr <- y_nr()
        corr <- r()
        #num_db <- numData()
        num_db <- numData()[,- y_nr()]
        var <- colnames(num_db)
        
          data.frame(Cor=corr,Var=var,Sign=ifelse(corr>0,"+","-"))
       
          
        
      })
      #very strong vs<- data.frame(V=ifelse(abs(r)<=1 & abs(r)>0.8,1,0),name=row.names(r),corr=r)
      veryStrong <- reactive({
        corDB() %>% filter(abs(Cor)<1 & abs(Cor)>0.8)
       
      })
      strong <- reactive({
         corDB() %>% filter(abs(Cor)<=0.8 & abs(Cor)>0.6)
        
      })
      medium <- reactive({
        corDB() %>% filter(abs(Cor)<=0.6 & abs(Cor)>0.4)
      })
      weak <- reactive({
        corDB() %>% filter(abs(Cor)<=0.4 & abs(Cor)>0.2)
      })
      veryWeak <- reactive({
        corDB() %>% filter(abs(Cor)<=0.2 & abs(Cor)>0)
      })
     
    #--NAV3 outputs---
      #1
      output$y_text <- renderText({
        if(is.null(numData())){return ()}
        HTML("Selected y: ",input$select_y)})
      #2
      output$yNr <- renderText({
        HTML("Number of column for selected y: ",y_nr())})
      #3
      output$yCor <- renderText({
        
        HTML("Vector of correlations with selected y: ",round(r()[-y_nr()],3))})
      #4.1
      output$corVS <- renderTable({
         veryStrong() %>% select(Cor,Sign,Var) %>% mutate(Cor=abs(Cor)) %>% arrange(desc(abs(Cor)))
       
      })
      #4.2
      output$corS <- renderTable({
           strong() %>% select(Cor,Sign,Var)  %>% mutate(Cor=abs(Cor)) %>% arrange(desc(Cor))
        
      })
      #4.3
      output$corM <- renderTable({
          medium() %>% select(Cor,Sign,Var) %>% mutate(Cor=abs(Cor)) %>% arrange(desc(Cor))
        
      })
      #5 PLOT 1
      output$plotCor <- renderPlot({
      #if(ncol(corDB)==0){return()}
     
       # DB() <- corDB() %>% filter(Cor!=1)
        r.n <- corDB() %>% filter(Cor<0) %>%  select(Cor)
        r.p <- corDB() %>% filter(Cor>0 & Cor<1) %>% select(Cor)
        rC.n <- as.vector(1-abs(r.n))
        rC.p <- as.vector(1-r.p)
        n <- nrow(rC.p)
        m <- nrow(rC.n)
        #ainult pos corr
        phi_d_p <- matrix()
        p <- seq(1:length(rC.p))
        phi_d_p <- 360/(2*p)
       
        #ainult neg corr
        i <- seq(1:length(rC.n))
        phi_d_n <- matrix()
        phi_d_n <- 360/(3*i)
        
       # leg.text=c("very weak", "weak", "medium", "strong", "very strong")
       # colors=c( "#ffcc33",  "#ffff99","#cce2cb","aquamarine3","chartreuse3")
        plot(-2:2,-2:2,type="n",xlab="",ylab="",main="Correlation effect dartboard", col.axis="white", col.main="brown")
        draw.circle(0,0,c(1,0.8,0.6,0.4,0.2),border="white",lty=1,lwd=1,
                    col=c( "#ffcc33",  "#ffff99","#cce2cb","aquamarine3","chartreuse3"))
        text(c(0,0,0,0,0), c(-0.7,-1.1,-1.5,-1.95,-2.3), 
             labels = c("0.8","0.6","0.4","0.2","0"),
             cex=0.6, font=1, pos=3)
        text(c(0,0,0,0,0), c(0.3,0.7,1.1,1.5,1.8), 
             labels = c("0.8","0.6","0.4","0.2","0"),
             cex=0.6, font=1, pos=3)
        legend(1, 0,legend = c("very weak", "weak", "medium", "strong", "very strong") ,
               col = c( "#ffcc33",  "#ffff99","#cce2cb","aquamarine3","chartreuse3"),
               text.col = "brown",
               fill=c( "#ffcc33",  "#ffff99","#cce2cb","aquamarine3","chartreuse3"),
               bg = "gray90")
        if(m!=0)
         
          draw.arc(x=0,y=0,radius=rC.n$Cor, deg1 = phi_d_n,deg2 = phi_d_n+3,lwd = 5,col = "#ff6666") 
        if(n!=0)
            draw.arc(x=0,y=0,radius=rC.p$Cor, deg1 = phi_d_p,deg2 = phi_d_p+3,lwd = 5,col = "#006666") 
        else
          draw.arc(0,0,0)
         
      })
      #PLOT 2 ggplot
      output$ggplotCor <- renderPlot({
        distance <- function(x, y) {sqrt((x)^2 + (y)^2)}
        d <- expand.grid(x = seq(-1, 1, 0.01), y = seq(-1, 1, 0.01))
        
        d$dist <- mapply(distance, x = d$x, y = d$y)
        d$incircle <- (d$x)^2 + (d$y)^2 < 1 
        d <- d[d$incircle,]
        #data.frame(Cor=corr,Var=var,Sign=ifelse(corr>0,"+","-"))
        DF <- corDB() %>% filter(Cor<1)
        n <- nrow(DF)
        alpha <- sample(seq(0,2*pi,2*pi/10),n,replace = TRUE)
        DF$x <- (1-abs(DF$Cor))*cos(alpha)
        DF$y <- (1-abs(DF$Cor))*sin(alpha)
        ggplot(data=d, aes(x, y)) +
          geom_raster(aes(fill = dist), interpolate = T) +
          stat_contour(aes(z = dist), col = 'white',breaks = seq(0, 1, 0.2)) +
          coord_fixed() + 
          scale_fill_gradient2(low="red", mid = 'yellow', high="blue",midpoint = 1, 
                               breaks = seq(0,0.8,0.2),labels = c("high", "", "medium", "", "low"),
                               name="")+
          geom_point(data=DF,aes(x,y,color=Sign,pch=Sign))+
          geom_text(data=DF,aes(x,y,label=Var,color=Sign),hjust=1, vjust=1, angle=45)+
          
          scale_color_manual(guide = FALSE, values = c("black", "darkgreen"))+
          theme_void()
      })
      #6
      output$vStrongV <- renderInfoBox({
        
        nData=veryStrong()
        
          valueBox( value = tags$p(nrow(nData), style = "font-size: 100%;"),
                  subtitle=tags$p("Very strong",style = "font-size: 100%;"),
                 icon = icon("sort-numeric-up")
               )
      })
      # 7 valueBoxOutput("strongV")
      output$strongV <- renderInfoBox({
        nData=strong()
        if(is.null(nData)){return()}
                 valueBox(value = tags$p(nrow(nData), style = "font-size: 95%;"),
                 subtitle=tags$p("Strong",style = "font-size: 95%;"),
                 icon = icon("sort-numeric-up"),color = "yellow"
                )
      })
      # 8 valueBoxOutput("mediumV")
      output$mediumV <- renderValueBox({
        
        nData=medium()
        if(is.null(nData)){return()}
                 valueBox(value = tags$p(nrow(nData), style = "font-size: 90%;"),
                 subtitle=tags$p("Medium",style = "font-size: 90%;"),
                 icon = icon("sort-numeric-up"),color = "yellow"
                 )
      })
      # 9 valueBoxOutput("weakV")
      output$weakV <- renderValueBox({
        
        nData=weak()
        if(is.null(nData)){return()}
       
          valueBox(value = tags$p(nrow(nData), style = "font-size: 85%;"),
                 subtitle=tags$p("Weak",style = "font-size: 85%;"),
                 icon = icon("sort-numeric-up"),color = "yellow"
                 )
      })
      # 10 valueBoxOutput("vWeakV")
      output$vWeakV <- renderValueBox({
        
        nData=veryWeak()
        if(is.null(nData)){return()}
 
          valueBox(value = tags$p(nrow(nData), style = "font-size: 80%;"),
                 subtitle=tags$p("Very weak",style = "font-size: 80%;"),
                 icon = icon("sort-numeric-up"),color = "yellow"
                 )
      })
    #NAV3 sidebarPanel
    output$sidebar3 <- renderUI({
      newData=numData()
      if(is.null(newData))
        p("Opens if some file is selected")
      else
       
          conditionalPanel(
          #Input
            br(),
            h5("Select a target variable y"),
          radioButtons(inputId = "select_y",
                       label="",
                       names(newData))
          
        )
        
    })
    # NAV3 mainpanel
    output$main3 <- renderUI({
      if(is.null(data()))
      {
        h5("TEXT")
        h5("Works in",tags$img(src='RStudio.png',heigth=170,width=170),
           tags$img(src='Virumaa_kolledz_gradient_EST_veeb.jpg',heigth=170,width=170))
      }
      else
        tabsetPanel(id="cor",
                    tabPanel("Correlation",
                      
                      br(),
                      h4("Predictable variable:", style = "color: #067BA8"),
                      textOutput("y_text"),
                     
                      br(),
                      textOutput("yCor"),
                    
                      fluidRow(
                      div(style="text-align:center;",
                       box(width = 12,
                          infoBoxOutput("vStrongV",width = box_size),
                          infoBoxOutput("strongV",width = box_size),
                          valueBoxOutput("mediumV",width = box_size),
                          valueBoxOutput("weakV",width = box_size),
                          valueBoxOutput("vWeakV",width = box_size)
                        )
                       )),
                      h4("The correlation dartboard #1", style = "color: #067BA8"),
                      plotOutput("plotCor", height = "450px"),
                      h4("The correlation dartboard #2", style = "color: #067BA8"),
                      plotOutput('ggplotCor'),
                      
                          fluidRow(
                            div(style="text-align:center",
                            if (nrow(veryStrong())==0)
                              p("")
                            else
                                box(title = "Very Strong",width = 4,
                                  tableOutput("corVS")
                            ),
                            if (nrow(strong())==0)
                              p("")
                            else
                            box(title = "Strong",width = 4,
                                #valueBoxOutput("strongV",width = box_size),
                                tableOutput("corS")
                            ),
                            if (nrow(medium())==0)
                              p("")
                            else
                            box(title = "Medium",width = 4,
                                # valueBoxOutput("mediumV",width = box_size),
                                tableOutput("corM")
                            )
                          )
                          )
                                 
                            
                        ))
                  
      
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
