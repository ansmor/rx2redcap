#load libraries
library(shiny)
library(dplyr)
library(janitor)
library(tidyr)

#define function
myfunction.rxredcap<-function(rx,visit,record_id){
    colnms.indx<-grep("slice",rx$V1)
    
    #identify table columns names
    colnms<-rx$V1[colnms.indx]
    colnmms.sel<-unlist(strsplit(unique(colnms),";"))[-c(1:3)]
    
    #identify table row names
    varoi<-rx$V1[colnms.indx-1]
    varoi<-janitor::make_clean_names(trimws(tolower(gsub('[[:punct:] ]+',' ',varoi))))
    
    #create results dataframe
    resultsoi<-rx$V1[colnms.indx+1]
    resultdf<-as.data.frame(do.call("rbind",strsplit(resultsoi,";"))) %>% 
        dplyr::select(-V1,-V2,-V3) %>% 
        'colnames<-'(c(colnmms.sel)) %>% 
        'rownames<-'(varoi) %>% 
        tibble::rownames_to_column(.,var="varoi")
    
    #conversion table
    conv.rows<-as.data.frame(rbind(cbind("surf_in_cm","scm2"),
                                   cbind("surf_raw_in_pixel","sraw"),
                                   cbind("mean_in_hu","mean"),
                                   cbind("min_in_hu","min"),
                                   cbind("max_in_hu","max"),
                                   cbind("sd","sd"))) %>% 
        'colnames<-'(c("varoi","redcap"))
    
    conv.cols<-as.data.frame(rbind(cbind("Muscle","musc"),
                                   cbind("IMAT","imat"),
                                   cbind("Bone","bone"),
                                   cbind("VAT","vat"),
                                   cbind("SAT","sat"))) %>% 
        'colnames<-'(c("varoi","redcap"))
    
    result.import<-reshape2::melt(resultdf,id.vars="varoi") %>% 
        merge(.,conv.rows,by="varoi") %>% 
        merge(.,conv.cols,by.x="variable",by.y="varoi") %>% 
        rowwise() %>% 
        mutate(var.import=paste(redcap.y,redcap.x,visit,sep="_")) %>% 
        dplyr::select(var.import,value) %>% 
        mutate(value=as.numeric(value))
    result.import.format<-tidyr::spread(result.import, key = var.import, value = value)
    result.import.format$record_id<-record_id
    result.import.format<-cbind(result.import.format[,ncol(result.import.format)],result.import.format[,1:(ncol(result.import.format)-1)])
}

#create shiny app
ui_upload <- sidebarLayout(
    sidebarPanel(
        h4("STEP 1: upload the data"),
        fileInput("file", "a. import data", buttonLabel = "Upload..."),
        textInput("visit", "b. indicate if v1 (visit 1), v2 (visit 2), etc..."),
        numericInput("record_id", "c. add the record ID",NULL,min=1)
    ),
    mainPanel(
        h5("Raw imported data (preview)"),
        tableOutput("preview1")
    )
)

ui_clean <- sidebarLayout(
    sidebarPanel(
        h4("STEP 2: transform the data"),
        tags$style(".well {background-color:#99EDC3;}"),
        checkboxInput("empty", "check the box to apply transformation")
    ),
    mainPanel(
        h5("Data ready to be imported in RedCap"),
        tableOutput("preview2")
    )
)

ui_download <- fluidRow(
    column(width = 12, downloadButton("download", class = "btn-block"))
)

ui <- fluidPage(
    title = "Rx2Red",
    headerPanel(
        h2("Rx2RedCap:\n convert radiology results data into RedCap format",
           style = "font-weight: 500; color: black")),
    ui_upload,
    ui_clean,
    ui_download,
    hr(),
    print("03.05.2023 - ansmor")
)


server <- function(input, output, session) {
    # Upload ---------------------------------------------------------
    raw <- reactive({
        req(input$file)
        read.csv(file=input$file$datapath,header=F)
    })
    output$preview1 <- renderTable(head(raw(), 5))
    
    # Clean ----------------------------------------------------------
    tidied <- reactive({
        out <- raw()
        if (input$empty) {
            out <- myfunction.rxredcap(out,input$visit,input$record_id)
        }
        out
    })
    output$preview2 <- renderTable(head(tidied(), 1))
    
    # Download -------------------------------------------------------
    output$download <- downloadHandler(
        filename = function() {
            paste0(tools::file_path_sans_ext(input$file$name), ".csv")
        },
        content = function(file) {
            vroom::vroom_write(tidied(), file)
        }
    )
}


shinyApp(ui, server)
