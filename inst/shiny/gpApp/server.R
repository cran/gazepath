library(shiny)
library(gazepath)
library(jpeg)
library(scales)
options(shiny.maxRequestSize = 50 * 1024^2)

options(trace = FALSE)

shinyServer(function(input, output) {
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- list()
    for(i in 1:nrow(inFile)){
      data[[i]] <- read.csv(inFile[i,4], header=input$header, sep=input$sep, quote=input$quote, na.strings = input$na)
      row.names(data[[i]]) <- 1:dim(data[[i]])[1]
    }
    names(data) <- as.vector(as.character(inFile[,1]))
    return(data)
  })
  
  rowNames <- reactive({
    names(dataInput()[[1]])
  })
  
  output$nameXVariables <- renderUI({
    selectInput('nameX', 'Name of the column with the left eye x-coordinates', rowNames())
  })
  
  output$nameYVariables <- renderUI({
    selectInput('nameY', 'Name of the column with the left eye y-coordinates', rowNames())
  })
  
  output$nameDVariables <- renderUI({
    selectInput('nameD', 'Name of the column with the distance', rowNames())
  })
  
  output$nameX2Variables <- renderUI({
    selectInput('nameX2', 'Name of the column with the right eye x-coordinates', rowNames())
  })
  
  output$nameY2Variables <- renderUI({
    selectInput('nameY2', 'Name of the column with the right eye y-coordinates', rowNames())
  })
  
  output$nameD2Variables <- renderUI({
    selectInput('nameD2', 'Name of the column with the distance', rowNames())
  })
  
  output$nameTVariables <- renderUI({
    selectInput('nameT', 'Name of the column with the trial index', rowNames())
  })

  plot_sizew <- function(){
    input$p_w
  }
  
  plot_sizeh <- function(){
    input$p_h
  }
  
  output$ppiVariables <- renderUI({
    selectInput('ppi', 'Participant', nameVar())
  })
  
  output$image <- renderPlot({
    infile <- input$image
    x <- readJPEG(infile$datapath, native = TRUE)
    res <- dim(x)[1:2]
    plot(1, 1, xlim = c((input$res_x - res[2]) / 2, input$res_x - ((input$res_x - res[2]) / 2)), ylim = c(input$res_y - ((input$res_y - res[1]) / 2), (input$res_y - res[1]) / 2), col = 'white', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', bty = 'n')
    rasterImage(x, (input$res_x - res[2]) / 2, input$res_y - ((input$res_y - res[1]) / 2), input$res_x - ((input$res_x - res[2]) / 2), (input$res_y - res[1]) / 2)
    sim <- gazepathInput()[[which(nameVar() == input$ppi)]][[16]][[as.numeric(input$ip)]]
    lines(gazepathInput()[[which(nameVar() == input$ppi)]][[2]][[as.numeric(input$ip)]], gazepathInput()[[which(nameVar() == input$ppi)]][[3]][[as.numeric(input$ip)]])
    points(sim[sim[,1] == 'f', 9:10], cex = sim[sim[,1] == 'f', 2] / 50, col = alpha(2, .5), pch = 16)
    points(sim[sim[,1] == 'f', 9:10], col = 3, pch = letters, cex = 2)
  }, width = plot_sizew, height = plot_sizeh)
  
  outVar <- reactive({
    if(length(which(nameVar() == input$ppi)) != 0){
      unique(summary(gazepathInput()[[which(nameVar() == input$ppi)]])$Trial)
    } 
  })
  
  outVarppp <- reactive({
    if(length(which(nameVar() == input$ppp)) != 0){
      unique(summary(gazepathInput()[[which(nameVar() == input$ppp)]])$Trial)
    } 
  })
  
  outVarpti <- reactive({
    if(length(which(nameVar() == input$pti)) != 0){
      unique(summary(gazepathInput()[[which(nameVar() == input$pti)]])$Trial)
    } 
  })
  
  output$trialVariables <- renderUI({
    selectInput('i', 'Trial Number', outVarppp())
  })
  
  output$treshVariables <- renderUI({
    selectInput('ii', 'Trial Number', outVarpti())
  })
  
  output$plotVariables <- renderUI({
    selectInput('ip', 'Select the corresponding trial', outVar())
  })
  
  output$textR <- renderText({
    'Click browse to select data from one or multiple participants, once the data is loaded you can see the first 10 and last 10 rows of the raw data of that participant. Use the options on the left to make sure the data is loaded correctly. If your data is loaded correctly you can proceed to the analyze data tab.'
  })
  
  output$textF <- renderText({
    'Select to correct variables and press the go button to perform the analyses, the data of all participants that were loaded in the previous step is analyzed, therefore it may take a while. \n\n After the analyses are done you can select the participants to see his or her first 20 fixations and saccades. \n\n Move on to the next tab to visualize the data.'
  })
  
  nameVar <- reactive({
    names(dataInput())
  })
  
  output$ppVariables <- renderUI({
    selectInput('pp', 'Participant', nameVar())
  })
  
  output$ptVariables <- renderUI({
    selectInput('pt', 'Participant', nameVar())
  })

  output$contenth <- renderTable({
    withProgress(
      if(length(which(nameVar() == input$pp)) != 0) head(dataInput()[[which(nameVar() == input$pp)]], 10),
      min = .3, max = 1,
      message = 'Loading data, this may take some time, please wait'
    )
  })
  
  output$contentt <- renderTable({
    withProgress(
      if(length(which(nameVar() == input$pp)) != 0) tail(dataInput()[[which(nameVar() == input$pp)]], 10),
      min = .3, max = 1,
      message = 'Loading data, this may take some time, please wait'
    )
  })
  
  gazepathInput <- eventReactive(input$go, {
    out <- list()
    withProgress(
    for(i in 1:length(nameVar())){
      incProgress(1/length(nameVar()), message = paste('Running the analysis of participant', nameVar()[i]), 'Please wait')
      out[[i]] <- gazepath(data = dataInput()[[i]],
                           x1 = input$nameX,
                           y1 = input$nameY,
                           x2 = input$nameX2,
                           y2 = input$nameY2,
                           d1 = input$nameD,
                           d2 = input$nameD2,
                           trial = input$nameT,
                           height_px = input$height_px,
                           height_mm = input$height_mm,
                           width_px = input$width_px,
                           width_mm = input$width_mm,
                           res_x = input$res_x,
                           res_y = input$res_y,
                           samplerate = input$samplerate,
                           method = input$method,
                           extra_var = ifelse(is.null(input$extra_var), NULL, strsplit(input$extra_var, ',')[[1]]))
    })
    return(out)
  })
  
  output$data <- renderTable({
    if(length(which(nameVar() == input$pt)) != 0) head(summary(gazepathInput()[[which(nameVar() == input$pt)]]), 20)
  })
  
  outData <- reactive({
    if(input$out == 'All fixations and saccades'){
      df <- numeric()
      for(i in 1:length(nameVar())){
        df <- rbind(df, cbind(summary(gazepathInput()[[i]]), nameVar()[i]))
      }
    } else {
      if(input$out == 'Only complete fixations and saccades'){
        df <- numeric()
        for(i in 1:length(nameVar())){
          df <- rbind(df, cbind(summary(gazepathInput()[[i]], complete_only = TRUE), nameVar()[i]))
        }
      } else {
        if(input$out == 'Fixations only'){
          df <- numeric()
          for(i in 1:length(nameVar())){
            df <- rbind(df, cbind(summary(gazepathInput()[[i]], fixations_only = TRUE), nameVar()[i]))
          } 
        } else {
          df <- numeric()
          for(i in 1:length(nameVar())){
            df <- rbind(df, cbind(summary(gazepathInput()[[i]], complete_only = TRUE, fixations_only = TRUE), nameVar()[i]))
          }
        }
      }
    }
    names(df)[dim(df)[2]] <- 'Participant'
    return(df)
  })
  
  output$datasum <- renderTable({
    head(outData(), 20)
  })
  
  plot_size_w <- function(){
    input$plot_w
  }
  
  plot_size_h <- function(){
    input$plot_h
  }
  
  output$pppVariables <- renderUI({
    selectInput('ppp', 'Participant', nameVar())
  })
  
  output$plot <- renderPlot({
    if(length(which(nameVar() == input$ppp)) != 0) plot(gazepathInput()[[which(nameVar() == input$ppp)]], trial_index = as.numeric(input$i))
  }, width = plot_size_w, height = plot_size_h
  )
  
  output$ptiVariables <- renderUI({
    selectInput('pti', 'Participant', nameVar())
  })
  
  output$plotMould <- renderPlot({
    if(length(which(nameVar() == input$pti)) != 0) gazepath:::Mould_vel(gazepathInput()[[which(nameVar() == input$pti)]][[9]][[as.numeric(input$ii)]], gazepathInput()[[which(nameVar() == input$pti)]][[10]], TRUE)
  })
  
  output$rob <- renderPlot({
    ROB <- sapply(1:length(nameVar()), function(i) mean(gazepathInput()[[i]][[5]], na.rm = T))
    MFD <- sapply(1:length(nameVar()), function(i) median(summary(gazepathInput()[[i]], fixations_only = T)[,2]))
    plot(ROB, MFD)
  })
  
  output$pre <- renderPlot({
    PRE <- sapply(1:length(nameVar()), function(i) mean(gazepathInput()[[i]][[6]], na.rm = T))
    MFD <- sapply(1:length(nameVar()), function(i) median(summary(gazepathInput()[[i]], fixations_only = T)[,2]))
    plot(PRE, MFD)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('data', Sys.Date(), '.csv', sep = '') 
    },
    content = function(file) {
      write.csv(outData(), file)
    }
  )
})

