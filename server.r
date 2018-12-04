library(shiny)
library(shinythemes)
library(ade4)
library(adegraphics)
library(DT)

shinyServer(function(input, output, session) {

### ===================  uploading data into shiny web environment====================###

###>>>>> R-table upload system
          
rMatrix <- reactive({

  inFileR <- input$fileR
    if (is.null(inFileR))
      return(NULL)
      rMat<-read.csv(inFileR$datapath, header=input$header, sep=input$sep,
                  quote = input$quote,row.names = 1,
                  stringsAsFactors = TRUE)
                  #check if the table is inserted
    rMat
    })

## display the R table 
          
output$rTab<-DT::renderDataTable({rMatrix()},options=list(pageLength=10,
                                                                searching=FALSE),
                                                                rownames=TRUE)
        
### link to table R tab 

observe({
  if ((input$showR>0)&(!is.null(rMatrix())))
    updateTabsetPanel(session, "upload", selected = "R table")
  else updateTabsetPanel(session, "upload", selected = "Configuration")
})
        
###>>>>> L-table upload system 
    
      lMatrix <- reactive({
            inFileL <- input$fileL
              if (is.null(inFileL))
                return(NULL)
            lMat<-read.csv(inFileL$datapath, header=input$header, sep=input$sep,
                          quote = input$quote,row.names = 1,
                          stringsAsFactors = TRUE)
            lMat
          })
  ## display the L table
      
  output$lTab<-DT::renderDataTable({lMatrix()},options=list(pageLength=10,
                                                            searching=FALSE),
                                                            rownames=TRUE)
  
  ## link to table L tab
  
  observe({
    if ((input$showL>0)&(!is.null(lMatrix())))
      updateTabsetPanel(session, "upload", selected = "L table")
    else updateTabsetPanel(session, "upload", selected = "Configuration")
  })
  

### >>>>> Q-table upload system                   
                    
qMatrix<- reactive({
      inFileQ <- input$fileQ
      if (is.null(inFileQ))
      return(NULL)
      qMat<-read.csv(inFileQ$datapath, header=input$header, sep=input$sep,
                    quote = input$quote,row.names = 1,
                    stringsAsFactors = TRUE)
      qMat
    })
            
## display the q table 
    
    output$qTab<-renderDataTable({qMatrix()},options=list(pageLength=10,
                                                          searching=FALSE),
                                                          rownames=TRUE)

## link to table Q tab
    
    observe({
      if ((input$showQ>0)&(!is.null(qMatrix())))
        updateTabsetPanel(session, "upload", selected = "Q table")
      else updateTabsetPanel(session, "upload", selected = "Configuration")
    })
            
### ==========================  Pre-test the data ===============================###
  
##>>>>> L pretest

lPreTest<- reactive({

  req({lMatrix()})
  
  ltes<-dudi.coa(lMatrix(),scannf = FALSE)
  ltes     
})

  ## text message when L table is uploaded and tested

output$textTL<-renderText({
req({lPreTest()})
"L table is ready!"
  }) 

output$lw_table<-renderPrint({
lw<-lPreTest()$lw
  return(print(lw))
  }) # ---- > remove this part (or make it a comment)

output$cw_table<-renderPrint({
  cw<-lPreTest()$cw
              
  return(print(cw))
  })
      
##>>>>> R pretest
rPreTest<- reactive({
  
  l_tab<-lPreTest()
  lweig<-l_tab$lw
  
  req(lweig)

  Rtes<-if (input$Rcat==TRUE)
    {dudi.hillsmith(rMatrix(),row.w=lweig,scannf= FALSE)}
      else {dudi.pca(rMatrix(),row.w =lweig, scannf=FALSE)}
  Rtes
})

## text message when R table is uploaded and tested    

output$textTR<-renderText({

rPreTest<-rPreTest()
req(rPreTest)
  "R table is ready!"
})    

##>>>>> Q pretest  

qPreTest<- reactive({

  l_tab<-lPreTest()
  cweig<-l_tab$cw
  
  req(cweig)

    qtes<- if (input$Qcat==TRUE) 
    {dudi.hillsmith(qMatrix(),row.w=cweig,scannf = FALSE)}
      else {dudi.pca(qMatrix(),row.w=cweig, scannf= FALSE)}
        qtes
}) 

## text message when Q table is uploaded and tested    

output$textTQ<-renderText({

qPreTest<-qPreTest()

  req(qPreTest)

      "Q table is ready!"
})  
### ========================== running the model ===============================### 
  
  RLQ_model<-reactive({
      rlqModel<-if((input$run>0)&(!is.null(lPreTest())))
      {rlq(rPreTest(),lPreTest(),qPreTest(),scannf = FALSE)}
       rlqModel    
  })

  output$textCompleted<-renderText({
      req(RLQ_model())
      "Analysis completed"
      })


### ========================== plot the model ===============================### 
      
      
    output$biplotQ<-renderPlot({
      
      RLQ<-RLQ_model()        
      s.label(RLQ$lQ,plot=TRUE,storeData = FALSE, add=FALSE)})
    
    output$biplotR<-renderPlot({
      
      RLQ<-RLQ_model()
      s.label(RLQ$lR)
      
    })
    
    output$RAxes<- renderPlot({
      
      s.arrow(RLQ_model()$l1,plot=TRUE)})

    
    output$QAxes<- renderPlot({
      
      s.arrow(RLQ_model()$c1,plot=TRUE)
      
    })     

    output$EigValues<- renderPlot({
      
      plotEig(RLQ_model()$eig,plot=TRUE)
      
    })       
        
    output$summary<-renderPrint({
        
      
        summary(RLQ_model())
        
        })
  


