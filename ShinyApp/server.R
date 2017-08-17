set.seed(1)

library(Matrix); library(irlba)
library("ggplot2"); library(sqldf);library(tm);
library(plyr);library(reshape);library(plotrix)


source('functions_shiny.R')
load('allpostfans.RData')
load('cc_sort.RData')
load("canstr_Bresult9.RData")
load("IssuestrResults.RData")
load("freqwords.RData")
load("wordsplacePCiCi.RData")
load("Allpostcon.RData")



altogether = allpostcon[ncol(allpostfans)+1:nrow(allpostcon),c(2,5,6,4)]

unifan = rownames(allpostfans); allfan = altogether[,2] 
matchfan = match(allfan,unifan)
altogether[,2] = format(matchfan)

ncandidate = 8; nscale = 1; scalescore = 1000

shinyServer(function(input, output) {
  
  createCandstrBallPlot <- reactive({
    main = "Average Number of Comments"
    B = B0result9[[1]];  
    rownames(B) = as.character(1:nrow(B))
    colball = as.factor(rep(1:9,8))
    balloonGgPlot(B, nscale, FALSE, FALSE, "Citizen-Clusters", "Candidate-Walls", main, colball) +
      ggtitle("Average Number of Comments") +
      theme_bw() +
      theme(
        #legend.position = "none",
        axis.text.x = element_text(size=10, face = "bold"),
        axis.text.y = element_text(size=10, face = "bold"),
        axis.title.x = element_text(size=12, face = "bold"),
        axis.title.y = element_text(size=12, face = "bold"),
        title = element_text(size = 14, face = "bold")
      )+
      scale_colour_manual(values = mycolor[1:9],
                          name="Citizen-Clusters",
                          breaks=factor(1:9),
                          labels=mycolor[1:9],
                          guide = "none"
      ) + theme(plot.title = element_text(hjust = 0.5))
    
      
  })
  
  output$CandstrBallPlot <- renderPlot({
    print(createCandstrBallPlot())
  })
  
  
  
  createCandstrFanPlot <- reactive({
    clusters = B0result9[[2]]
    
    labels = 1:length(clusters)
    Candidate = factor(clusters)
    names(Candidate) = NULL
    levels(Candidate) = c(cn_sort,"Split")
    Citizen_cluster = factor(clusters)
    df = data.frame(Candidate,labels,Citizen_cluster)
    
    xlab = "Citizen-Clusters"; ylab = "Sizes"; main = "Sizes of the Citizen-Clusters";
    GgPlotClusSizes(df, Citizen_cluster, 9, xlab, ylab, main) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$CandstrFanPlot <- renderPlot({
    print(createCandstrFanPlot())
  })
  
  
  output$downloadCandstrBallPlot <- downloadHandler(
    filename = "CandstrBallPlot.png",
    content = function(file) {
      ggsave(file,createCandstrBallPlot())
    }
  )
  
  output$downloadCandstrFanPlot <- downloadHandler(
    filename = "CandstrFanPlot.png",
    content = function(file) {
      ggsave(file,createCandstrFanPlot())
    }
  )
  
  output$downloadCandstrPostwallPlot <- downloadHandler(
    filename = "Postwall.png",
    content = function(file) {
      ggsave(file,createPostwallPlot())
    }
  )
  
  
  
  # Issue Structure
  ncluster = 4;
  
  RotatePost = RotateCitizen = list()
  
  for(j in 1:8){RotatePost[[j]] = 1:4; RotateCitizen[[j]] = c(3,4,1,2)}
  
  RotatePost[[5]] = c(1,3,2,4)
  RotatePost[[6]] = c(4,2,3,1)
  #RotatePost[[7]] = c(2,3,4,1)
  RotatePost[[7]] = c(4,1,2,3)
  RotatePost[[8]] = c(3,4,1,2)
  
  RotateCitizen[[8]] = c(3,4,2,1)
  
  for(i in 1:8)
  {
    IssuestrResults[[i]][[3]][[1]] = IssuestrResults[[i]][[3]][[1]][RotateCitizen[[i]],RotatePost[[i]]]
    IssuestrResults[[i]][[3]][[2]] = RotateCitizen[[i]][IssuestrResults[[i]][[3]][[2]]]
    IssuestrResults[[i]][[3]][[3]] = RotatePost[[i]][IssuestrResults[[i]][[3]][[3]]]
    clusternames = colnames(IssuestrResults[[i]][[6]])
    IssuestrResults[[i]][[6]] = IssuestrResults[[i]][[6]][,RotatePost[[i]]]
    IssuestrResults[[i]][[7]] = IssuestrResults[[i]][[7]][,RotateCitizen[[i]]]
    colnames(IssuestrResults[[i]][[6]]) = clusternames
    colnames(IssuestrResults[[i]][[7]]) = clusternames
  }
  
  
  
  output$text1 <- renderText({ 
    i = input$myweight+1;
    if(IssuestrResults[[i]][[1]][2]==1)
      paste("You have selected weight infinity (not 1000000, it is just for displaying convenience)")
    else{
      if(IssuestrResults[[i]][[1]][1]==10)
      paste("You have selected weight", IssuestrResults[[i]][[1]][1], 
              ".  This is the case when h = 0.035 in the paper.")
      else{paste("You have selected weight", IssuestrResults[[i]][[1]][1])}
      }
    
  })
  
  output$text2 <- renderText({ 
    paste("The posts that contains the interested thread-word", input$postword)
  })
  
  output$text3 <- renderText({ 
    paste("The comments that contains the interested citizen-word", input$citizenword)
  })
  
  createIssuestrBallPlot <- reactive({
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    main = "How Citizen-Clusters interact with Post-Clusters"
    #B = Bresult[[1]][RotateCitizen[[i]], ]
    balloonGgPlot(B,1, FALSE, FALSE, "Citizen-Clusters", "Post-Clusters", main, NULL)
    
  })
  
  output$IssuestrBallPlot <- renderPlot({
    createIssuestrBallPlot()
  })
  
  createIssuestrCitiCandPlot <- reactive({
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    kmCitiClus <- Bresult[[2]]
    B <- createB_general_nosort(allpostfans, kmCitiClus, cc_sort)
    colnames(B) <- cn_sort; rownames(B) <- paste( "Cluster", as.character(1:4))
    main = "How Citizen-Clusters interact on Candidate-Walls"
    Brt = B
    #Brt = B[RotateCitizen[[i]],]; rownames(Brt) = rownames(B);
    p = balloonGgPlot(Brt,nscale,FALSE, FALSE, "Citizen-Clusters", "Candidate-Walls", main, NULL)
    p
  })
  
  output$IssuestrCitiCandPlot <- renderPlot({
    createIssuestrCitiCandPlot()
    
  })
  
  createIssuestrPostcandPlot <- reactive({
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    kmPosClus <- Bresult[[3]]
    B <- createB_postcand(kmPosClus,cc_sort, ncluster)
    Ds = diag(1/summary(as.factor(kmPosClus)))
    Dr = diag(1/summary(as.factor(cc_sort)))
    Bn = Ds %*% B %*% Dr
    #Bn = Bn[RotatePost[[i]], ]
    colnames(Bn) <- cn_sort; rownames(Bn) <- paste( "Cluster", as.character(1:4))
    main = "How Post-Clusters distribute on Candidate-Walls"
    p = balloonGgPlot(Bn,nscale,FALSE, FALSE, "Post-Clusters", "Candidate-Walls", main, NULL)
    p
  })
  
  output$IssuestrPostcandPlot <- renderPlot({
    createIssuestrPostcandPlot()
    
  })
  
 
  
  Issuepostword_signif <- reactive({
    i = input$myweight+1;
    postsigword = IssuestrResults[[i]][[6]];
    postsigword
  })
  
  output$IssuePostsigwordTable <- renderDataTable(
    Issuepostword_signif(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20, 25))
  )
  
  Issuefanword_signif <- reactive({
    i = input$myweight+1;
    fansigword = IssuestrResults[[i]][[7]];
    fansigword
  })
  
  output$IssueFansigwordTable <- renderDataTable(
    Issuefanword_signif(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20, 25))
  )
  
  
#  output$downloadIssuestrSvdPlot <- downloadHandler(
#    filename = "IssuestrSvdPlot.png",
#    content = function(file) {
#      ggsave(file,createIssuestrSvdPlot())
#    }
#  )
  
  output$downloadIssuestrBallPlot <- downloadHandler(
    filename = "IssuestrBallPlot.png",
    content = function(file) {
      ggsave(file,createIssuestrBallPlot())
    }
  )
  
  output$downloadIssuestrPostcandPlot <- downloadHandler(
    filename = "IssuestrPostcandPlot.png",
    content = function(file) {
      ggsave(file,createIssuestrPostcandPlot())
    }
  )
  
  
  output$downloadIssuestrPostWordSigTable <- downloadHandler(
    filename = function() { paste('IssuestrPostWordSig', '.csv', sep='') },
    content = function(file) {
      write.csv(Issuepostword_signif(), file)
    }
  )
  
  output$downloadIssuestrFanWordSigTable <- downloadHandler(
    filename = function() { paste('IssuestrFanWordSig', '.csv', sep='') },
    content = function(file) {
      write.csv(Issuefanword_signif(), file)
    }
  )
  
  
  createIssuestrFanTable <- reactive({
    word = input$citizenword
    clusid = input$citizenword_clusid
    wi = match(word, fanfreqwords)
    ci = Ci[[wi]]
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    FanScore = IssuestrResults[[i]][[5]][matchfan];
    kmcluster = Bresult[[2]][matchfan];
    comi = cbind(altogether[ci,],kmcluster[ci],FanScore[ci])
    clusi = which(comi[,5]==clusid)
    comi = comi[clusi,]
    colnames(comi) = c("Candidate-Wall","Citizen-ID","Comment-Text", "Post-Text","Citizen-Cluster","Citizen-Score")
    comiorder = comi[with(comi,order(-comi[,6])),]
    display = comiorder[1:min(input$issuemaxrows,nrow(comi)),]
    display[,6] = format(display[,6]*scalescore, digits = 3)
    display
  })
  
  output$IssuestrFanTable <- renderDataTable(
    createIssuestrFanTable(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20))
  )
  
  createIssuestrPostTable <- reactive({
    word = input$threadword
    clusid = input$threadword_clusid
    wi = match(word, postfreqwords)
    pci = PCi[[wi]]
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    PostScore = IssuestrResults[[i]][[4]][matchpost];
    kmcluster = Bresult[[3]][matchpost];
    comi = cbind(allpostcon[pci,-3],kmcluster[pci],PostScore[pci])
    clusi = which(comi[,(ncol(comi)-1)]==clusid)
    comi = comi[clusi,]
    colnames(comi) = c("Source","Candidate-Wall", "Post-Text","Citizen-ID","Comment-Text","Post-Cluster","Post-Score")
    comiorder = comi[with(comi,order(-comi[,ncol(comi)])),]
    display = comiorder[1:min(input$issuemaxrows,nrow(comi)),]
    display[,ncol(comi)] = format(display[,ncol(comi)]*scalescore, digits = 3)
    display
  })
  
  output$IssuestrPostTable <- renderDataTable(
    createIssuestrPostTable(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20))
  )
  
  output$downloadIssuestrPostWord <- downloadHandler(
    filename = function() { paste('IssuestrPostWord', '.csv', sep='') },
    content = function(file) {
      write.csv(createIssuestrPostTable(), file)
    }
  )
  
  output$downloadIssuestrFanWord <- downloadHandler(
    filename = function() { paste('IssuestrFanWord', '.csv', sep='') },
    content = function(file) {
      write.csv(createIssuestrFanTable(), file)
    }
  )
  
})
