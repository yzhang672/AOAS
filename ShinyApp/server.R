set.seed(1)

library(Matrix); library(irlba)
library("ggplot2"); library(sqldf);library(tm);
library(plyr);library(reshape);library(plotrix)


source('functions_shiny.R')
load('allpostfans.RData')
load('cc.RData')
load("canstr_Bresult9.RData")
load("IssuestrResults.RData")
load("freqwords.RData")
load("wordsplacePCiCi.RData")
load("Allpostcon.RData")
load("CallResMat.RData")

cc_sort = cc
for(i in 1:8)
{
  ci = which(cc==i)
  cc_sort[ci] = cn_match[i]
}

altogether = allpostcon[ncol(allpostfans)+1:nrow(allpostcon),c(2,5,6,4)]

unifan = rownames(allpostfans); allfan = altogether[,2] 
matchfan = match(allfan,unifan)
altogether[,2] = format(matchfan)

ncandidate = 8; nscale = 1000; scalescore = 1000

shinyServer(function(input, output) {
  
  createCandstrBallPlot <- reactive({
    main = "Average Number of Comments"
    B = B0result9[[1]];  
    log(B*nscale+1)
    rownames(B) = as.character(1:nrow(B))
    colball = as.factor(rep(1:9,8))
    balloonGgPlot(B, nscale, TRUE, FALSE, "Citizen-clusters", "Candidates' Walls", main, colball) +
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
                          name="Citizen-clusters",
                          breaks=factor(1:9),
                          labels=mycolor[1:9],
                          guide = "none"
      ) 
    
      
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
    GgPlotClusSizes(df, Citizen_cluster, 9, xlab, ylab, main)
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
  
  output$text1 <- renderText({ 
    i = input$myweight+1;
    if(IssuestrResults[[i]][[1]][2]==0)
    paste("You have selected weight", IssuestrResults[[i]][[1]][1])
    else{paste("You have selected weight infinity (not 100, 100 is just for displaying convenience)")}
  })
  
  output$text2 <- renderText({ 
    paste("The posts that contains the interested post-word", input$postword)
  })
  
  output$text3 <- renderText({ 
    paste("The comments that contains the interested fan-word", input$fanword)
  })
  
  createIssuestrBallPlot <- reactive({
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    main = "Fan cluster by Post cluster"
    # no need to nscale here since already scaled in creating B
    balloonGgPlot(Bresult[[1]],1, TRUE, FALSE, "Fan Clusters", "Post Clusters", main, NULL)
    
  })
  
  output$IssuestrBallPlot <- renderPlot({
    createIssuestrBallPlot()
  })
  
  createIssuestrCitiCandPlot <- reactive({
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    kmCitiClus <- Bresult[[2]]
    B <- createB_general_nosort(allpostfans, kmCitiClus, cc_sort)
    main = "Citizen Cluster by Candidates"
    p = balloonGgPlot(B,nscale,TRUE, FALSE, "Candidates", "Post Clusters", main, NULL)
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
    main = "Post Cluster by Candidates"
    p = balloonGgPlot(t(B),nscale,TRUE, FALSE, "Candidates", "Post Clusters", main, NULL)
    p
  })
  
  output$IssuestrPostcandPlot <- renderPlot({
    createIssuestrPostcandPlot()
    
  })
  
  createCallResponsePlot <- reactive({
    Mat.m <- melt(as.matrix(Mat))
    colnames(Mat.m) = c("Citizen_Words", "Post_Words", "Value")
    p <- ggplot(Mat.m, aes(Post_Words,Citizen_Words)) + 
      geom_tile(aes(fill = Value),colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") +
      ggtitle("(Part of) Call-Response Matrix") +
      xlab("Post-Words") + ylab("Citizen-Words")+
      theme(
        axis.text.x = element_text(size=10, face = "bold",angle = 80),
        axis.text.y = element_text(size=10, face = "bold"),
        axis.title.x = element_text(size=12, face = "bold"),
        axis.title.y = element_text(size=12, face = "bold"),
        title = element_text(size = 12, face = "bold")
      )
    p  
  })
  
  output$CallResponsePlot <- renderPlot({
    createCallResponsePlot()
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
  
  output$downloadIssuestrFanPlot <- downloadHandler(
    filename = "IssuestrFanPlot.png",
    content = function(file) {
      ggsave(file,createIssuestrFanPlot())
    }
  )
  
  output$downloadIssuestrPostPlot <- downloadHandler(
    filename = "IssuestrPostPlot.png",
    content = function(file) {
      ggsave(file,createIssuestrPostPlot())
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
    word = input$fanword
    clusid = input$fanword_clusid
    wi = match(word, fanfreqwords)
    ci = Ci[[wi]]
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    FanScore = IssuestrResults[[i]][[5]][matchfan];
    kmcluster = Bresult[[2]][matchfan];
    comi = cbind(altogether[ci,],kmcluster[ci],FanScore[ci])
    clusi = which(comi[,5]==clusid)
    comi = comi[clusi,]
    colnames(comi) = c("candidate wall","fan id","comment text", "post text","fan cluster","score of fans")
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
    word = input$postword
    clusid = input$postword_clusid
    wi = match(word, postfreqwords)
    pci = PCi[[wi]]
    i = input$myweight+1;
    Bresult = IssuestrResults[[i]][[3]];
    PostScore = IssuestrResults[[i]][[4]][matchpost];
    kmcluster = Bresult[[3]][matchpost];
    comi = cbind(allpostcon[pci,-3],kmcluster[pci],PostScore[pci])
    clusi = which(comi[,(ncol(comi)-1)]==clusid)
    comi = comi[clusi,]
    colnames(comi) = c("source","candidate wall", "post text","fan id","comment text","post cluster","score of posts")
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