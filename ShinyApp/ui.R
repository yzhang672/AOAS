
library(shiny)
JScode <-
  "$(function() {
setTimeout(function(){
$('#myweight').data('ionRangeSlider').update({'values':[0,0.5,1,1.5,2,5,10,100]})
}, 100)})"


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage("2012 French Election", 
             tabPanel( "Candidate Structure",
                       
                       mainPanel(
                         h4("Plots"),
                         
                         strong("The Balloon Plot of Stochastic Block matrix."),

                         p("The sizes of dots show the average number of comments of a citizen from ith citizen-group under a post on jth candidate's wall. 
                           The sizes are after transformation log(1000x+1)."),
                         
                         plotOutput("CandstrBallPlot"),
                         downloadButton('downloadCandstrBallPlot', 'Balloon Plot'),
                         
                         p(""),
                         strong("Sizes of the Citizen-clusters"),
                         plotOutput("CandstrFanPlot"),
                         downloadButton('downloadCandstrFanPlot', 'Citizen Sizes')
                         #,
                         
                        # h4("Tables"),
                        # numericInput("clusid", 
                        #             label = ("interested fan-cluster to display (1:9)"), 
                        #            min = 1, max = 9, value = 9) ,
                        #numericInput("maxrows", 
                        #             label = ("max number of rows to display"), 
                        #             value = 100000) ,
                        #p("interested fan cluster (including fan id's, their comments, and under which posts they commented)"),
                        #p("You can sort the table by the leverage score of fans or the fan id"),
                        #p("The scores (inner product to cluster center) are scaled by 1000 and digits = 3 (the smallest (in magnitude) number has 3 digits)"),
                        #dataTableOutput("FanClustersTable")
                         
                         )),
             
             tabPanel( "Issue Structure",
                       #titlePanel("Study the Issue Structure of 2012 French Election Discussion Threads on Facebook"),
                       sidebarLayout(
                         sidebarPanel(
                           
                           tags$head(tags$script(JScode)),
                           sliderInput("myweight",
                                       label = "Weight for text assisted part h:",
                                       min = 0, max = 7, value = 0),
                           numericInput("nsigword", 
                                        label = ("Max Number of significant words displayed"), 
                                        value = 100000),
                           numericInput("issuemaxrows", 
                                        label = ("Issue Structure max number of rows to display"), 
                                        value = 100000) ,
                           
                           
                           h4("Download Results"),
                           downloadButton('downloadIssuestrBallPlot', 'Balloon Plot'),
                           downloadButton('downloadIssuestrPostcandPlot', 'Post by Candidate Plot'),
                           downloadButton('downloadIssuestrPostWordSigTable', 'sigwords in Post Clusters'),
                           downloadButton('downloadIssuestrFanWordSigTable', 'sigwords in Fan Clusters')
                           
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           
                           textOutput("text1"),
                           h3("Plots"),
                           
                           strong("The Number of Posts on each Candidate's Wall"),
                           plotOutput("IssuestrPostcandPlot"),
                           
                           strong("The Average Number of Comments from each Citizen-cluster on each Candidate's Wall"),
                           plotOutput("IssuestrCitiCandPlot"),
                           
                           strong("The Balloon Plot of Stochastic Block Matrix"),
                           plotOutput("IssuestrBallPlot"),
                           
                           strong("Call-Response Matrix"),
                           plotOutput("CallResponsePlot"),
                           
                          
                           h3("Tables"),
                           h4("Posts"),
                           p("the significant words of Post Clusters (sorted by significance from high to low)"),
                           dataTableOutput("IssuePostsigwordTable"),
                           numericInput("postword_clusid", 
                                        label = ("interested post-cluster to display (1:4)"), 
                                        min = 1, max = 4, value = 3) ,
                           textInput("postword", label = ("Interested post-word (from the above table)"), 
                                     value = "coran"),
                           textOutput("text2"),
                           p("You can sort the table by the leverage score of posts or the post-cluster id"),
                           p("The scores (inner product to cluster center) are scaled by 1000 and digits = 3 (the smallest (in magnitude) number has 3 digits)"),
                          dataTableOutput("IssuestrPostTable"),
                           
                           h4("Fans"),
                           p("the significant words of Fan Clusters (sorted by significance from high to low)"),
                           dataTableOutput("IssueFansigwordTable"), 
                           numericInput("fanword_clusid", 
                                        label = ("interested fan-cluster to display (1:4)"), 
                                        min = 1, max = 4, value = 2) ,
                           textInput("fanword", label = ("Interested fan-word (from the above table)"), 
                                     value = "fdg"),
                           textOutput("text3"),
                           p("You can sort the table by the leverage score of fans, the fan-cluster id, or the fan id"),
                           p("The scores (inner product to cluster center) are scaled by 1000 and digits = 3 (the smallest (in magnitude) number has 3 digits)"),
                          dataTableOutput("IssuestrFanTable")
                           )))
             
                       )
             ))