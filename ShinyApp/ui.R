
library(shiny)
JScode <-
  "$(function() {
setTimeout(function(){
$('#myweight').data('ionRangeSlider').update({'values':[0,0.5,1,1.5,2,5,10, 10000]})
}, 10000)})"


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage("2012 French Election", 
             tabPanel( "Candidate-Centered Structure",
                       
                       mainPanel(
                         h4("Plots"),
                         
                         strong("The interactions between the citizen-clusters and candidates."),

                         p("Each dot size shows the average number of comments of a citizen 
                           from ith citizen-group under a post on jth candidate's wall."),
                         
                         p("Each candidate has a corresponding citizen-cluster that mainly comment on
                           their wall, while there is a splitting cluster between the two leading candidates, 
                           Hollande and Sarkozy."),
                         
                         plotOutput("CandstrBallPlot"),
                         downloadButton('downloadCandstrBallPlot', 'Balloon Plot'),
                         
                         p(""),
                         strong("Sizes of the Citizen-clusters"),
                         p("Leading candidates attract larger clusters of citizens to comment on their walls."),
                         plotOutput("CandstrFanPlot"),
                         downloadButton('downloadCandstrFanPlot', 'Sizes of the Citizen-Clusters')
                               
                         )),
             
             tabPanel( "Issue-Centered Structure",
                       #titlePanel("Study the Issue Structure of 2012 French Election Discussion Threads on Facebook"),
                       sidebarLayout(
                         sidebarPanel(
                           
                           tags$head(tags$script(JScode)),
                           sliderInput("myweight",
                                       label = "Weight for text assisted part h: (scaled by 288)",
                                       min = 0, max = 7, value = 7)
                           
                           
                           #h4("Download Results"),
                           #downloadButton('downloadIssuestrPostWordSigTable', 'sigwords in Post-Clusters'),
                           #downloadButton('downloadIssuestrFanWordSigTable', 'sigwords in Citizen-Clusters')
                           
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           
                           textOutput("text1"),
                           h3("Plots"),
                           
                           plotOutput("IssuestrPostcandPlot"),
                           p("Each dot size shows the average number of posts on each candidate-wall."),
                          
                           plotOutput("IssuestrCitiCandPlot"),
                           p("Each dot size shows the average number of comments from each citizen-cluster on each candidate-wall."),
                           
                           #plotOutput("IssuestrBallPlot"),
                           #p("Each dot size shows the average number of comments from each citizen-cluster on each post-cluster."),
                          
                           h3("Keywords and Central Conversations"),
                           h4("Posts"),
                           p("The keywords for post-clusters (sorted by significance from high to low)"),
                           dataTableOutput("IssuePostsigwordTable"),
                           numericInput("threadword_clusid", 
                                        label = ("interested post-cluster to display (1:4)"), 
                                        min = 1, max = 4, value = 3) ,
                           textInput("threadword", label = ("Interested thread-word (from the above table)"), 
                                     value = "religion"),
                           textOutput("text2"),
                           p("You can sort the table by the leverage score of posts or the post-cluster id"),
                           p("The scores (inner product to cluster center) are scaled by 1000 and 
                             digits = 3 (the smallest (in magnitude) number has 3 digits)"),
                          dataTableOutput("IssuestrPostTable"),
                           
                           h4("Citizens"),
                           p("The keywords of citizen-clusters (sorted by significance from high to low)"),
                           dataTableOutput("IssueFansigwordTable"), 
                           numericInput("citizenword_clusid", 
                                        label = ("interested fan-cluster to display (1:4)"), 
                                        min = 1, max = 4, value = 3) ,
                           textInput("citizenword", label = ("Interested citizen-word (from the above table)"), 
                                     value = "coran"),
                           textOutput("text3"),
                           p("You can sort the table by the leverage score of citizens, the citizen-cluster id, or the citizen id"),
                           p("The scores (inner product to cluster center) are scaled by 1000 and digits = 3 (the smallest (in magnitude) number has 3 digits)"),
                          dataTableOutput("IssuestrFanTable")
                           )))
             
                       )
             ))