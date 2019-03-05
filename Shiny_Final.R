#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###                        SHINY AP                                   ###
#########################################################################

if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("shinyWidgets")) install.packages("shinyWidgets"); library("shinyWidgets")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("png")) install.packages("png"); library("png")

basetable_engagement <- read.csv("./basetable_topic_SA_keyword.csv", stringsAsFactors = F)
basetable_engagement$screen_name <- as.factor(basetable_engagement$screen_name)  
basetable_engagement$sentiment_analysis <- round(basetable_engagement$sentiment_analysis,2)
basetable_engagement$created_at <- as.Date(basetable_engagement$created_at)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Darken hr to make the horizontal line visible
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #003366;}"))
  ),
  
  # Application title
  titlePanel("Online Behaviour in the Sports Apparel Industry"),
  
  # Sidebar with filters to select brand or date range 
  sidebarLayout(
    sidebarPanel(width = 4,
                 
                 wellPanel(
                   
                   h4("Filter Tweets"),
      
      
      pickerInput("Brand","Select the Brand Name:",
                  choices = levels(basetable_engagement$screen_name),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),
                  selected = unique(basetable_engagement$screen_name)),
      
      dateRangeInput("Daterange", "Select Date Range:",
                     start  = "2017-01-01",
                     end    = "2019-02-28",
                     min    = "2017-01-01",
                     max    = "2019-02-28",
                     format = "yyyy-mm-dd",
                     separator = "/"))
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id= "tabsetpanel", 
                  tabPanel(title = "Tweets Metrics",
                           splitLayout(cellWidths = c("50%","50%"),plotOutput("likes"),plotOutput("retweet")),
                           h6("*Note: For Visualization purpose, only tweets with less than 1000 likes/retweets are showed."),
                           plotOutput("sentiment")),
                  tabPanel(title = "Time Metrics ",
                           splitLayout(cellWidths = c("50%","50%"),plotOutput("day"),plotOutput("hour")), plotOutput("month")),
                  tabPanel(title = "Key Words & Topics",
                           plotOutput("key_word"),plotOutput("topic"),
                           br(),
                           h5("Betas values of top 10 words within each of the 5 Topics.", align = "center"),
                           br(),
                           imageOutput("mytopic")),
                  tabPanel(title = "Evaluation Likes",
                           h3("AUC Scores for Predicting Likes Using Logistic Regression and Random Forest",align = "center"),
                           br(),
                           p("This charts shows that the logistic target favorites normalized has the highest test AUC and lowest gap between test and train datasets. 
                              This shows that it has the highest predictive power out the four model and minimal overfitting."),
                           tableOutput("auc_fav"),
                           h3("F1 Score for Logistic Regression and Random Forest",align = "center"),
                           br(),
                           p("Low target beacuse while the recall is very high, the precision is low due to many FP which leads to a low F1 score.
                              The normalized target have more balance recall and precision matrix leading to good F1 scores."),
                           tableOutput("F1_fav"),
                           br(),
                           h3("Confusion Matrix for Logistic Regression and Random Forest",align = "center"),
                           br(),
                           tableOutput("CM_fav"),
                           h3("AUC Curve and Predictors Variables",align = "center"),
                           p("Based on the AUC and F1 scores of the various models, the logistic regression using the normalized target has the highest predictive power. and classification accuracy.
                              As a result, we looked at the AUC for that model to determine the number of variables to include in the stepwise logistic regression.
                              Based on this model, we chose 7 variables. If the brand's tweet is a retweet, has an url or a question, it results in low engagement.
                              The is_retweet variable is more impactful on the engagement because of its large weight.
                              On the other hand, existence of videos, photos, hashtags, and large display_text_width are indicators
                              of high engagement"),
                           br(),
                           splitLayout(cellWidths = c("70%","50%"),imageOutput("AUC_LI"),tableOutput("coeff_fav"))),
                  tabPanel(title = "Evaluation Retweets",
                           h3("AUC Scores for Predicting Retweets Using Logistic Regression and Random Forest",align = "center"),
                           br(),
                           p("This charts shows that the logistic target retweets has the highest test AUC and lowest gap between test and train datasets. 
                              This shows that it has the highest predictive power out the four model and minimal overfitting."),
                           tableOutput("auc_rt"),
                           h3("F1 Score for Logistic Regression and Random Forest",align = "center"),
                           br(),
                           p("There is a low target because the recall is very high and the precision is low due to many False Positive which leads to a low F1 score.
                              The normalized target have once again more balance recall and precision matrix leading to better F1 scores."),
                           tableOutput("F1_rt"),
                           br(),
                           h3("Confusion Matrix for Logistic Regression and Random Forest",align = "center"),
                           br(),
                           tableOutput("CM_rt"),
                           h3("AUC Curve and Predictors Variables",align = "center"),
                           p("Based on the AUC and F1 scores of the various models, the logistic regression using the normalized target performs better as
                              it combines high predictive power and classification accuracy.
                              As a result, we looked at the AUC for that model to determine the number of variables to include in the stepwise logistic regression.
                              Based on this model, we chose 6 variables. The presence of an url or a question mark is negatively related to high engagement.
                            On the contrary, if it is a retweet, has a video, a photo or a hashtag, it is positively related to high engagement.  "),
                           br(),
                           splitLayout(cellWidths = c("70%","50%"),imageOutput("AUC_rt"),tableOutput("coeff_rt")),
                           br())
                  
                  
      )
    ),
    fluid = FALSE
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$auc_fav <- renderTable({
    auc_likes <- read.csv("./AUC_fav.csv", stringsAsFactors = F)   
    
    
  })
  output$CM_fav <- renderTable({
    CM_likes <- read.csv("./CM_fav.csv", stringsAsFactors = F)   
    
    
  })
  
  output$F1_fav <- renderTable({
    F1_likes <- read.csv("./F1_fav.csv", stringsAsFactors = F)   
    
    
  })
  
  output$AUC_LI <- renderImage({
    
    return(list(src='AUC_norm_fav.png', contentType = 'image/png'))
    
  }, deleteFile = FALSE)
  

  output$coeff_fav <- renderTable({
    F1_likes <- read.csv("./model_fav_coefficients.csv", stringsAsFactors = F)   
    
    
  })
  
  
  output$auc_rt <- renderTable({
    auc_RTs <- read.csv("./AUC_RT.csv", stringsAsFactors = F)   
    
    
  })
  output$CM_rt <- renderTable({
    CM_RTs <- read.csv("./CM_RT.csv", stringsAsFactors = F)   
    
    
  })
  
  output$F1_rt <- renderTable({
    F1_RTs <- read.csv("./F1_RT.csv", stringsAsFactors = F)   
    
    
  })
  
  output$AUC_rt <- renderImage({
    
    return(list(src='AUC_RT_norm.png', contentType = 'image/png'))
    
  }, deleteFile = FALSE)
  
  output$coeff_rt <- renderTable({
    F1_likes <- read.csv("./model_RT_coefficients.csv", stringsAsFactors = F)   
    
    
  })
  
  
  
  
  output$mytopic <- renderImage({
    
    return(list(src='TopicWords.png', contentType = 'image/png'))
    
  }, deleteFile = FALSE)
  
      #plot for the distribution of number of likes per tweets
      output$likes <- renderPlot({
      sd <- mean(basetable_engagement$favorite_count) + 2*sd(basetable_engagement$favorite_count)
      basetable_engagement$favorite_count_s <- basetable_engagement$favorite_count
      basetable_engagement$favorite_count_s<- ifelse(basetable_engagement$favorite_count_s >sd, sd, basetable_engagement$favorite_count_s) 
      basetable_engagement_fav <- basetable_engagement[basetable_engagement$favorite_count_s < 1000,]
        
      myData <- basetable_engagement_fav[basetable_engagement_fav$created_at >= input$Daterange[1] & basetable_engagement_fav$created_at <= input$Daterange[2], ]
      data <- subset(myData, screen_name %in% input$Brand)
      
      hist(data$favorite_count_s,
           xlab = "Number of Likes", 
           ylab = "Number of Tweets Posted", 
           col = "light blue",
           main = "Distribution of Likes")   
     

       
    
  })
      #plot for the distribution of number of retweet per tweets
      output$retweet <- renderPlot({
        sd <- mean(basetable_engagement$retweet_count) + 2*sd(basetable_engagement$retweet_count)
        basetable_engagement$retweet_count_s <- basetable_engagement$retweet_count
        basetable_engagement$retweet_count_s<- ifelse(basetable_engagement$retweet_count_s >sd, sd, basetable_engagement$retweet_count_s) 
        basetable_engagement_ret <- basetable_engagement[basetable_engagement$retweet_count_s < 1000,]
        
        myData <- basetable_engagement_ret[basetable_engagement_ret$created_at >= input$Daterange[1] & basetable_engagement_ret$created_at <= input$Daterange[2], ]
        data <- subset(myData, screen_name %in% input$Brand)
        
        hist(data$retweet_count_s,
             xlab = "Number of Retweets", 
             ylab = "Number of Tweets Posted", 
             col = "light blue",
             main = "Distribution of Retweets")   

        
      })
      #plot for the distribution of sentiment scores per tweets
      output$sentiment <- renderPlot({
        
        myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
        data <- subset(myData, screen_name %in% input$Brand)
      
      hist(data$sentiment_analysis,
           xlab = "Sentiments Scores", 
           ylab = "Number of Tweets Posted", 
           col = "light blue",
           main = "Distribution of Sentiments Score")     
      
      })
      
       #plot for the preferred day to post tweets
        output$day <- renderPlot({
          myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
          data <- subset(myData, screen_name %in% input$Brand)    
         
          Monday_P <- sum(data$WD == "Monday", na.rm=TRUE)
          Tuesday_P <- sum(data$WD == "Tuesday", na.rm=TRUE)
          Wednesday_P <- sum(data$WD == "Wednesday", na.rm=TRUE)
          Thursday_P <- sum(data$WD == "Thursday", na.rm=TRUE)
          Friday_P <- sum(data$WD == "Friday", na.rm=TRUE)
          Saturday_P <-sum(data$WD == "Saturday", na.rm=TRUE)
          Sunday_P <- sum(data$WD == "Sunday", na.rm=TRUE)
          
          Day_P <- c(Monday_P,Tuesday_P,Wednesday_P,Thursday_P,Friday_P,Saturday_P,Sunday_P)
          names(Day_P) <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
          D_Plot <- barplot(Day_P, 
                         names.arg = names(Day_P), 
                         xlab = " ", 
                         ylab = "Number of Tweets Posted", 
                         col = blues9[7:1],
                         main = "Distribution of Days")
          text(x = D_Plot, y = Day_P, label = Day_P ,pos = 1, cex =1 , col = "black")
          
          
        })
        #plot for the preferred months to post tweets
        output$month <- renderPlot({
          myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
          data <- subset(myData, screen_name %in% input$Brand)     
          
          Jan <- sum(data$month == 'January', na.rm = TRUE)
          Feb <- sum(data$month == 'February', na.rm = TRUE)
          Mar <- sum(data$month == 'March', na.rm = TRUE)
          April <- sum(data$month == 'April', na.rm = TRUE)
          May <- sum(data$month == 'May', na.rm = TRUE)
          June <- sum(data$month == 'June', na.rm = TRUE)
          July <- sum(data$month == 'July', na.rm = TRUE)
          Aug <- sum(data$month == 'August', na.rm = TRUE)
          Sep <- sum(data$month == 'September', na.rm = TRUE)
          Oct <- sum(data$month == 'October', na.rm = TRUE)
          Nov <- sum(data$month == 'November', na.rm = TRUE)
          Dec <- sum(data$month == 'December', na.rm = TRUE)
          
          Month_P <- c(Jan, Feb, Mar, April, May, June, July, Aug,Sep,Oct,Nov,Dec)
          names(Month_P) <- c("January", "February", "March", "April","May","June","July","August","September","October","November","December")
          M_Plot <- barplot(Month_P, 
                         names.arg = names(Month_P), 
                         xlab = " ", 
                         ylab = "Number of Tweets Posted", 
                         col = blues9[7:1],
                         main = "Distribution of Months")
          text(x = M_Plot, y = Month_P, label = Month_P ,pos = 1, cex =1 , col = "black")
          
          
        })
      
        #plot for the preferred time of the day to post tweets
        output$hour <- renderPlot({
          myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
          data <- subset(myData, screen_name %in% input$Brand)    
          
          Morning_P <- sum(data$morning == 1 , na.rm=TRUE)
          Afternoon_P <- sum(data$afternoon == 1, na.rm=TRUE)
          Night_P <- sum(data$morning == 0 & data$afternoon == 0, na.rm=TRUE)
          
          
          Hour_P <- c(Morning_P,Afternoon_P,Night_P)
          names(Hour_P) <- c("Morning", "Afternoon", "Night")
          H_Plot <- barplot(Hour_P, 
                            names.arg = names(Hour_P), 
                            xlab = " ", 
                            ylab = "Number of Tweets Posted", 
                            col = blues9[7:1],
                            main = "Distribution of Time")
          text(x = H_Plot, y = Hour_P, label = Hour_P ,pos = 1, cex =1 , col = "black")
          
          
        })
        
        #plot for distribution of number of keywords per tweets
        output$key_word <- renderPlot({
          myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
          data <- subset(myData, screen_name %in% input$Brand)  
          
          K_0 <- sum(data$keyword_count == 0, na.rm = TRUE)
          K_1 <- sum(data$keyword_count == 1, na.rm = TRUE)
          K_2 <- sum(data$keyword_count == 2, na.rm = TRUE)
          K_3 <- sum(data$keyword_count == 3, na.rm = TRUE)
          K_4 <- sum(data$keyword_count == 4, na.rm = TRUE)
          K_5 <- sum(data$keyword_count == 5, na.rm = TRUE)
          K_P <- c(K_0,K_1,K_2,K_3,K_4,K_5)
          names(K_P) <- c("0","1","2","3","4","5")
          k_plot <- barplot(K_P,
                            names.arg = names(K_P), 
                            xlab = "Number of Keywords", 
                            ylab = "Number of Tweets Posted", 
                            col = blues9[7:1],
                            main = "Distribution of Keywords",
                            ylim= c(0,1500))
          text(x = k_plot, y = K_P, label = K_P ,pos = 3, Cas = 3, cex =1 , col = "black")
          
        })
        #plot for distribution of number of topics per tweets
        output$topic <- renderPlot({
          myData <- basetable_engagement[basetable_engagement$created_at >= input$Daterange[1] & basetable_engagement$created_at <= input$Daterange[2], ]
          data <- subset(myData, screen_name %in% input$Brand)  
          
          T_1 <- sum(data$topic_1 == 1, na.rm = TRUE)
          T_2 <- sum(data$topic_2 == 1, na.rm = TRUE)
          T_3 <- sum(data$topic_3 == 1, na.rm = TRUE)
          T_4 <- sum(data$topic_4 == 1, na.rm = TRUE)
          T_5 <- sum(data$topic_5 == 1, na.rm = TRUE)
          
          T_P <- c(T_1,T_2,T_3,T_4,T_5)
          names(T_P) <- c("Topic 1: Athletes","Topic 2: GameDay","Topic 3: Equipments","Topic 4: Inspirational","Topic 5: Workouts")
          T_plot <- barplot(T_P,
                            names.arg = names(T_P), 
                            xlab = "", 
                            ylab = "Number of Tweets Posted", 
                            col = blues9[7:1],
                            main = "Distribution of Topics",
                            ylim= c(0,900))
          text(x = T_plot, y = T_P, label = T_P ,pos = 3,Cas = 3, cex =1 , col = "black")
          
        })
        
        
}


# Run the application 
shinyApp(ui = ui, server = server)