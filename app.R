# LIBRARIES ========================================================================================================================

library(ggplot2)
library(reshape)
library(scales)
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(ggthemes)
library(RColorBrewer)
library(highcharter)
library(quantmod)
library(dtplyr)
library(data.table)
library(readr)
library(lubridate)
library(MASS)
library(fitdistrplus)
# library(jsonlite)

# DATA LOAD ========================================================================================================================

options(scipen = 999)

# LinkData <- read.csv("data/LinkData.csv", header = TRUE, stringsAsFactors = FALSE)
# VideoData <- read.csv("data/VideoData.csv", header = TRUE, stringsAsFactors = FALSE)
# PhotoData <- read.csv("data/PhotoData.csv", header = TRUE, stringsAsFactors = FALSE)

library(RMySQL)
mydb = dbConnect(MySQL(), host = "104.198.210.36", user = "root", password = "tacozombies54992", db = "analytics")

rs <- dbSendQuery(mydb, "select * from PAGE_DATA where from_name = 'We are mitu'")
WamPageData <- fetch(rs, -1)

# rs <- dbSendQuery(mydb, "select * from FEED_DATA")
# WamFeedData <- fetch(rs, -1)
# 
# rs <- dbSendQuery(mydb, "select * from POST_DATA")
# WamPostData <- fetch(rs, -1)
# 
# rs <- dbSendQuery(mydb, "select * from CONTENT_TRACK")
# ContentTrackData <- fetch(rs, -1)
# 
# rs <- dbSendQuery(mydb, "select * from EDITORIAL_AUTHOR")
# EditorialData <- fetch(rs, -1)

dbClearResult(rs)
dbDisconnect(mydb)

WamPageData$date <- as.Date(WamPageData$date)
# WamFeedData$date <- as.Date(WamFeedData$date)
# WamPostData$date <- as.Date(WamPostData$date)

# Data <- merge(WamFeedData, WamPostData, by = "status_id", all.x = TRUE)


load("data/DataArticles.Rda")
load("data/DataVideos.Rda")
load("data/DataPhotos.Rda")
load("data/PostData.Rda")

DataGoals <- read.csv("data/WAM KPIs 2017.csv", header = TRUE, stringsAsFactors = FALSE)
DataGoals$date <- strptime(DataGoals$date, "%d/%m/%Y")

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# Data <- Data[!duplicated(Data),]
# Data$created_time <- as.POSIXct(strptime(Data$created_time, "%Y-%m-%d %H:%M"), tz = "GMT")
# # Data$date = strptime(Data$date, "%d/%m/%Y")
# Data$date <- as.Date(Data$date)
# # Data[Data$sharetext == "",]$sharetext <- "No Share Text"
# Data[Data$sharetext == "",]$sharetext <- as.character(Data[Data$sharetext == "",]$status_id)
# Data[Data$headline == "",]$headline <- as.character(Data[Data$headline == "",]$status_id)
# Encoding(Data$sharetext) <- "latin1"
# Encoding(Data$headline) <- "latin1"
# Data$total_interactions <- Data$total_comments+Data$total_likes + Data$total_shares
# Data$interaction_rate <- (Data$total_comments+Data$total_likes + Data$total_shares)/Data$post_reach
# Data$ctr <- Data$link_clicks/Data$post_reach
# Data$views_rate <- Data$post_video_views/Data$post_reach
# Data$viral_fan_rate <- Data$post_reach_viral/Data$post_reach_fan
# Data$share_rate <- Data$total_shares/(Data$total_comments + Data$total_likes + Data$total_shares)
# Data$total_reactions <- (Data$feed_likes + Data$love + Data$wow + Data$haha + Data$sad + Data$angry)
# Data$feed_likes_rate <- round(Data$feed_likes/Data$total_reactions, 4)
# Data$love_rate <- round(Data$love/Data$total_reactions, 4)
# Data$wow_rate <- round(Data$wow/Data$total_reactions, 4)
# Data$haha_rate <- round(Data$haha/Data$total_reactions, 4)
# Data$sad_rate <- round(Data$sad/Data$total_reactions, 4)
# Data$angry_rate <- round(Data$angry/Data$total_reactions, 4)
# Data$viral_rate <- (Data$post_reach_viral/(Data$post_reach_fan + Data$post_reach_viral))
# Data$fan_rate <- (Data$post_reach_fan/(Data$post_reach_fan + Data$post_reach_viral))
# Data$post_image <- paste("<img src ='", Data$full_picture,"'",'title=""', 'alt="" border="0" height="100" width="100">')
# 
# DataArticles <- Data[Data$post_type == "link",]
# DataArticles <- merge(DataArticles, EditorialData, by = "status_id", all.x = TRUE)
# DataArticles <- merge(DataArticles[,], LinkData[,c("status_id", "mitu_link", "category", "sponsored", "reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataArticles <- ddply(DataArticles, "mitu_link", transform, average_ctr = mean(ctr), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_link_clicks = mean(link_clicks))
# 
# DataArticles$author_status <- ifelse(!(DataArticles$author %in% c("Jorge Rodriguez-Jimenez", "Omar Villegas", "Lucas Molandes", "Jessica Garcia", "Andrew Santiago", "Jason Marcus")), "Contributor", DataArticles$author)
# 
# DataArticles$author_status <- ifelse(DataArticles$author %in% c("mitÃº Staff", "Adriana Venegas", "Fidel Martinez", "Alex Alvarez", "Wendy Barba"), "Old Staff", DataArticles$author_status)
# 
# DataVideos <- Data[Data$post_type == "video",]
# DataVideos <- merge(DataVideos[,], VideoData[,c("status_id", "video_repost_sharetext", "video_meme", "series", "category", "format", "sponsored", "reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataVideos <- ddply(DataVideos, "video_repost_sharetext", transform, average_views_rate = mean(views_rate), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_video_views = mean(post_video_views), average_viral_fan_rate = mean(viral_fan_rate))
# # DataVideos$created_time <- as.POSIXct(strptime(DataVideos$created_time, "%d/%m/%Y %H:%M"), tz = "GMT")
# 
# DataPhotos <- Data[Data$post_type == "photo",]
# DataPhotos <- merge(DataPhotos[,],PhotoData[,c("status_id", "image_text_py", "reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataPhotos <- ddply(DataPhotos, "image_text_py", transform, average_share_rate = mean(share_rate), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_viral_fan_rate = mean(viral_fan_rate))
# # DataPhotos$created_time <- as.POSIXct(strptime(DataPhotos$created_time, "%d/%m/%Y %H:%M"), tz = "GMT")

plot_histogram <- function(data, x_var, title){
  
  hist(data, # histogram
       col = "peachpuff", # column color
       border = "black", 
       prob = FALSE, # show densities instead of frequencies
       breaks = 200,
       xlab = x_var,
       main = paste(title, x_var),
       xaxt='n')

  # lines(density(data), # density plot
  #       lwd = 2, # thickness of line
  #       col = "chocolate3")
  # 
  
  abline(v = quantile(data, probs = 0.05),
         col = "blue",
         lwd = 2)
  
  abline(v = mean(data),
         col = "royalblue",
         lwd = 2)
  
  abline(v = median(data),
         col = "red",
         lwd = 2)
  
  abline(v = quantile(data, probs = 0.95),
         col = "green",
         lwd = 2)
  
  # quantile(DataArticles$link_clicks, probs = c(0.05, 0.95))
  
  legend(x = "topright", # location of legend within plot area
         c(paste("P < 5%: ", formatC(quantile(data, probs = c(0.05)), format="d", big.mark=',')), paste("Mean: ", formatC(mean(data), format="d", big.mark=',')), paste("Median: ", formatC(median(data), format="d", big.mark=',')), paste("P < 95%: ", formatC(quantile(data, probs = c(0.95)), format="d", big.mark=','))),
         col = c("blue", "royalblue", "red", "green"),
         lwd = c(2, 2))
  
  axis(side=1, at = axTicks(1), 
       labels = formatC(axTicks(1), format="d", big.mark=','))
}


# UI ================================================================================================================================


ui <- dashboardPage(skin = "blue",
                    
                    # Dashboard Header ------------------------------------------------------------------------------------------------------------------     
                    
                    dashboardHeader(title = "KPIs"),
                    
                    # Dashboard Sidebar -----------------------------------------------------------------------------------------------------------------
                    
                    dashboardSidebar(
                      
                      tags$head(tags$style(HTML(".sidebar { height: 200vh; }"))),
                      
                      sidebarMenu(
                        
                        menuItem("We are Mitú", tabName = "wam", icon = icon("fa fa-file-text-o")),
                        menuItem("We are Mitú Goals Model", tabName = "wam_goals_model", icon = icon("fa fa-file-text-o"))
                        
                      )
                    ),
                    
                    # Dashboard Body --------------------------------------------------------------------------------------------------------------------               
                    dashboardBody(fluidRow(
                      
                      tabItems(
                        
                        #  1. Articles WAM -----------------------------------------------------------------------------------------------------------   
                        
                        tabItem(tabName = "wam", fluidRow(
                          
                          tabBox( title = "",
                                  
                                  
                                  #  1.1 Articles Repost Suggestions ------------------------------------------------------------------------------------
                                  tabPanel("We Are Mitú",
                                           
                                           tabsetPanel(
                                             
                                             tabPanel("Page Data",
                                                      
                                                      box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_content_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisContentOutputMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisContentOutputTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(12, selectizeInput(inputId = "kpis_month_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisContentOutput"))
                                                      ),
                                                      
                                                      box(title = "Total Content Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_content_views", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisContentViewsMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisContentViewsTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(12, 
                                                                 # column(8, highchartOutput("PlotKpisContentViewsTitle", height = 30)),
                                                                 column(12, selectizeInput(inputId = "kpis_month_content_views", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                          ),
                                                          column(12, highchartOutput("PlotKpisContentViews"))
                                                          
                                                      ),
                                                      
                                                      box(title = "Total Followers", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_total_followers", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          column(12, highchartOutput("PlotKpisTotalFollowers")),
                                                          column(12, DT::dataTableOutput("PlotKpisTotalFollowersTable"))
                                                          
                                                      ),
                                                      
                                                      box(title = "New Followers", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_new_followers", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisNewFollowersMonth")),
                                                          column(12, DT::dataTableOutput("PlotKpisNewFollowersTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          # column(12, highchartOutput("PlotKpisNewFollowersTitle", height = 30)),
                                                          column(6, selectizeInput(inputId = "kpis_month_new_followers", label = "Month:", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          column(6, selectizeInput(inputId = "kpis_chart_type_new_followers", label = "Chart: ", choices = c("waterfall", "column"), selected = "waterfall")),
                                                          
                                                          column(12, highchartOutput("PlotKpisNewFollowers"))
                                                          
                                                      )
                                                      


                                                      # 
                                                      # box(title = "Meme Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                      #     
                                                      #     column(12, selectizeInput(inputId = "kpis_year_meme_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                      #     
                                                      #     column(12, highchartOutput("PlotKpisMemeContentOutputMonth")),
                                                      #     
                                                      #     column(12, DT::dataTableOutput("PlotKpisMemeContentOutputTable")),
                                                      #     column(12, 
                                                      #            column(8, highchartOutput("PlotKpisMemeContentOutputTitle", height = 30)),
                                                      #            column(4, selectizeInput(inputId = "kpis_month_meme_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                      #     ),
                                                      #     column(12, highchartOutput("PlotKpisMemeContentOutput"))
                                                      # )
                                             ),
                                             
                                             
                                             tabPanel("Articles",
                                                      
                                                      box(title = "Article Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_article_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisArticleContentOutputMonth")),
                                                          column(12, DT::dataTableOutput("PlotKpisArticleContentOutputTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(12, selectizeInput(inputId = "kpis_month_article_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisArticleContentOutput"))
                                                          
                                                      ),
                                                      
                                                      box(title = "Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_articles", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisArticlesMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisArticlesTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(4, selectizeInput(inputId = "kpis_month_articles", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          column(4, selectizeInput(inputId = "articles_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                          
                                                          column(4, selectizeInput(inputId = "articles_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                          
                                                          column(12, highchartOutput("PlotKpisArticles")))
                                                      
                                             ),
                                             
                                             tabPanel("Videos",
                                                      
                                                      #DIVIDIR VIDEOS DE VIDEO MEMES OUTPUT
                                                      
                                                      box(title = "Video Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_video_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisVideoContentOutputMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisVideoContentOutputTable")),
                                                          column(12,
                                                                 column(8, highchartOutput("PlotKpisVideoContentOutputTitle", height = 30)),
                                                                 column(4, selectizeInput(inputId = "kpis_month_video_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                          ),
                                                          column(12, highchartOutput("PlotKpisVideoContentOutput"))
                                                          
                                                      ),
                                                      
                                                      
                                                      box(title = "Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_videos", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisVideosMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisVideosTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(4, selectizeInput(inputId = "kpis_month_videos", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          column(4, selectizeInput(inputId = "videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                          
                                                          column(4, selectizeInput(inputId = "videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                          
                                                          column(12, highchartOutput("PlotKpisVideos")))
                                                     
                                                      
                                                      # box(title = "All Videos", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                      #     column(12, selectizeInput(inputId = "kpis_year_all_videos", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                      #     
                                                      #     column(12, highchartOutput("PlotKpisAllVideosMonth")),
                                                      #     
                                                      #     column(12, DT::dataTableOutput("PlotKpisAllVideosTable")),
                                                      #     
                                                      #     column(12, br()),
                                                      #     
                                                      #     column(4, selectizeInput(inputId = "kpis_month_all_videos", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                      #     column(4, selectizeInput(inputId = "all_videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                      #     
                                                      #     column(4, selectizeInput(inputId = "all_videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                      #     
                                                      #     column(12, highchartOutput("PlotKpisAllVideos"))),
                                                      
                                                      
                                             ),
                                             
                                             tabPanel("Video Memes",
                                               
                                                      box(title = "Video Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_video_memes", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisVideoMemesMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisVideoMemesTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(4, selectizeInput(inputId = "kpis_month_video_memes", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          column(4, selectizeInput(inputId = "video_memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                          
                                                          column(4, selectizeInput(inputId = "video_memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                          
                                                          column(12, highchartOutput("PlotKpisVideoMemes")))
                                               
                                             ),
                                             
                                             tabPanel("Memes",
                                                      
                                                      box(title = "Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_memes", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisMemesMonth")),
                                                          
                                                          column(12, DT::dataTableOutput("PlotKpisMemesTable")),
                                                          
                                                          column(12, br()),
                                                          
                                                          column(4, selectizeInput(inputId = "kpis_month_memes", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                          column(4, selectizeInput(inputId = "memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions"), selected = "Content Views")),
                                                          
                                                          column(4, selectizeInput(inputId = "memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                          
                                                          column(12, highchartOutput("PlotKpisMemes")))
                                                      
                                             ),
                                             
                                             tabPanel("Reach & Engagement",
                                                      
                                                      box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_reach", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisReachMonth")),
                                                          column(12, 
                                                                 column(8, highchartOutput("PlotKpisReachTitle", height = 30)),
                                                                 column(4, selectizeInput(inputId = "kpis_month_reach", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                          ),
                                                          column(12, highchartOutput("PlotKpisReach"))
                                                      ),
                                                      
                                                      box(title = "Engagement", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                          column(12, selectizeInput(inputId = "kpis_year_engagement", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                          
                                                          column(12, highchartOutput("PlotKpisEngagementMonth")),
                                                          column(12, 
                                                                 column(8, highchartOutput("PlotKpisEngagementTitle", height = 30)),
                                                                 column(4, selectizeInput(inputId = "kpis_month_engagement", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                          ),
                                                          column(12, highchartOutput("PlotKpisEngagement"))
                                                          
                                                      )
                                                      
                                             ))
                                  )
                                  
                                  , width = 12))),
                        
                        tabItem(tabName = "wam_goals_model", fluidRow(
                          
                          tabBox( title = "",
                                  
                                  tabPanel("We Are Mitú",
                                           
                                           box(title = "Model Controls", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               column(12, dateRangeInput('model_date_range',label = "Date Range", start = range(Data$date)[2]-60, end = range(Data$date)[2], min = "2015-10-01", max = range(Data$date)[2])
                                                      ),
                                               column(3, numericInput("model_num_articles", label = "Articles Originals", value = 155)),
                                               column(3, numericInput("model_num_articles_reposts", label = "Articles Reposts", value = 50)),
                                               column(3, numericInput("model_num_videos", label = "Video Originals", value = 62)),
                                               column(3, numericInput("model_num_videos_reposts", label = "Video Reposts", value = 25)),
                                               column(3, numericInput("model_num_video_memes", label = "Video Meme Originals", value = 62)),
                                               column(3, numericInput("model_num_video_memes_reposts", label = "Video Meme Reposts", value = 25)),
                                               column(3, numericInput("model_num_memes", label = "Memes Originals", value = 186)),
                                               column(3, numericInput("model_num_memes_reposts", label = "Memes Reposts", value = 100)),
                                               column(3, numericInput("model_num_simulations", label = "Number of Simulations", value = 10000)),
                                               column(3, actionButton(inputId = "model_simulation_button", label = "Simulate", width = "100%", style = "height:60px"))
                                           ),
                                           
                                           box(title = "Content Distributions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               tabBox( title = "",
                                                       
                                                       tabPanel("Articles",
                                                                
                                                                tabsetPanel(
                                                                  
                                                                  tabPanel("Link Clicks",
                                                                           
                                                                           column(12, plotOutput("PlotArticlesDistributionLinkClicks")),
                                                                           column(12, plotOutput("PlotArticlesLogDistributionLinkClicks")),
                                                                           column(12, plotOutput("PlotArticlesRepostsDistributionLinkClicks")),
                                                                           column(12, plotOutput("PlotArticlesRepostsLogDistributionLinkClicks"))
                                                                  ),
                                                                  
                                                                  tabPanel("Engagements",
                                                                           
                                                                           column(12, plotOutput("PlotArticlesDistributionEngagements")),
                                                                           column(12, plotOutput("PlotArticlesLogDistributionEngagements")),
                                                                           column(12, plotOutput("PlotArticlesRepostsDistributionEngagements")),
                                                                           column(12, plotOutput("PlotArticlesRepostsLogDistributionEngagements"))
                                                                           
                                                                  ),
                                                                  
                                                                  tabPanel("Reach",
                                                                           
                                                                           column(12, plotOutput("PlotArticlesDistributionReach")),
                                                                           column(12, plotOutput("PlotArticlesLogDistributionReach")),
                                                                           column(12, plotOutput("PlotArticlesRepostsDistributionReach")),
                                                                           column(12, plotOutput("PlotArticlesRepostsLogDistributionReach"))
                                                                  )
                                                                )
                                                       ),
                                                       
                                                       tabPanel("Videos",
                                                                
                                                                tabsetPanel(
                                                                  
                                                                  tabPanel("Video Views",
                                                                           
                                                                           column(12, plotOutput("PlotVideosDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideosLogDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideosRepostsDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideosRepostsLogDistributionVideoViews"))
                                                                  ),
                                                                  
                                                                  tabPanel("Engagements",
                                                                           
                                                                           column(12, plotOutput("PlotVideosDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideosLogDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideosRepostsDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideosRepostsLogDistributionEngagements"))
                                                                  ),
                                                                  
                                                                  tabPanel("Reach",
                                                                           
                                                                           column(12, plotOutput("PlotVideosDistributionReach")),
                                                                           column(12, plotOutput("PlotVideosLogDistributionReach")),
                                                                           column(12, plotOutput("PlotVideosRepostsDistributionReach")),
                                                                           column(12, plotOutput("PlotVideosRepostsLogDistributionReach"))
                                                                  )
                                                                )
                                                       ),
                                                       
                                                       tabPanel("Video Memes",
                                                                
                                                                tabsetPanel(
                                                                  
                                                                  tabPanel("Video Views",
                                                                           
                                                                           column(12, plotOutput("PlotVideoMemesDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideoMemesLogDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsDistributionVideoViews")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsLogDistributionVideoViews"))
                                                                           
                                                                  ),
                                                                  
                                                                  tabPanel("Engagements",
                                                                           
                                                                           column(12, plotOutput("PlotVideoMemesDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideoMemesLogDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsDistributionEngagements")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsLogDistributionEngagements"))
                                                                           
                                                                  ),
                                                                  
                                                                  tabPanel("Reach",
                                                                           
                                                                           column(12, plotOutput("PlotVideoMemesDistributionReach")),
                                                                           column(12, plotOutput("PlotVideoMemesLogDistributionReach")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsDistributionReach")),
                                                                           column(12, plotOutput("PlotVideoMemesRepostsLogDistributionReach"))
                                                                           
                                                                  )
                                                                )
                                                       ),
                                                       tabPanel("Memes",
                                                                
                                                                tabsetPanel(
                                                                  
                                                                  tabPanel("Reach",
                                                                           
                                                                           column(12, plotOutput("PlotMemesDistributionReach")),
                                                                           column(12, plotOutput("PlotMemesLogDistributionReach")),
                                                                           column(12, plotOutput("PlotMemesRepostsDistributionReach")),
                                                                           column(12, plotOutput("PlotMemesRepostsLogDistributionReach"))
                                                                           
                                                                  ),
                                                                  
                                                                  tabPanel("Engagements",
                                                                           
                                                                           column(12, plotOutput("PlotMemesDistributionEngagements")),
                                                                           column(12, plotOutput("PlotMemesLogDistributionEngagements")),
                                                                           column(12, plotOutput("PlotMemesRepostsDistributionEngagements")),
                                                                           column(12, plotOutput("PlotMemesRepostsLogDistributionEngagements"))
                                                                           
                                                                  )
                                                                )
                                                       ), width = 12)
                                           ),
                                           
                                           box(title = "Simulation Distributions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               column(12, plotOutput("PlotPageSimlatedLogDistributionContentViews", height = 500)),
                                               
                                               tabBox( title = "",
                                                       tabPanel("Articles",
                                                                
                                                                column(12, plotOutput("PlotArticleSimlatedLogDistributionLinkClicks", height = 500)),
                                                                column(12, plotOutput("PlotArticleRepostSimlatedLogDistributionLinkClicks", height = 500)),
                                                                column(12, plotOutput("PlotArticleSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotArticleRepostsSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotArticleSimlatedLogDistributionReach", height = 500)),
                                                                column(12, plotOutput("PlotArticleRepostsSimlatedLogDistributionReach", height = 500))
                                                       ),
                                                       
                                                       tabPanel("Videos",
                                                                           
                                                                column(12, plotOutput("PlotVideoSimlatedLogDistributionVideoViews", height = 500)),
                                                                column(12, plotOutput("PlotVideoRepostsSimlatedLogDistributionVideoViews", height = 500)),
                                                                column(12, plotOutput("PlotVideoSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotVideoRepostsSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotVideoSimlatedLogDistributionReach", height = 500)),
                                                                column(12, plotOutput("PlotVideoRepostsSimlatedLogDistributionReach", height = 500))
                                                       ),
                                                                
                                                       tabPanel("Video Memes",
                                                                
                                                                column(12, plotOutput("PlotVideosMemeSimlatedLogDistributionVideoViews", height = 500)),
                                                                column(12, plotOutput("PlotVideosMemeRepostsSimlatedLogDistributionVideoViews", height = 500)),
                                                                column(12, plotOutput("PlotVideosMemeSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotVideosMemeRepostsSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotVideosMemeSimlatedLogDistributionReach", height = 500)),
                                                                column(12, plotOutput("PlotVideosMemeRepostsSimlatedLogDistributionReach", height = 500))
                                                                
                                                       ),
                                                       
                                                       tabPanel("Memes",
                                                                
                                                                column(12, plotOutput("PlotMemeSimlatedLogDistributionReach", height = 500)),
                                                                column(12, plotOutput("PlotMemeRepostsSimlatedLogDistributionReach", height = 500)),
                                                                column(12, plotOutput("PlotMemeSimlatedLogDistributionEngagements", height = 500)),
                                                                column(12, plotOutput("PlotMemeRepostsSimlatedLogDistributionEngagements", height = 500))
                                                       ), width = 12)))
                                  
                                  , width = 12)))
                        
                      
                        
                        )))
)




# SERVER  ===========================================================================================================================

server <- function(input, output, session){
  
  # 3. KPIS --------------------------------------------------------------------------------------------------------------------
  
  # 3.1. KPIS - We Are Mitú ---------------------------------------------------------------------------------------------------------------------------
  
  output$PlotKpis <- renderHighchart({
    
    input$plot_kpis
    isolate({
      
      dates <- data.frame(date = Data$date)
      dates$date <- as.POSIXct(dates$date)
      
      articles_originals <- merge(dates, data.frame(date = DataArticles[which(DataArticles$original == 1),]$date, num = rep(1,nrow(DataArticles[which(DataArticles$original == 1),]))), by = "date", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"date"])
      
      videos_originals <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]))), by = "date", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"date"])
      
      video_memes_originals <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]))), by = "date", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"date"])
      
      memes_originals <- merge(dates, data.frame(date = DataPhotos[which(DataPhotos$original == 1),]$date, num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 1),]))), by = "date", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"date"])
      
      
      articles_reposts <- merge(dates, data.frame(date = DataArticles[which(DataArticles$original == 0),]$date, num = rep(1,nrow(DataArticles[which(DataArticles$original == 0),]))), by = "date", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"date"])
      
      videos_reposts <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]))), by = "date", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"date"])
      
      video_memes_reposts <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]))), by = "date", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"date"])
      
      memes_reposts <- merge(dates, data.frame(date = DataPhotos[which(DataPhotos$original == 0),]$date, num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 0),]))), by = "date", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"date"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      # colores<- c('#08415C', '#08415C', '#CC2936', '#CC2936', '#EBBAB9', '#EBBAB9','#388697','#388697')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Output")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis == "area" || input$chart_type_kpis == "column"){if(input$chart_stack_kpis != "none"){input$chart_stack_kpis}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })
  
  output$PlotKpis1 <- renderHighchart({
    
    input$plot_kpis1
    isolate({
      
      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$link_clicks), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$link_clicks), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Views")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis1 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis1 == "area" || input$chart_type_kpis1 == "column"){if(input$chart_stack_kpis1 != "none"){input$chart_stack_kpis1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      
      hc
    })
  })
  
  output$PlotKpis2 <- renderHighchart({
    
    input$plot_kpis2
    isolate({
      
      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Interactions")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis2 == "area" || input$chart_type_kpis2 == "column"){if(input$chart_stack_kpis2 != "none"){input$chart_stack_kpis2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })
  
  output$PlotKpis3 <- renderHighchart({
    
    input$plot_kpis3
    isolate({
      
      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$post_reach), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$post_reach), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis3 == "area" || input$chart_type_kpis3 == "column"){if(input$chart_stack_kpis3 != "none"){input$chart_stack_kpis3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  Plot_Month <- function (ds_goal, ds, ds_total, plot_title){
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = paste(plot_title, " - Monthly"), align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = paste(plot_title, " Goal"), pointPadding = 0) %>%
      hc_add_series(data = ds, name = plot_title, pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = paste("Total ", plot_title), type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Day <- function (ds_goal, ds, ds_total, ds_forecast, plot_title){
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text =  paste(plot_title, " - Daily"), align = "center") %>%
      hc_yAxis(title = list(text = "Content Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Output")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_total, name = paste("Total ", plot_title), type = "spline") %>%
      # hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = paste(plot_title, "Forecast"), type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds, name = paste("Daily ", plot_title), type = "waterfall") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  output$PlotKpisContentOutputMonth <- renderHighchart({
    
    plot_title <- "Content Output"
    
    year_selected <- as.Date(paste(input$kpis_year_content_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_output <- cumsum(as.numeric(DataMonth$content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_original_output + DataGoal[x,]$video_repost_output + DataGoal[x,]$video_meme_output + DataGoal[x,]$article_original_output + DataGoal[x,]$article_repost_output + DataGoal[x,]$meme_original_output + DataGoal[x,]$meme_repost_output)
    })
   
    Plot_Month(ds_goal, ds, ds_total, plot_title)
    
  })
  
  output$PlotKpisContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= "2017-01-01"),], .(date = format(Data[which(Data$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$video_original_output + DataGoals$video_repost_output + DataGoals$video_meme_output + DataGoals$article_original_output + DataGoals$article_repost_output + DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$page_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$page_content_output <- format( DataMonth$page_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))

  output$PlotKpisContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_content_output <- cumsum(DataMonth$content_output)
    
    slope <- lm(cumsum(content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoals$total_content_output <- rowSums(cbind(DataGoals$video_original_output, DataGoals$video_repost_output, DataGoals$video_meme_output, DataGoals$article_original_output, DataGoals$article_repost_output, DataGoals$meme_original_output, DataGoals$meme_repost_output), na.rm = TRUE)
    
    goal <- as.numeric(ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_content_output, 0))
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    # hc <-highchart() %>%
    #   hc_chart(type = "waterfall") %>%
    #   hc_title(text = "Content Output - Daily", align = "center") %>%
    #   hc_yAxis(title = list(text = "Content Output - Daily")) %>%
    #   hc_xAxis(type = "category") %>%
    #   hc_yAxis(title = list(text = "Content Output")) %>%
    #   hc_legend(enabled = TRUE) %>%
    #   hc_add_series(data = ds_total, name = "Total Content Output", type = "spline") %>%
    #   # hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
    #   hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
    #   hc_add_series(data = ds, name = "Daily Content Output", type = "waterfall") %>%
    #   hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
    #   hc_tooltip(shared = TRUE)%>% 
    #   hc_add_theme(hc_theme_smpl())
    # hc
    
    plot_title <- "Content Output"
    
    Plot_Day(ds_goal, ds, ds_total, ds_forecast, plot_title)
    
  })
  
  
  output$PlotKpisArticleContentOutputMonth <- renderHighchart({
    
    plot_title <- "Article Output"
    
    year_selected <- as.Date(paste(input$kpis_year_article_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),], .(date = format(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_article_content_output <- cumsum(as.numeric(DataMonth$article_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$article_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$article_original_output + DataGoal[x,]$article_repost_output)
    })
 
    Plot_Month(ds_goal, ds, ds_total, plot_title)
    
  })
  
  output$PlotKpisArticleContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= "2017-01-01"),], .(date = format(DataArticles[which(DataArticles$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$article_original_output + DataGoals$article_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$article_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$article_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$article_content_output <- format( DataMonth$article_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))

  output$PlotKpisArticleContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_article_content_output, "01"), "%b %Y %d")
    
    # date_selected <- as.Date(paste("abr. 2017", "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "link"),], "date", summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_article_content_output <- cumsum(DataMonth$article_content_output)
    
    slope <- lm(cumsum(article_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$article_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoals$total_article_content_output <- DataGoals$article_original_output + DataGoals$article_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_article_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Article Output - Daily", align = "center") %>%
      hc_yAxis(title = list(text = "Article Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Article Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Article Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  output$PlotKpisVideoContentOutputMonth <- renderHighchart({
    
    plot_title <- "Video Output"
    
    year_selected <- as.Date(paste(input$kpis_year_video_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_content_output <- cumsum(as.numeric(DataMonth$video_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_original_output + DataGoal[x,]$video_repost_output)
    })
    
    Plot_Month(ds_goal, ds, ds_total, plot_title)
    
  })
  
  output$PlotKpisVideoContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$video_original_output + DataGoals$video_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$video_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$video_content_output <- format( DataMonth$video_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideoContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Video Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideoContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_video_content_output, "01"), "%b %Y %d")
    
    # date_selected <- as.Date(paste("abr. 2017", "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "video"),], "date", summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_content_output <- cumsum(DataMonth$video_content_output)
    
    slope <- lm(cumsum(video_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$video_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoals$total_video_content_output <- DataGoals$video_original_output + DataGoals$video_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_video_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Video Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Video Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Video Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  output$PlotKpisMemeContentOutputMonth <- renderHighchart({
    
    plot_title <- "Meme Output"
    
    year_selected <- as.Date(paste(input$kpis_year_meme_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),], .(date = format(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_meme_content_output <- cumsum(as.numeric(DataMonth$meme_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$meme_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$meme_original_output + DataGoal[x,]$meme_repost_output)
    })
    
    Plot_Month(ds_goal, ds, ds_total, plot_title)
    
  })
  
  output$PlotKpisMemeContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= "2017-01-01"),], .(date = format(DataPhotos[which(DataPhotos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$meme_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$meme_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$meme_content_output <- format( DataMonth$meme_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisMemeContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Meme Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisMemeContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_meme_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "photo"),], "date", summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_meme_content_output <- cumsum(DataMonth$meme_content_output)
    
    slope <- lm(cumsum(meme_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$meme_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoals$total_meme_content_output <- DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_meme_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Meme Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Meme Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Meme Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  
  output$PlotKpisTotalFollowers <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_total_followers, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- WamPageData
    DataMonth$page_followers <- DataMonth$page_fans
    DataMonth <- DataMonth[which(DataMonth$date >= year_selected & DataMonth$date < year_selected + years(1)),]
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, page_followers = max(page_followers))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$page_followers))
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$total_followers)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Total Followers", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Totla Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Total Followers Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Total Followers", pointPadding = 0.2) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisTotalFollowersTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- WamPageData
    DataMonth$page_followers <- DataMonth$page_fans
    DataMonth <- DataMonth[which(DataMonth$date >= year_selected & DataMonth$date < year_selected + years(1)),]
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, page_followers = max(page_followers))
    
    # DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "total_followers")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$total_followers-DataMonth$page_followers)/DataMonth$total_followers
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_followers)),]
    
    DataMonth$total_followers <- format( DataMonth$total_followers, big.mark = ",")
    DataMonth$page_followers <- format( DataMonth$page_followers, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  
  
  output$PlotKpisNewFollowersMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_new_followers, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, new_followers = sum(page_fan_adds) - sum(page_fan_removes))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_new_followers <- cumsum(as.numeric(DataMonth$new_followers))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$new_followers))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$new_followers)
    })
    
    # DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "New Followers - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "New Followers Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "New Followers", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisNewFollowersTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= "2017-01-01"),], .(date = format(WamPageData[which(WamPageData$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_new_followers = sum(page_fan_adds) - sum(page_fan_removes))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "new_followers")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$new_followers-DataMonth$page_new_followers)/DataMonth$new_followers
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_new_followers)),]
    
    DataMonth$new_followers <- format( DataMonth$new_followers, big.mark = ",")
    DataMonth$page_new_followers <- format( DataMonth$page_new_followers, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisNewFollowers <- renderHighchart({
    
    chart_type <- input$kpis_chart_type_new_followers
    
    date_selected <- as.Date(paste(input$kpis_month_new_followers, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    # DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, new_followers = sum(page_fan_adds) - sum(page_fan_removes))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_new_followers <- cumsum(DataMonth$new_followers)
    
    slope <- lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$new_followers)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$new_followers, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "New Followers - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily New Followers", type = chart_type) %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  
  
  output$PlotKpisReachMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_reach, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, reach = sum(page_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_reach <- cumsum(as.numeric(DataMonth$reach))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$reach))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$reach)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Reach - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Reach Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Reach", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisReachTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Reach - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisReach <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_reach, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    # DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, reach = sum(post_reach))
    # 
    DataMonth <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, reach = sum(page_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$reach)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$reach, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Reach - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Reach", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisEngagementMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_engagement, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_interactions <- cumsum(as.numeric(DataMonth$interactions))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$engagement)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, interactions = sum(total_interactions))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Engagement - Monthly", align = "center") %>%
      hc_yAxis(title = list(text = "Engagement - Monthly")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Engagement Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Engagement", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisEngagementTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Engagement - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisEngagement<- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_engagement, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$interactions)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$engagement, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Engagement - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Engagement", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisContentViewsMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_content_views, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, content_views = sum(post_reach[post_type == "photo"]) + sum(link_clicks[post_type == "link"]))
    
    DataMonthVideo <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1) & WamPageData$date %in% Data$date),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1) & WamPageData$date %in% Data$date),]$date, "%Y-%m")), summarize, content_views_video = sum(page_video_views))
    
    DataMonth$content_views <- DataMonth$content_views + DataMonthVideo$content_views_video
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_views <- cumsum(as.numeric(DataMonth$content_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$content_views)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), video_views = sum(post_video_views[post_type == "video"]), content_views = photo_views + article_views + video_views)
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Content Views - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Content Views Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Content Views", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisContentViewsTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= "2017-01-01"),], .(date = format(Data[which(Data$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_content_views = sum(post_reach[post_type == "photo"]) + sum(link_clicks[post_type == "link"]))
    
    DataMonthVideo <- ddply(WamPageData[which(WamPageData$date >= "2017-01-01" & WamPageData$date %in% Data$date),], .(date = format(WamPageData[which(WamPageData$date >= "2017-01-01" & WamPageData$date %in% Data$date),]$date, "%Y-%m")), summarize, content_views_video = sum(page_video_views))
    
    DataMonth$page_content_views <- DataMonth$page_content_views + DataMonthVideo$content_views_video
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_views-DataMonth$page_content_views)/DataMonth$content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_content_views)),]
    
    DataMonth$content_views <- format( DataMonth$content_views, big.mark = ",")
    DataMonth$page_content_views <- format( DataMonth$page_content_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisContentViews <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_content_views, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), content_views = photo_views + article_views)
    
    DataMonthVideo <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, video_views = sum(page_video_views))
    
    DataMonth$content_views <- DataMonth$content_views + DataMonthVideo[which(DataMonthVideo$date %in% DataMonth$date),]$video_views
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_content_views <- cumsum(DataMonth$content_views)
    
    slope <- lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Content Views - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Content Views", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  
  
  output$PlotKpisArticlesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_articles, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),], .(date = format(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, link_clicks = sum(link_clicks))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_link_clicks <- cumsum(as.numeric(DataMonth$link_clicks))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$link_clicks))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_link_clicks)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$article_content_views)
    })
    
    DataMonth <- ddply(DataArticles[which(format.Date(DataArticles$date, "%m") == format.Date(max(DataArticles$date), "%m") & format.Date(DataArticles$date, "%Y") == format.Date(max(DataArticles$date), "%Y")),], "date", summarize, link_clicks = sum(link_clicks))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(link_clicks) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Articles CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Articles CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Articles CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Articles CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisArticlesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= "2017-01-01"),], .(date = format(DataArticles[which(DataArticles$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, link_clicks = sum(link_clicks))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "article_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$article_content_views-DataMonth$link_clicks)/DataMonth$article_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$link_clicks)),]
    
    DataMonth$article_content_views <- format( DataMonth$article_content_views, big.mark = ",")
    DataMonth$link_clicks <- format( DataMonth$link_clicks, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisArticles <- renderHighchart({
    
    if(input$articles_kpi_select_repost == "Originals"){
      
      DataArticles <- DataArticles[which(DataArticles$original == 1),]
      
    }
    
    else if (input$articles_kpi_select_repost == "Reposts"){
      
      DataArticles <- DataArticles[which(DataArticles$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_articles, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataArticles[which(format.Date(DataArticles$date, "%m") == month_selected & format.Date(DataArticles$date, "%Y") == year_selected),], "date", summarize, link_clicks = sum(link_clicks), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_link_clicks <- cumsum(DataMonth$link_clicks)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$articles_kpi_select_variable == "Content Views",lm(cumsum(link_clicks) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$articles_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$articles_kpi_select_variable == "Content Views", DataMonth[x,]$link_clicks, ifelse(input$articles_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$articles_kpi_select_variable == "Content Views", DataMonth[x,]$total_link_clicks, ifelse(input$articles_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$article_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Articles CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$articles_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$articles_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$articles_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisAllVideosMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_all_videos, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$all_video_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "All Videos CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "All Videos CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "All Videos CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total All Videos CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisAllVideosTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "all_video_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$all_video_content_views-DataMonth$video_views)/DataMonth$all_video_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$all_video_content_views <- format( DataMonth$all_video_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisAllVideos <- renderHighchart({
    
    if(input$all_videos_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$all_videos_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_all_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideos[which(format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$all_videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$all_videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$all_video_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "All Videos CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$all_videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$all_videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$all_videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisVideosMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_videos, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Videos CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Videos CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Videos CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Videos CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideosTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "video_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$video_content_views-DataMonth$video_views)/DataMonth$video_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$video_content_views <- format( DataMonth$video_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideos <- renderHighchart({
    
    if(input$videos_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$videos_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$video_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Videos CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisVideoMemesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_video_memes, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_meme_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Video Memes CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Video Memes CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Video Memes CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Video Memes CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideoMemesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "video_meme_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$video_meme_content_views-DataMonth$video_views)/DataMonth$video_meme_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$video_meme_content_views <- format( DataMonth$video_meme_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideoMemes <- renderHighchart({
    
    if(input$video_memes_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$video_memes_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_video_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$video_memes_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$video_memes_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$video_meme_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Video Memes CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$video_memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$video_memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$video_memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisMemesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_memes, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),], .(date = format(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, photo_views = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_photo_views <- cumsum(as.numeric(DataMonth$photo_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$photo_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_photo_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$meme_content_views)
    })
    
    DataMonth <- ddply(DataPhotos[which(format.Date(DataPhotos$date, "%m") == format.Date(max(DataPhotos$date), "%m") & format.Date(DataPhotos$date, "%Y") == format.Date(max(DataPhotos$date), "%Y")),], "date", summarize, photo_views = sum(post_reach))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(photo_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Memes CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Memes CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Memes CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Memes CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisMemesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= "2017-01-01"),], .(date = format(DataPhotos[which(DataPhotos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, photo_views = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "meme_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$meme_content_views-DataMonth$photo_views)/DataMonth$meme_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$photo_views)),]
    
    DataMonth$meme_content_views <- format( DataMonth$meme_content_views, big.mark = ",")
    DataMonth$photo_views <- format( DataMonth$photo_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisMemes <- renderHighchart({
    
    if(input$memes_kpi_select_repost == "Originals"){
      
      DataPhotos <- DataPhotos[which(DataPhotos$original == 1),]
      
    }
    
    else if (input$memes_kpi_select_repost == "Reposts"){
      
      DataPhotos <- DataPhotos[which(DataPhotos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataPhotos[which(format.Date(DataPhotos$date, "%m") == month_selected & format.Date(DataPhotos$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach), interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_photo_views <- cumsum(DataMonth$photo_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- ifelse(input$memes_kpi_select_variable == "Content Views", lm(cumsum(photo_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$photo_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_photo_views)
    })
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$memes_kpi_select_variable == "Content Views", DataMonth[x,]$photo_views, DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_photo_views, DataMonth[x,]$total_interactions))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$meme_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Memes CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
 
  ## Goal Simulation ----------------------------------------------------------------------------
  
  output$PlotArticlesDistributionLinkClicks <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesLogDistributionLinkClicks <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
  
  })
  
  output$PlotArticlesRepostsDistributionLinkClicks <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$link_clicks
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesRepostsLogDistributionLinkClicks <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$link_clicks
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
    
  })
  
  
  output$PlotArticlesDistributionEngagements <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$total_interactions
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesLogDistributionEngagements <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$total_interactions
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
    
  })
  
  output$PlotArticlesRepostsDistributionEngagements <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$total_interactions
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesRepostsLogDistributionEngagements <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$total_interactions
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
    
  })
  
  
  output$PlotArticlesDistributionReach <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$post_reach
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesLogDistributionReach <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$post_reach
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
    
  })
  
  output$PlotArticlesRepostsDistributionReach <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$post_reach
    
    n_articles <- fitdist((article_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(article_data)
    
  })
  
  output$PlotArticlesRepostsLogDistributionReach <- renderPlot({
    
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$post_reach
    
    n_articles <- fitdist(log(article_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_articles), legendtext = plot.legend)
    # qqcomp(list(n_articles), legendtext = plot.legend)
    cdfcomp(list(n_articles), legendtext = plot.legend)
    # ppcomp(list(n_articles), legendtext = plot.legend)
    
    # hchart(log(article_data))
    
  })
  
  
  
  output$PlotVideosDistributionVideoViews <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosLogDistributionVideoViews <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  output$PlotVideosRepostsDistributionVideoViews <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosRepostsLogDistributionVideoViews <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  
  output$PlotVideosDistributionEngagements <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosLogDistributionEngagements  <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  output$PlotVideosRepostsDistributionEngagements <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosRepostsLogDistributionEngagements  <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  
  output$PlotVideosDistributionReach <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosLogDistributionReach  <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  output$PlotVideosRepostsDistributionReach <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
    
    n_videos <- fitdist((video_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_data)
    
  })
  
  output$PlotVideosRepostsLogDistributionReach  <- renderPlot({
    
    video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
    
    n_videos <- fitdist(log(video_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_videos), legendtext = plot.legend)
    # qqcomp(list(n_videos), legendtext = plot.legend)
    cdfcomp(list(n_videos), legendtext = plot.legend)
    # ppcomp(list(n_videos), legendtext = plot.legend)
    
    # hchart(log(video_data))
    
  })
  
  
  
  output$PlotVideoMemesDistributionVideoViews <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesLogDistributionVideoViews <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  output$PlotVideoMemesRepostsDistributionVideoViews <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesRepostsLogDistributionVideoViews <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  
  output$PlotVideoMemesDistributionEngagements <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesLogDistributionEngagements <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  output$PlotVideoMemesRepostsDistributionEngagements <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesRepostsLogDistributionEngagements <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  
  output$PlotVideoMemesDistributionReach <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesLogDistributionReach <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  output$PlotVideoMemesRepostsDistributionReach <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
    n_video_memes <- fitdist((video_meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(video_meme_data)
    
  })
  
  output$PlotVideoMemesRepostsLogDistributionReach <- renderPlot({
    
    video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
    n_video_memes <- fitdist(log(video_meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_video_memes), legendtext = plot.legend)
    # qqcomp(list(n_video_memes), legendtext = plot.legend)
    cdfcomp(list(n_video_memes), legendtext = plot.legend)
    # ppcomp(list(n_video_memes), legendtext = plot.legend)
    
    
  })
  
  
  output$PlotMemesDistributionReach <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$post_reach
   
    n_memes <- fitdist((meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(meme_data)
    
  })
  
  output$PlotMemesLogDistributionReach <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$post_reach
    
    n_memes <- fitdist(log(meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # hchart(log(meme_data))
    
  })
  
  output$PlotMemesRepostsDistributionReach <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$post_reach
    
    n_memes <- fitdist((meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(meme_data)
    
  })
  
  output$PlotMemesRepostsLogDistributionReach <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$post_reach
    
    n_memes <- fitdist(log(meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # hchart(log(meme_data))
    
  })
  
  output$PlotMemesDistributionEngagements <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$total_interactions
    
    n_memes <- fitdist((meme_data), "lnorm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Lognormal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # histogram <- hist(article_data, breaks = 100, plot = FALSE)
    # hchart(meme_data)
    
  })
  
  output$PlotMemesLogDistributionEngagements <- renderPlot({
    
    meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$total_interactions
    
    n_memes <- fitdist(log(meme_data), "norm")
    par(mfrow = c(1, 2))
    plot.legend <- c("Normal")
    denscomp(list(n_memes), legendtext = plot.legend)
    # qqcomp(list(n_memes), legendtext = plot.legend)
    cdfcomp(list(n_memes), legendtext = plot.legend)
    # ppcomp(list(n_memes), legendtext = plot.legend)
    
    
    # hchart(log(meme_data))
    
  })
  
  
  
  
  output$PlotPageSimlatedLogDistributionContentViews <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
      
      meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$post_reach
      
      
      article_data_repo <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$link_clicks
      
      video_data_repo <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
      
      video_meme_data_repo <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
      
      meme_data_repo <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$post_reach
      
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      n_videos <- fitdist(log(video_data), "norm")
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      n_memes <- fitdist(log(meme_data), "norm")
      
      n_articles_repo <- fitdist(log(article_data_repo), "norm")
      n_videos_repo <- fitdist(log(video_data_repo), "norm")
      n_video_memes_repo <- fitdist(log(video_meme_data_repo), "norm")
      n_memes_repo <- fitdist(log(meme_data_repo), "norm")
      
      monte_carlo_articles <- NA
      monte_carlo_videos <- NA
      monte_carlo_video_memes <- NA
      monte_carlo_memes <- NA
      
      monte_carlo_articles_repo <- NA
      monte_carlo_videos_repo <- NA
      monte_carlo_video_memes_repo <- NA
      monte_carlo_memes_repo <- NA
   
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        monte_carlo_memes[i] <- sum(exp(rnorm(input$model_num_memes, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
        
        monte_carlo_articles_repo[i] <- sum(exp(rnorm(input$model_num_articles_reposts, mean = n_articles_repo$estimate[1], sd = n_articles_repo$estimate[2])))
        monte_carlo_videos_repo[i] <- sum(exp(rnorm(input$model_num_videos_reposts, mean = n_videos_repo$estimate[1], sd = n_videos_repo$estimate[2])))
        monte_carlo_video_memes_repo[i] <- sum(exp(rnorm(input$model_num_video_memes_reposts, mean = n_video_memes_repo$estimate[1], sd = n_video_memes_repo$estimate[2])))
        monte_carlo_memes_repo[i] <- sum(exp(rnorm(input$model_num_memes_reposts, mean = n_memes_repo$estimate[1], sd = n_memes_repo$estimate[2])))

      }
      
      monte_carlo_page <- monte_carlo_articles + monte_carlo_videos + monte_carlo_video_memes + monte_carlo_memes + monte_carlo_articles_repo + monte_carlo_videos_repo + monte_carlo_video_memes_repo + monte_carlo_memes_repo
      
      plot_histogram(monte_carlo_page, x_var = "Content Views", title = "WAM")
      
    })
    
  })
  
  
  output$PlotArticleSimlatedLogDistributionLinkClicks <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
    article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks
    
    set.seed(0)
    
    n_articles <- fitdist(log(article_data), "norm")
      
    monte_carlo_articles <- NA
 
    for(i in 1:input$model_num_simulations){
      
      monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
     
    }

    plot_histogram(monte_carlo_articles, x_var = "Link Clicks", title = "Originals")

    })
    
  })
  
  output$PlotArticleSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      
      monte_carlo_articles <- NA
     
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_articles, x_var = "Engagements", title = "Originals")
     
    })
    
  })
  
  output$PlotArticleSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      
      monte_carlo_articles <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
       
      }
      
      plot_histogram(monte_carlo_articles, x_var = "Reach", title = "Originals")
      
    })
    
  })
  
  output$PlotArticleRepostSimlatedLogDistributionLinkClicks <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$link_clicks
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      
      monte_carlo_articles <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles_reposts, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_articles, x_var = "Link Clicks", title = "Reposts")
      
    })
    
  })
  
  output$PlotArticleRepostsSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      
      monte_carlo_articles <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles_reposts, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_articles, x_var = "Engagements", title = "Reposts")
      
    })
    
  })
  
  output$PlotArticleRepostsSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      article_data <- DataArticles[which(DataArticles$date >= input$model_date_range[1] & DataArticles$date < input$model_date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_articles <- fitdist(log(article_data), "norm")
      
      monte_carlo_articles <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_articles[i] <- sum(exp(rnorm(input$model_num_articles_reposts, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_articles, x_var = "Reach", title = "Reposts")
      
    })
    
  })
  
  
  output$PlotVideoSimlatedLogDistributionVideoViews <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Video Views", title = "Originals")
      
    })
    
  })
  
  output$PlotVideoSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
  
      for(i in 1:input$model_num_simulations){
       
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Engagements", title = "Originals")
      
    })
    
  })
  
  output$PlotVideoSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Reach", title = "Originals")
      
    })
    
  })
  
  output$PlotVideoRepostsSimlatedLogDistributionVideoViews <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos_reposts, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Video Views", title = "Reposts")
      
    })
    
  })
  
  output$PlotVideoRepostsSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos_reposts, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Engagements", title = "Reposts")
      
    })
    
  })
  
  output$PlotVideoRepostsSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 0  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_videos <- fitdist(log(video_data), "norm")
      
      monte_carlo_videos <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_videos[i] <- sum(exp(rnorm(input$model_num_videos_reposts, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_videos, x_var = "Reach", title = "Reposts", title = "Reposts")
      
    })
    
  })
  
  
  output$PlotVideosMemeSimlatedLogDistributionVideoViews <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
       
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Video Views", title = "Originals")
      
    })
    
  })
  
  output$PlotVideosMemeSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$total_interactions
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
       
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Engagements", title = "Originals")
      
    })
    
  })
  
  output$PlotVideosMemeSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_reach
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
       
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Reach", title = "Originals")
   
    })
    
  })
  
  output$PlotVideosMemeRepostsSimlatedLogDistributionVideoViews <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes_reposts, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Video Views", title = "Reposts")
      
    })
    
  })
  
  output$PlotVideosMemeRepostsSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$total_interactions
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes_reposts, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Engagements", title = "Reposts")
      
    })
    
  })
  
  output$PlotVideosMemeRepostsSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      video_meme_data <- DataVideos[which(DataVideos$date >= input$model_date_range[1] & DataVideos$date < input$model_date_range[2] & DataVideos$video_meme == 1  & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_reach
      set.seed(0)
      
      n_video_memes <- fitdist(log(video_meme_data), "norm")
      
      monte_carlo_video_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_video_memes[i] <- sum(exp(rnorm(input$model_num_video_memes_reposts, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_video_memes, x_var = "Reach")
      
    })
    
  })
  
  
  
  output$PlotMemeSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_memes <- fitdist(log(meme_data), "norm")
      
      monte_carlo_memes <- NA
    
      for(i in 1:input$model_num_simulations){
       
        monte_carlo_memes[i] <- sum(exp(rnorm(input$model_num_memes, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_memes, x_var = "Engagements", title = "Originals")
      
    })
    
  })
  
  output$PlotMemeSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_memes <- fitdist(log(meme_data), "norm")
      
      monte_carlo_memes <- NA
   
      for(i in 1:input$model_num_simulations){
   
        monte_carlo_memes[i] <- sum(exp(rnorm(input$model_num_memes, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_memes, x_var = "Reach", title = "Originals")
       
    })
    
  })
  
  output$PlotMemeRepostsSimlatedLogDistributionEngagements <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$total_interactions
      
      set.seed(0)
      
      n_memes <- fitdist(log(meme_data), "norm")
      
      monte_carlo_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_memes[i] <- sum(exp(rnorm(input$model_num_memes_reposts, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_memes, x_var = "Engagements", title = "Reposts")
      
    })
    
  })
  
  output$PlotMemeRepostsSimlatedLogDistributionReach <- renderPlot({
    
    input$model_simulation_button
    isolate({  
      
      meme_data <- DataPhotos[which(DataPhotos$date >= input$model_date_range[1] & DataPhotos$date < input$model_date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$post_reach
      
      set.seed(0)
      
      n_memes <- fitdist(log(meme_data), "norm")
      
      monte_carlo_memes <- NA
      
      for(i in 1:input$model_num_simulations){
        
        monte_carlo_memes[i] <- sum(exp(rnorm(input$model_num_memes_reposts, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
        
      }
      
      plot_histogram(monte_carlo_memes, x_var = "Reach", title = "Reposts")
      
    })
    
  })
  
  }


# SHINY APP =========================================================================================================================

shinyApp(ui = ui, server = server)