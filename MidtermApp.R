library(shiny)

library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)

reefdata <- read.csv("Reef_Life_Survey_(RLS)#_Survey_Locations-survey_locations.csv",header=T)
rapply(reefdata, function(x) length(unique(x)))
reef.clean <- subset(reefdata, select=-c(Hour,FID,StateArea,SiteName,PQZipUrl,geom))
reef.clean %<>% separate(SurveyDate, c("Date","Time"),sep="T")
reef.clean <- subset(reef.clean, select=-c(Time,Direction))
reef.clean %<>% mutate(HasRugosityScores = ifelse(HasRugosityScores=="No",0,1))
reef.clean %<>% mutate(HasVisibility = ifelse(Visibility==0, 0, 1))
reef.clean$HasVisibility[is.na(reef.clean$HasVisibility)] <- 0
reef.clean %<>% mutate(Local = ifelse(CountryRegion=="Australia",1,0))
reef.summary <- reef.clean %>% group_by(CountryRegion) %>% summarise(SurveyNumber= n(), 
                                                                     Record.Rugosity=sum(HasRugosityScores)/n(),
                                                                     Record.Visibility=sum(HasVisibility)/n(),
                                                                     Earliest=min(Date),Latest=max(Date),
                                                                     Deepest=max(Depth))
reef.summary %<>% mutate(SurveyLength = interval(ymd(Earliest),ymd(Latest)))
reef.summary %<>% mutate(SurveyLength = SurveyLength%/% ddays(1))
reef.summary$Record.Rugosity <- round(reef.summary$Record.Rugosity, digits=2)
reef.summary$Record.Visibility <- round(reef.summary$Record.Visibility, digits=2)
reef.summary$`Earliest` <- ymd(reef.summary$`Earliest`)
reef.summary$`Latest` <- ymd(reef.summary$`Latest`)
reef.sites <- reef.clean %>% unite(col=Location, c("CountryRegion","Location"), sep = "_")
reef.sites %<>% group_by(Location) %>% summarise(n= n(), Record.Rugosity=sum(HasRugosityScores)/n(),
                                                 Record.Visibility=sum(HasVisibility)/n(),Earliest=min(Date),Latest=max(Date),
                                                 Deepest=max(Depth))
reef.sites %<>% separate(col=Location, into = c("CountryRegion","Location"),sep = "_")
reef.sites$`Earliest` <- ymd(reef.sites$`Earliest`)
reef.sites$`Latest` <- ymd(reef.sites$`Latest`)
reef.sites %<>% group_by(CountryRegion) %>% summarize(SiteNumber=n(),MinSite=min(n))
reef.summary <- cbind(reef.summary,reef.sites$SiteNumber,reef.sites$MinSite)
colnames(reef.summary) <- c("Country","Number of Surveys", "Recorded Rugosity", 
                            "Recorded Visibility", "Earliest Survey", 
                            "Latest Survey", "Lowest Depth", "Days Between",
                            "Number of Sites", "Lowest Surveys per Site")
reef.summary$'Average Surveys per Site' <- reef.summary$"Number of Surveys"/reef.summary$`Number of Sites`
reef.summary$'Average Surveys per Site' <- round(reef.summary$'Average Surveys per Site',digits=0)
reef.summary <- reef.summary[, c(1,5,6,8,2,9,11,10,3,4,7)]
reef.display <- reef.summary
reef.display$`Earliest Survey` <- as.character(reef.display$`Earliest Survey`)
reef.display$`Latest Survey` <- as.character(reef.display$`Latest Survey`)
reef.summary$Earliest <- reef.summary$`Earliest Survey`
reef.summary$Latest <- reef.summary$`Latest`

country.order <- reef.summary[order(-reef.summary$"Number of Surveys"),c("Country")]

reef.divers <- reef.clean %>% group_by(Divers) %>% summarize(n=n())
reef.top <- reef.divers[order(-reef.divers$n),]

reef.sum <- sum(reef.summary$`Number of Surveys`)
reef.sites <- sum(reef.summary$`Number of Sites`)
reef.total <- cbind(reef.sum,reef.sites)
colnames(reef.total) <- c("Total Surveys", "Total Sites")


ui <- fluidPage(
  
  titlePanel("Hogan Midterm ShinyApp"),
  navbarPage("Contents",
             tabPanel(
               "About",
               h2("About"),
               p("Hello! This is the Shiny App coming with the midterm."),
               p("If you haven't, please run the code before UI. Right now, this won't work without it."),
               p("This project is exploring the Reef Life Survey Dataset. 
                 Right now, this app will let you see similar things to what I did!"),
               p("I'll briefly describe what each page is for."),
               br(),
               strong("The Dataset"),
               p("The Reef Life Survey Dataset was compiled by 
                 University of Tasmania. It aggregates their reef
                 survey data from 2006 to 2019, including countries.
                 This analysis focuses on their Location Data,
                 assessing data by location more than the counted numbers.
                 The goal of this is to assess how well each location was
                 measured and studied and compare them."),
               br(),
               strong("Summary by Country"),
               p("This goes over data categorized by country. There are four tables.
                 The first shows total values over all countries as reference.
                 The second shows the five highest variables. You can pick the variable
                 you want to look at by using the bullets at the side.
                 You can also select a country. The last two tables will show you
                 that country's summarised data."),
               br(),
               strong("Measurements over Time"),
               p("This shows a plot of the key site measurements over time.
                 You can select a variable of interest and a reference time
                 (earliest/first or latest/most recent survey in country)
                 in the sidebar."),
               br(),
               strong("Diver Analysis"),
               p("You can select a diver or combination of divers.
                 The plot will then show you a plot highlighting
                 the sites they surveyed. The x axis will be
                 the total number of times that site was surveyed,
                 and the y axis is the proportion of surveys that 
                 the selected diver was involved in."),
               p("Thank you very much!")
               
             ),
             tabPanel(
               "Summary by Country",
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("variable", "Select a Variable:",
                                choices=c("Number of Surveys","Number of Sites",
                                          "Average Surveys per Site","Lowest Surveys per Site")),
               selectInput("country","Pick a Country!", country.order)
               
               ),
              mainPanel(
                strong("Surveys and Sites for all Countries"),
                tableOutput("Total"),
                br(),
                strong("Top 5 for Selected"),
                tableOutput("Top5"),
                br(),
               br(),
               strong("Summary of Data for Selected Country"),
               tableOutput("Summary1"),
               tableOutput("Summary2")
               
              )
               )
             ),
             
             tabPanel(
               "Measurements over Time", 
               sidebarLayout(
                 sidebarPanel(
               radioButtons("units","Select a Measurement:", 
                            choices=c("Recorded Rugosity", "Recorded Visibility","Lowest Depth")),
               radioButtons("surv","Earliest or Latest Survey?",
                            choices=c("Earliest Survey", "Latest Survey"))
                 ),
               mainPanel(
               plotOutput("Time")
               )
               )
               ),
             tabPanel(
               "Diver Analysis", 
               sidebarLayout(
                 sidebarPanel(
               selectInput("diver","Pick a Diver!", reef.top)
                 ),
               mainPanel(
               plotOutput("Dives")
               )
               )
             )
))

server = function(input,output){
    
  
  output$Total <- renderTable(reef.total, align="c")
  
  output$Summary1 <- renderTable(reef.display[reef.display$Country==input$country,c(1,5,6,7,8)], align="r")
  
  output$Summary2 <- renderTable(reef.display[reef.display$Country==input$country,c(1,2,3,4,9,10,11)], align="c")
  
  
  output$Top5 <- renderTable(
    reef.two <- reef.summary[order(-reef.summary[,c(input$variable)]),][1:5,c(1,5,6,7,8)],
    align="r"
  )

  
  output$Time <- renderPlot({
    ggplot()+
      geom_line(aes(x = reef.summary[,c(input$surv)], 
                    y=reef.summary[,c(input$units)], 
                    group=1),
                color = "#FC4E07",size=1.5) +
      labs(x=input$surv, y= input$units,title=input$units,subtitle="over Time")
  })
  
  
  reefdiver <- reactive({
    test <- filter(reef.clean, grepl(input$diver,Divers))%>% subset(select=c(Location))
    subset(reef.clean, Location %in% test[1:2170,])
    })
  
  diverdata <- reactive({  
      reefdiver() %>% mutate(is.selected = ifelse(grepl(input$diver,Divers)==T,1,0)) %>%
      unite(col=Location, c("CountryRegion",Location),sep="_",remove=T) %>%
      group_by(Location) %>% summarise(SiteTot = n(),PercentSelected=sum(is.selected)/n(),totSelected=sum(is.selected)) %>%
      separate(col=Location, c("Country","Location"),sep="_") %>%
      mutate(isAustralia = ifelse(Country=="Australia",1,0))
  })
  
  
  output$Dives <- renderPlot({
    ggplot(diverdata()) +
      aes(x = SiteTot, y = PercentSelected,colour = as.factor(isAustralia)) +
      geom_point(size = 1L) + 
      labs(x = "Total Site Surveys", y = "Diver's Proportion of Surveys", title = "Proportion of Surveys done by", subtitle= input$diver, color = "Australia? (1=yes)")
  })
  }

shinyApp(ui,server)
