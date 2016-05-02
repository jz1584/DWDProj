## app.R ##
library(shinydashboard)
library(shiny)
library(repmis)
library(rsconnect)
library(stringi)
library(RMySQL)
library(ggplot2)
library(ggmap)
options(mysql = list(
      "host" = "52.71.124.188",
      "port" = 3306,
      "user" = "root",
      "password" = "dwdstudent2015"
))
databaseName <- "project_db"
table <- "indeed"
table2<-"careerjet"


ui <- dashboardPage(
      dashboardHeader(title = "Jobs Opening Dashboard"),
      dashboardSidebar(
            sidebarUserPanel('hello word'),
            sidebarMenu(
                  id="tabls",
                  menuItem("Indeed Job Posting Stats",tabName = "indeed",icon = icon('th')),
                  menuItem("Careerjet Job Posting Stats",tabName = "jet",icon = icon('th')),
                  menuItem('Where are the Jobs ?',tabName = 'mapping',icon = icon('th')),
                  menuItem('Comparison',tabName = 'etc',icon=icon('th'))
                        )
                        ),
                       
      dashboardBody(
            # Boxes need to be put in a row (or column)
            tabItems(
                  tabItem('indeed',
                     fluidRow(
                        box(plotOutput("plot1"),collapsible = TRUE,height = 500),
                        box(plotOutput('plot3'),collapsible = TRUE,height = 500),
                        box(textOutput("plot2"),collapsible = TRUE,height = 500,
                            title = 'Temp: Real Time Testing Only'),
                        box(tableOutput('plot4'),collapsible = TRUE,height = 500,
                            title = 'Ranking Refresh Every 60 mins '))
                              ),
                  tabItem('jet',
                          fluidRow(
                                box(plotOutput("plotj1"),collapsible = TRUE,height = 500),
                                box(plotOutput('plotj3'),collapsible = TRUE,height = 500),
                                box(textOutput("plotj2"),collapsible = TRUE,height = 500),
                                box(tableOutput('plotj4'),collapsible = TRUE,height = 500))
                  ),
                  tabItem('mapping',
                          fluidRow(
                                column(plotOutput('map'),width = 6),#check needed
                                column(plotOutput('map2'),width = 6)
                              )
                  ),
                  tabItem('etc',
                          div(p('hello world in constrcution~~~'))    
                              )
                  
                  )
      )

)

#company, formattedLocationFull,formattedRelativeTime,jobtitle,date
server <- function(input, output,session) {

      loadData <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT date 
                             FROM (select distinct date, company,formattedLocation,jobtitle from %s) as T
                             order by date", table)
            #order by rand() limit 1000
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      loadData2 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("select company,count(*) OpenJobs
                             FROM (select distinct date, company,formattedLocation,jobtitle from %s) as T
                             where company!=''
                             group by company
                             order by OpenJobs desc 
                             limit 10", table)
            #order by rand() limit 1000
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data$company<-factor(data$company)
            data

      }
      
      loadData3 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT jobtitle Top10InDemandJobs,count(*) OpenPositions
                       FROM (select distinct date, company,formattedLocation,jobtitle from %s) as T
                       group by jobtitle
                       order by OpenPositions desc", table)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      loadData.map4 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT JobLocation,count(*) JobPostings
                       FROM (select distinct date, company,right(formattedLocation,2) JobLocation,jobtitle from %s) as T
                       group by JobLocation
                       order by JobPostings desc", table)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      dataFresh<-reactive({
            #refresh data
            invalidateLater(600000,session)
            a<-loadData()
            #a<-a[!is.na(a$jobtitle)&!is.na(a$company),]
            #a$company<-stri_sub(a$company,1,12)
            
      })
      
      dataFresh2<-reactive({
            #refresh data
            invalidateLater(600000,session)
            a<-loadData2()

      })

      dataFresh.map<-reactive({
            #refresh data
            invalidateLater(3600000,session)#24 hrs
            a<-loadData.map4()
            geo<-geocode(a$JobLocation)
            a$lon<-geo$lon
            a$lat<-geo$lat
            a
            
            #a<-a[!is.na(a$jobtitle)&!is.na(a$company),]
            #a$company<-stri_sub(a$company,1,12)
            
      })

      
      
      dataTest<-reactive({
            invalidateLater(1000,session)
            b<-sample(100)
      })

     
      
      output$plot1 <- renderPlot({
            #plot(factor(dataFresh()$state),main='Jobs in Demand')
            #s<-as.data.frame(summary(factor(stri_sub(a$company,1,12)))[1:10])
            a<-dataFresh()#
            qplot(as.Date(a$date),binwidth=1,xlab = 'Date',ylab='Number of Jobs Opening',
                  main = 'Testing: Total Job Openings by date \n Updates every 60 mins')+
                  theme(axis.text=element_text(size=14,face = 'bold'),axis.title=element_text(size=14,face = 'bold'))
            
      })
      
      output$plot3 <- renderPlot({
            a<-dataFresh2()
            ggplot(a,aes(company,OpenJobs,size=OpenJobs))+
                  geom_point(color='red',show.legend=FALSE)+
                  geom_text(aes(label=company),size=4,hjust=0.5, vjust=1.2,show.legend = FALSE)+
                  theme(axis.text=element_text(size=14,face = 'bold'),
                        axis.title=element_text(size=14,face = 'bold'),axis.text.x = element_blank())+
                  ggtitle('Top 10 Companies that have the most Job openings \n Updates every 60 mins')
                  
            
      })
      
      output$plot2 <- renderText({
             #summary(factor(dataFresh()$company))
             paste('Real time testing purpose:',dataTest())
             
      })
      output$plot4 <- renderTable({
            text<-loadData3()[1:10,]

      })
      
      output$map<-renderPlot({
            getMap<-get_map('United States',zoom=4,source='google',maptype ='roadmap')
            dat<-dataFresh.map()
            ggmap(getMap,extent = "device")+geom_point(aes(x=lon,y=lat,size=JobPostings,color=JobLocation),data=dat)
      },width = 720,height = 720)
      
      ###########################################################################################################
      loadjet1 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT date 
                             FROM (select distinct date, company,locations,title from %s) as T
                             where date >= '2016-04-30'
                             order by date", table2)
            #order by rand() limit 1000
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      loadjet2 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("select company,count(*) OpenJobs
                             FROM (select distinct date, company,locations,title from %s) as T
                             where company!='' and date >= '2016-04-30'
                             group by company
                             order by OpenJobs desc 
                             limit 10", table2)
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data$company<-factor(data$company)
            data
            
      }
      
      loadjet3 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT title Top10InDemandJobs,count(*) OpenPositions
                       FROM (select distinct date, company,locations,title from %s) as T
                       where date >= '2016-04-30'
                       group by title
                       order by OpenPositions desc", table2)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      
      dataFresh.jet1<-reactive({
            #refresh data
            invalidateLater(600000,session)
            a<-loadjet1()
      })
      
      dataFresh.jet2<-reactive({
            #refresh data
            invalidateLater(600000,session)
            a<-loadjet2()
            
      })
      
      dataFresh.jet3<-reactive({
            #refresh data
            invalidateLater(600000,session)
            a<-loadjet3()
            
      })
      
      output$plotj1 <- renderPlot({
            a<-dataFresh.jet1()
            qplot(as.Date(a$date),binwidth=1,xlab = 'Date',ylab='Number of Jobs Opening',
                  main = 'Testing: Total Job Openings by date \n Updates every 60 mins')+
                  theme(axis.text=element_text(size=14,face = 'bold'),axis.title=element_text(size=14,face = 'bold'))
            
      })
      
      output$plotj3 <- renderPlot({
            a<-dataFresh.jet2()
            ggplot(a,aes(company,OpenJobs,size=OpenJobs))+
                  geom_point(color='red',show.legend=FALSE)+
                  geom_text(aes(label=company),size=4,hjust=0.5, vjust=1.2,show.legend = FALSE)+
                  theme(axis.text=element_text(size=14,face = 'bold'),
                        axis.title=element_text(size=14,face = 'bold'),axis.text.x = element_blank())+
                  ggtitle('Top 10 Companies that have the most Job openings \n Updates every 60 mins')
            
            
      })
      
      output$plotj4 <- renderTable({
            text<-dataFresh.jet3()[1:10,]
            
      })
      
      
      ###########################################################################################################
     
       
}

shinyApp(ui,server)
# rsconnect::setAccountInfo(name='jianming',
#                           token='53AD78ADA46CAD3C1420D690696A29D6',
#                           secret='Lyor1wPUNWi1RWRjP20BjLtNCDDuSv2RRFNA5xOD')
#rsconnect::deployApp()


#data$company<-factor(stri_sub(data$company,1,12))


