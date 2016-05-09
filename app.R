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
      dashboardHeader(title = "Job Openings Dashboard"),
      dashboardSidebar(
            sidebarUserPanel('Team #8 @_@'),
            sidebarMenu(
                  id="tabls",
                  menuItem("Indeed Job Posting Stats",tabName = "indeed",icon = icon('th')),
                  menuItem("Careerjet Job Posting Stats",tabName = "jet",icon = icon('th')),
                  menuItem('Where are the Jobs ?',tabName = 'mapping',icon = icon('th')),
                  menuItem('Appendix',tabName = 'etc',icon=icon('th'))
            )
      ),
      
      dashboardBody(
            # Boxes need to be put in a row (or column)
            tabItems(
                  tabItem('indeed',
                          fluidRow(
                                column(textOutput('Ctime'),width = 12),
                                column(textOutput('time'),width = 12),
                                box(plotOutput("plot1"),collapsible = TRUE,height = 500,
                                    title = 'Total Job Openings by date '),
                                box(tableOutput('plot3'),collapsible = TRUE,height = 500,
                                    title = 'Top 10 Companies that have the most Job openings'),
                                box(plotOutput("plot2"),collapsible = TRUE,height = 500,
                                    title = 'Top 10 States with the most Job openings'),
                                box(tableOutput('plot4'),collapsible = TRUE,height = 500,
                                    title = 'Top 10 In-Demand Jobs '))
                  ),
                  tabItem('jet',
                          fluidRow(
                                box(tableOutput("plotj1"),collapsible = TRUE,height = 500,
                                    title = '10 Recent Posted Jobs'
                                    ),
                                box(tableOutput('plotj3'),collapsible = TRUE,height = 500,title = 'Top 10 Companies that have the most Job openings'),
                                box(plotOutput("plotj.state"),collapsible = TRUE,height = 500,
                                    title = 'Top 10 States with the most Job openings'),
                                box(tableOutput('plotj4'),collapsible = TRUE,height = 500,
                                    title='Top 10 In-Demand Jobs'))
                  ),
                  tabItem('mapping',
                          fluidRow(
                                plotOutput('map'),#check needed
                                plotOutput('map2')
                          )
                  ),
                  tabItem('etc',
                          fluidRow(
                                textOutput('appendix1'),
                                textOutput('appendix2')
                                )  
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
      
      loadData.state <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT JobLocation,count(*) JobPostings
                             FROM (select distinct date, company,state JobLocation,jobtitle from %s) as T
                             group by JobLocation
                             order by JobPostings desc limit 10", table)
            
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
                             FROM (select distinct date, company,state JobLocation,jobtitle from %s) as T
                             group by JobLocation
                             order by JobPostings desc", table)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      dataFresh<-reactive({
            #refresh data
            invalidateLater(1800000,session)
            a<-loadData()
            #a<-a[!is.na(a$jobtitle)&!is.na(a$company),]
            #a$company<-stri_sub(a$company,1,12)
            
      })
      
      dataFresh2<-reactive({
            #refresh data
            invalidateLater(1800000,session)
            a<-loadData2()
            a$OpenJobs<-as.integer(a$OpenJobs)
            a
            
      })
      
      dataFresh.map<-reactive({
            #refresh data
            invalidateLater(1800000,session)#12 hrs
            a<-loadData.map4()
            geo<-geocode(a$JobLocation)
            a$lon<-geo$lon
            a$lat<-geo$lat
            a
            
            #a<-a[!is.na(a$jobtitle)&!is.na(a$company),]
            #a$company<-stri_sub(a$company,1,12)
            
      })
      
      
      dataTest<-reactive({
            invalidateLater(1800000,session)#12 hrs
            b<-loadData.state()
            b
      })
      
      dataFresh3<-reactive({
            invalidateLater(1800000,session)#12 hrs
            b<-loadData3()
            b$OpenPositions<-as.integer(b$OpenPositions)
            b
      })
      
      
      output$plot1 <- renderPlot({
            #plot(factor(dataFresh()$state),main='Jobs in Demand')
            #s<-as.data.frame(summary(factor(stri_sub(a$company,1,12)))[1:10])
            a<-dataFresh()#
            qplot(as.Date(a$date),binwidth=1,xlab = 'Date',ylab='Number of Jobs Opening',
                  main = 'Updates every 12 hours')+
                  theme(axis.text=element_text(size=14,face = 'bold'),axis.title=element_text(size=14,face = 'bold'))
            
      })
      
      output$plot3 <- renderTable({
            a<-dataFresh2()
            table2<-a[1:10,]
            
            #             ggplot(a,aes(company,OpenJobs,size=OpenJobs))+
            #                   geom_point(color='red',show.legend=FALSE)+
            #                   geom_text(aes(label=company),size=4,hjust=0.5, vjust=1.2,show.legend = FALSE)+
            #                   theme(axis.text=element_text(size=14,face = 'bold'),
            #                         axis.title=element_text(size=14,face = 'bold'),axis.text.x = element_blank())+
            #                   ggtitle('Top 10 Companies that have the most Job openings')
            
            
            
            
      })
      
      output$plot2 <- renderPlot({### state ranking 
            State<-dataTest()
            ggplot(State,aes(x=reorder(JobLocation,JobPostings),y=JobPostings))+
                  geom_bar(stat='identity',fill='blue')+coord_flip()+
                  theme(axis.text=element_text(size=14,face = 'bold'),axis.title=element_text(size=14,face = 'bold'),
                        axis.title.y = element_blank())+
                  ggtitle('Top 10 States that have the most Job openings')
            
      })
      output$plot4 <- renderTable({
            text<-dataFresh3()[1:10,]
            
      })
      
      output$map<-renderPlot({
            getMap<-get_map('United States',zoom=4,source='google',maptype ='roadmap')
            dat<-dataFresh.map()
            ggmap(getMap,extent = "device")+geom_point(aes(x=lon,y=lat,size=JobPostings,color=JobLocation),data=dat)
      })
      
      ###########################################################################################################
      loadjet1 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT date 
                             FROM (select distinct company,locations,title from %s) as T
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
                             FROM (select distinct company,locations,title from %s) as T
                             where company!=''
                             group by company
                             order by OpenJobs desc 
                             limit 10", table2)
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data$company<-factor(data$company)
            data
            
      }
      #where date >= '2016-04-30
      loadjet3 <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("select title Top10InDemandJobs,count(*) OpenPositions
                             FROM (select distinct company,locations,title from %s) as T
                             group by title
                             order by OpenPositions desc", table2)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      loadj.recent <- function() { #slightly complicate one
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf(
                             "SELECT company, title, locations
                             from  %s
                             order by date desc
                              limit 10", table2)
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data$company<-factor(data$company)
            data
      }
      


      loadData.Jstate <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("select right(jobLocation,2) as JobLocation,count(*) JobPostings
                             FROM (select distinct company,locations jobLocation,title from %s) as T
                             group by JobLocation
                             order by JobPostings desc
                             limit 1000000", table2)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      dataFresh.Jstate<-reactive({
            invalidateLater(1800000,session)#12 hrs
            jstate<-loadData.Jstate()
            jstate$JobLocation<-toupper(jstate$JobLocation)
            jstate<-aggregate(x=jstate$JobPostings,by=list(jstate$JobLocation),FUN=sum)
            names(jstate)<-c("JobLocation","JobPostings")
            jstate<-jstate[order(jstate$JobPostings,decreasing = TRUE),]
            jstate<-jstate[1:10,]#top 10 only
            
      })
      
      
      dataFresh.jet1<-reactive({
            #refresh data
            invalidateLater(1800000,session)
            a<-loadjet1()
      })
      
      dataFresh.jet2<-reactive({
            #refresh data
            invalidateLater(1800000,session)
            a<-loadjet2()
            a$OpenJobs<-as.integer(a$OpenJobs)
            a
            
      })
      
      dataFresh.jet3<-reactive({# top 10 high demand job refreshing part
            #refresh data
            invalidateLater(1800000,session)
            a<-loadjet3()
            a$Top10InDemandJobs<-substr(stri_trim(a$Top10InDemandJobs),1,25)
            a$OpenPositions<-as.integer(a$OpenPositions)
            a
            
      })
      
      dataFresh.jet.recent<-reactive({
            #refresh data
            invalidateLater(1800000,session)
            a<-loadj.recent()
      })
      
      output$plotj.state <- renderPlot({
            jjstate<-dataFresh.Jstate()
            ggplot(jjstate,aes(x=reorder(JobLocation,JobPostings),y=JobPostings))+
                  geom_bar(stat='identity',fill='blue')+coord_flip()+
                  theme(axis.text=element_text(size=14,face = 'bold'),axis.title=element_text(size=14,face = 'bold'),
                        axis.title.y = element_blank())+
                  ggtitle('Top 10 States that have the most Job openings')
            
      })
      
      
      
      output$plotj3 <- renderTable({
            d<-dataFresh.jet2()
            d
            #             ggplot(a,aes(company,OpenJobs,size=OpenJobs))+
            #                   geom_point(color='red',show.legend=FALSE)+
            #                   geom_text(aes(label=company),size=4,hjust=0.5, vjust=1.2,show.legend = FALSE)+
            #                   theme(axis.text=element_text(size=14,face = 'bold'),
            #                         axis.title=element_text(size=14,face = 'bold'),axis.text.x = element_blank())+
            #                   ggtitle('Top 10 Companies that have the most Job openings \n Updates every 12 hours')
            
            
      })
      
      output$plotj4 <- renderTable({
            text<-dataFresh.jet3()[1:10,]
            
      })
      
      output$plotj1<-renderTable({
            recent<-dataFresh.jet.recent()
      })
      
      ############################################  Update time session:
      loadData.time <- function() {
            # Connect to the database
            db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
            # Construct the fetching query`
            query <- sprintf("SELECT date 
                             FROM (select distinct date, company,formattedLocation,jobtitle from %s) as T
                             order by date desc limit 1", table)
            #order by rand() limit 1000
            
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            dbDisconnect(db)
            data
      }
      
      dataFresh.time<-reactive({
            invalidateLater(1800000,session)#12 hrs
            d<-loadData.time()
            timeFresh<-print(as.character(as.POSIXct(d$date)-3600*4))
            timeFresh
            
      })
      
      dataFresh.CurrentTime<-reactive({    #current time
            invalidateLater(1000,session)#second 
            CtimeFresh<-print(as.character(Sys.time()-3600*4))
            CtimeFresh
            
      })
      
      output$time<-renderText({
            reactive.time<-paste('Last Updated from mysql:', dataFresh.time())
      })
      
      output$Ctime<-renderText({
            reactive.time<-paste('Current Time:',dataFresh.CurrentTime())
      })
      
      output$appendix1<-renderText({
            paste('Team Members: Rohit Shivaji Bhangale, Jianming Zhou, Tianpei Liu, Jonathan Borch')
      })
      output$appendix2<-renderText({
            paste('Source Code saved here: https://github.com/jz1584/DWDProj/blob/master/app.R')
      })
      
                  
      
      ###########################################################################################################
      
      
}

shinyApp(ui,server)