# Load R packages
library(shiny)
library(shinythemes)
library(shinyBS)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Market Segmentation and Product Recommendation System for a Credit Issuing Company"),
                navbarPage(
                  "",
                  tabPanel("Customer Segmentation",
                           sidebarLayout(
                           sidebarPanel(
                             tags$h4("Customer Profile:"),
                             selectInput(inputId = "var1", label = "SchoolEnrolment", choices = ""),
                             selectInput(inputId = "var2", label = "Sex", choices = ""),
                             selectInput(inputId = "var3", label = "Occupation", choices = ""),
                             selectInput(inputId = "var4", label = "WorkClass", choices = ""),
                             selectInput(inputId = "var5", label = "Education", choices = ""),
                             selectInput(inputId = "var6", label = "MaritalStatus", choices = ""),
                             selectInput(inputId = "var7", label = "Relationship", choices = ""),
                             selectInput(inputId = "var8", label = "Race", choices = ""),
                             selectInput(inputId = "var9", label = "NativeCountry", choices = ""),
                             textInput("var10", "Age", "55"),
                             textInput("var11", "HoursPerWeek", "40"),
                             textInput("var12", "Wages/Salary", "1000"),
                             textInput("var13", "CapitalGain", "100"),
                             selectInput(inputId = "var14", label = "MetroSize", choices = ""),
                             selectInput(inputId = "var15", label = "State", choices = ""),
                             selectInput(inputId = "var16", label = "Region", choices = ""),
                             actionButton(inputId = "update", label = "Fit Customer")
                           ), # sidebarPanel
                           mainPanel(
                             bsCollapsePanel("Clusters and Customer mapping (click here for explanation)",
                                             p("The plot below shows the distribution of FOUR distinct clusters in the population against the two  principal component scales which best presents the variance."),
                                             p("The population dataset used is the UCI Adult data: https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data."),
                                             p("These clusters were identified by unsupervised learning algorithm based on the overall population's profile and manually validated to check it's fit for the business case of this application. The application attempts to map the right credit product (like a credit card) with the right customers based on their risk score which is determined by their mapping to one of the four clusters named as - Platinum, Gold, Silver and Bronze."),
                                             p("The company has FOUR suite of credit products - Platinum, Gold, Silver and Bronze in the descending order of risk scores. The application will recommend the ideal suite for a customer based on their mapped cluster.")
                                             ),
                             plotOutput(outputId = "plot"),
                             strong(verbatimTextOutput(outputId= "txtout_rec"))
                           ) # mainPanel
                  )), # Customer Segmentation, tabPanel
                  tabPanel("Platinum", 
                           verticalLayout(
                             strong(p("This cluster has the best credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age3"),
                             verbatimTextOutput(outputId= "txtout_ed3"),
                             verbatimTextOutput(outputId= "txtout_cg3"),
                             verbatimTextOutput(outputId= "txtout_cl3"),
                             plotlyOutput(outputId = "plot3")
                           )
                  ),
                  tabPanel("Gold", 
                           verticalLayout(
                             strong(p("This cluster has a good credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age1"),
                             verbatimTextOutput(outputId= "txtout_ed1"),
                             verbatimTextOutput(outputId= "txtout_cg1"),
                             verbatimTextOutput(outputId= "txtout_cl1"),
                             plotlyOutput(outputId = "plot1")
                           )
                  ),
                  tabPanel("Silver", 
                           verticalLayout(
                             strong(p("This cluster has an average risk profile")),
                             verbatimTextOutput(outputId= "txtout_age4"),
                             verbatimTextOutput(outputId= "txtout_ed4"),
                             verbatimTextOutput(outputId= "txtout_cg4"),
                             verbatimTextOutput(outputId= "txtout_cl4"),
                             plotlyOutput(outputId = "plot4")
                           )
                           ),
                  tabPanel("Bronze", 
                           verticalLayout(
                             strong(p("This cluster has a below average credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age2"),
                             verbatimTextOutput(outputId= "txtout_ed2"),
                             verbatimTextOutput(outputId= "txtout_cg2"),
                             verbatimTextOutput(outputId= "txtout_cl2"),
                             plotlyOutput(outputId = "plot2")
                           )
                  )
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output, session) {

  library(dplyr)
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(ggthemes) 
  library(FactoMineR)
  library(factoextra)
  library(tidyr)
  library(reshape2)
  library(cluster)
  library(class)
  set.seed(919)

  raw_df=read.csv("https://github.com/robinmath/US_census_clustering/raw/main/US_Census_2020.csv", header = TRUE)
  
  raw_df<-na.omit(raw_df)
  raw_df <- dplyr::select(raw_df, -X,-hours_per_week_bins,-age_bins,-Wages_n_salary_bins_50K,-Wages_n_salary_bins,-Wages_n_salary_bins_incometax,-capital_gains_bins)
  raw_df<-raw_df %>% mutate(across(where(is.character), str_trim))
  raw_df$hours_per_week[raw_df$hours_per_week<0] <- -1
  
  df<-raw_df

  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.integer)
  
  dt<-setDT(df)
  scaled_dt<-scale(dt)
  
  pop_cluster<-kmeans(scaled_dt,4,nstart=50)
  
  pca<-prcomp(scaled_dt, scale = FALSE)
  pca_scaled_dt <- predict(pca, newdata = scaled_dt)
  
  cluster_pca_scaled_dt <- cbind(pca_scaled_dt, cluster = pop_cluster$cluster)
  cluster_pca_scaled_df <- as.data.frame(cluster_pca_scaled_dt)
  
  cluster_raw_df <- cbind(raw_df, cluster = pop_cluster$cluster)
  
  final_df <- cbind(raw_df,PC1=cluster_pca_scaled_df$PC1,PC2=cluster_pca_scaled_df$PC2,cluster=cluster_pca_scaled_df$cluster)
  # remove variables that can't be part of input for new customer
  final_df<-select(final_df,-fnlwgt)
  
  pc1_model<-lm(PC1 ~.,select(final_df,-PC2,-cluster))
  pc2_model<-lm(PC2 ~.,select(final_df,-PC1,-cluster))
  
  final_plot_df<-cluster_pca_scaled_df
  final_plot_df$cluster[final_plot_df$cluster==1]<-'Gold'
  final_plot_df$cluster[final_plot_df$cluster==2]<-'Bronze'
  final_plot_df$cluster[final_plot_df$cluster==3]<-'Platinum'
  final_plot_df$cluster[final_plot_df$cluster==4]<-'Silver'
  final_plot_df$cluster <- factor(final_plot_df$cluster, levels = c('Platinum','Gold','Silver','Bronze'))
  
  ggObj<- ggplot(final_plot_df, aes(PC1,PC2))+
    geom_point(aes(color = as.factor(cluster)),size=1) +
    labs(color="Customer Type") +
    guides(color = guide_legend(override.aes = list(size = 3)))

  output$plot <- renderPlot(ggObj)
  
  #Cluster4 tab

  output$txtout_age4 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("age")]))
  })
  
  output$txtout_ed4 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("Wages_n_salary")]))
  })
  
  output$txtout_cg4 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("capital_gains")]))
  })
  
  output$txtout_cl4 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==4,c("hours_per_week")]))
  })
  
  #Cluster1 tab
  
  output$txtout_age1 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("age")]))
  })
  
  output$txtout_ed1 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("Wages_n_salary")]))
  })
  
  output$txtout_cg1 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("capital_gains")]))
  })
  
  output$txtout_cl1 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==1,c("hours_per_week")]))
  })
  
  #Cluster2 tab
  
  output$txtout_age2 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("age")]))
  })
  
  output$txtout_ed2 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("Wages_n_salary")]))
  })
  
  output$txtout_cg2 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("capital_gains")]))
  })
  
  output$txtout_cl2 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==2,c("hours_per_week")]))
  })
  
  #Cluster3 tab
  
  output$txtout_age3 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("age")]))
  })
  
  output$txtout_ed3 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("Wages_n_salary")]))
  })
  
  output$txtout_cg3 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("capital_gains")]))
  })
  
  output$txtout_cl3 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==3,c("hours_per_week")]))
  })
  
  cluster_raw_chr_df<-select(cluster_raw_df,-c("hours_per_week","capital_gains","Wages_n_salary","age","fnlwgt"))
  
  ##Cluster1
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==1,-13]
  
  ggObj1 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
    geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
    facet_wrap(~key, scales = 'free_x') +
    theme_few() +
    theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot1 <- renderPlotly(ggplotly(ggObj1, width = 1200, height = 1200))
  
  ##Cluster2
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==2,-13]
  
  ggObj2 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
    geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
    facet_wrap(~key, scales = 'free_x') +
    theme_few() +
    theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot2 <- renderPlotly(ggplotly(ggObj2, width = 1200, height = 1200))
  
  ##Cluster3
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==3,-13]
  
  ggObj3 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
    geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
    facet_wrap(~key, scales = 'free_x') +
    theme_few() +
    theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot3 <- renderPlotly(ggplotly(ggObj3, width = 1200, height = 1200))
  
  ##Cluster4
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==4,-13]
  
  ggObj4 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
    geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
    facet_wrap(~key, scales = 'free_x') +
    theme_few() +
    theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot4 <- renderPlotly(ggplotly(ggObj4, width = 1200, height = 1200))
  
  observe({
    updateSelectInput(session, "var1", choices = unique(final_df$School_enrolment))
    updateSelectInput(session, "var2", choices = unique(final_df$sex))
    updateSelectInput(session, "var3", choices = unique(final_df$occupation))
    updateSelectInput(session, "var4", choices = unique(final_df$workclass))
    updateSelectInput(session, "var5", choices = unique(final_df$education))
    updateSelectInput(session, "var6", choices = unique(final_df$marital_status))
    updateSelectInput(session, "var7", choices = unique(final_df$relationship))
    updateSelectInput(session, "var8", choices = unique(final_df$race))
    updateSelectInput(session, "var9", choices = unique(final_df$native_country))
    updateSelectInput(session, "var14", choices = unique(final_df$metropoly_size))
    updateSelectInput(session, "var15", choices = unique(final_df$state))
    updateSelectInput(session, "var16", choices = unique(final_df$region))
  })
  
  observeEvent(input$update,{
  new_cust<-as.data.frame(t(c(input$var1,input$var2,input$var3,input$var4,input$var5,input$var6,input$var7,input$var8,input$var9,input$var10,input$var11,input$var12,input$var13,input$var14,input$var15,input$var16)))
  colnames(new_cust)<-colnames(final_df[,-c(17,18,19)])
  
  new_cust$age<-as.integer(new_cust$age)
  new_cust$Wages_n_salary<-as.integer(new_cust$Wages_n_salary)
  new_cust$capital_gains<-as.integer(new_cust$capital_gains)
  new_cust$hours_per_week<-as.integer(new_cust$hours_per_week)
  
  pred_PC1<-predict(pc1_model,new_cust)
  pred_PC2<-predict(pc2_model,new_cust)
  new_cust$PC1<-pred_PC1
  new_cust$PC2<-pred_PC2
  
  pred_cluster<-knn(final_df[,c("PC1","PC2")],new_cust[,c("PC1","PC2")],final_df[,"cluster"],k=30)
  new_cust$cluster<-pred_cluster
  
  ggObj_cust<-ggObj +
    geom_point(aes(x=new_cust$PC1,y=new_cust$PC2),colour="black", fill="darkred", shape=23, size=3)
  
  output$plot <- renderPlot(ggObj_cust)
  
  output$txtout_rec <- renderText({
    paste( "The input customer falls in cluster: ", ifelse(new_cust$cluster==1,"Gold",ifelse(new_cust$cluster==2,"Bronze",ifelse(new_cust$cluster==3,"Platinum","Silver"))))
  })
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)