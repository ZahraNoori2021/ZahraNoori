library(shiny)
library(ggplot2)
library(caret)
library(plsdepot)
library(plotly)

data = read.csv("data/ultrasound-drying-time.csv")

# Define UI ----
ui <- fluidPage(
  
  titlePanel("DS501-Case Study 3:***Linear Regression Prediction of Ultrasound Drying Time for Paper Samples***; Created by Zahra Noori"),

  
  sidebarLayout(
    
    sidebarPanel(
      "Please click the button below to select the data from the set for training the linear regression model.",
      br(),
      "*Note: equations and results will not appear until button is clicked to run the regression on first set of training data.",
      br(),
      br(),
      actionButton(inputId = "refresh", label = "New Random Set of Data"),
      br(),
      br(),
      br(),
      "Please select an initial DBMC and a WRV to see the predicted time for ultrasound drying:",
      br(),
      br(),
      sliderInput(inputId = "DBMC", label = "Choose an Initial DBMC (%)", value = 120, min = 100, max = 150),
      sliderInput(inputId = "WRV", label = "Choose a Water Retention Value (WRV)", value = 1.71, min = 1.5, max = 1.9),
      br(),
      textOutput("TimeDBMC"),
      br(),
      textOutput("TimeWRV"),
      br(),
      textOutput("Timeboth")
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Description",
                           h3("Collected Data"),
                           "The collected data comes from my own experiments on paper drying using ultrasound mechanism. The data set includes the total time of drying for different initial dry-basis moisture contents (DBMC) of the paper and different water retention values (WRV). The dry-basis moisture content is described by the ratio of the weight of water to the weight of the dry sample and water retention value is a measure that determines the ability of the paper to retain the water. It is important to be able to predict the relationship between the total time of drying and the initial DBMC and WRV in paper industry. However, it is very expensive to conduct the experiments for all the data points. Therefore, I have decided to use single variable and multiple variable linear regression methods to predict the behavior between the parameters. The data set includes the results of 23 experiments and there are three levels for initial DBMC and water retention value. The trend is very obvious and it predicts the total time for ultrasound drying. Using these results, the user can decide whether ultrasound drying is faster compare to their current technology or not. ",
                           br(),
                           br(),
                           "The current technology for paper drying is using hot air impinging jet nozzles, which is not energy efficient and it consumes 70% of the total energy in the industry, annually. Therefore, it is very important and necessary to develop new technologies that are more energy efficient and environment friendly. As a mechanical engineer who works in the field of heat and mass transfer phenomena, I am focused on developing new drying technologies for smart dryers. One part of my research is considering a novel drying method using ultrasound mechanism. In general, this technology applies the ultrasound vibrations to the wet sample and due to the atomization, the moisture can be removed from the paper. There are different effective parameters in the drying phenomena that need to be considered. Two of the most important parameters are the initial moisture content of the sample and the water retention value. My research has proved that ultrasound mechanism is more effective in higher moisture content levels. In addition, higher water retention values mean the ability of the fiber to retain the water is higher and therefore the drying time is higher. The outcome of this research is very important for paper industry.",
                           br(),
                           br(),
                           "To learn more about the novel drying technologies in our center, Center for Advanced Research in Drying (CARD), please visit our website at dryingresearch.org",
                           br(),
                           br(),
                           h3("Motivation"),
                           "Industrial drying consumes approximately 12 % of the total end-use energy used in
manufacturing, corresponding to 1.2 quads annually. The forest products industry sector is among
the highest energy consuming industry sectors, mainly after the chemicals. In paper making, the
drying process consumes the highest energy. Thus, any improvement in paper drying process may
have a significant impact on the energy consumption at the national level. Hence, in my research, I am developing a novel drying technology to reduce the energy consumption, globally. The results of this linear regression analysis would help industry to predict the total time of ultrasound drying at a given initial DBMC and WRV, in the range of the studied parameters.",
                           br(),
                           br(),
                           h3("Data Analysis"),
                           "R programming is used for the data analysis using simple built-in linear regression function. The libraries 'ggplot2' and 'plotly' are used for 2D and 3D visualization, respectively. The libraries 'caret' and 'plsdepot' are used for splitting the data into training and test sets to observe the effect of data on the regressions. The 'shiny' library is also utilized to build the interactive web interface so a user can interact with the data. To do the analysis, the user can simply click *New Random Set of Data*. The training data is specified to be 75% of the full data set. The user can click the button again at any time to randomly select new data points and observe the change in predictions. Two slider bars are present, allowing the user to pick values of initial DBMC and WRV and evaluate the predicted total time of drying. The 'lr' command is used to perform the linear regression on the training data. The 'lr' command returns coefficient for the linear regression equation, which are then used to plot the results.",
                           br(),
                           br(),
                           "Three plots are presented in the second tab for linear regression analysis. The first two plots are for the single variable linear regression, one for initial DBMC only and one for WRV only. These results assume that there is only one dependent variable on the total time of drying. The sliders will adjust the predicted point on the trend line to show the total time of drying. A similar plot is done in 3D for observing the effect of both initial DBMC and WRV on total time of drying. In the 3D plot, the trend is plotted as a surface, and the red prediction point moves along the surface based on the slider input. The graphic can also be used interactively to zoom in and rotate the view. The legend on the 3D plot also applies to the 2D plots. The numeric result of predicted total time of drying for each regression is also displayed in the left pane.",
                           br(),
                           br(),
                           "The third tab, model evaluation, includes one plot that compares the predicted value for total time of ultrasound drying with the reference value of total time of drying. If the slope of the plot is closer to 1, it means that the model prediction aligns well with the reference value. The linear regression is used to find an equation for describing the dependent variable (total time of drying) to the independent variables (initial DBMC and water retention value). This equation is a linear combination of coefficients and the independent variables. For the case of one single dependent variable and one single independent variable, the equations looksy as follows:",
                           #insert regression equation here
                           "$$y = a_{0} + a_{1}x$$",
                           "where $y$ is the dependent variable, $a_{0}$ is the intercept, $x$ is the independent variable, and $a_{1}$ is the coefficient of the independent variable that is descibing how y changes with x. This equation can be extended to n independent variables.",
                           "$$y = a_{0} + \\sum_{i=1}^{n}a_{n}x_{n}$$",
                           "The coefficients in the linear regression algorithm get solved by minimizing the residual, which means that the difference between the predicted value and the reference value in the data is minimal. The residual calculations are discussed in more details in the model evaluation tab. In this case study, we are looking to find the relationship between a dependent variable (total time of drying) and two independent variables (initial DBMC and water retention value). After the user clicks the bottun, a random training data selects from the data set and the linear regression results in the following equation:",
                           uiOutput("coef"),
                           "where DBMC is the initial dry-basis moisture content and WRV is the water retention value. The above equation is used to calculate the total time of ultrasound drying based on the slider inputs in the left pane. The same method is used when calculating correlations for each independent variable individually, and is used to produce the results on the 2D plots in the second tab. The positive coefficeints for initial DBMC and water retention value imply that total time of drying increases by increasing both initial DBMC and WRV.",
                           br(),
                           br(),
                           h3("Results"),
                           "While the resulting model produced by the regression changes depending on the test data, the overall trend remains the same. Total time of drying increases with increasing both the initial DBMC and WRV. The evaluation of the test data showcases that the linear regression model is a good choice for analysing this data set. It should be mentoned that the results of this linear regression model is more acceptable and more accurate in the range of the parameters studeid int his research. Using the results of this linear regressin analysis, the paper industry can predict the total time of drying using ultrasound mechanism at different initial DBMC and WRV.",
                           br(),
                           br(),
                           h3("Conclusions"),
                           "Prediction of the total time of drying as a function of the controlling parameters in the experiments is very important in order to be able to compare the efficiency of different drying technologies. It is also a beneficial statistical example of the strengths of the linear regression analysis. We can very clearly see that there is a trend based on initial DBMC and WRV. In the range of parameters in the data set, a simple method such as linear regression model is predicting the behavior of the data and the relationship between different parameters in a reasonable accuracy for the preliminary estimations in industry. However, the range of the available data is limited to a small range and therefore, the results of the linear regression model is only limited to this range of studied parameters.",
                           br()
                           ),
                  tabPanel("Linear Regression Analysis",
                           h4("Single Variable Linear Regression Plots"),
                           plotOutput("scatterdbmc"),
                           plotOutput("scatterwrv"),
                           h4("Multiple Variable Linear Regression Plot"),
                           plotlyOutput("both"),
                           ),
                  tabPanel("Model Evaluation",
                           withMathJax(),
                           br(),
                           "It seems that the results of multiple linear regression analysis is more accurate for the collected data set compare to the single variable linear regression analysis. Therefore, the model evaluation is performed on multiple variable linear regression both for initial DBMC and WRV. The plot below represents the predicted total time of drying from the linear regression vs the actual total time of drying. Since the linear model predicts continuously and the total time of drying is integer only, the values will almost never coincide perfectly.",
                           br(),
                           br(),
                           plotOutput("test"),
                           br(),
                           br(),
                          
                           "Using the predicted residual sum of squares (PRESS), we can evaluate how well the actual total time of drying coincide with the predicted total time of drying",
                           uiOutput('PRESS'),
                           "We can also evaluate the root mean squared error of the prediction (RMSEP)",
                           uiOutput('rmsep'),
                           "The total sum of squares (SST) can be used to find the value of $R^{2}$",
                           uiOutput('sst'),
                           uiOutput('rs'),
                           "Since the testing and training data is selected randomly, it is important to note that the values for $R^{2}$ can vary widely. However, $R^{2}$ for most of the selected data set is larger than 0.93 and it means that the linear regression model is an appropriate method for analysing this set of data. For some data sets, the $R^{2}$ has a low value, which means the prediction of linear regression analysis is very poor in those cases, which shows the limitations of this method"
                           
                           
                           )
                  
                  
                  )
    
              )
    
  )
)

# Define server logic ----
server <- function(input, output) {

  observeEvent(input$refresh,{
  #Subset into test and training data sets
  
  splitdata = caret::createDataPartition(data[,5], p=0.75, list=F, times=1)
  traindata = data[splitdata,]
  testdata =  data[-splitdata,] 
  
  
  
  #Linear regression on Initial DBMC
  lrt = lm(Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec. ~ Initial.DBMC..Percent., data=traindata)
  fitted(lrt)
  resid(lrt)
  lrt
  #Plot the full data set as a scatter plot for Initial DBMC
  p1 = qplot(data$Initial.DBMC..Percent.,data$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec.)+geom_point(size = 3,na.rm=T, color="green")+xlim(100,150)+ylim(80,170)
  #Plot training data set in a different color on top
  p2 = geom_point(aes(traindata$Initial.DBMC..Percent.,traindata$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec.),size=3,color="red", na.rm=T)
  
  #Linear regression on WRV
  lrp = lm(Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec. ~ Water.Retention.Value, data=traindata)
  fitted(lrp)
  resid(lrp)
  lrp
  
  #Plot the full data set as a scatter plot for WRV
  p3 = qplot(data$Water.Retention.Value,data$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec.)+geom_point(size = 3,na.rm=T, color="green")+xlim(1.5,1.9)+ylim(80,170)
  #Plot training data set in a different color on top
  p4 = geom_point(aes(traindata$Water.Retention.Value,traindata$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec.),size=3,color="red", na.rm=T)
  
  output$scatterdbmc = renderPlot({
    p1 + p2 + geom_abline(intercept = lrt[1]$coefficients[1], slope = lrt[1]$coefficients[2], color="black", size = 1, na.rm=T)+
    geom_point(aes(x=input$DBMC, y=lrt[1]$coefficients[2]*input$DBMC+lrt[1]$coefficients[1]), colour = "black", size = 5, na.rm=T)+
    xlab("Initial DBMC (%)")+
    ylab("Total Time of Ultrasound Drying (sec)")+
    ggtitle("Total Drying Time vs Initial DBMC")
    })
  
  output$scatterwrv = renderPlot({
    p3 + p4 + geom_abline(intercept = lrp[1]$coefficients[1], slope = lrp[1]$coefficients[2], color="black", size = 1)+
      geom_point(aes(x=input$WRV, y=lrp[1]$coefficients[2]*input$WRV+lrp[1]$coefficients[1]), colour = "black", size = 5)+
      xlab("Water Retention Value")+
      ylab("Total Time of Ultrasound Drying (sec)")+
      ggtitle("Total Drying Time vs Water Retention Value")
  })
  
  output$TimeDBMC = renderText({
    paste("Predicted Time of Drying at Independent Initial DBMC:", format(round(lrt[1]$coefficients[2]*input$DBMC+lrt[1]$coefficients[1],2),nsmall = 2))
    })
  
  output$TimeWRV = renderText({
    paste("Predicted Time of Drying at Independent Water Retention Value:", format(round(lrp[1]$coefficients[2]*input$WRV+lrp[1]$coefficients[1],2),nsmall = 2))
  })  
    #Make 3D scatter plot for both DBMC and WRV
    
    #Linear regression on both DBMC and WRV
    lrpt = lm( Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec. ~ Water.Retention.Value+Initial.DBMC..Percent., data=traindata)
    fitted(lrpt)
    resid(lrpt)
    lrpt
    
    #Prediction grid
    #DBMC = c(50:200)
    DBMC = data$Initial.DBMC..Percent.
    #WRV = seq(1, 2, by=.02)
    WRV = data$Water.Retention.Value
  #grid = expand.grid(DBMC,WRV)
    vals = (lrpt[1]$coefficients[1]+lrpt[1]$coefficients[3]*DBMC+lrpt[1]$coefficients[2]*WRV)
    Time_of_Drying = matrix(vals, nrow = length(DBMC), ncol=length(WRV))  

    
    
    
    #Plot the data plus the trendline
    output$both = renderPlotly({
        plot_ly(data,x=data$Initial.DBMC..Percent., y=data$Water.Retention.Value, z = data$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec.,type='scatter3d', mode='markers', name = "Test Data") %>%
        layout(scene=list(xaxis = list(title='Initial DBMC (Percent)',range=c(100,150)),
                        yaxis = list(title='Water Retention Value',range=c(1, 2)),
                        zaxis = list(title='Total Time of Ultrasound Drying',range=c(50,200)))) %>%
        add_trace(x=traindata$Initial.DBMC..Percent., y=traindata$Water.Retention.Value, z=traindata$Total.time.of.Paper.Drying.Using.Ultrasound.Mechanism..sec., type='scatter3d', mode='markers', name = "Training Data") %>%
       # add_surface(x=~WRV,y=~DBMC, z=~TimeofDrying) %>%
        
        add_surface(data,x=data$Initial.DBMC..Percent., y=data$Water.Retention.Value, z = ~Time_of_Drying) %>%
        
        add_trace(x=input$DBMC, y=input$WRV, z=lrpt[1]$coefficients[1]+lrpt[1]$coefficients[2]*input$WRV+lrpt[1]$coefficients[3]*input$DBMC, type='scatter3d', mode='markers', name = "Predicted Point")
    
    
    })
    
    output$Timeboth = renderText({
      paste("Predicted Time of Drying at Combined Initial DBMC and WRV:", format(lrpt[1]$coefficients[1]+lrpt[1]$coefficients[2]*input$WRV+lrpt[1]$coefficients[3]*input$DBMC))
    })  
    
    #Model evaluation parameters
    prediction = data.frame(predict(lrpt, testdata))
    names(prediction)[1] = 'Predicted'
    prediction$Reference = testdata[,2]
    p6 = qplot(Reference, Predicted, data = prediction) + geom_point(size = 3) + ggtitle("Predicted Value vs Reference Test Data Value")
    output$test = renderPlot(p6)
    
    #PRESS
    PRESS = sum((prediction$Reference - prediction$Predicted)^2)
      
    output$PRESS = renderUI({
      withMathJax(sprintf("$$PRESS = \\sum_{i=1}^{n} (y^{ref}_{i}-y^{pred}_{i})^{2} = %.03f$$", PRESS))
    })
    
    #RMSEP
    RMSEP = sqrt(PRESS/ nrow(prediction))
    
    output$rmsep = renderUI({
      withMathJax(sprintf("$$RMSEP = \\sqrt{\\frac{1}{n_{T}}\\sum_{1}^{n_{T}} (y_{i}^{ref} - y_{i}^{pred})^{2}} = %.03f$$", RMSEP))
    })
    
    #SST
    SST = sum((prediction$Reference - mean(prediction$Reference))^2)
    
    output$sst = renderUI({
      withMathJax(sprintf("$$SST = \\sum_{i=1}^{n} (y^{ref}_{i}-y^{mean}_{i})^{2} = %.03f$$", SST))
    })
    
    #R-squared
    rsquared = 1 - (PRESS/SST)
    
    output$rs = renderUI({
      withMathJax(sprintf("$$R^{2} = 1 - \\frac{PRESS}{SST} = %.03f$$", rsquared))
    })
    
    #Equation coefficients
    output$coef = renderUI({
      withMathJax(sprintf("$$Time-of-Drying = (%.03f)_{0} + (%.03f)_{DBMC} DBMC + (%.03f)_{WRV} WRV$$",lrpt[1]$coefficients[1],lrpt[1]$coefficients[3],lrpt[1]$coefficients[2]))
    })
    
  })
  
  
  }

# Run the app ----
shinyApp(ui = ui, server = server)