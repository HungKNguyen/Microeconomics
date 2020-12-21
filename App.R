library(shiny)
library(plotly)

#Function construction

setting <- function(a, Px, Py, I, hick, newI, newPx, newPy) {
  list("c" = a,"d" = 1-a,"Px" = c(Px, newPx),"Py" = c(Py, newPy),"I" = c(I, newI),"hick" = hick)
}

consummer_optimization <- function(setting) {
  
  x_sol <- (setting$I[1]*setting$c[1])/setting$Px[1]
  y_sol <- (setting$I[1]*setting$d[1])/setting$Py[1]
  
  switch(setting$hick[1],
    "NA" = {
    
    text <- HTML(sprintf(
         "<h4>Problem:</h4> Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> <br/> <br/>",
                setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1], round(x_sol, digits = 3), round(y_sol, digits = 3)
              ))
            
    mylist <- list( "solution" = c(x_sol, y_sol), "text" = text)
    
    return(mylist)
  },
    "Hicksian" = {
      
       newX_sol = (setting$I[2]*setting$c[1])/setting$Px[2]
       newY_sol = (setting$I[2]*setting$d[1])/setting$Py[2]
       
       u = x_sol^setting$c[1]*y_sol^setting$d[1]
       
       alpha = (setting$Px[2] * setting$d[1])/(setting$Py[2] * setting$c[1])
       
       hick_x = (u/(alpha^setting$d[1]))^(1/(setting$d[1]+setting$c[1]))
       hick_y = alpha * hick_x
       
       text <- HTML(sprintf(
         "<h4>Problem:</h4> 
         Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/>
         New constraint of &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> 
          \\(x' = %s \\qquad y' = %s \\) <br/> 
          \\(x_{Hick} = %s \\qquad y_{Hick} = %s\\) <br/> <br/>
          <h4>Decomposition</h4>
          \\(Income \\ Effect \\ x = %s\\) <br/>
          \\(Substitution \\ Effect \\ x = %s\\) <br/>
          \\(Income \\ Effect \\ y = %s\\) <br/>
          \\(Substitution \\ Effect \\ y = %s\\) <br/> <br/> <br/>",
         setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1],
         setting$Px[2], setting$Py[2], setting$I[2],
         round(x_sol, digits = 3), round(y_sol, digits = 3), round(newX_sol, digits = 3), round(newY_sol, digits = 3),
         round(hick_x, digits = 3), round(hick_y, digits = 3),
         round(newX_sol-hick_x, digits = 3),
         round(-x_sol+hick_x, digits = 3),
         round(newY_sol-hick_y, digits = 3),
         round(-y_sol+hick_y, digits = 3)
       ))
       
       mylist <- list( "solution" = c(x_sol,y_sol, newX_sol, newY_sol, hick_x, hick_y), "text" = text)
       
       return(mylist)
      },
    "Slutsky" ={
      
      newX_sol = (setting$I[2]*setting$c[1])/setting$Px[2]
      newY_sol = (setting$I[2]*setting$d[1])/setting$Py[2]
      
      slutskyI = x_sol*setting$Px[2] + y_sol*setting$Py[2]
      
      ssky_x = (slutskyI*setting$c[1])/setting$Px[2]
      ssky_y = (slutskyI*setting$d[1])/setting$Py[2]
      
      text <- HTML(sprintf(
        "<h4>Problem:</h4> 
        Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> 
        New constraint of &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> 
          \\(x' = %s \\qquad y' = %s \\) <br/> 
          \\(x_{Slutsky} = %s \\qquad y_{Slutsky} = %s\\) <br/> <br/>
          <h4>Decomposition</h4>
          \\(Income \\ Effect \\ x = %s\\) <br/>
          \\(Substitution \\ Effect \\ x = %s\\) <br/>
          \\(Income \\ Effect \\ y = %s\\) <br/>
          \\(Substitution \\ Effect \\ y = %s\\) <br/> <br/> <br/>",
        setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1], 
        setting$Px[2], setting$Py[2], setting$I[2],
        round(x_sol, digits = 3), round(y_sol, digits = 3), round(newX_sol, digits = 3), round(newY_sol, digits = 3),
        round(ssky_x, digits = 3), round(ssky_y, digits = 3),
        round(newX_sol-ssky_x, digits = 3),
        round(-x_sol+ssky_x, digits = 3),
        round(newY_sol-ssky_y, digits = 3),
        round(-y_sol+ssky_y, digits = 3)
      ))
      
      mylist <- list( "solution" = c(x_sol,y_sol, newX_sol, newY_sol, ssky_x, ssky_y), "text" = text)
    }
  )
}

graphing <- function(solution, setting, x_scale, y_scale) {
  
  u = solution[1]^setting$c[1]*solution[2]^setting$d[1]
  
  budget= function(x){-(setting$Px[1]*x-setting$I[1])/setting$Py[1]}
  
  ultility = function(x){(u/(x^(setting$c[1])))^(1/(setting$d[1]))}
  
  if(setting$hick[1] == "Hicksian") {
    newU = solution[3]^setting$c[1]*solution[4]^setting$d[1]
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[2]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    hickIncome = solution[6]*setting$Py[2] + solution[5]*setting$Px[2]
    
    hickBudget= function(x) {-(setting$Px[2]*x-hickIncome)/setting$Py[2]}
  }
  
  if(setting$hick[1] == "Slutsky") {
    newU = solution[3]^setting$c[1]*solution[4]^setting$d[1]
    
    sskyU = solution[5]^setting$c[1]*solution[6]^setting$d[1]
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[2]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    slutskyIncome = solution[5]*setting$Px[2] + solution[6]*setting$Py[2]
    
    slutskyBudget= function(x) {-(setting$Px[2]*x-slutskyIncome)/setting$Py[2]}
    
    sskyUltility = function(x){(sskyU/(x^(setting$c[1])))^(1/(setting$d[1]))}
  }
  
  # x_intercept <- max(c(setting$I[1]/setting$Px[1], setting$I[2]/setting$Px[2] + solution[5]*0, solution[5]), na.rm = TRUE)*1.2
  # y_intercept <- max(c(setting$I[1]/setting$Py[1], setting$I[2]/setting$Py[1] + solution[6]*0, solution[6]), na.rm = TRUE)*1.2
    
    #Create dataframe
    x <- seq(from = 0, to = x_scale, length.out = 10000)
    budgetline <- budget(x)
    indiffcurve <- ultility(x)
    switch(setting$hick[1],
           "NA" = {
             data <- data.frame(x, budgetline, indiffcurve)
           },
           "Hicksian" = {
             budgetline2 <- newBudget(x)
             indiffcurve2 <- newUltility(x)
             hicksline <- hickBudget(x)
             data <- data.frame(x, budgetline, budgetline2, indiffcurve, indiffcurve2, hicksline)
           },
           "Slutsky" = {
             budgetline2 <- newBudget(x)
             indiffcurve2 <- newUltility(x)
             sskyline <- slutskyBudget(x)
             indiffcurve3 <- sskyUltility(x)
             data <- data.frame(x, budgetline, budgetline2, indiffcurve, indiffcurve2, sskyline, indiffcurve3)
           }
    )
    
    # Base plot
    fig <- plot_ly(data, x = ~x) 
    fig <- fig %>% add_trace(y = ~budgetline, name = 'Budget Constaint',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#66c7fa'))
    fig <- fig %>% add_trace(y = ~indiffcurve, name = 'Indifference Curve',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#aadffc'))
    fig <- fig %>% add_trace(x = ~c(round(solution[1], digits =3)), y = ~c(round(solution[2], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#1f4051'), showlegend = FALSE)
    if(setting$hick[1] == "Hicksian") {
      fig <- fig %>% add_trace(y = ~budgetline2, name = 'New Budget Constaint',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#33b34c'))
      fig <- fig %>% add_trace(y = ~indiffcurve2, name = 'New Indifference Curve',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#91cf9d'))
      fig <- fig %>% add_trace(y = ~hicksline, name = 'Hicksian Compensated Demand',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668'))
      fig <- fig %>% add_trace(x = ~c(round(solution[3], digits =3)), y = ~c(round(solution[4], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#344b38'), showlegend = FALSE)
      fig <- fig %>% add_trace(x = ~c(round(solution[5], digits =3)), y = ~c(round(solution[6], digits =3)), name = "Hicksian Decomposition", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#8a3435'), showlegend = FALSE)
    }
    if(setting$hick[1] == "Slutsky") {
      fig <- fig %>% add_trace(y = ~budgetline2, name = 'New Budget Constaint',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#33b34c'))
      fig <- fig %>% add_trace(y = ~indiffcurve2, name = 'New Indifference Curve',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#91cf9d'))
      fig <- fig %>% add_trace(y = ~sskyline, name = 'Slutsky Compensated Demand',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668'))
      fig <- fig %>% add_trace(y = ~indiffcurve3, name = 'Decomposition Indifference Curve',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#aadffc', dash = 'dash'))
      fig <- fig %>% add_trace(x = ~c(round(solution[3], digits =3)), y = ~c(round(solution[4], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#344b38'), showlegend = FALSE)
      fig <- fig %>% add_trace(x = ~c(round(solution[5], digits =3)), y = ~c(round(solution[6], digits =3)), name = "Slutsky's Decomposition", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#8a3435'), showlegend = FALSE)
    }
    fig <- fig %>% layout(
      yaxis = list(title = "Quantity of Product Y", range = c(0,y_scale)),
      legend = (list(orientation = 'h',
                     xanchor = "center",
                     x = 0.5,
                     y= -0.25)
                ),
      xaxis= list(title = "Quantity of Product X"))
}

subplots <- function(solution, setting, x_scale, y_scale, IOC_scale, POC_scale1, POC_scale2) {
  scale <- max(c(x_scale, y_scale))
  POC_scale <- max(c(POC_scale1, POC_scale2))
  
  # IOC curves
  IOCX = function(x){(x*setting$Px[1])/setting$c[1]}
  NewIOCX = function(x){(x*setting$Px[2])/setting$c[1]}
  IOCY = function(x){(x*setting$Py[1])/setting$d[1]}
  NewIOCY = function(x){(x*setting$Py[2])/setting$d[1]}
  # POC curves
  POCX = function(x){(setting$I[1]*setting$c[1])/x}
  NewPOCX = function(x){(setting$I[2]*setting$c[1])/x}
  POCY = function(x){(setting$I[1]*setting$d[1])/x}
  NewPOCY = function(x){(setting$I[2]*setting$d[1])/x}
  
  #Create dataframe
  x <- seq(from = 0, to = scale, length.out = 10000)
  IOClineX <- IOCX(x)
  IOClineY <- IOCY(x)
  POClineX <- POCX(x)
  POClineY <- POCY(x)
  NewIOClineX <- NewIOCX(x)
  NewIOClineY <- NewIOCY(x)
  NewPOClineX <- NewPOCX(x)
  NewPOClineY <- NewPOCY(x)
  data <- data.frame(x, IOClineX, IOClineY, POClineX, POClineY,
                     NewIOClineX, NewIOClineY, NewPOClineX, NewPOClineY)
  
  #IOCs
  IOCs <- plot_ly(data, x = ~x)
  IOCs <- IOCs %>% add_trace(y = ~IOClineX, name = 'Income offer curve for product X',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#66c7fa'))
  IOCs <- IOCs %>% add_trace(y = ~IOClineY, name = 'Income offer curve for product Y',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668'))
  IOCs <- IOCs %>% add_trace(x = ~c(round(solution[1], digits =3)), y = ~c(round(setting$I[1], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#1f4051'), showlegend = FALSE)
  IOCs <- IOCs %>% add_trace(x = ~c(round(solution[2], digits =3)), y = ~c(round(setting$I[1], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#8a3435'), showlegend = FALSE)
  if(setting$hick[1] != "NA") {
    IOCs <- IOCs %>% add_trace(y = ~NewIOClineX, name = 'New income offer curve for product X',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#66c7fa', dash = 'dash'))
    IOCs <- IOCs %>% add_trace(y = ~NewIOClineY, name = 'New income offer curve for product Y',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668', dash = 'dash'))
    IOCs <- IOCs %>% add_trace(x = ~c(round(solution[3], digits =3)), y = ~c(round(setting$I[2], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#98cae3', line = list(color = '#1f4051', width =1)), showlegend = FALSE)
    IOCs <- IOCs %>% add_trace(x = ~c(round(solution[4], digits =3)), y = ~c(round(setting$I[2], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#edabac', line = list(color = '#8a3435', width =1)), showlegend = FALSE)
  }
  IOCs <- IOCs %>% layout(
    yaxis = list(title = "Income", range = c(0,IOC_scale)),
    legend = (list(orientation = 'h',
                   xanchor = "center",
                   x = 0.5,
                   y= -0.25)
    ),
    xaxis= list(title = "Quantity of Product X"))
  
  #POCs
  POCs <- plot_ly(data, x = ~x)
  POCs <- POCs %>% add_trace(y = ~POClineX, name = 'Price offer curve for product X',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#66c7fa'))
  POCs <- POCs %>% add_trace(y = ~POClineY, name = 'Price offer curve for product Y',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668'), yaxis = "y2")
  POCs <- POCs %>% add_trace(x = ~c(round(solution[1], digits =3)), y = ~c(round(setting$Px[1], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#1f4051'), showlegend = FALSE)
  POCs <- POCs %>% add_trace(x = ~c(round(solution[2], digits =3)), y = ~c(round(setting$Py[1], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#8a3435'), showlegend = FALSE, yaxis = "y2")
  if(setting$hick[1] != "NA") {
    POCs <- POCs %>% add_trace(y = ~NewPOClineX, name = 'New price offer curve for product X',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#66c7fa', dash = 'dash'))
    POCs <- POCs %>% add_trace(y = ~NewPOClineY, name = 'New price offer curve for product Y',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '#f96668', dash = 'dash'), yaxis = "y2")
    POCs <- POCs %>% add_trace(x = ~c(round(solution[3], digits =3)), y = ~c(round(setting$Px[2], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#98cae3', line = list(color = '#1f4051', width =1)), showlegend = FALSE)
    POCs <- POCs %>% add_trace(x = ~c(round(solution[4], digits =3)), y = ~c(round(setting$Py[2], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers', hovertemplate = "X: %{x} <br>Y: %{y}",marker = list(color = '#edabac', line = list(color = '#8a3435', width =1)), showlegend = FALSE, yaxis = "y2")
  }
  POCs <- POCs %>% layout(
    yaxis = list(title = "Price of Product X", range = c(0,POC_scale)),
    yaxis2 = list(title = "Price of Product Y",
                  overlaying = "y",
                  side = "right",
                  range = c(0,POC_scale),
                  automargin = TRUE
                  ),
    legend = (list(orientation = 'h',
                   xanchor = "center",
                   x = 0.5,
                   y= -0.25)
    ),
    xaxis= list(title = "Quantity of Products "))
  
  mylist <- list("IOCs" = IOCs, "POCs" = POCs)
}

# R shiny

ui <- navbarPage("MicroEconomics by Luke and Hung",
                 tabPanel("Consumer", fluidPage(
                   
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
                   ),
                   
                   titlePanel("Utility Maximization: Cobb-Douglas & Compensated Demands"),
                   
                   fluidRow(
                      column(5, 
                       tabsetPanel(type = "tabs",
                          tabPanel("Utility Maximization & Constraint", align = "center",
                             withMathJax(),
                             helpText(h4("Utility  \\(u(x,y)=x^\\alpha y^{1-\\alpha}\\)")),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("alpha", "\\(\\alpha\\):",min = 0, max = 1, value = .3, step = 1/200)    
                               ),
                               column(3,
                                      checkboxInput("settingalpha", "Advance Setting")
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingalpha == true",
                               column(4,
                                      numericInput(inputId = "minalpha", value = 0, label = "Min \\(\\alpha\\)")
                               ),
                               column(4,
                                      numericInput(inputId = "maxalpha", value = 1, label = "Max \\(\\alpha\\)")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusalpha", value = .3, label = "Value of \\(\\alpha\\)")   
                               )
                             ),
                             
                             br(),
                             
                             helpText(h4("Budget  \\(I=P_x\\cdot x+P_y\\cdot y\\)")),
                             
                             fluidRow(
                               column(9,
                                  sliderInput("I", "I:",min = 0, max = 250, value = 125, step = 250/200)    
                                ),
                               column(3,
                                  checkboxInput("settingI", "Advance Setting")
                                )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingI == true",
                               column(4,
                                      numericInput(inputId = "minI", value = 0, label = "Min I")
                               ),
                               column(4,
                                      numericInput(inputId = "maxI", value = 250, label = "Max I")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusI", value = 125, label = "Value of I")   
                               )
                             ),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("Px", "\\(P_x\\):",min = 0, max = 20, value = 10, step = 20/200)    
                               ),
                               column(3,
                                      checkboxInput("settingPx", "Advance Setting")
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingPx == true",
                               column(4,
                                      numericInput(inputId = "minPx", value = 0, label = "Min \\(P_x\\)")
                               ),
                               column(4,
                                      numericInput(inputId = "maxPx", value = 20, label = "Max \\(P_x\\)")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusPx", value = 10, label = "Value of \\(P_x\\)")   
                               )
                             ),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("Py", "\\(P_y\\):",min = 0, max = 20, value = 10, step = 20/200)   
                               ),
                               column(3,
                                      checkboxInput("settingPy", "Advance Setting") 
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingPy == true",
                               column(4,
                                      numericInput(inputId = "minPy", value = 0, label = "Min \\(P_y\\)")
                               ),
                               column(4,
                                      numericInput(inputId = "maxPy", value = 20, label = "Max \\(P_y\\)")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusPy", value = 10, label = "Value of \\(P_y\\)")   
                               )
                             )
                          ),
                          
                          tabPanel("Compensated Demand", align = "center",
                             selectInput("hick", "Draw Income and Substitution Effect (Optional):", c("NA" = "NA", "Hicksian" = "Hicksian", "Slutsky" = "Slutsky")),
                             
                             br(),
                             
                             helpText(h4("New Budget  \\(I'=P'_x\\cdot x+P_y\\cdot y\\)")),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("NewI", "\\(I'\\):",min = 0, max = 250, value = 125, step = 250/200)  
                               ),
                               column(3,
                                      checkboxInput("settingNewI", "Advance Setting") 
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingNewI == true",
                               column(4,
                                      numericInput(inputId = "minNewI", value = 0, label = "Min I'")
                               ),
                               column(4,
                                      numericInput(inputId = "maxNewI", value = 250, label = "Max I'")    
                               ),
                               column(4,
                                      numericInput(inputId = "cusNewI", value = 125, label = "Value of I'")   
                               )
                             ),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("NewPx", "\\(P'_x\\):",min = 0, max = 40, value = 20, step = 40/200)
                               ),
                               column(3,
                                      checkboxInput("settingNewPx", "Advance Setting")   
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingNewPx == true",
                               column(4,
                                      numericInput(inputId = "minNewPx", value = 0, label = "Min \\(P'_x\\)")
                               ),
                               column(4,
                                      numericInput(inputId = "maxNewPx", value = 20, label = "Max \\(P'_x\\)")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusNewPx", value = 10, label = "Value of \\(P'_x\\)")   
                               )
                             ),
                             
                             fluidRow(
                               column(9,
                                      sliderInput("NewPy", "\\(P'_y\\):",min = 0, max = 20, value = 10, step = 20/200)
                               ),
                               column(3,
                                      checkboxInput("settingNewPy", "Advance Setting")   
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.settingNewPy == true",
                               column(4,
                                      numericInput(inputId = "minNewPy", value = 0, label = "Min \\(P'_y\\)")
                               ),
                               column(4,
                                      numericInput(inputId = "maxNewPy", value = 20, label = "Max \\(P'_y\\)")   
                               ),
                               column(4,
                                      numericInput(inputId = "cusNewPy", value = 10, label = "Value of \\(P'_y\\)")   
                               )
                             ),
                          )
                        )
                      ),
                      
                      column(7,
                        plotlyOutput("graph"),
                        uiOutput("result")
                      )
                    ),
                   h4("Income offer curves and Price offer curves"),
                   
                   fluidRow(
                     column(6,
                            plotlyOutput("IOCs")
                            ),
                     column(6,
                            plotlyOutput("POCs")
                            )
                   ),
                   br(),
                   br()
                 )),
                 
                  tabPanel("Producer",
                          helpText(h4("Coming Soon"))
                          ),
                 
                 tabPanel("Market",
                          helpText(h4("Coming Soon"))
                          )
)

server <- function(input, output, session) {
  
  observe({
    
    minalpha <- input$minalpha
    maxalpha <- input$maxalpha
    cusalpha <- input$cusalpha
    updateSliderInput(session, "alpha", value = cusalpha,
                      min = minalpha, max = maxalpha, step = (maxalpha - minalpha)/200,)
    
    minI <- input$minI
    maxI <- input$maxI
    cusI <- input$cusI
    updateSliderInput(session, "I", value = cusI,
                      min = minI, max = maxI, step = (maxI - minI)/200,)
    
    minPx <- input$minPx
    maxPx <- input$maxPx
    cusPx <- input$cusPx
    updateSliderInput(session, "Px", value = cusPx,
                      min = minPx, max = maxPx, step = (maxPx - minPx)/200,)
    
    minPy <- input$minPy
    maxPy <- input$maxPy
    cusPy <- input$cusPy
    updateSliderInput(session, "Py", value = cusPy,
                      min = minPy, max = maxPy, step = (maxPy - minPy)/200,)
    
    minNewI <- input$minNewI
    maxNewI <- input$maxNewI
    cusNewI <- input$cusNewI
    updateSliderInput(session, "NewI", value = cusNewI,
                      min = minNewI, max = maxNewI, step = (maxNewI - minNewI)/200,)
    
    minNewPx <- input$minNewPx
    maxNewPx <- input$maxNewPx
    cusNewPx <- input$cusNewPx
    updateSliderInput(session, "NewPx", value = cusNewPx,
                      min = minNewPx, max = maxNewPx, step = (maxNewPx - minNewPx)/200,)
    
    minNewPy <- input$minNewPy
    maxNewPy <- input$maxNewPy
    cusNewPy <- input$cusNewPy
    updateSliderInput(session, "NewPy", value = cusNewPy,
                      min = minNewPy, max = maxNewPy, step = (maxNewPy - minNewPy)/200,)
    
  })
  
  settingA <- reactive({ 
    setting(input$alpha,input$Px,input$Py,input$I,input$hick,input$NewI,input$NewPx,input$NewPy)
  })
  
  A <- reactive({
    consummer_optimization(settingA())
  })
  
  text <- reactive({ A()$text })
  
  x_scale <- reactive({
    max(c(input$maxI/(input$minPx+1), input$maxI/(input$minNewPx+1),
          input$maxNewI/(input$minPx+1), input$maxNewI/(input$minNewPx+1),
          input$I*12/input$Px, input$NewI*12/input$Px,
          input$I*12/input$NewPx, input$NewI*12/input$NewPx))/12
  })
  y_scale <- reactive({
    max(c(input$maxI/(input$minPy+1), input$maxI/(input$minNewPy+1),
          input$maxNewI/(input$minPy+1), input$maxNewI/(input$minNewPy+1),
          input$I*12/input$Py, input$NewI*12/input$Py,
          input$I*12/input$NewPy, input$NewI*12/input$NewPy))/12
  })
  graph <- reactive({ graphing(A()$solution, settingA(), x_scale(), y_scale()) })
  
  output$graph <- renderPlotly({graph()})
  
  output$result <- renderUI({
    withMathJax(
      text()
    )})
  
  sub <- reactive({ subplots(A()$solution, settingA(), x_scale(), y_scale(),input$maxI, input$maxPx, input$maxPy) })
  
  output$IOCs <- renderPlotly({sub()$IOCs})
  output$POCs <- renderPlotly({sub()$POCs})
}

shinyApp(ui = ui, server = server)
