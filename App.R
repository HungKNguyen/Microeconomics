library(Rsolnp)
library(tidyverse)
library(shiny)
library(plotly)

#Function construction
x_sol <- 6.25 # Base solution
y_sol <- 6.25

setting <- function (a, Px, Py, I, seed_x, seed_y, hick, newPx, newI) {
  list("c" = a,"d" = 1-a,"Px" = c(Px, newPx),"Py" = Py,"I" = c(I, newI),"seed" = c(seed_x,seed_y),"hick" = hick)
}

consummer_optimization <- function(setting) {
  # Utility function u(x,y) = x^c*y^d
  
  ufn <- function(x) { 
    -x[1]^setting$c[1]*x[2]^setting$d[1]
  }
  
  # Budget Constraint
  
  minI <- 0 # Don't change, income always more than 0
  
  bline <- function(x){
    setting$I[1] = x[1]*setting$Px[1] + x[2]*setting$Py[1] # Budget line
    return(c(setting$I[1]))
  }
  
  solution <- solnp(c(setting$seed[1], setting$seed[2]), fun = ufn, ineqfun = bline, ineqLB = minI, ineqUB=setting$I[1], LB = c(0,0))
  
  x_sol <- solution$pars[1]
  y_sol <- solution$pars[2]
  
  switch(setting$hick[1],
    "NA" = {
    # text <- paste("X equals to ", round(x_sol, digits = 3), " Y equals to ", round(y_sol, digits = 3), ". Check the graph if the solution is appropriate.")
    
    
    
    text <- HTML(sprintf(
         "<h4>Problem:</h4> Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> <br/> <br/> <br/> <br/>",
                setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1], round(x_sol, digits = 3), round(y_sol, digits = 3)
              ))
            
    mylist <- list( "solution" = c(x_sol, y_sol), "text" = text)
    
    return(mylist)
  },
    "Hicksian" = {
       new_bline <- function(x){
       setting$I[2] = x[1]*setting$Px[2] + x[2]*setting$Py[1] # New Budget line
       return(c(setting$I[2]))
        }
     
       new_solution <- solnp(c(setting$seed[1], setting$seed[2]), fun = ufn, ineqfun = new_bline, ineqLB = minI, ineqUB=setting$I[2], LB = c(0,0))
       
       newX_sol = new_solution$pars[1]
       newY_sol = new_solution$pars[2]
       
       u = x_sol^setting$c[1]*y_sol^setting$d[1]
       
       alpha = (setting$Px[2] * setting$d[1])/(setting$Py[1] * setting$c[1])
       
       hick_x = (u/(alpha^setting$d[1]))^(1/(setting$d[1]+setting$c[1]))
       hick_y = alpha * hick_x
       
       text <- HTML(sprintf(
         "<h4>Problem:</h4> Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> 
          \\(x' = %s \\qquad y' = %s \\) <br/> 
          \\(x_{Hick} = %s \\qquad y_{Hick} = %s\\) <br/> <br/>
          <h4>Decomposition</h4>
          \\(Income \\ Effect \\ x = %s\\) <br/>
          \\(Substitution \\ Effect \\ x = %s\\) <br/>
          \\(Income \\ Effect \\ y = %s\\) <br/>
          \\(Substitution \\ Effect \\ y = %s\\) <br/> <br/> <br/> <br/> <br/>",
         setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1], 
         round(x_sol, digits = 3), round(y_sol, digits = 3), round(newX_sol, digits = 3), round(newY_sol, digits = 3),
         round(hick_x, digits = 3), round(hick_y, digits = 3),
         round(newX_sol-hick_x, digits = 3),
         round(newY_sol-hick_y, digits = 3),
         round(-x_sol+hick_x, digits = 3),
         round(-y_sol+hick_y, digits = 3)
       ))
       
       mylist <- list( "solution" = c(x_sol,y_sol, newX_sol, newY_sol, hick_x, hick_y), "text" = text)
       
       return(mylist)
      },
    "Slutsky" ={
      new_bline <- function(x){
        setting$I[2] = x[1]*setting$Px[2] + x[2]*setting$Py[1] # New Budget line
        return(c(setting$I[2]))
      }
      
      new_solution <- solnp(c(setting$seed[1], setting$seed[2]), fun = ufn, ineqfun = new_bline, ineqLB = minI, ineqUB=setting$I[2], LB = c(0,0))
      
      newX_sol = new_solution$pars[1]
      newY_sol = new_solution$pars[2]
      
      slutskyI = x_sol*setting$Px[2] + y_sol*setting$Py[1]
      
      hypobline <- function(x){
        slutskyI = x[1]*setting$Px[2] + x[2]*setting$Py[1] # Hypo Budget Line
        return(c(slutskyI))
      }
      
      ssky_solution <- solnp(c(setting$seed[1], setting$seed[2]), fun = ufn, ineqfun = hypobline, ineqLB = minI, ineqUB=slutskyI, LB = c(0,0))
      
      ssky_x = ssky_solution$pars[1]
      ssky_y = ssky_solution$pars[2]
      
      text <- HTML(sprintf(
        "<h4>Problem:</h4> Maximize &nbsp; \\(u = x^{%s}y^{%s}\\) &nbsp; &nbsp; with constraint &nbsp; \\(%s\\cdot x + %s\\cdot y = %s\\). <br/> <br/> 
          <h4>Solution:</h4> 
          \\(x = %s \\qquad y = %s\\) <br/> 
          \\(x' = %s \\qquad y' = %s \\) <br/> 
          \\(x_{Slutsky} = %s \\qquad y_{Slutsky} = %s\\) <br/> <br/>
          <h4>Decomposition</h4>
          \\(Income \\ Effect \\ x = %s\\) <br/>
          \\(Substitution \\ Effect \\ x = %s\\) <br/>
          \\(Income \\ Effect \\ y = %s\\) <br/>
          \\(Substitution \\ Effect \\ y = %s\\) <br/> <br/> <br/> <br/> <br/>",
        setting$c[1], setting$d[1], setting$Px[1], setting$Py[1], setting$I[1], 
        round(x_sol, digits = 3), round(y_sol, digits = 3), round(newX_sol, digits = 3), round(newY_sol, digits = 3),
        round(ssky_x, digits = 3), round(ssky_y, digits = 3),
        round(newX_sol-ssky_x, digits = 3),
        round(newY_sol-ssky_y, digits = 3),
        round(-x_sol+ssky_x, digits = 3),
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
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[1]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    hickIncome = solution[6]*setting$Py[1] + solution[5]*setting$Px[2]
    
    hickBudget= function(x) {-(setting$Px[2]*x-hickIncome)/setting$Py[1]}
  }
  
  if(setting$hick[1] == "Slutsky") {
    newU = solution[3]^setting$c[1]*solution[4]^setting$d[1]
    
    sskyU = solution[5]^setting$c[1]*solution[6]^setting$d[1]
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[1]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    slutskyIncome = solution[5]*setting$Px[2] + solution[6]*setting$Py[1]
    
    slutskyBudget= function(x) {-(setting$Px[2]*x-slutskyIncome)/setting$Py[1]}
    
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
      fig <- fig %>% add_trace(y = ~indiffcurve2, name = 'New Indifference Curve',mode = 'lines', hovertemplate = "X: %{x} <br>Y: %{y}",line = list(color = '91cf9d'))
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
                             sliderInput("alpha", "\\(\\alpha\\):",min = 0, max = 1, value = .5, step = 0.005),
                             
                             br(),
                             
                             helpText(h4("Budget  \\(I=P_x\\cdot x+P_y\\cdot y\\)")),
                             
                             
                             fluidRow(
                               column(3,
                                  numericInput(inputId = "minI", value = 0, label = "Min")
                                ),
                               column(6,
                                  sliderInput("I", "I:",min = 0, max = 250, value = 250)    
                                ),
                               column(3,
                                  numericInput(inputId = "maxI", value = 250, label = "Max")   
                                )
                             ),
                            
                             fluidRow(
                               column(3,
                                      numericInput(inputId = "minPx", value = 0, label = "Min \\(P_x\\)")
                               ),
                               column(6,
                                      sliderInput("Px", "\\(P_x\\):",min = 0, max = 20, value = 20)    
                               ),
                               column(3,
                                      numericInput(inputId = "maxPx", value = 20, label = "Max \\(P_x\\)")   
                               )
                             ),
                             
                             fluidRow(
                               column(3,
                                      numericInput(inputId = "minPy", value = 0, label = "Min \\(P_y\\)")
                               ),
                               column(6,
                                      sliderInput("Py", "\\(P_y\\):",min = 0, max = 20, value = 20)   
                               ),
                               column(3,
                                      numericInput(inputId = "maxPy", value = 20, label = "Max \\(P_y\\)") 
                               )
                             )
                          ),
                          
                          tabPanel("Compensated Demand", align = "center",
                             selectInput("hick", "Draw Income and Substitution Effect (Optional):", c("NA" = "NA", "Hicksian" = "Hicksian", "Slutsky" = "Slutsky")),
                             
                             br(),
                             
                             helpText(h4("New Budget  \\(I'=P'_x\\cdot x+P_y\\cdot y\\)")),
                             
                             fluidRow(
                               column(3,
                                      numericInput(inputId = "minNewPx", value = 0, label = "Min \\(P'_x\\)")
                               ),
                               column(6,
                                      sliderInput("NewPx", "\\(P'_x\\):",min = 0, max = 40, value = 40)
                               ),
                               column(3,
                                      numericInput(inputId = "maxNewPx", value = 40, label = "Max \\(P_x\\)")   
                               )
                             ),
                             
                             fluidRow(
                               column(3,
                                      numericInput(inputId = "minNewI", value = 0, label = "Min New I")
                               ),
                               column(6,
                                      sliderInput("NewI", "\\(I'\\):",min = 0, max = 250, value = 40)  
                               ),
                               column(3,
                                      numericInput(inputId = "maxNewI", value = 250, label = "Max New I")   
                               )
                             )
                          )
                        )
                      ),
                      
                      column(7,
                        plotlyOutput("graph"),
                        uiOutput("result")
                      )
                    )
                 )),
                 
<<<<<<< HEAD
                 tabPanel("Producer",
=======
                  tabPanel("Producer",
>>>>>>> 283697df3a3cbb4ec4814b76fb3be829ccc38804
                          helpText(h4("Coming Soon"))
                          ),
                 
                 tabPanel("Market",
<<<<<<< HEAD
                          helpText(h4("Coming Soon")))
=======
                          helpText(h4("Coming Soon"))
                          )
>>>>>>> 283697df3a3cbb4ec4814b76fb3be829ccc38804
)

server <- function(input, output, session) {
  
  observe({
    
    minI <- input$minI
    maxI <- input$maxI
    updateSliderInput(session, "I", value = maxI,
                      min = minI, max = maxI, step = 1)
    
    minPx <- input$minPx
    maxPx <- input$maxPx
    updateSliderInput(session, "Px", value = maxPx,
                      min = minPx, max = maxPx, step = 1)
    
    minPy <- input$minPy
    maxPy <- input$maxPy
    updateSliderInput(session, "Py", value = maxPy,
                      min = minPy, max = maxPy, step = 1)
    
    minNewPx <- input$minNewPx
    maxNewPx <- input$maxNewPx
    updateSliderInput(session, "NewPx", value = maxNewPx,
                      min = minNewPx, max = maxNewPx, step = 1)
    
    minNewI <- input$minNewI
    maxNewI <- input$maxNewI
    updateSliderInput(session, "NewI", value = maxNewI,
                      min = minNewI, max = maxNewI, step = 1)
    
  })
  
  settingA <- reactive({ 
    setting(input$alpha,input$Px,input$Py,input$I, x_sol, y_sol,input$hick,input$NewPx,input$NewI)
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
    max(c(input$maxI/(input$minPy+1), input$maxNewI/(input$minPy+1),
          input$I*12/input$Py, input$NewI*12/input$Py))/12
  })
  graph <- reactive({ graphing(A()$solution, settingA(), x_scale(), y_scale()) })
  
  output$graph <- renderPlotly({graph()})
  
  output$result <- renderUI({
    withMathJax(
      text()
    )})
}

shinyApp(ui = ui, server = server)
