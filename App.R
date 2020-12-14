library(Rsolnp)
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)

#Function construction
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
  
  if(setting$hick[1] == "No") {
    text <- paste("X equals to ", round(x_sol, digits = 3), " Y equals to ", round(y_sol, digits = 3), ". Check the graph if the solution is appropriate.")
    
    mylist <- list( "solution" = c(x_sol, y_sol), "text" = text)
    
    return(mylist)
  } else {
    
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
    
    text <- paste("X equals to ", round(x_sol, digits = 3), " Y equals to ", round(y_sol, digits = 3),
                  ". New X equals to ", round(newX_sol, digits =3), "New Y equals to ", round(newY_sol, digits = 3),
                  ". Substitution effect for x ", round(hick_x - x_sol, digits =3), ", for y ", round(hick_y - y_sol, digits= 3),
                  ". Income effect for x ", round(newX_sol - hick_x, digits =3), ", for y ", round(newY_sol - hick_y, digits =3),
                  ". Check the graph if the solution is appropriate.")
    
    mylist <- list( "solution" = c(x_sol,y_sol, newX_sol, newY_sol, hick_x, hick_y), "text" = text)
    
    return(mylist)
  }
}

graphing <- function(solution, setting) {
  
  u = solution[1]^setting$c[1]*solution[2]^setting$d[1]
  
  budget= function(x){-(setting$Px[1]*x-setting$I[1])/setting$Py[1]}
  
  ultility = function(x){(u/(x^(setting$c[1])))^(1/(setting$d[1]))}
  
  if(setting$hick[1] == "No") {
    
    #Create dataframe
    x <- seq(from = 0, to = setting$I[1]/setting$Px[1]*1.2, length.out = 10000)
    budgetline <- budget(x)
    indiffcurve <- ultility(x)
    data <- data.frame(x, budgetline, indiffcurve)
    
    # Base plot
    fig <- plot_ly(data, x = ~x) 
    fig <- fig %>% add_trace(y = ~budgetline, name = 'Budget Constaint',mode = 'lines')
    fig <- fig %>% add_trace(y = ~indiffcurve, name = 'Indifference Curve',mode = 'lines')
    fig <- fig %>% add_trace(x = ~c(round(solution[1], digits =3)), y = ~c(round(solution[2], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers')
    fig <- fig %>% layout(
      yaxis = list(title = "Quantity of Product Y", range = c(0,setting$I[1]/setting$Py[1]*1.2 )),
      legend = list(x = 0.9, y = 0.9),
      xaxis= list(title = "Quantity of Product X"),
      title = "Graphical Solution for Consumer Optimization")
    
  } else {
    newU = solution[3]^setting$c[1]*solution[4]^setting$d[1]
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[1]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    hickIncome = solution[6]*setting$Py[1] + solution[5]*setting$Px[2]
    
    hickBudget= function(x) {-(setting$Px[2]*x-hickIncome)/setting$Py[1]}
    
    x_intercept <- max(c(setting$I[1]/setting$Px[1], setting$I[2]/setting$Px[2], solution[5]))*1.2
    y_intercept <- max(c(setting$I[1]/setting$Py[1], setting$I[2]/setting$Py[1], solution[6]))*1.2
    
    #Create dataframe
    x <- seq(from = 0, to = x_intercept, length.out = 10000)
    budgetline <- budget(x)
    budgetline2 <- newBudget(x)
    indiffcurve <- ultility(x)
    indiffcurve2 <- newUltility(x)
    hicksline <- hickBudget(x)
    data <- data.frame(x, budgetline, budgetline2, indiffcurve, indiffcurve2, hicksline)
    
    # Base plot
    fig <- plot_ly(data, x = ~x)
    fig <- fig %>% add_trace(y = ~budgetline, name = 'Budget Constaint',mode = 'lines')
    fig <- fig %>% add_trace(y = ~budgetline2, name = 'New Budget Constaint',mode = 'lines')
    fig <- fig %>% add_trace(y = ~indiffcurve, name = 'Indifference Curve',mode = 'lines')
    fig <- fig %>% add_trace(y = ~indiffcurve2, name = 'New Indifference Curve',mode = 'lines')
    fig <- fig %>% add_trace(y = ~hicksline, name = 'Hicksian Compensated Demand',mode = 'lines')
    fig <- fig %>% add_trace(x = ~c(round(solution[1], digits =3)), y = ~c(round(solution[2], digits =3)), name = "Optimal Bundle", type = 'scatter', mode = 'markers')
    fig <- fig %>% add_trace(x = ~c(round(solution[3], digits =3)), y = ~c(round(solution[4], digits =3)), name = "New Optimal Bundle", type = 'scatter', mode = 'markers')
    fig <- fig %>% add_trace(x = ~c(round(solution[5], digits =3)), y = ~c(round(solution[6], digits =3)), name = "Hicksian Decomposition", type = 'scatter', mode = 'markers')
    fig <- fig %>% layout(
      yaxis = list(title = "Quantity of Product Y", range = c(0,y_intercept)),
      legend = list(x = 0.9, y = 0.9),
      xaxis= list(title = "Quantity of Product X"),
      title = "Graphical Solution for Consumer Optimization")
  }
}

# R shiny

ui <- {fluidPage(
  sliderInput("a", "Alpha Parameter:", min = 0, max =1, value = 0.5, step =0.005),
  numericInput("Px", "Price of x:", "", min = 0),
  numericInput("Py", "Price of y:", "", min = 0),
  numericInput("I", "Consumer's Income:", "", min = 0),
  numericInput("seed_x", "Seed value for x: ", 1, min = 0),
  numericInput("seed_y", "Seed value for y: ", 1, min = 0),
  
  selectInput("hick", "Draw Hicksian compensated demand (Optional):",
              c("No" = "No",
                "Yes" = "Yes")),
  numericInput("newPx", "Price of x for compensated demand:", 0, min = 0),
  numericInput("newI", "Consumer's Income for compensated demand:", 0, min = 0),
  
  actionButton("button", "Run"),
  
  plotlyOutput("graph"),
  textOutput("result")
)}

server <- function(input, output){
  
  settingA <- eventReactive(input$button,{ 
    setting(input$a,input$Px,input$Py,input$I,input$seed_x,input$seed_y,input$hick,input$newPx,input$newI)
  })
  
  A <- eventReactive(input$button,{
    consummer_optimization(settingA())
  })
  
  text <- reactive({ A()$text })
  
  graph <- reactive({ graphing(A()$solution, settingA()) })
  
  output$result <- renderText({text()})
  
  output$graph <- renderPlotly({graph()})
}

shinyApp(ui = ui, server = server)
