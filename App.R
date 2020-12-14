library(Rsolnp)
library(tidyverse)
library(ggplot2)
library(shiny)

#Function construction
setting <- function (c, d, Px, Py, I, seed_x, seed_y, hick, newPx, newI) {
  list("c" = c,"d" = d,"Px" = c(Px, newPx),"Py" = Py,"I" = c(I, newI),"seed" = c(seed_x,seed_y),"hick" = hick)
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
    
    coords = sprintf("(%s , %s)", format(round(solution[1], digits = 3)), format(round(solution[2], digits = 3)))
    
    print(ggplot(data.frame(x=c(0,setting$I[1]/setting$Px[1]*1.2)), aes(x)) + 
            stat_function(fun=budget, color = "red", size = 1) +
            stat_function(fun=ultility, color = "blue", size = 1) +
            ylim(0,setting$I[1]/setting$Py[1]*1.2) +
            geom_point(x=c(solution[1]), y =c(solution[2]), size =2) +
            geom_label(aes(solution[1], solution[2]-(setting$I[1]/setting$Py[1]*0.1), label=coords), alpha = 0.2) +
            theme_minimal() +
            ggtitle("Visual for the Solution")) +
      xlab("Quantity of product X") +
      ylab("Qunatity of product Y")
  } else {
    newU = solution[3]^setting$c[1]*solution[4]^setting$d[1]
    
    newBudget= function(x) {-(setting$Px[2]*x-setting$I[2])/setting$Py[1]}
    
    newUltility = function(x){(newU/(x^(setting$c[1])))^(1/(setting$d[1]))}
    
    hick_Income = solution[6]*setting$Py[1] + solution[5]*setting$Px[2]
    
    hickBudget= function(x) {-(setting$Px[2]*x-hick_Income)/setting$Py[1]}
    
    coords_sol = sprintf("(%s , %s)", format(round(solution[1], digits = 3)), format(round(solution[2], digits = 3)))
    coords_new = sprintf("(%s , %s)", format(round(solution[3], digits = 3)), format(round(solution[4], digits = 3)))
    coords_hick = sprintf("(%s , %s)", format(round(solution[5], digits = 3)), format(round(solution[6], digits = 3)))
    
    x_intercept <- max(c(setting$I[1]/setting$Px[1]*1.2, setting$I[2]/setting$Px[2]*1.5))
    y_intercept <- max(c(setting$I[1]/setting$Py[1]*1.2, setting$I[2]/setting$Py*1.5))
    
    print(ggplot(data.frame(x=c(0,x_intercept)), aes(x)) + 
            stat_function(fun=budget, color = "red", size = 1) +
            stat_function(fun=newBudget, color = "orange", size = 1) +
            stat_function(fun=ultility, color = "blue", size = 1) +
            stat_function(fun=newUltility, color = "green", size = 1) +
            stat_function(fun=hickBudget, color = "black", size = 1) +
            ylim(0,y_intercept) +
            geom_point(x=c(solution[1]), y =c(solution[2]), size =2) +
            geom_point(x=c(solution[3]), y =c(solution[4]), size =2) +
            geom_point(x=c(solution[5]), y =c(solution[6]), size =2) +
            geom_label(aes(solution[1], solution[2]-(y_intercept*0.07), label=coords_sol), alpha = 0.2) +
            geom_label(aes(solution[3], solution[4]-(y_intercept*0.07), label=coords_new), alpha = 0.2) +
            geom_label(aes(solution[5], solution[6]-(y_intercept*0.07), label=coords_hick), alpha = 0.2) +
            theme_minimal() +
            ggtitle("Visual for the Solution")) +
      xlab("Quantity of product X") +
      ylab("Qunatity of product Y")
  }
}

# R shiny
{
ui <- fluidPage(
  numericInput("c", "Value for c:", "", min = 0, width = "10%"),
  numericInput("d", "Value for d:", "", min = 0, width = "10%"),
  numericInput("Px", "Price of x:", "", min = 0, width = "10%"),
  numericInput("Py", "Price of y:", "", min = 0, width = "10%"),
  numericInput("I", "Consumer's Income:", "", min = 0, width = "10%"),
  numericInput("seed_x", "Seed value for x: ", 1, min = 0, width = "10%"),
  numericInput("seed_y", "Seed value for y: ", 1, min = 0, width = "10%"),
  
  selectInput("hick", "Draw Hicksian compensated demand (Optional):",
              c("No" = "No",
                "Yes" = "Yes")),
  numericInput("newPx", "Price of x for compensated demand:", 0, min = 0, width = "10%"),
  numericInput("newI", "Consumer's Income for compensated demand:", 0, min = 0, width = "10%"),
  
  actionButton("button", "Run"),
  
  plotOutput("graph"),
  textOutput("result")
)

server <- function(input, output){
  
  settingA <- eventReactive(input$button,{ 
    setting(input$c,input$d,input$Px,input$Py,input$I,input$seed_x,input$seed_y,input$hick,input$newPx,input$newI)
  })
  
  A <- eventReactive(input$button,{
    consummer_optimization(settingA())
  })
  
  text <- reactive({ A()$text })
  
  graph <- reactive({ graphing(A()$solution, settingA()) })
  
  output$result <- renderText({text()})
  
  output$graph <- renderPlot({graph()})
}

shinyApp(ui = ui, server = server)
}