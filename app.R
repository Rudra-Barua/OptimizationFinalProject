#BakeryProblem
library(shiny)
library(pracma)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Bakery Optimization",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("Recipes"),
      h4("Select the amount of ingredients needed for a batch of cakes:"),
      sliderInput("flour_cake","Flour",min = 0, max = 10, value = 3, step = 1),
      sliderInput("sugar_cake", "Sugar", min = 0, max = 10, value = 2, step = 1),
      sliderInput("choc_cake", "Chocolate", min = 0, max = 10, value = 1, step=1),
      h4("Select the amount of ingredients needed for a batch of cookies:"),
      sliderInput("flour_cook","Flour",min = 0, max = 10, value = 0, step = 1),
      sliderInput("sugar_cook", "Sugar", min = 0, max = 10, value = 1, step = 1),
      sliderInput("choc_cook", "Chocolate", min = 0, max = 10, value = 2, step=1)
    ),
    column(width = 4,
      actionBttn("solveBasis","Find the optimal final inventory"),
      uiOutput("basis"),
      h4(""),
      radioButtons("changeNorm", "Choose the shipping cost:", choices = c("Sum of absolute values of ingredient transfers" = "p_1",
                                                             "Single largest amount transferred" = "p_inf"), selected = "p_1"),
      h4(""),
      uiOutput("normeq"),
      uiOutput("normdis"),
      uiOutput("normcalc"),
      uiOutput("calc1"),
      uiOutput("calc2"),
      uiOutput("correctd"),
      uiOutput("alignment_text"),
      uiOutput("alignment1"),
      uiOutput("alignment2"),
      uiOutput("solve"),
    ),
    column(width = 4,
           h3("Inventory"),
           h4("Select the current inventory of ingredients:"),
           sliderInput("flour_inv","Flour",min = 0, max = 20, value = 9, step = 1),
           sliderInput("sugar_inv", "Sugar", min = 0, max = 20, value = 4, step = 1),
           sliderInput("choc_inv", "Chocolate", min = 0, max = 20, value = 11, step=1),
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available



server <- function(session, input, output) {
  flour_inv <- 0
  sugar_inv <- 0
  choc_inv <- 0
  flour_cake <- 0
  sugar_cake <- 0
  choc_cake <- 0
  flour_cook <- 0
  sugar_cook <- 0
  choc_cook <- 0
  
  basis <- 0
  
  findBasis <- function(cake, cookie){
    basis <- cross(cake, cookie)
    output$basis <- renderUI(h4(paste("A basis for the annilhilator space is ","(",paste(basis[1],basis[2],basis[3]),")")))
    return(basis)
  }
  findNorm <- function(inven, norm_choice, basis){
    if(input$changeNorm == "p_1"){
      output$normeq <- renderUI(h4(jaxD(("||x|| = |u|+|v|+|w|"))))
      output$normdis <- renderUI(h4("This is the taxicab norm. T
                                    herefore, ", jaxD(("x `in l_1, x^* `in l_`infty"))))
      norm <- max(abs(basis[1]), abs(basis[2]), abs(basis[3]))
      x_star = basis/norm
      x_star_ = -1*x_star
      d1 = dot(inven, x_star)
      d2 = dot(inven, x_star_)
      output$normcalc <- renderUI(h4(jaxD("`text{For } x^*, `text{ our norm is given by max(|u|,|v|,|w|)}"),
                                     paste("Hence, the infinity norm of our basis is ", norm, ". So for x* we want to try"),
                                           "(",paste(x_star[1],x_star[2],x_star[3]),")", " and ", "(",paste(x_star_[1],x_star_[2],x_star_[3]),")."))
      output$calc1 <- renderUI(h4(paste("d=<x,x*>=<(",paste(inven[1],inven[2],inven[3]),"),", "(",paste(x_star[1],x_star[2],x_star[3]),")>=", d1)))
      output$calc2 <- renderUI(h4(paste("d=<x,x*>=<(",paste(inven[1],inven[2],inven[3]),"),", "(",paste(x_star_[1],x_star_[2],x_star_[3]),")>=", d2)))
      if (d2 > d1){
        x_star <- x_star_
        d <- d2
      } else {
        d <- d1
      }
      output$correctd <- renderUI(h4(paste("We want the x* that gives us a positive value. Thus, the minimum possible shipping cost is d = "),d))
      output$alignment_text <- renderUI(h4("Now we want to use alignment to find the correct final inventory"))
      output$alignment1 <- renderUI(h4("x* = (",paste(x_star[1],x_star[2],x_star[3]),")"))
      align <- x_star
      for (i in 1:3){
        if (abs(x_star[i]) == max(abs(x_star))){
          align[i] <- sign(x_star[i])
        } else {
          align[i] <- 0
        }
      }
      align_ <- align*d
      output$alignment2 <- renderUI(h4(paste("x - m = ",d,"(",paste(align[1],align[2],align[3]),")=", "(",paste(align_[1],align_[2],align_[3]),")")))
      sol <- inven - align_
      output$solve <- renderUI(h4(paste("m = x - (x - m) = ","(",paste(inven[1],inven[2],inven[3]),")-", "(",paste(align_[1],align_[2],align_[3]),")=","(",paste(sol[1],sol[2],sol[3]),")")))
    } else {
      output$normeq <- renderUI(h4(jaxD(("||x|| = max(|u|,|v|,|w|)"))))
      output$normdis <- renderUI(h4("This is the infinity norm. Therefore, ", jaxD(("x `in l_`infty, x^* `in l_1"))))
      norm <- abs(basis[1]) + abs(basis[2]) + abs(basis[3])
      x_star = basis/norm
      x_star_ = -1*x_star
      d1 = dot(inven, x_star)
      d2 = dot(inven, x_star_)
      output$normcalc <- renderUI(h4(jaxD("`text{For } x^*, `text{ our norm is given by |u|+|v|+|w|}"),
                                     paste("Hence, the taxicab norm of our basis is ", norm, ". So for x* we want to try"),
                                           "(",paste(x_star[1],x_star[2],x_star[3]),")", " and ", "(",paste(x_star_[1],x_star_[2],x_star_[3]),")."))
      output$calc1 <- renderUI(h4(paste("d=<x,x*>=<(",paste(inven[1],inven[2],inven[3]),"),", "(",paste(x_star[1],x_star[2],x_star[3]),")>=", d1)))
      output$calc2 <- renderUI(h4(paste("d=<x,x*>=<(",paste(inven[1],inven[2],inven[3]),"),", "(",paste(x_star_[1],x_star_[2],x_star_[3]),")>=", d2)))
      if (d2 > d1){
        x_star <- x_star_
        d <- d2
      } else {
        d <- d1
      }
      output$correctd <- renderUI(h4(paste("We want the x* that gives us a positive value. Thus, the minimum possible shipping cost is d = "),d))
      output$alignment_text <- renderUI(h4("Now we want to use alignment to find the correct final inventory"))
      output$alignment1 <- renderUI(h4("x* = (",paste(x_star[1],x_star[2],x_star[3]),")"))
      align <- x_star
      for (i in 1:3){
        align[i] <- sign(x_star[i])
      }
      align_ <- align*d
      output$alignment2 <- renderUI(h4(paste("x - m = ",d,"(",paste(align[1],align[2],align[3]),")=", "(",paste(align_[1],align_[2],align_[3]),")")))
      sol <- inven - align_
      output$solve <- renderUI(h4(paste("m = x - (x - m) = ","(",paste(inven[1],inven[2],inven[3]),")-", "(",paste(align_[1],align_[2],align_[3]),")=","(",paste(sol[1],sol[2],sol[3]),")")))
    }
  }
  observeEvent(input$solveBasis,{
    flour_inv <<- input$flour_inv
    sugar_inv <<- input$sugar_inv
    choc_inv <<- input$choc_inv
    flour_cake <<- input$flour_cake
    sugar_cake <<- input$sugar_cake
    choc_cake <<- input$choc_cake
    flour_cook <<- input$flour_cook
    sugar_cook <<- input$sugar_cook
    choc_cook <<- input$choc_cook
    
    cake <- c(flour_cake, sugar_cake, choc_cake)
    cookie <- c(flour_cook, sugar_cook, choc_cook)

    basis <<- findBasis(cake, cookie)
    
    observeEvent(input$changeNorm,{
      norm_choice <- input$changeNorm
      inven <- c(flour_inv, sugar_inv, choc_inv)
      findNorm(inven, norm_choice, basis)
    })
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)