library(tidyverse)
library(ggthemes)
library(stringr)
library(shiny)
library(DT)

# Global
n=30
x<-1:n/n
set.seed(2)

y<-2-x+2*pmax(x-.5,0) +rnorm(n)*.1



psi=.8
Y<-matrix(,n,10)
psiV<-vector(length=10)
V_coef <- vector(length=10)
U_coef <- vector(length=10)

for(i in 1:10){
  U<-pmax(x-psi,0)
  V<- -I(x>psi)
  o<-lm(y~x+U+V)
  Y[,i]<-fitted(o)
  psi1<-psi+coef(o)["V"]/coef(o)["U"]
  psiV[i]<-psi<-psi1
  V_coef[i] <- coef(o)["V"]
  U_coef[i] <- coef(o)["U"]
  
}

# Create complete dataframe:
df <- tibble::as_tibble(Y)
df <- df %>% mutate(x = x)
df <- df %>% gather(Iteration, Y, -x)
df <- df %>% mutate(y = rep(y, 10))
df <- df %>% mutate(Iteration = str_extract(Iteration, "[[:digit:]]+"))
df$Iteration <- as.numeric(df$Iteration)
df <- df %>% mutate(psiV = rep(psiV, each = 30),
                    gamma = rep(V_coef, each = 30),
                    beta2 = rep(U_coef, each = 30))
df$psiV <- round(df$psiV,5)
df$gamma <- round(df$gamma,5)
df$beta2 <- round(df$beta2,5)

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Segmented Algorithm (Muggeo, 2008)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("it", label = h3("Iteration #"), min = 1, 
                    max = 10, value = 1, animate = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot"),
         br(),
         dataTableOutput("tble")
         # br(),
         # withMathJax(), 
         # uiOutput("eq")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$plot <- renderPlot({
     
     data <- df %>% filter(Iteration == input$it)
     
     p <- ggplot() +
       geom_point(data = data,
                  aes(x = x, y = y)) +
       geom_point(data = data,
                  aes(x = x, y = Y), size =3.5, color = "red") +
       geom_point(data = data %>% filter(x>=psiV) %>% do(data.frame(head(., 1))),
                  aes(x = psiV, y = Y),
                  shape = 13, size = 7, color = "red") +
      
       geom_text(data = data,
                 aes(x = 0.16, y = 1.45, 
                      
                     label = paste("Changepoint = ", psiV)),
                 size = 5, color = "red") +
       geom_text(data = data,
                 aes(x = 0.16, y = 1.5, 
                      
                     label = paste("Iteration # = ", Iteration)),
                 size = 5) +
       geom_text(data = data,
                 aes(x = 0.16, y = 1.4, 
                      
                     label = paste("beta2 = ", beta2)),
                 size = 5) +
       geom_text(data = data,
                 aes(x = 0.16, y = 1.35, 
                      
                     label = paste("gamma = ", gamma)),
                 size = 5) +
       theme_bw() +
       theme_stata()
     
     p
   })
   
   
   output$tble <- renderDataTable({
     data <- df %>% filter(Iteration == input$it)
     datatable(data)
   })
   
   # output$eq <- renderUI({
   #   withMathJax(helpText("$E[Y] = \\beta_0 + \\beta_1x_i + \\beta _2(x_i - \\widetilde{\\psi })_+ +\\gamma I(x_i>\\widetilde{\\psi})^-$"))
   #   
   # })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

