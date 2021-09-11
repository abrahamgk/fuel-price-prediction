## app.R ##
library(shiny)
library(shinydashboard)
library(HMM)


#Reading data
Final_data <- read.csv("C:/R1/Final_data.csv", header=TRUE)
final_fuel <- read.csv("C:/R1/final_fuel.csv", header=TRUE) 
current_fuel <- read.csv("C:/R1/current_fuel_price.csv", header=TRUE)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Fuel Price forecasting")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(id="fuel",
    menuItem("City", tabName = "dashboard", icon = icon("dashboard"),
             selectInput("city", "Choose your city:", choices = c('Bangalore'=1,
                                                                  'Chennai'=2,
                                                                  'Delhi'=3,
                                                                  'Mumbai'=4,
                                                                  'Hyderabad'=5),
                         selected = "Bangalore",
                         width='200px')),
    menuItem("Fuel", tabName = "dashboard", icon = icon("dashboard"),
             selectInput("fuel1", "Choose your fuel:", choices = c('Petrol'=5,
                                                                   'Diesel'=0),
                         selected = "Petrol",
                         width='200px'))
  )
)



frow1 <- fluidRow(
   valueBoxOutput("value1"),
   valueBoxOutput("value2")
)
frow2 <- fluidRow( 
  box(
    title = "Predicted price"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("output_plot", height = "300px")
  )
  ,box(
    title = "Transition Probablity matrix"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,tableOutput("transition_matrix")
  ) 
  ,box(
    title = "Emission Probablity matrix"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,tableOutput("emission_matrix")
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)



#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, 
                    sidebar, body, skin='red')



# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  #Initializing initial guess HMM
  guess_model = initHMM(c("Upward","Stable","Downward"),
                        c("Up","Same","Down"),
                        transProbs=matrix(c(.5,.25,.2,
                                            .3,.5,.3,
                                            .2,.25,.5),3),
                        
                        emissionProbs=matrix(c(.35,.25,.32,
                                               .33,.5,.33,
                                               .32,.25,.35),3))
  #print(guess_model)
  
  #print(hmm_model)

  dates <- as.Date(c( "2021-04-23","2021-04-24","2021-04-25",
                      "2021-04-26","2021-04-27","2021-04-28",
                      "2021-04-29","2021-04-30","2021-05-01"))

  all_list <- list()
  
  for(i in c(2:11)){
    observation = Final_data[,i]
    
    # Baum-Welch to find the emission and transition probabilities
   all_list[i-1] <-  baumWelch(guess_model,observation,100,delta=1e-3)
  }
  


  blr_dsl <- c(0.19,-0.09)
  blr_ptr <- c(0.21,-0.11)
  chn_dsl <- c(0.20,-0.12)
  chn_ptr <- c(0.19,-0.11)
  del_dsl <- c(0.21,-0.11)
  del_ptr <- c(0.21,-0.11)
  hyd_dsl <- c(0.18,-0.13)
  hyd_ptr <- c(0.22,-0.11)
  mub_dsl <- c(0.21,-0.12)
  mub_ptr <- c(0.21,-0.11)
  
  avg_risefall <- cbind(blr_dsl, chn_dsl, del_dsl, hyd_dsl, mub_dsl, blr_ptr, 
                        chn_ptr,del_ptr, hyd_ptr, mub_ptr)
 
  
  
  output$value1 <- renderValueBox({
    curr_list <- all_list
    
    values <- current_fuel[,as.integer(input$city)+as.integer(input$fuel1)+1]
    
    #Function to convert numerical values to states
    state_convert <- function(values){
      n <- length(values)
      states <- c("Up","Down","Same")
      obs <- c()
      for(i in c(2:n)){
        if(values[i]>values[i-1]){
          obs[i-1]=states[1]
        }
        if(values[i]<values[i-1]){
          obs[i-1]=states[2]
        }
        if(values[i]==values[i-1]){
          obs[i-1]=states[3]
        }
      }
      return(obs)
    }
    
    seq_vector <-  state_convert(values)
    
    seq_length <- length(seq_vector)
    
    curr_model <- curr_list[[as.integer(input$city)+as.integer(input$fuel1)]]
    hmm_model_trans <- curr_model$transProbs
    hmm_model_emiss <- curr_model$emissionProb
    
    #Function for predict the next n days of price
    emission_predict <- function(hmm_model, seq_vector, n=3){
      
      #Running the viterbi model to find the the probable hidden state sequence
      prob_viterbi_model = viterbi(hmm_model,seq_vector)
      
      #All possible emissions
      emiss_poss_matrix <- c("Up","Same","Down")
      
      #Making a copy of the sequence
      most_prob_seq <- seq_vector
      
      
      for(i in c(1:n)){
        
        #Current hidden state
        prob_curr_trend <- prob_viterbi_model[seq_length-1]
        
        #Getting the probabilities of next emission. 
        emiss_prob_matrix <- hmm_model_trans[,prob_curr_trend]%*%hmm_model_emiss
        
        #Getting the most probable emission
        most_prob_emiss <- emiss_poss_matrix[which.max(emiss_prob_matrix)]
        
        #Adding the most probable observation to the sequence
        most_prob_seq[length(most_prob_seq)+1] <- most_prob_emiss
        
        #Running the viterbi model to find the the probable hidden state sequence
        prob_viterbi_model = viterbi(hmm_model,most_prob_seq)
        
      }
      
      
      states <- prob_viterbi_model
      emissions <- most_prob_seq
      
      
      #using rbind to return multiple outputs as one 
      output <- rbind(states,emissions)
      return(output)
      
    }
    
    #Initializing our markov model
    hmm_model = initHMM(c("Upward","Stable","Downward"), 
                        c("Up","Same","Down"), 
                        transProbs=hmm_model_trans,
                        emissionProbs=hmm_model_emiss)
    
    #Running the viterbi model to find the the probable hidden state sequence
    viterbi_model = viterbi(hmm_model,seq_vector)
    
    
    #storing the predictions of states and emissions 
    emission_predict_output <- emission_predict(hmm_model,seq_vector)
    pred_seq_vector <- emission_predict_output["emissions",]
    
    #storing the length of sequence of predicted emissions 
    pred_seq_length <- length(pred_seq_vector)

    
    icon1 <- "grip-lines"
    color1 <- "orange"
    if(viterbi_model[length(viterbi_model)]=="Upward"){icon1 = "angle-up"
    color1 = "green"}
    if(viterbi_model[length(viterbi_model)]=="Downward"){icon1 = "angle-down"
    color1 = "red"}
    
    
    valueBox(
      formatC(viterbi_model[length(viterbi_model)], format="d", big.mark=',')
      ,paste('Current Hidden State')
      ,icon = icon(icon1,lib='font-awesome')
      ,color = color1)  
  })
  
  output$value2 <- renderValueBox({
    values <- current_fuel[,as.integer(input$city)+as.integer(input$fuel1)+1]
    
    valueBox(values[length(values)]
      ,paste('Current fuel Price in Rs')
      ,icon = icon("gas-pump",lib='font-awesome')
      ,color = "blue")  
  })


  #creating the plotOutput content
  output$output_plot <- renderPlot({
    
    values <- current_fuel[,as.integer(input$city)+as.integer(input$fuel1)+1]
    
    #Function to convert numerical values to states
    state_convert <- function(values){
      n <- length(values)
      states <- c("Up","Down","Same")
      obs <- c()
      for(i in c(2:n)){
        if(values[i]>values[i-1]){
          obs[i-1]=states[1]
        }
        if(values[i]<values[i-1]){
          obs[i-1]=states[2]
        }
        if(values[i]==values[i-1]){
          obs[i-1]=states[3]
        }
      }
      return(obs)
    }
    
    seq_vector <-  state_convert(values)
    
    seq_length <- length(seq_vector)
    curr_list <- all_list
    
    
    curr_model <- curr_list[[as.integer(input$city)+as.integer(input$fuel1)]]
    hmm_model_trans <- curr_model$transProbs
    hmm_model_emiss <- curr_model$emissionProb
    
    #Function for predict the next n days of price
    emission_predict <- function(hmm_model, seq_vector, n=3){
      
      #Running the viterbi model to find the the probable hidden state sequence
      prob_viterbi_model = viterbi(hmm_model,seq_vector)
      
      #All possible emissions
      emiss_poss_matrix <- c("Up","Same","Down")
      
      #Making a copy of the sequence
      most_prob_seq <- seq_vector
      
      
      for(i in c(1:n)){
        
        #Current hidden state
        prob_curr_trend <- prob_viterbi_model[seq_length-1]
        
        #Getting the probabilities of next emission. 
        emiss_prob_matrix <- hmm_model_trans[,prob_curr_trend]%*%hmm_model_emiss
        
        #Getting the most probable emission
        most_prob_emiss <- emiss_poss_matrix[which.max(emiss_prob_matrix)]
        
        #Adding the most probable observation to the sequence
        most_prob_seq[length(most_prob_seq)+1] <- most_prob_emiss
        
        #Running the viterbi model to find the the probable hidden state sequence
        prob_viterbi_model = viterbi(hmm_model,most_prob_seq)
        
      }
      
      
      states <- prob_viterbi_model
      emissions <- most_prob_seq
      
      
      #using rbind to return multiple outputs as one 
      output <- rbind(states,emissions)
      return(output)
      
    }
    
    #Initializing our markov model
    hmm_model = initHMM(c("Upward","Stable","Downward"), 
                        c("Up","Same","Down"), 
                        transProbs=hmm_model_trans,
                        emissionProbs=hmm_model_emiss)
    
    #Running the viterbi model to find the the probable hidden state sequence
    viterbi_model = viterbi(hmm_model,seq_vector)
    
    
    #storing the predictions of states and emissions 
    emission_predict_output <- emission_predict(hmm_model,seq_vector)
    pred_seq_vector <- emission_predict_output["emissions",]
    
    #storing the length of sequence of predicted emissions 
    pred_seq_length <- length(pred_seq_vector)
    
    what <- seq_length+1
    prob_values <- values
    
    #Writing a for loop to get values for the graph
    for(i in c(what:pred_seq_length)){
      if(pred_seq_vector[i]=="Up"){
        prob_values[i] <- prob_values[i-1] + avg_risefall[[1,as.integer(input$city)+as.integer(input$fuel1)]]
      }
      if(pred_seq_vector[i]=="Down"){
        prob_values[i] <- prob_values[i-1] + avg_risefall[[2,as.integer(input$city)+as.integer(input$fuel1)]]
      }
      if(pred_seq_vector[i]=="Same"){
        prob_values[i] <- prob_values[i-1]+0
      }
    }
    matplot(x = dates,
            y = prob_values, 
            type = 'l',
            xlab = "Days",
            ylab = "Price in Rs."
    )
    
  })
  
  
  output$transition_matrix <- renderTable({
    curr_list <- all_list
    curr_model <- curr_list[[as.integer(input$city)+as.integer(input$fuel1)]]
    curr_model$transProbs
    
  }, rownames = TRUE,  width = "200px")
  
  
  output$emission_matrix <- renderTable({
    
    curr_list <- all_list
    curr_model <- curr_list[[as.integer(input$city)+as.integer(input$fuel1)]]
    curr_model$emissionProbs
    
  }, rownames = TRUE)
 
}


shinyApp(ui, server)
