cat("\014")
rm(list = ls())
gc()
graphics.off()


set.seed(42)
options(warn=-1)
library(tidyverse, warn.conflicts = FALSE)
library(tidyquant, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(TTR, warn.conflicts = FALSE)
library(data.table)
library(foreach)
library(doParallel)
library(parallel)

# Import data (Source: Yahoo Finance)
raw_data <- tq_get("AAPL", from = "2014-12-31", to = "2024-12-31", get = "stock.prices")
df <- raw_data[, c("date", "close")]

# Plot the AAPL time series
ggplot(df, aes(x = date, y = close)) + 
  geom_line(color = "black") +
  labs(title = "Stock Price", y = "Price ($)", x = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Creation of Bollinger Bands
aapl_bbands <- BBands(df$close, n = 10,sd = 2)
aapl_bbands <- aapl_bbands[,c("dn","mavg", "up")]
# Clear row names to use default indexing
rownames(aapl_bbands) <- NULL

# Creation of MACD indicator
aapl_macd <- MACD(df$close, nFast = 12, nSlow = 24, nSig = 9, wilder=FALSE)

# Add the indicators to the original df
df$bb_lower <- aapl_bbands[,1]
df$bb_middle <- aapl_bbands[,2]
df$bb_upper <- aapl_bbands[,3]
df$macd <- aapl_macd[,1]
df$signal <- aapl_macd[,2]

# Drop "na" values
aapl_data <- df %>% drop_na()

# Plot Bollinger Bands
ggplot(aapl_data, aes(x = date)) +
  geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper), fill = "lightgreen", alpha = 0.25) +
  geom_line(aes(y = close), color = "black", size=0.5) +
  geom_line(aes(y = bb_upper), color = "#006400", linewidth=0.1) +
  geom_line(aes(y = bb_lower), color = "#006400", linewidth=0.1) +
  geom_line(aes(y = bb_middle), color = "red", linewidth=0.2) +
  labs(title = "Bollinger Bands and Price", y = "Price ($)", x = "") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot MACD and Signal line
ggplot(aapl_data, aes(x = date)) +
  geom_line(aes(y = macd), color = "blue") +
  geom_line(aes(y = signal), color = "red") +
  labs(title = "MACD and Signal Line", y = "MACD", x = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Creation of Trading Signals based on the behavior of Bollinger Bands and MACD Indicator
aapl_data$Buy_Signal <- ifelse((aapl_data$close <= aapl_data$bb_lower) &
                                 (aapl_data$macd > aapl_data$signal), 1, 0)
aapl_data$Sell_Signal <- ifelse((aapl_data$close >= aapl_data$bb_upper) &
                                  (aapl_data$macd < aapl_data$signal), 1, 0)

# Trading Strategy setup
aapl_data$Position <- 0 #I set the initial position to zero (in other words "Neutral" position)
max_hold_days <- 10 #The strategy will hold the position for 10 days

current_position <- 0 #Needed to understand if the strategy is long (1), short (-1) or neutral (0)
days_in_position <- 0 #Counter for how long the strategy holds a position

#Algorithm for the creation of the trading position
#The idea is to maintain the current position unless there's a different signal or threshold that indicates otherwise
for(i in 1:nrow(aapl_data)){
  #Increment days_in_position if I'm holding a long or short position
  if(current_position != 0) {
    days_in_position <- days_in_position + 1
  }
  #Based on the trading signals I have 3 options: open a new position, maintain the position, close the position
  #If the current position is zero and I have a buy or sell signal, the algorithm takes a long or a short position
  if(current_position == 0) {
    if(aapl_data$Buy_Signal[i] == 1) {
      current_position <- 1
      days_in_position <- 0 #The counter days_in_position is set to zero if a directional position is taken
    } else if(aapl_data$Sell_Signal[i] == 1) {
      current_position <- -1
      days_in_position <- 0
    }
    #If the current position is different from zero, I have two choices: maintain the position or close the position. I close the position if I have a contrary signal or I surpass max_hold_days
  } else {
    if(current_position == 1 && (aapl_data$Sell_Signal[i] == 1 || days_in_position >= max_hold_days)) {
      current_position <- 0 
      days_in_position <- 0
    }
    
    if(current_position == -1 && (aapl_data$Buy_Signal[i] == 1 || days_in_position >= max_hold_days)) {
      current_position <- 0
      days_in_position <- 0
    }
  }
  #With each iteration I am creating a vector in which I synthesize the positions in the strategy
  aapl_data$Position[i] <- current_position
}

# Plot the trading positions
ggplot(aapl_data, aes(x = date)) +
  geom_line(aes(y = close), color = "black") +
  geom_point(data = subset(aapl_data, Position == 1), aes(y = close), color = "green", size = 2) +
  geom_point(data = subset(aapl_data, Position == -1), aes(y = close), color = "red", size = 2) +
  labs(title = "Price and Trading positions", y = "Price ($)", x = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Compute returns
#Added column daily_return
aapl_data <- aapl_data %>% mutate(daily_return = close / lag(close) - 1)
aapl_data <- aapl_data %>% drop_na()

#Added column strategy_return. I open/close the position in the day after I have the signal
aapl_data <- aapl_data %>% mutate(strategy_return = daily_return * lag(Position, default = 0))  

#Add cumulative_market_return and cumulative_strategy_return columns to the dataset
aapl_data <- aapl_data %>% mutate(cumulative_market_return = cumprod(1 + daily_return),
                                  cumulative_strategy_return = cumprod(1 + strategy_return))

#Plot of the Market return and strategy return
ggplot(aapl_data, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return*100, color = "Market Return"), size = 0.5) +
  geom_line(aes(y = cumulative_strategy_return*100, color = "Strategy Return"), size = 1) +
  labs(title = "Cumulative Returns: Strategy vs Market", x = "Date", y = "Cumulative Return (%)", color="Legend") +
  scale_color_manual(values = c("Market Return" = "black", "Strategy Return" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5))

#I create the State column which represents the environment of the trading strategy, then I create the possible action of the trading strategy
#State combines Bollinger Bands, MACD, trading signals and trading positions into a single categorical variable
aapl_data$State <- paste0(
  ifelse(aapl_data$close < aapl_data$bb_lower, "Below_BB", 
         ifelse(aapl_data$close > aapl_data$bb_upper, "Above_BB", "Middle_BB")),
  "_",
  ifelse(aapl_data$macd > aapl_data$signal, "MACD_Bullish", "MACD_Bearish"),
  "_",
  ifelse(aapl_data$Buy_Signal == 1, "Buy", 
         ifelse(aapl_data$Sell_Signal == 1, "Sell", "No_Signal")),
  "_",
  ifelse(aapl_data$Position == 1, "Long", 
         ifelse(aapl_data$Position == -1, "Short", "Flat"))
)

actions <- c("Long", "Short", "Flat")

#the Q-learning model will choose the best action based on the state

#Dividing data in train and test respectively 80% and 20% of the total data
df_train <- aapl_data[aapl_data$date <= "2022-12-31", ]
df_test <- aapl_data[aapl_data$date > "2022-12-31", ]

#Calculating again from the new starting point cumulative_market_return and cumulative_strategy_return for fair comparison
df_test <- df_test %>% mutate(cumulative_market_return = cumprod(1 + daily_return),
                              cumulative_strategy_return = cumprod(1 + strategy_return))

#Creation of the Q-table where rows: states, columns: actions. Initialization with all values equal to zero
Q_table <- data.table(State = unique(df_train$State))
for (a in actions) {
  Q_table[[a]] <- 0
}

#Optimize research in the q-table by setting State as key
setkey(Q_table, State)

#Hyperparameters
alpha <- 0.1   #learning rate: how much info overrides the Q-Table
gamma <- 0.5   #discount factor: long term vs short term rewards
epsilon <- 1 #casual exploration
epsilon_decay <- 0.999 #decay of epsilon as time passes
min_epsilon <- 0.01 #minimum level of epsilon

#Create the reward function
reward_function <- function(df_row, action) {
  if (action == "Long") {
    return(df_row$daily_return)
  } else if (action == "Short") {
    return(-df_row$daily_return)
  } else {
    return(-1) #Agent penalized for inactivity
  }
}

#Training of the Q-Learning algorithm
training <- function(j, Q_table, df_train, actions, alpha, gamma, epsilon, min_epsilon, epsilon_decay) {
  
  set.seed(42 + j)
  #Copies of the df, q_table and epsilon
  Q_table_local <- copy(Q_table)  
  df_train_local <- copy(df_train)
  local_epsilon <- epsilon
  
  for (i in 1:(nrow(df_train_local) - 1)) {
    #Definition of the states
    current_state <- df_train_local$State[i]
    next_state <- df_train_local$State[i + 1]
    
    #Exploration vs Exploitation: the agent explores a random action with probability epsilon, otherwise the agent selects the best action based on the current state. As time goes by, epsilon decays. This choice was made to make the model explore more at the beginning
    if (runif(1) < local_epsilon) { #runif(1) creates a random number between 0 and 1
      action <- sample(actions, 1) #sample chooses a single random action
    } else {
      action <- actions[which.max(Q_table_local[J(current_state), ..actions])] #if runif(1)>epsilon, the agent chooses the best action
    } #action returns on of the actions chosen
    
    #Function defined above that takes as reward the return of i+1 based on the agent's action
    reward <- reward_function(df_train_local[i+1, ], action)
    #Select best future action
    best_future_q <- max(Q_table_local[J(next_state), ..actions], na.rm = TRUE)
    
    # Bellman Equation
    Q_table_local[J(current_state), (action) := (1 - alpha) * get(action) + alpha * (reward + gamma * best_future_q)] #get(action) returns the value on the Q_table that is linked to that action given a state
    
    #Update of epsilon
    local_epsilon <- max(min_epsilon, local_epsilon * epsilon_decay)
  }
  
  #The function when iterated will return a list with the Q-tables
  return(Q_table_local = Q_table_local)
}

#Number of iterations
iter <- 100

#Initialization for parallel computation
cores <- detectCores()
cl <- makeCluster(cores - 1)  
registerDoParallel(cl)

#Time counter
start_time <- Sys.time()

#Results stored in a list. Added .packages because the function training uses package data.table
results <- foreach(j = 1:iter, .packages = c("data.table")) %dopar% {
  training(j, Q_table, df_train, actions, alpha, gamma, epsilon, min_epsilon, epsilon_decay)
}

stopCluster(cl)

end_time <- Sys.time()
print(end_time - start_time)

#Unwrapping the results
#I first transform from data.table format to a matrix of numeric values
Q_tables_num <- lapply(results, function(q_table) {
  #Excluding first column (so the states)
  q_matrix <- as.matrix(q_table[, -1, with = FALSE])  
  rownames(q_matrix) <- q_table$State 
  return(q_matrix)
})
#Q_tables_num returns a list of Q-tables in numerical format 

#Creation of a Q-table which is the average of all the Q-tables created using the training function iteration
#Reduce function applies + to the elements of a list
Q_table_avg_matrix <- Reduce("+", Q_tables_num) / iter
#Convert again into a data.table
Q_table_avg <- data.table(State = results[[1]]$State, Q_table_avg_matrix)

#Generating the trading signals using the train data
setkey(Q_table_avg, State)

#Based on the state, I choose the action with the maximum value
df_train$RL_Position <- 0  
for (i in 1:(nrow(df_train) - 1)) {
  state <- df_train$State[i]
  #I save into a variable the action
  action <- actions[which.max(Q_table_avg[J(state), ..actions])]
  #Creation of a column with the trading positions
  if (action == "Long") {
    df_train$RL_Position[i] <- 1
  } else if (action == "Short") {
    df_train$RL_Position[i] <- -1
  } else {
    df_train$RL_Position[i] <- 0
  }
}

#Calculations of the returns
#As before, I open a position the day after receiving a signal
df_train$RL_strategy_return <- df_train$daily_return * lag(df_train$RL_Position, default = 0)
df_train$cumulative_RL_return <- cumprod(1 + df_train$RL_strategy_return)

#Plotting train results
ggplot(df_train, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return*100, color = "Market")) +
  geom_line(aes(y = cumulative_strategy_return*100, color = "Bollinger & MACD")) +
  geom_line(aes(y = cumulative_RL_return*100, color = "Q-Learning RL (Avg)")) +
  labs(title = "Comparison Strategy RL vs Bollinger & MACD - Train Data", y = "Return (%)", x = "", color="Legend") +
  scale_color_manual(values = c("Market" = "blue", "Bollinger & MACD" = "green", "Q-Learning RL (Avg)" = "red")) +
  theme(plot.title = element_text(hjust = 0.5))

#I repeat the same procedure using the same code as above but this time I use test data to evaluate the RL performance
setkey(Q_table_avg, State)

df_test$RL_Position <- 0  
for (i in 1:(nrow(df_test) - 1)) {
  state <- df_test$State[i]
  action <- actions[which.max(Q_table_avg[J(state), ..actions])]
  
  if (action == "Long") {
    df_test$RL_Position[i] <- 1
  } else if (action == "Short") {
    df_test$RL_Position[i] <- -1
  } else {
    df_test$RL_Position[i] <- 0
  }
}

#I calculate returns
df_test$RL_strategy_return <- df_test$daily_return * lag(df_test$RL_Position, default = 0)
df_test$cumulative_RL_return <- cumprod(1 + df_test$RL_strategy_return)

#Plotting test results
ggplot(df_test, aes(x = date)) +
  geom_line(aes(y = cumulative_market_return*100, color = "Market")) +
  geom_line(aes(y = cumulative_strategy_return*100, color = "Bollinger & MACD")) +
  geom_line(aes(y = cumulative_RL_return*100, color = "Q-Learning RL")) +
  labs(title = "Comparison Strategy RL vs Bollinger & MACD - Test Data", y = "Return (%)", x = "", color="Legend") +
  scale_color_manual(values = c("Market" = "blue", "Bollinger & MACD" = "green", "Q-Learning RL" = "red")) +
  theme(plot.title = element_text(hjust = 0.5))

#I define the risk free for the calculation of the Sharpe ratio
risk_free <- 0.025
daily_rf <- ((1+0.025)^(1/360))-1

#Train statistics
aapl_return_train <- (tail(df_train$cumulative_market_return, 1))
tech_ret_train <- (tail(df_train$cumulative_strategy_return, 1))
RL_ret_train <- (tail(df_train$cumulative_RL_return, 1))

aapl_std_train <- sd(df_train$daily_return)
tech_std_train <- sd(df_train$strategy_return)
RL_std_train <- sd(df_train$RL_strategy_return)

alpha_tech_train <- mean(df_train$strategy_return) - mean(df_train$daily_return)
alpha_RL_train <- mean(df_train$RL_strategy_return) - mean(df_train$daily_return)

sharpe_tech_train <- (mean(df_train$strategy_return) - daily_rf)/tech_std_train
sharpe_RL_train <- (mean(df_train$RL_strategy_return) - daily_rf)/RL_std_train

#Test statistics
aapl_return_test <- (tail(df_test$cumulative_market_return, 1))
tech_ret_test <- (tail(df_test$cumulative_strategy_return, 1))
RL_ret_test <- (tail(df_test$cumulative_RL_return, 1))

aapl_std_test <- sd(df_test$daily_return)
tech_std_test <- sd(df_test$strategy_return)
RL_std_test <- sd(df_test$RL_strategy_return)

alpha_tech_test <- mean(df_test$strategy_return) - mean(df_test$daily_return)
alpha_RL_test <- mean(df_test$RL_strategy_return) - mean(df_test$daily_return)

sharpe_tech_test <- (mean(df_test$strategy_return) - daily_rf)/tech_std_test
sharpe_RL_test <- (mean(df_test$RL_strategy_return) - daily_rf)/RL_std_test

#Creating the dataframe for the results
strategies_statistics <- data.frame(
  Metric = c("Cumul AAPL Return", "Cumul Tech Strat Return", 
             "Cumul RL Return", "AAPL Std", "Tech Strat Std", 
             "RL Std", "Alpha Tech Strat", "Alpha RL", "Sharpe Tech Strat", "Sharpe RL"),
  Train_Data = c(aapl_return_train, tech_ret_train, RL_ret_train, aapl_std_train, tech_std_train,
                 RL_std_train, alpha_tech_train, alpha_RL_train, sharpe_tech_train,
                 sharpe_RL_train),
  Test_Data = c(aapl_return_test, tech_ret_test, RL_ret_test, aapl_std_test, tech_std_test,
                RL_std_test, alpha_tech_test, alpha_RL_test, sharpe_tech_test, sharpe_RL_test)
)

strategies_statistics
















