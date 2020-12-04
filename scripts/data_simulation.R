library(tidyverse)
library(dplyr)

set.seed(1004806061)
n <- 2500
ID <- c(1:n)

# roughly based on 2016 census data 
gender <- sample(c("Female", "Male"), size = n, replace = TRUE, prob = c(0.52, 0.48))
age_group <- sample(c("under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 and over"), size = n, 
                    replace = TRUE, prob = c(0.02, 0.15, 0.23, 0.15, 0.15, 0.15, 0.15))
immigrant<- sample(c("Yes", "No", "Prefer not to say"), size = n, prob = c(0.33, 0.66, 0.01), replace = TRUE)
vismin<- sample(c("Yes", "No", "Prefer not to say"), size = n, prob = c(0.35, 0.64, 0.01), replace = TRUE)
responded <- sample(c("yes", "no"), prob = c(0.40, 0.60), size = n, replace = TRUE)
citizen <- sample(c("yes", "no"), prob = c(0.80, 0.20), size = n, replace = TRUE)

survey_data <- data.frame(ID, gender, age_group, citizen, vismin, immigrant, responded)

survey_data <- survey_data %>% filter(responded == "yes") # simulating non response

voter_data <- survey_data %>% filter(age_group != "under 18") %>% filter(citizen == "yes") 
# filtering eligible voters
n_voter <- nrow(voter_data) # number of eligible voters 

voter_data$voted_2019 <- sample(c("yes", "no", "idk"), size = n_voter , replace = TRUE, prob = c(0.89, 0.10, 0.01)) 
n_2019 <- nrow(voter_data %>% filter(voted_2019 == "yes")) # simulating voters who voted in 2019

voter_data <- voter_data %>%  # the party they voted for in 2019 based on last years numbers
  mutate(past_vote = 
           ifelse(voted_2019 == "yes", 
                  sample(c("Conservative", "Liberal", "NDP","Green",  "Other/Prefer not to answer"), 
                         replace = TRUE, 
                         prob = c(0.16, 0.52, 0.21, 0.08, 0.03), size = n_2019 ), "Did not vote")) 

# plan to vote in next election based on whether or not they voted in last election
voter_data <- voter_data %>% 
  mutate(plan_to_vote = ifelse(voted_2019 == "yes", 
                               sample(c("Yes", "No", "Don't know"), size = n_2019 , replace = TRUE, 
                                      prob = c(0.98, 0.01, 0.01)), 
                               sample(c("Yes", "No", "Don't know"), size = n_voter - n_2019 , replace = TRUE, 
                                      prob = c(0.60, 0.30, 0.10)))) 

plan_vote_n <- nrow(voter_data %>% filter(plan_to_vote != "No")) # number of ppl who plan to vote 

# the party they plan to vote for if they plan to vote 
voter_data <- voter_data %>% 
  mutate(likely_party = ifelse(plan_to_vote != "No", 
                               sample(c("Conservative", "Liberal", "NDP", "Green", "Undecided"), 
                                      size = plan_vote_n, replace = TRUE, 
                                      prob = c(0.20, 0.45, 0.20, 0.07, 0.08)), "Don't plan to vote")) 


# simulating responses to important issues question 

options <- c("Extremely", "Very", "Moderately", "Slightly", "Not", "Don't know")
low_prob <- c(0.05, 0.05, 0.30, 0.2, 0.35, 0.05)

voter_data$Healthcare <- sample(options, size = n_voter, prob = c(0.20, 0.15, 0.15, 0.4, 0.5, 0.05), replace = TRUE)
voter_data$Education <- sample(options, size = n_voter, replace = TRUE)
voter_data$Taxes <- sample(options, size = n_voter, prob = c(0.10, 0.10, 0.35, 0.25, 0.05, 0.15), replace = TRUE)
voter_data$Climate <- sample(options, size = n_voter, prob = c(0.10, 0.20, 0.40, 0.12, 0.14, 0.04), replace = TRUE)
voter_data$Jobs <- sample(options, size = n_voter, replace = TRUE, prob = c(0.10, 0.05, 0.7, 0.1, 0.04, 0.01))
voter_data$Housing <- sample(options, size = n_voter, prob = c(0.10, 0.15, 0.40, 0.15, 0.15, 0.05), replace = TRUE)
voter_data$Indigenous <- sample(options, size = n_voter, replace = TRUE, prob = low_prob)
voter_data$Immigration <- sample(options, size = n_voter, prob = low_prob, replace = TRUE)
voter_data$Racial <- sample(options, size = n_voter, replace = TRUE, prob = low_prob)
voter_data$Seniors <- sample(options, size = n_voter, replace = TRUE, prob = low_prob)
voter_data$Education <- sample(options, size = n_voter, replace = TRUE, prob = low_prob)
voter_data$Childcare <- sample(options, size = n_voter, replace = TRUE, prob = low_prob)
voter_data$COVID19 <- sample(options, size = n_voter, prob = c(0.25, 0.15, 0.50, 0.05, 0.03, 0.02), replace = TRUE)
voter_data$Debt <- sample(options, size = n_voter, prob = c(0.15, 0.10, 0.45, 0.25, 0.03, 0.02), replace = TRUE)

# simulating responses to questions about ads  
voter_data$ads<- sample(c("Online", "TV", "Radio", "Mail", "Print", "Signs/Billboards", "None"), prob = c(0.40, 0.23, 0.02, 0.10, 0.04, 0.10, 0.10), size = n_voter, replace = TRUE)

voter_data  <- voter_data %>% mutate(decided = ifelse(likely_party == "Undecided", "Undecided", "Decided")) 
decided_n <- (voter_data %>% group_by(decided) %>% count() %>% filter(decided == "Decided"))$n
undecided_n <- (voter_data %>% group_by(decided) %>% count() %>% filter(decided == "Undecided"))$n

# simulating different effect rates based on decided vs undecided voters 
voter_data <- voter_data %>% 
  mutate(ad_affect = 
           ifelse(likely_party == "Undecided", 
                  sample(c("not at all", "a little", "somewhat", "a lot", "a great deal"), 
                         prob = c(0.7, 0.20, 0.05, 0.03, 0.02), size = undecided_n, replace = TRUE), 
                  sample(c("not at all", "a little", "somewhat", "a lot", "a great deal"), 
                         prob = c(0.89, 0.05, 0.03, 0.01, 0.01),size = decided_n, replace = TRUE)))


# Citations - numbers used for simulation loosely based on 2016 Census and Angus Reid polls
# - Statistics Canada. 2017. University--Rosedale [Federal electoral district], Ontario and Ontario [Province] (table). Census Profile. 2016 Census. Statistics Canada Catalogue no. 98-316-X2016001. Ottawa. Released November 29, 2017.
# - Angus Reid (2020, September 2). Federal Politics: Election speculation season begins with main contenders in dead heat. http://angusreid.org/federal-politics-september-2020/.