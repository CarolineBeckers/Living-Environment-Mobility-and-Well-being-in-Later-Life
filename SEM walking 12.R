#SEM model walking frequency

#Check working directory
getwd()

#Install package lavaan & SEM
install.packages("lavaan")
install.packages("semPlot")
install.packages("semTools")
library(lavaan)
library(semPlot)
library(semTools)
library(dplyr)


#Read dataset
data <- read.csv("data_SEM.csv", header = TRUE, sep=";")
View(data)

#checking if variables are numeric or categorical variables in data
#overview of numeric variables
numeric_columns <- data[sapply(data, is.numeric)]
print(numeric_columns)

#overview of character variables
character_columns <- data[sapply(data, is.character)]
print(character_columns)

#Setting missing values to NA so SEM won't interpret this as a separate category
data$Main_travel_mode[data$Main_travel_mode == ""] <- NA  
data$Frequency_bike[data$Frequency_bike == ""] <- NA
data$Frequency_walking[data$Frequency_walking == ""] <- NA
data$Frequency_car[data$Frequency_car == ""] <- NA
data$Frequency_public_transport[data$Frequency_public_transport == ""] <- NA
data$Gender[data$Gender == ""] <- NA
data$Gender[data$Gender == "Anders"] <- NA
data$Gender[data$Gender == "Zeg ik liever niet"] <- NA
data$Neighbourhood_group[data$Neighbourhood_group == ""] <- NA
data$Age_group[data$Age_group == ""] <- NA
data$Education_level[data$Education_level == ""] <- NA
data$Income[data$Income == ""] <- NA
data$Bike_ownership[data$Bike_ownership == ""] <- NA
data$Drivers_license[data$Drivers_license == ""] <- NA
data$Access_car[data$Access_car == ""] <- NA
data$PT_subscription[data$PT_subscription == ""] <- NA
data$Nb_household_group[data$Nb_household_group == ""] <- NA

#Model specification
model <- '
#latent constructs Built environment
Poor_walking_infrastructure =~ Walkable_with_walker_or_wheelchair + Maintained_sidewalks + Little_obstacles_on_sidewalks + Easy_to_cross_streets
No_walking_friendly_environment =~ Walking_friendly_neighbourhood + Enough_benches + Enough_greenery + Public_spaces_are_pleasant_meeting_places
Qualitative_cycling_infrastructure =~ Qualitative_cycling_lanes + Maintained_cycling_infrastructure + Sufficient_cycling_infrastructure + Maintained_streets_._squares
Cycling_friendly_environment =~ Cyclingfriendly_neighbourhood + Enjoyable_cycling + Feeling_safe_in_traffic_as_cyclist + Busy_traffic_causing_unsafe_cycling_experiences
Busy_traffic =~ A_lot_of_traffic + A_lot_of_freight_traffic + Nuisance_due_to_traffic + Few_traffic_accidents
Traffic_safety =~ Enough_public_lighting + Feeling_safe_in_traffic_as_pedestrian + Feeling_safe_in_traffic_as_car_user + Feeling_safe_on_public_transport
Negative_parking_experiences =~ Accessible_by_car + Easily_finding_parking_spots_for_residents + Easily_finding_parking_spots_for_visitors
Qualitative_public_transport =~ Satisfied_with_public_transport + Enough_public_transport + Sufficiently_frequent_public_transport + Well_equipped_public_transport_stops + Accessible_public_transport_stops + Enough_seats_on_public_transport + Easy_to_find_information_on_public_transport_for_older_people

#latent constructs Social environment
Neighbourhood_social_cohesion =~ I_know_my_neighbours_well + Friendships_and_relationships_in_neighbourhood_mean_a_lot + Chat_regularly_with_neighbours + Regular_visitings_of_neighbours + Neighbours_are_willing_to_help_each_other + I_can_go_to_neighbours_if_I_need_advise + Neighbours_would_help_in_emergency + I_often_borrow_things_to_neighbours + My_neighbours_are_trustworthy + My_neighbours_get_along + My_neighbourhood_is_close.knit + Living_in_this_neighbourhood_gives_a_sense_of_community
Attractive_living_environment =~ I_live_in_a_attractive_neighbourhood + I_feel_like_I_belong_to_this_neighbourhood + I_would_like_to_move_given_the_opportunity + Planning_to_live_here_for_several_years + Good_place_to_bring_up_children
Encouragement_for_PA =~ I_can_count_on_people_to_be_physically_active_with_me + Family_and_friends_encourage_me_to_be_active + People_important_to_me_encourage_me_to_participate_in_physical_activities + Family_and_friends_participate_in_physical_activities_regularly + Seeing_other_participating_in_physical_activities_makes_me_want_to_participate
Satisfaction_with_friendships_in_neighbourhood =~ Satisfied_with_number_of_friends + Visit_friends_at_their_place + Satisfied_with_number_of_people_I_know_in_neighbourhood
Satisfaction_with_events_in_neighbourhood =~ Older_adults_are_informed_about_activities + Sufficient_activities_are_aimed_at_or_adapted_to_older_adults + Activities_are_easily_accessible + Older_adults_are_sufficiently_involved_in_the_organisation_of_activities

#latent constructs mental health
#Satisfaction_with_life =~ SWLS_ideal_world + SWLS_excellent_conditions + SWLS_satisfied_with_life + SWLS_important_goals + SWLS_change_nothing
#Stress =~ Loss_of_sleep + Under_stress + Hard_to_overcome_difficulties + Feeling_unhappy_and_depressed 
#No_self_esteem =~ Playing_useful_part + Lost_confidence + Feeling_worthless + Feeling_reasonable_happy
#Successful_coping =~ Face_problems + Able_to_concentrate + Able_to_make_decisions + Able_to_enjoy_normal_activities

#latent constructs health-related wellbeing
Health_related_wellbeing =~ EQ_5D_index + VAS + Activity_rate_compared_to_peers

#latent constructs model

Built_environment =~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport
Social_environment =~ Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
#Mental_health =~ Satisfaction_with_life + Stress + No_self_esteem + Successful_coping
#Wellbeing =~ Health_related_wellbeing + Mental_health

#regressions

#Frequency_walking ~ Built_environment + Social_environment

#Frequency_walking ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
Frequency_walking ~ No_walking_friendly_environment + Cycling_friendly_environment + Negative_parking_experiences + Qualitative_public_transport + Attractive_living_environment + Satisfaction_with_friendships_in_neighbourhood

#Wellbeing ~ Frequency_walking

#Wellbeing ~ Built_environment
#Wellbeing ~ Social_environment 

#Wellbeing ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood

Health_related_wellbeing ~ Frequency_walking
#Mental_health ~ Frequency_walking

Health_related_wellbeing ~ Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Qualitative_public_transport + Encouragement_for_PA + Satisfaction_with_events_in_neighbourhood
#Mental_health ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
#Mental_health ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Traffic_safety + Qualitative_public_transport + Attractive_living_environment

#EQ_5D_index ~ Frequency_walking
#Stress ~ Frequency_walking
#No_self_esteem ~ Frequency_walking
#Successful_coping ~ Frequency_walking

#EQ_5D_index ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
#Stress ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
#No_self_esteem ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood
#Successful_coping ~ Poor_walking_infrastructure + No_walking_friendly_environment + Qualitative_cycling_infrastructure + Cycling_friendly_environment + Busy_traffic + Traffic_safety + Negative_parking_experiences + Qualitative_public_transport + Neighbourhood_social_cohesion + Attractive_living_environment + Encouragement_for_PA + Satisfaction_with_friendships_in_neighbourhood + Satisfaction_with_events_in_neighbourhood



#residual correlations / covariates
Social_environment ~~ Built_environment
#Health_related_wellbeing ~~ Mental_health

#Neighbourhood_social_cohesion ~~ Attractive_living_environment
#Neighbourhood_social_cohesion ~~ Encouragement_for_PA
#Neighbourhood_social_cohesion ~~ Satisfaction_with_friendships_in_neighbourhood
#Attractive_living_environment ~~ Encouragement_for_PA
#Attractive_living_environment ~~ Satisfaction_with_friendships_in_neighbourhood
#Encouragement_for_PA ~~ Satisfaction_with_friendships_in_neighbourhood
#EQ_5D_index ~~ VAS
#Stress ~~ No_self_esteem
#Stress ~~ Successful_coping
#No_self_esteem ~~ Successful_coping

'

fit <- sem(model, data = data, ordered = c("Frequency_walking"), std.ov = TRUE, estimator = "WLSMV", parameterization = "theta")

summary(fit, standardized = TRUE, fit.measures = TRUE)
semPaths(fit, 'std', 'est', curveAdjacent = TRUE, style = "lisrel")

# Get modification indices, filtering out indices below a threshold (e.g., 10)
mod_indices <- modificationIndices(fit)

#Filter on useful MI (above 10), sort, and show top 100
mod_df <- as.data.frame(mod_indices)

mod_filtered <- mod_df %>%
  filter(mi > 10) %>%
  arrange(desc(mi))

print(mod_filtered)
View(mod_filtered)









#A-priori power analyse RMSEA
install.packages("semPower")
library(semPower)

df     <- fitMeasures(fit, "df")
rmsea  <- fitMeasures(fit, "rmsea")
N      <- nobs(fit)

power_apriori <- semPower.aPriori(
  effect = .05,
  effect.measure = "RMSEA",
  alpha = .05,
  power = .80,
  df = df
)

summary(power_apriori)

#Monte Carlo power analyse
standardizedSolution(fit)
std <- standardizedSolution(fit)
head(std)
write.csv(std, "std_solution.csv", row.names = FALSE)



# Packages
library(lavaan)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)


ordered_vars <- c("Frequency_walking")

sample_sizes <- c(100, 150, 200, 250, 300, 400, 500, 600, 800, 1000)

n_rep <- 100      
alpha <- 0.05
set.seed(1234)


pop_par <- parTable(fit)


pop_par$ustart <- pop_par$est
pop_par$free <- 0


target_paths <- parameterEstimates(fit) %>%
  filter(op == "~") %>%
  select(lhs, op, rhs)

print(target_paths)


run_one_sim <- function(N, rep_id) {
  
  out_empty <- target_paths %>%
    mutate(
      N = N,
      rep = rep_id,
      converged = FALSE,
      est = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      significant = NA
    )
  
  simdat <- tryCatch(
    simulateData(
      model = pop_par,
      sample.nobs = N,
      ordered = ordered_vars
    ),
    error = function(e) {
      message("simulateData error at N = ", N, ", rep = ", rep_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(simdat)) return(out_empty)
  
  for (v in ordered_vars) {
    if (v %in% names(simdat)) {
      simdat[[v]] <- ordered(simdat[[v]])
    }
  }
  
  fit_sim <- tryCatch(
    suppressWarnings(
      sem(
        model,
        data = simdat,
        ordered = ordered_vars,
        std.ov = TRUE,
        estimator = "WLSMV",
        parameterization = "theta"
      )
    ),
    error = function(e) {
      message("lavaan error at N = ", N, ", rep = ", rep_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(fit_sim)) return(out_empty)
  
  if (!inspect(fit_sim, "converged")) return(out_empty)
  
  pe <- parameterEstimates(fit_sim) %>%
    filter(op == "~") %>%
    select(lhs, op, rhs, est, se, pvalue)
  
  target_paths %>%
    left_join(pe, by = c("lhs", "op", "rhs")) %>%
    mutate(
      N = N,
      rep = rep_id,
      converged = TRUE,
      significant = pvalue < alpha
    )
}


mc_results <- map_dfr(sample_sizes, function(N) {
  message("Running N = ", N)
  
  map_dfr(1:n_rep, function(i) {
    run_one_sim(N, i)
  })
})

# Controle
dim(mc_results)
names(mc_results)
head(mc_results)


power_table <- mc_results %>%
  group_by(N, lhs, rhs) %>%
  summarise(
    n_rep = n(),
    n_converged = sum(converged, na.rm = TRUE),
    convergence_rate = mean(converged, na.rm = TRUE),
    power = mean(significant, na.rm = TRUE),
    mean_est = mean(est, na.rm = TRUE),
    mean_se = mean(se, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(lhs, rhs, N)

print(power_table)

write.csv(power_table, "monte_carlo_power_results.csv", row.names = FALSE)


required_N_80 <- power_table %>%
  filter(power >= .80) %>%
  group_by(lhs, rhs) %>%
  summarise(
    required_N_80 = min(N),
    .groups = "drop"
  )

print(required_N_80)

write.csv(required_N_80, "required_N_80_power.csv", row.names = FALSE)


power_plot <- power_table %>%
  mutate(path = paste(rhs, "→", lhs))

ggplot(power_plot, aes(x = N, y = power, group = path)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .80, linetype = "dashed") +
  facet_wrap(~ path) +
  labs(
    x = "Sample size",
    y = "Power",
    title = "Monte Carlo power analysis for SEM regression paths"
  ) +
  theme_minimal()

ggsave("monte_carlo_power_plot.png", width = 14, height = 10, dpi = 300)