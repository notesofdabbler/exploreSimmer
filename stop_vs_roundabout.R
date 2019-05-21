
library(simmer)
source("helper_fns.R")

# stop sign

intersection_time = 5

env = simmer()

carsN = trajectory() %>%
        seize("intersection") %>%
        timeout(intersection_time) %>% 
        release("intersection")

carsS = trajectory() %>%
  seize("intersection") %>%
  timeout(intersection_time) %>% 
  release("intersection")

carsE = trajectory() %>%
  seize("intersection") %>%
  timeout(intersection_time) %>% 
  release("intersection")

carsW = trajectory() %>%
  seize("intersection") %>%
  timeout(intersection_time) %>% 
  release("intersection")

envs = lapply(1:100, function(i) simmer() %>% add_resource("intersection", 1) %>%
        add_generator("carsN", carsN, function() rexp(1, 1/30)) %>%
        add_generator("carsS", carsS, function() rexp(1, 1/30)) %>%
        add_generator("carsE", carsE, function() rexp(1, 1/30)) %>%
        add_generator("carsW", carsW, function() rexp(1, 1/30)) %>%
        run(until = 3600))

arrivals_df = env %>% get_mon_arrivals()
resources_df = envs %>% get_mon_resources()

utilization_df = get_utilization_df(resources_df)
get_avgQlength_df(resources_df)

# round about

segment_time = 5

env = simmer()

carsN = trajectory() %>%
        seize("segmentNW") %>%
        timeout(segment_time) %>%
        release("segmentNW") %>%
        leave(prob = 0.33) %>%
        seize("segmentWS") %>%
        timeout(segment_time) %>%
        release("segmentWS") %>% 
        leave(prob = 0.5) %>%
        seize("segmentSE") %>%
        timeout(segment_time) %>%
        release("segmentSE")

carsW = trajectory() %>%
  seize("segmentWS") %>%
  timeout(segment_time) %>%
  release("segmentWS") %>%
  leave(prob = 0.33) %>%
  seize("segmentSE") %>%
  timeout(segment_time) %>%
  release("segmentSE") %>% 
  leave(prob = 0.5) %>%
  seize("segmentEN") %>%
  timeout(segment_time) %>%
  release("segmentEN")

carsS = trajectory() %>%
  seize("segmentSE") %>%
  timeout(segment_time) %>%
  release("segmentSE") %>%
  leave(prob = 0.33) %>%
  seize("segmentEN") %>%
  timeout(segment_time) %>%
  release("segmentEN") %>% 
  leave(prob = 0.5) %>%
  seize("segmentNW") %>%
  timeout(segment_time) %>%
  release("segmentNW")

carsE = trajectory() %>%
  seize("segmentEN") %>%
  timeout(segment_time) %>%
  release("segmentEN") %>%
  leave(prob = 0.33) %>%
  seize("segmentNW") %>%
  timeout(segment_time) %>%
  release("segmentNW") %>% 
  leave(prob = 0.5) %>%
  seize("segmentWS") %>%
  timeout(segment_time) %>%
  release("segmentWS")

envs = lapply(1:100, function(i) simmer() %>% add_resource("segmentNW", 1) %>%
        add_resource("segmentWS", 1) %>%
        add_resource("segmentSE", 1) %>%
        add_resource("segmentEN", 1) %>%
        add_generator("carsN", carsN, function() rexp(1, 1/30)) %>%
        add_generator("carsS", carsS, function() rexp(1, 1/30)) %>%
        add_generator("carsE", carsE, function() rexp(1, 1/30)) %>%
        add_generator("carsW", carsW, function() rexp(1, 1/30)) %>%
        run(until = 3600))

arrivals_df = envs %>% get_mon_arrivals()
resources_df = envs %>% get_mon_resources()

utilization_df = get_utilization_df(resources_df)
get_avgQlength_df(resources_df)
