
#
# Working through
# https://r-simmer.org/articles/simmer-01-introduction.html
#

# load library
library(simmer)
library(simmer.plot)

set.seed(1234)

env = simmer("testenv")
env

patient = trajectory("patient's path") %>%
        seize("nurse", 1) %>% 
        timeout(function() rnorm(1, 15)) %>%
        release("nurse", 1) %>%
        seize("doctor", 1) %>%
        timeout(function() rnorm(1, 20)) %>%
        release("doctor", 1) %>%
        seize("administration", 1) %>%
        timeout(function() rnorm(1, 5)) %>%
        release("administration", 1) 
        
env %>% add_resource("nurse", 1) %>%
        add_resource("doctor", 2) %>%
        add_resource("administration", 1) %>%
        add_generator("patient", patient, function() rexp(1, 0.1))

env %>% run(80)
now(env)

env %>% get_mon_resources()
env %>% get_mon_arrivals(ongoing = TRUE)
env %>% get_mon_arrivals(per_resource = TRUE, ongoing = TRUE)

resources_df = env %>% get_mon_resources()
plot(resources_df, metric = "utilization")
plot(resources_df, metric = "usage", c("nurse", "doctor"), items = "server")

plot(resources_df, metric = "usage", c("nurse", "doctor"), items = "server", steps = TRUE)

envs <- lapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80)
})

resources_df = envs %>% get_mon_resources()
plot(resources_df, metric = "utilization")
