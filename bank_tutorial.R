
#
# Working through
# https://r-simmer.org/articles/simmer-04-bank-1.html
#

library(simmer)
library(dplyr)
library(simmer.plot)

# 2 counters but with single queue

customer = trajectory() %>%
           seize("counter") %>%
           timeout(function() rexp(1, 1/8)) %>% 
           release("counter")
           
banks = lapply(1:100, function(i) simmer("bank") %>% add_resource("counter", 2) %>%
        add_generator("customer", customer, function() rexp(1, 1/5)) %>%
        run(until = 4000))

arrivals_df = banks %>% get_mon_arrivals() %>% mutate(wait_time = end_time - start_time - activity_time)
resources_df = banks %>% get_mon_resources()

utilization_df = get_utilization_df(resources_df)
ggplot(utilization_df) + 
  geom_bar(aes(x = resource, y = Q50), stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(x = resource, ymin = Q25, ymax = Q75), width = 0.3) + 
  xlab("") + ylab("utilization") + scale_y_continuous(labels = scales::percent) + 
  theme_bw()

timeint = seq(0, 4000, 10)
timeintlab = timeint[2:length(timeint)]
arrivals_df = arrivals_df %>% filter(!is.na(end_time)) %>% 
  mutate(endtimebuck = cut(end_time, timeint, timeintlab))
arrivals_df = arrivals_df %>% mutate(endtimebuck = as.numeric(as.character(endtimebuck)))
arrivals_df = arrivals_df %>% mutate(const = 1)

arrivals_df2 = arrivals_df %>% group_by(replication) %>% arrange(replication, end_time) %>% mutate(cum_avg_wait_time = cumsum(wait_time) / cumsum(const))
arrivals_df3 = arrivals_df2 %>% ungroup() %>% group_by(replication, endtimebuck) %>% slice(n())
arrivals_df4 = arrivals_df3 %>% group_by(endtimebuck) %>% summarize(cumavg_wait_time_lo = quantile(cum_avg_wait_time, 0.25),
                                                                    cumavg_wait_time_med = quantile(cum_avg_wait_time, 0.5),
                                                                    cumavg_wait_time_hi = quantile(cum_avg_wait_time, 0.75)
                                                                    )

ggplot(arrivals_df4) + geom_line(aes(x = endtimebuck, y = cumavg_wait_time_med)) +
    geom_ribbon(aes(x = endtimebuck, ymin = cumavg_wait_time_lo, ymax = cumavg_wait_time_hi), alpha = 0.2) + 
    theme_bw()

wait_time_summ = arrivals_df %>% group_by(replication) %>% summarize(avg_wait_time = mean(wait_time))
wait_time_summ2 = wait_time_summ %>% 
                  summarize(avg_wait_time_lo = quantile(avg_wait_time, 0.25),
                            avg_wait_time_med = quantile(avg_wait_time, 0.5),  
                            avg_wait_time_hi = quantile(avg_wait_time, 0.75)
                                               )

# 2 counters with separate queues

customer = trajectory() %>%
  simmer::select(c("counter1", "counter2"), policy = "shortest-queue") %>%
  seize_selected() %>%
  timeout(function() rexp(1, 1/8)) %>% 
  release_selected()

banks = lapply(1:100, function(i) simmer("bank") %>% add_resource("counter1", 1) %>%
                 add_resource("counter2", 1) %>%
                 add_generator("customer", customer, function() rexp(1, 1/5)) %>%
                 run(until = 4000))

arrivals_df = banks %>% get_mon_arrivals() %>% mutate(wait_time = end_time - start_time - activity_time)
resources_df = banks %>% get_mon_resources()

utilization_df = get_utilization_df(resources_df)
ggplot(utilization_df) + 
  geom_bar(aes(x = resource, y = Q50), stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(x = resource, ymin = Q25, ymax = Q75), width = 0.3) + 
  xlab("") + ylab("utilization") + scale_y_continuous(labels = scales::percent) + 
  theme_bw()

timeint = seq(0, 4000, 10)
timeintlab = timeint[2:length(timeint)]
arrivals_df = arrivals_df %>% filter(!is.na(end_time)) %>% 
  mutate(endtimebuck = cut(end_time, timeint, timeintlab))
arrivals_df = arrivals_df %>% mutate(endtimebuck = as.numeric(as.character(endtimebuck)))
arrivals_df = arrivals_df %>% mutate(const = 1)

arrivals_df2 = arrivals_df %>% group_by(replication) %>% arrange(replication, end_time) %>% mutate(cum_avg_wait_time = cumsum(wait_time) / cumsum(const))
arrivals_df3 = arrivals_df2 %>% ungroup() %>% group_by(replication, endtimebuck) %>% slice(n())
arrivals_df4 = arrivals_df3 %>% group_by(endtimebuck) %>% summarize(cumavg_wait_time_lo = quantile(cum_avg_wait_time, 0.25),
                                                                    cumavg_wait_time_med = quantile(cum_avg_wait_time, 0.5),
                                                                    cumavg_wait_time_hi = quantile(cum_avg_wait_time, 0.75)
)

ggplot(arrivals_df4) + geom_line(aes(x = endtimebuck, y = cumavg_wait_time_med)) +
  geom_ribbon(aes(x = endtimebuck, ymin = cumavg_wait_time_lo, ymax = cumavg_wait_time_hi), alpha = 0.2) + 
  theme_bw()

wait_time_summ = arrivals_df %>% group_by(replication) %>% summarize(avg_wait_time = mean(wait_time))
wait_time_summ2 = wait_time_summ %>% 
  summarize(avg_wait_time_lo = quantile(avg_wait_time, 0.25),
            avg_wait_time_med = quantile(avg_wait_time, 0.5),  
            avg_wait_time_hi = quantile(avg_wait_time, 0.75)
  )
wait_time_summ2

# 2 types of customers (one has more service time, other has less)
# 2 resources (one is for customers with less service time, other for customers 
#  with more service time but will process others when free)

arrtime1 = 15
servicetime1 = 10

arrtime2 = 8
servicetime2 = 5

env = simmer()

cust1 = trajectory() %>%
        log_(function() paste0("arrive at ", now(env))) %>% 
        seize("counter1") %>%
        timeout(function() servicetime1) %>%
        release("counter1")

cust2 = trajectory() %>%
        log_(function() paste0("counter1 queue count is", get_queue_count(env, "counter1"))) %>%
        log_(function() paste0("counter1 server count is", get_server_count(env, "counter1"))) %>%
        seize("counter2") %>%
        timeout(function() servicetime2) %>%
        release("counter2")

env %>% add_resource("counter1", 1) %>%
        add_resource("counter2", 1) %>%
        add_generator("cust1", cust1, at(0)) %>%
        add_generator("cust2", cust2, at(0, 3)) %>%
        run(until = 100)


