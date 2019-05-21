#
# https://r-simmer.org/articles/simmer-05-simpy.html
# Gas station example
#

library(simmer)

GAS_STATION_SIZE = 200 # liters
THRESHOLD = 10 # threshold for calling tanker truck (in %)
FUEL_TANK_SIZE = 50 # liters
FUEL_TANK_LEVEL = c(5, 25) # Min/Max level of fuel tank
REFUELING_SPEED = 2 # liters per second
TANK_TRUCK_TIME = 300 # time it takes for tanker truck to arrive
T_INTER = c(30, 100) # create a car every [min, max] seconds
SIM_TIME = 1000 # simulation time in seconds

GAS_STATION_LEVEL = GAS_STATION_SIZE
signal = "gas station refilled"

env = simmer()

refuelling <- trajectory() %>%
  # check if there is enough fuel available
  branch(function() FUEL_TANK_SIZE - get_attribute(env, "level") > GAS_STATION_LEVEL, 
         continue = TRUE,
         # if not, block until the signal "gas station refilled" is received
         trajectory() %>%
           trap(signal) %>%
           wait() %>%
           untrap(signal)
  ) %>%
  # refuel
  timeout(function() {
    liters_required <- FUEL_TANK_SIZE - get_attribute(env, "level")
    GAS_STATION_LEVEL <<- GAS_STATION_LEVEL - liters_required
    return(liters_required / REFUELING_SPEED)
  })

tank_truck <- trajectory() %>%
  timeout(TANK_TRUCK_TIME) %>%
  log_("tank truck arriving at gas station") %>%
  log_(function() {
    refill <- GAS_STATION_SIZE - GAS_STATION_LEVEL
    GAS_STATION_LEVEL <<- GAS_STATION_SIZE
    paste0("tank truck refilling ", refill, " liters")
  }) %>%
  send(signal)

car = trajectory() %>%
      log_("arriving at gas station") %>%
      set_attribute(c("start", "level"), function() 
        c(now(env), sample(FUEL_TANK_LEVEL[1]:FUEL_TANK_LEVEL[2], 1))) %>%
      seize("pump", 1) %>%
      join(refuelling) %>%
      release("pump", 1) %>%
      log_(function() paste0("Finished refeuling in ", now(env) - get_attribute(env, "start")," seconds"))

controller = trajectory() %>%
             branch(function() GAS_STATION_LEVEL / GAS_STATION_SIZE * 100 < THRESHOLD,
                    continue = TRUE,
                    trajectory() %>%
                    log_("calling tanker truck") %>%
                    join(tank_truck)
                    ) %>%
              timeout(10) %>%
              rollback(2, Inf)

env %>%
  add_resource("pump", 2) %>%
  add_generator("controller", controller, at(0)) %>%
  add_generator("car", car, function() sample(T_INTER[1]:T_INTER[2], 1)) %>%
  run(SIM_TIME)