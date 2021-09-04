# The Most Basic of our SOC Simulations. SIMPLISTIC approach for sure...

# All "alerts" are treated the same.
#   In this version, there is a regular alert generation period (1 alert / period of time)
# And some Analysts to consume alerts

#library(testthat) # We'll use that at some point...
library(plyr)

# Initialize the System:
# An object to represent our System (Very BASIC SOC simulation)
initialize_soc_simulator <- function(alerts_rcv_interval = 10,
                                     alerts_cons_speed = c(5)) {
  c_agents <- rbind.fill(lapply(1:length(alerts_cons_speed), function(x) {
    data.frame(agent_code = x, 
               ag_resolution_time = alerts_cons_speed[x], 
               ag_occupied = FALSE,
               ag_current_alert = -1,
               ag_next_avail = 0)
  }))
  
  system_status <- list(a_rcv_int = alerts_rcv_interval,
                        alerts_queue = c(),
                        cons_agents = c_agents, # Analysts are "Consumer Agents"
                        treated_alerts_hist = data.frame(alert_generation_time = NULL, 
                                                          alerts_resolved_time = NULL, 
                                                          agent_code = NULL)) 
  system_status
}

# We logically treat our "alerts_queue" as a FIFO with the following functions:
add_alerts_stable <- function(alerts_queue, alerts_recv_interval, t_mark) {
  # Add to the end of the FIFO
  if(t_mark %% alerts_recv_interval == 0) alerts_queue <- c(alerts_queue, t_mark)
  alerts_queue
}

consume_1_alert <- function(alerts_queue, t_mark) { # Get 1 alert from the FIFO
  if(length(alerts_queue) > 0) { # There are alerts to consume
    alert_orig_t <- alerts_queue[1]
    alerts_queue <- alerts_queue[-1]
    return(list(alert_orig_t, alerts_queue))
  }
  NULL
}

consume_alerts <- function(system_status, t_mark) {
  # For actual simulation: Passing the system_status object with each call will slow things down a lot.
  # We'll consider using a Global Object for the System status later...

  for(i in 1:nrow(system_status$cons_agents)) {
    agent_status <- system_status$cons_agents[i,]
    
    if(agent_status$ag_occupied) { # Agent (e.g. SOC Analyst) is currently busy
      # Has the agent finished with its last alert?
      if(t_mark == agent_status$ag_next_avail) {
        # Add resolution to history of alerts treatment table
        system_status$treated_alerts_hist <- rbind(system_status$treated_alerts_hist,
                                                   data.frame(alert_generation_time = agent_status$ag_current_alert,
                                                              alerts_resolved_time = t_mark,
                                                              agent_code = agent_status$agent_code))
        # Update the Agent Status
        agent_status$ag_occupied <- FALSE
        agent_status$ag_current_alert <- -1
      }
    } else { # Agent is available
      changed_alerts_queue <- consume_1_alert(system_status$alerts_queue, t_mark)
      if(!is.null(changed_alerts_queue)) { 
        t_alert <- changed_alerts_queue[[1]]
        system_status$alerts_queue <- changed_alerts_queue[[2]]
        
        agent_status$ag_occupied <- TRUE
        agent_status$ag_current_alert <- t_alert
        agent_status$ag_next_avail <- t_mark + agent_status$ag_resolution_time
      }
    }
    
    # Update the system's agents' data.frame according to changes of Agent's status:
    system_status$cons_agents[i,] <- agent_status
  } # End loop on agents' status
  
  system_status
}

run_system_simulation_v001 <- function(system_status, simulation_minutes) {
  for(t_mark in 1:simulation_minutes) {
    system_status$alerts_queue <- add_alerts_stable(system_status$alerts_queue,
                                            system_status$a_rcv_int,
                                            t_mark)
    system_status <- consume_alerts(system_status, t_mark)
  }
  system_status
}

present_results <- function(system_status, simulation_time) {
  # Basic stuff for a simple visualization of results...
  print(paste("Ran simulation on a period of", simulation_time, "minutes."))
  print(paste("Total Alerts Left untreated:", length(system_status$alerts_queue)))
  print(paste("Alerts in Queue, by reception time:", paste0(system_status$alerts_queue, collapse=",")))
  print("Consumed Alerts Table:")
  print(head(system_status$treated_alerts_hist))
  print("...")
  print(tail(system_status$treated_alerts_hist))
}

##
# Main run
##

simulation_period <- 8*60 # 1 work turn (8*60 minutes)

# Now let's run our system for , just to see what happens:
system_simu_001 <- initialize_soc_simulator(alerts_rcv_interval = 10, 
                                            alerts_cons_speed = c(11))
system_simu_001 <- run_system_simulation_v001(system_simu_001, simulation_period)
present_results(system_simu_001, simulation_period)
