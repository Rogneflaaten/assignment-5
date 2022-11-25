# backup

est_str <- emmeans(m1_str, specs = ~ time|sets)

figA_str <- est_str %>%
  data.frame() %>%
  
  ggplot(aes(time, emmean, group = sets, color = sets) ) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                position = position_dodge(width = 0.2), 
                width = 0.15,
                size = 0.4,
                color = "black") +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2),
             size = 3) +
  theme_classic() +
  
  # Changing axis titles and title in the legend
  labs(y = "kg", 
       color = "Treningsvolum") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())

# Save the confidence intervals
conf_intervals_str <- confint(m1_str)

# Save the regression coefficients
coefs_str <- summary(m1_str)$coef

# Using cbind (column bind) to combine the two data frames
coef_summary_str <- cbind(coefs_str, data.frame(conf_intervals_str)[3:14, ]) 

figB_str <- coef_summary_str %>%
  mutate(coef = rownames(.)) %>%
  # Filter only the interaction variables
  filter(coef %in% c("setsmultiple",
                     "timesession1:setsmultiple", 
                     "timeweek2:setsmultiple",
                     "timeweek5:setsmultiple",
                     "timeweek9:setsmultiple",
                     "timepost:setsmultiple")) %>%
  # Make a "timepoint" variable to represent the "dat" data set.
  mutate(time = gsub("time", "", coef), 
         time = gsub(":setsmultiple", "", time), 
         time = if_else(time == "setsmultiple", "pre", time)) %>%
  # Fix order of the time variable
  mutate(time = factor(time, levels = c("pre", "session1", "week2", "week5", "week9", "post"))) %>%
  
  # Create the plot
  ggplot(aes(time, Estimate)) + 
  
  
  # Add a line indicating zero geom_hline (horizontal line)
  geom_hline(yintercept = 0, lty = 2) +
  
  geom_errorbar(aes(ymin = X2.5.., ymax = X97.5..), width = 0.1) +
  geom_point(shape = 24, size = 3, fill = "orange") +
  theme_classic() +
  # Changing axis titles and title in the legend
  labs(y = "Gj.snitt gruppevis forskjell\n(kg, 95% CI)",
       x = "Tidspunkt") 


# Plot grid using figA and figB
plot_grid(figA_str, figB_str, ncol = 1, 
          # Align vertically so that the x axis matches
          align = "v", 
          # Set alignment so that axis align both on the left (l) and right (r)
          # axis
          axis = "lr")