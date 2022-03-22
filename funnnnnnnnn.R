setwd("~/Downloads/Untitled Folder")
df <- read_csv("rankings_eff_df.csv")

df_sort <- df %>% 
  arrange(Team)

s_curve <- c(0,-1,1,0,-4,0,1,1,-2,1,-1,-4,-2,-9,0,-1,-4,1,-1,11,-1,
             6,6,0,0,0,0,-2,4,1,-8,9,-7,6,-4,3,-6,-2,1,6,3,3,-13,1,-4,-7,
             2,4,8,11,-5,4,-4,5,6,0,0,-1,2,2,-9,4,-1,13,-10,-1,-4,-3)

df_new <- df_sort %>% 
  mutate(s_curve = s_curve)

imp_df <- df_new %>% 
  select(Team, Tempo, Off_eff, Def_eff, Rk, s_curve)

avg_o <- mean(imp_df$Off_eff)
avg_d <- mean(imp_df$Def_eff)


matchup <- function(team1, team2) {
  
  team1_df <- imp_df %>% filter(Team == team1)
  team2_df <- imp_df %>% filter(Team == team2)
  
  kenpom <- team2_df$Rk - team1_df$Rk
  
  gg <- imp_df %>%
    filter(Team == team1 | Team == team2) %>% 
    ggplot(., aes(x = Off_eff, y = Def_eff, label = Team)) +
    geom_point() +
    xlab("Offensive Efficiency") +
    ylab("Defensive Efficiency") +
    geom_vline(xintercept = mean(imp_df$Off_eff), linetype = "dashed") +
    geom_hline(yintercept = mean(imp_df$Def_eff), linetype = "dashed") +
    geom_text(hjust = 0, vjust = 0) +
    annotate("text", x = 118, y = 84, label = c("Better at Offense, Better at Defense"),
             fontface = "bold") +
    annotate("text", x = 118, y = 110, label = c("Better at Offense, Worse at Defense"),
             fontface = "bold")+
    annotate("text", x = 103, y = 84, label = c("Worse at Offense, Better at Defense"),
             fontface = "bold")+
    annotate("text", x = 103, y = 110, label = c("Worse at Offense, Worse at Defense"),
             fontface = "bold") +
    theme_classic() +
    scale_y_reverse()
  
  s_curve <- paste("Team 1:", team1_df$s_curve, " Team 2:", team2_df$s_curve)
  
  data <- rbind(team1_df, team2_df)
  
  dif <- paste("Team1: ",round((team1_df$Off_eff - team2_df$Off_eff) - 
                 (team1_df$Def_eff - team2_df$Def_eff),2))
  
  list(gg = gg,
       data = data,
       dif = dif)
}

matchup("Murray St.", "San Francisco")
