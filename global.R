# ----- 3. Loading in datasets -----
TidyData <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 10/Practical Files/Files for Practical 13-20240319/Tidy Data (World Cup 2018)_Migrated Data.csv")
ShotData <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 10/Practical Files/Files for Practical 13-20240319/Shot Timeline (World Cup 2018)_Migrated Data.csv")

# ----- 4. Creating new Dataframes -----

# First we want to create a dataframe which contains the unique matches and their relevant stage. This will help our filtering later on.
Match_list <- TidyData %>%
  group_by(Match, Stage) %>%
  reframe()

# ----- 5. Creating plot Functions -----

# Remember in a function you add the input (in our case that is the data set we will use, and the two filter settings users of our dashboard will select)

lineup_home <- function(data, stage="Final", match="France vs. Croatia"){
  data <- data %>%
    filter(Stage == stage & Match == match & Team==Home & Sub.=="FALSE" & Player.Name!= "Own G") %>%
    mutate(P = ifelse(Position == "GK", 1, ifelse(Position == "DR",2,ifelse(Position == "DC",3, ifelse(Position == "DL",4,ifelse(Position == "DMR",5,
                                                                                                                                 ifelse(Position == "DMC",6, ifelse(Position == "DML",7, ifelse(Position == "MR",8, ifelse(Position == "MC",9, ifelse(Position == "ML",10,
                                                                                                                                                                                                                                                      ifelse(Position == "AMR",11,ifelse(Position == "AMC",12, ifelse(Position == "AML",13, ifelse(Position == "FWR",14, ifelse(Position == "FW",15,
                                                                                                                                                                                                                                                                                                                                                                                ifelse(Position == "FWL",16, 17))))))))))))))))) %>%
    arrange(P)
  data <- (data[,c("Player.Name","Subbed.","Position")])
  datatable(data)
}

# this part of the function is your usual code for creating your visualisation. We first filter based on the stage, match and home team (note I also filter out an error entry called "Own G"). Then we give each position a number via mutate (this will help us order our table) and last we create a new dataset with just the Player.name, whether they were subbed and their position which we then list in a datatable.


# do the same for the away team

lineup_away <- function(data, stage="Final", match="France vs. Croatia"){
  data <- data %>%
    filter(Stage==stage & Match==match & Team==Away1 & Sub.=="FALSE" & Player.Name!= "Own G") %>%
    mutate(P = ifelse(Position == "GK", 1, ifelse(Position == "DR",2,ifelse(Position == "DC",3, ifelse(Position == "DL",4,ifelse(Position == "DMR",5,
                                                                                                                                 ifelse(Position == "DMC",6, ifelse(Position == "DML",7, ifelse(Position == "MR",8, ifelse(Position == "MC",9, ifelse(Position == "ML",10,
                                                                                                                                                                                                                                                      ifelse(Position == "AMR",11,ifelse(Position == "AMC",12, ifelse(Position == "AML",13, ifelse(Position == "FWR",14, ifelse(Position == "FW",15,
                                                                                                                                                                                                                                                                                                                                                                                ifelse(Position == "FWL",16, 17))))))))))))))))) %>%
    arrange(P)
  data <- data[,c("Player.Name","Subbed.","Position")]
  datatable(data)
}

# ----- 6. Functions to put home and away summary statistics -----

stats_home <- function(data, stage="Final", match="France vs. Croatia"){
  data <- data %>%
    filter(Stage==stage & Match==match & Team==Home) %>%
    reframe(Shots = sum(Shots,na.rm=TRUE),
            ShotsOnTarget = sum(Shots.OT,na.rm=TRUE),
            ShotAccuracy = as.integer((ShotsOnTarget/Shots)*100),
            Passes = sum(Passes,na.rm=TRUE),
            KeyPasses = sum(Key.Passes,na.rm=TRUE),
            Regains=sum(Interceptions,na.rm=TRUE)+sum(Total.Tackles,na.rm=TRUE))
}

stats_away <- function(data, stage="Final", match="France vs. Croatia"){
  data <- data %>%
    filter(Stage==stage & Match==match & Team==Away1) %>%
    reframe(Shots = sum(Shots,na.rm=TRUE),
            ShotsOnTarget = sum(Shots.OT,na.rm=TRUE),
            ShotAccuracy = as.integer((ShotsOnTarget/Shots)*100),
            Passes = sum(Passes,na.rm=TRUE),
            KeyPasses = sum(Key.Passes,na.rm=TRUE),
            Regains=sum(Interceptions,na.rm=TRUE)+sum(Total.Tackles,na.rm=TRUE))
}

# ----- 7. Functions for Shot Timeline -----

plot_timeline <- function(data, stage="Final", match="France vs. Croatia") {
  data <- data %>%
    filter(Stage==stage & Match == match) %>%
    arrange(Time1, Goal)%>%
    group_by(Team1) %>%
    mutate(CS=cumsum(Number.of.Records),
           Goals=ifelse(Goal==1,CS, NA))
  
  ggplot(data,aes(Time1,CS, color= Team1, group = Team1))+
    geom_step()+ geom_point(aes(Time1,Goals, group=Team1))+theme_minimal()+
    labs(y="Number of shots", x="Time of Play")
}

plot_shots <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Shots)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Shots), x=Shots, fill=Team))+
    geom_col()+
    labs(y="",x="Shots taken")
}

plot_passes <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Passes)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Passes), x=Passes, fill=Team))+
    geom_col()+
    labs(y="",x="Shots taken")
}

plot_keypasses <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Key.Passes)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Key.Passes), x=Key.Passes, fill=Team))+
    geom_col()+
    labs(y="",x="Passes")
}
plot_regains <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Tackle...Interceptions)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Tackle...Interceptions), x=Tackle...Interceptions, fill=Team))+
    geom_col()+
    labs(y="",x="Regains")
}
plot_touches <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Touches)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Touches), x=Touches, fill=Team))+
    geom_col()+
    labs(y="",x="Touches")
}
plot_turnovers <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- TidyData %>%
    filter(Stage==stage & Match == match) %>%
    arrange(desc(Turnovers)) %>%
    slice_head(n=5)
  
  data %>%
    ggplot(aes(y=reorder(Player.Name, Turnovers), x=Turnovers, fill=Team))+
    geom_col()+
    labs(y="",x="Turnovers")
}

text_results <- function(data, stage="Final", match="France vs. Croatia") {
  
  data <- data %>%
    filter(Stage==stage & Match==match & Team==Home) %>%
    reframe(goals=sum(Goal, na.rm=TRUE))
    goalshome <- as.data.frame(data$goals)
  
  data <- TidyData %>%
    filter(Stage==stage & Match==match & Team==Away1) %>%
    reframe(goals=sum(Goal, na.rm=TRUE))
    goalsaway <- as.data.frame(data$goals)
  
  results <- paste(goalshome, " - ", goalsaway)
}

