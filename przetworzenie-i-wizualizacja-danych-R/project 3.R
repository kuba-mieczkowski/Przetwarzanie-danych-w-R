#Projekt R 3 ~ Paulina Kulczyk & Jakbu Mieczkowski

##biblioteki
library(readr)
library(sqldf)
library(ggplot2)
library(wordcloud2)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
library(SnowballC)
library(dplyr)
library(rgl)
library(DT)



### Ramki danych
Comments_Cooking <- read_csv("Projekt2/Cooking/Comments.xml.csv")
Posts_Cooking <- read_csv("Projekt2/Cooking/Posts.xml.csv")
Tags_Cooking <- read_csv("Projekt2/Cooking/Tags.xml.csv")
Users_Cooking <- read_csv("Projekt2/Cooking/Users.xml.csv")

Comments_Gardening <- read_csv("Projekt2/gardening/Comments.xml.csv")
Posts_Gardening <- read_csv("Projekt2/gardening/Posts.xml.csv")
Tags_Gardening <- read_csv("Projekt2/gardening/Tags.xml.csv")
Users_Gardening <- read_csv("Projekt2/gardening/Users.xml.csv")


Comments_Physics <- read_csv("Projekt2/physics/Comments.xml.csv")
Posts_Physics <- read_csv("Projekt2/physics/Posts.xml.csv")
Tags_Physics <- read_csv("Projekt2/physics/Tags.xml.csv")
Users_Physics <- read_csv("Projekt2/physics/Users.xml.csv")

# Kod

### The best users

The_best_users_cooking <- function(Users_Cooking){
  sqldf("SELECT AccountId, Reputation, DisplayName, Location
  FROM Users_Cooking
  ORDER BY Reputation DESC
  LIMIT 100")}
The_best_users_cooking <- The_best_users_cooking(Users_Cooking)
The_best_users_c_tab <- function(The_best_users_cooking){
  coul <- brewer.pal(8, "Set2")
  barplot(The_best_users_cooking[1:10,2],
          legend.text = The_best_users_cooking[1:10,3],
          col=coul, main = "The best users cooking", 
          xlab = "Best Users", ylab = "Reputation" )
  options(scipen = 5)
  geom_text()
}
The_best_users_c_tab <- The_best_users_c_tab(The_best_users_cooking)

The_best_users_gardening <- function(Users_Gardening){
  sqldf("SELECT AccountId, Reputation, DisplayName, Location
  FROM Users_Gardening
  ORDER BY Reputation DESC
  LIMIT 100")}
The_best_users_gardening <- The_best_users_gardening(Users_Gardening)
The_best_users_g_tab <- function(The_best_users_gardening){
  coul <- brewer.pal(8, "Set2")
  barplot(The_best_users_gardening[1:10,2],
          legend.text = The_best_users_gardening[1:10,3],
          col=coul, main = "The best users gardening", 
          xlab = "Best Users", ylab = "Reputation" )
  options(scipen = 5)
  
}
The_best_users_g_tab <- The_best_users_g_tab(The_best_users_gardening)


The_best_users_physics <- function(Users_Physics){
  sqldf("SELECT AccountId, Reputation, DisplayName, Location
  FROM Users_Physics
  ORDER BY Reputation DESC
  LIMIT 100")}
The_best_users_physics<- The_best_users_physics(Users_Physics)

The_best_users_p_tab <- function(The_best_users_physics){
  coul <- brewer.pal(8, "Set2")
  barplot(The_best_users_physics[1:10,2],
          legend.text = The_best_users_physics[1:10,3],
          col=coul, main = "The best users physics",
          xlab = "Best Users", ylab = "Reputation" )
  options(scipen = 5)
  
}
The_best_users_p_tab <- The_best_users_p_tab(The_best_users_physics)


The_best_of_all <- function(Users_Cooking, Users_Gardening, Users_Physics){
  sqldf("SELECT
  tab_physics.AccountId,
  tab_physics.DisplayName,
  tab_gardening.Reputation_g,
  tab_physics.Reputation_p,
  tab_cooking.Reputation_c,
  tab_physics.Reputation_p+tab_gardening.Reputation_g+
  tab_cooking.Reputation_c AS sum_reputation
  FROM (
  SELECT AccountId,
  Reputation AS Reputation_p,
  DisplayName
  FROM Users_Physics) AS tab_physics
  LEFT JOIN (
  SELECT AccountId AS AccountId_g,
  Reputation AS Reputation_g,
  DisplayName  AS DisplayName_g
  FROM Users_Gardening) AS tab_gardening
  ON tab_physics.AccountId=tab_gardening.AccountId_g
  LEFT JOIN (
  SELECT AccountId AS AccountId_c,
  Reputation AS Reputation_c,
  DisplayName AS DisplayName_c
  FROM Users_Cooking) AS tab_cooking
  ON tab_gardening.AccountId_g=tab_cooking.AccountId_c
  ORDER BY sum_reputation DESC
  LIMIT 10
        ")
}


The_best_of_all <- The_best_of_all(
  Users_Cooking, Users_Gardening, Users_Physics)

nazwy <- c(rep(The_best_of_all[1,2], 3), rep(The_best_of_all[2,2], 3),
           rep(The_best_of_all[3,2], 3),rep(The_best_of_all[4,2], 3),
           rep(The_best_of_all[5,2], 3),rep(The_best_of_all[6,2], 3),
           rep(The_best_of_all[7,2], 3),rep(The_best_of_all[8,2], 3),
           rep(The_best_of_all[9,2], 3), rep(The_best_of_all[10,2], 3))
fora <- rep(c("Gardening", "Physics", "Cooking"), 10)
wartosci <- c(The_best_of_all[1,3], The_best_of_all[1,4],The_best_of_all[1,5],
              The_best_of_all[2,3], The_best_of_all[2,4],The_best_of_all[2,5],
              The_best_of_all[3,3], The_best_of_all[3,4],The_best_of_all[3,5],
              The_best_of_all[4,3], The_best_of_all[4,4],The_best_of_all[4,5],
              The_best_of_all[5,3], The_best_of_all[5,4],The_best_of_all[5,5],
              The_best_of_all[6,3], The_best_of_all[6,4],The_best_of_all[6,5],
              The_best_of_all[7,3], The_best_of_all[7,4],The_best_of_all[7,5],
              The_best_of_all[8,3], The_best_of_all[8,4],The_best_of_all[8,5],
              The_best_of_all[9,3], The_best_of_all[9,4],The_best_of_all[9,5],
              The_best_of_all[10,3],The_best_of_all[10,4],The_best_of_all[10,5])
data <- data.frame(nazwy,fora, wartosci)

ggplot(data, aes(fill=fora, y=wartosci, x=nazwy)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Best users") +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")


### The most active users

The_most_active_users_C_Q <- function(Posts_Cooking, Users_Cooking){
  sqldf("SELECT tab2.DisplayName, tab1.Question_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Question_Freq
        FROM Posts_Cooking
        WHERE PostTypeId == 1
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Cooking) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Question_Freq DESC
        LIMIT 100")
}
The_most_active_users_C_A <- function(Posts_Cooking, Users_Cooking){
  sqldf("SELECT tab2.DisplayName, tab1.Answer_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Answer_Freq
        FROM Posts_Cooking
        WHERE PostTypeId == 2
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Cooking) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Answer_Freq DESC
        LIMIT 100")
  
}

The_most_active_users_C_Q <- The_most_active_users_C_Q(Posts_Cooking, Users_Cooking)
The_most_active_users_C_A <-  The_most_active_users_C_A(Posts_Cooking, Users_Cooking)

ggplot(The_most_active_users_C_Q[1:10,], aes(x=DisplayName, y=Question_Freq, fill =Question_Freq))+
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = 7) +
  ggtitle("The most questions cooking")+
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(The_most_active_users_C_A[1:10,], aes(x=DisplayName, y=Answer_Freq, fill = Answer_Freq))+
  geom_bar(stat = "identity") +
  ggtitle("The most answers cooking")+
  scale_fill_distiller(palette = 7) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

correlation_c <- function(The_most_active_users_C_A, The_most_active_users_C_Q, The_best_users_cooking){
  Active_users_c <- merge(The_most_active_users_C_A, The_most_active_users_C_Q, by="DisplayName", all=TRUE)
  Active_users_c_ <- merge(Active_users_c, The_best_users_cooking, by="DisplayName", all=TRUE)
  Active_users_c_$Answer_Freq[is.na(Active_users_c_$Answer_Freq)] <- 0
  Active_users_c_$Question_Freq[is.na(Active_users_c_$Question_Freq)] <- 0
  Active_users_c_$Reputation[is.na(Active_users_c_$Reputation)] <- 0
  Active_users_c_ <-Active_users_c_[order(Active_users_c_$Reputation, decreasing = T)[1:50], ]
  Active_users_c_
}
cos <-correlation_c(The_most_active_users_C_A, The_most_active_users_C_Q, The_best_users_cooking)
ggplot(correlation_c(The_most_active_users_C_A, The_most_active_users_C_Q, The_best_users_cooking),
       aes(x=Question_Freq, y=Answer_Freq, size=Reputation))+
  scale_x_continuous(limits = c(0,200))+
  scale_y_continuous(limits = c(0,1800))+
  ggtitle("Correlation")+
  geom_point(alpha=0.4, color = "pink")+
  theme_ipsum() 




The_most_active_users_G_A <- function(Posts_Gardening, Users_Gardening){
  sqldf("SELECT tab2.DisplayName,  tab1.Answer_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Answer_Freq
        FROM Posts_Gardening
        WHERE PostTypeId == 2
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Gardening) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Answer_Freq DESC
        LIMIT 100")
}

The_most_active_users_G_Q <- function(Posts_Gardening, Users_Gardening){
  sqldf("SELECT tab2.DisplayName, tab1.Question_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Question_Freq
        FROM Posts_Gardening
        WHERE PostTypeId == 1
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Gardening) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Question_Freq DESC
        LIMIT 100")
}
The_most_active_users_G_Q <- The_most_active_users_G_Q(Posts_Gardening, Users_Gardening)
The_most_active_users_G_A <-  The_most_active_users_G_A(Posts_Gardening, Users_Gardening)

ggplot(The_most_active_users_G_Q[1:10,], aes(x=DisplayName, y=Question_Freq, fill =Question_Freq))+
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = 2) +
  ggtitle("The most questions gardening") +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(The_most_active_users_G_A[1:10,], aes(x=DisplayName, y=Answer_Freq, fill = Answer_Freq))+
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = 2) +
  ggtitle("The most answers gardening")+
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
correlation_g <- function(The_most_active_users_G_A, The_most_active_users_G_Q, The_best_users_gardening){
  Active_users_g <- merge(The_most_active_users_G_A, The_most_active_users_G_Q, by="DisplayName", all=TRUE)
  Active_users_g_ <- merge(Active_users_g, The_best_users_gardening, by="DisplayName", all=TRUE)
  Active_users_g_$Answer_Freq[is.na(Active_users_g_$Answer_Freq)] <- 0
  Active_users_g_$Question_Freq[is.na(Active_users_g_$Question_Freq)] <- 0
  Active_users_g_$Reputation[is.na(Active_users_g_$Reputation)] <- 0
  Active_users_g_ <-Active_users_g_[order(Active_users_g_$Reputation, decreasing = T)[1:50], ]
  Active_users_g_
}
cos <-correlation_g(The_most_active_users_G_A, The_most_active_users_G_Q, The_best_users_gardening)
ggplot(correlation_g(The_most_active_users_G_A, The_most_active_users_G_Q, The_best_users_gardening),
       aes(x=Question_Freq, y=Answer_Freq, size=Reputation))+
  scale_x_continuous(limits = c(0,400))+
  scale_y_continuous(limits = c(0,3300))+
  ggtitle("Correlation")+
  geom_point(alpha=0.5, color = c("pink"))+
  theme_ipsum()
  

The_most_active_users_P_A <- function(Posts_Physics, Users_Physics){
  sqldf("SELECT tab2.DisplayName, tab1.OwnerUserId, tab1.Answer_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Answer_Freq
        FROM Posts_Physics
        WHERE PostTypeId == 2
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Physics) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Answer_Freq DESC
        LIMIT 100")
}

The_most_active_users_P_Q <- function(Posts_Physics, Users_Physics){
  sqldf("SELECT tab2.DisplayName, tab1.OwnerUserId, tab1.Question_Freq
        FROM
        (SELECT OwnerUserId, COUNT(PostTypeId) as Question_Freq
        FROM Posts_Physics
        WHERE PostTypeId == 1
        GROUP BY OwnerUserId) AS tab1
        LEFT JOIN
        (SELECT DisplayName, Id
        FROM Users_Physics) AS tab2
        ON tab1.OwnerUserId = tab2.Id
        WHERE DisplayName !='NA'
        ORDER BY Question_Freq DESC
        LIMIT 100")
}

The_most_active_users_P_Q <- The_most_active_users_P_Q(Posts_Physics, Users_Physics)
The_most_active_users_P_A <-  The_most_active_users_P_A(Posts_Physics, Users_Physics)

ggplot(The_most_active_users_P_Q[1:10,], aes(x=DisplayName, y=Question_Freq, fill =Question_Freq))+
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = 1) +
  ggtitle("The most questions physics")+
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(The_most_active_users_P_A[1:10,], aes(x=DisplayName, y=Answer_Freq, fill = Answer_Freq))+
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = 1) +
  ggtitle("The most answers physics")+
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

correlation_p <- function(
  The_most_active_users_P_A, The_most_active_users_P_Q, The_best_users_physics){
  Active_users_p <- merge(The_most_active_users_P_A, The_most_active_users_P_Q, by="DisplayName", all=TRUE)
  Active_users_p_ <- merge(Active_users_p, The_best_users_physics, by="DisplayName", all=TRUE)
  Active_users_p_$Answer_Freq[is.na(Active_users_p_$Answer_Freq)] <- 0
  Active_users_p_$Question_Freq[is.na(Active_users_p_$Question_Freq)] <- 0
  Active_users_p_$Reputation[is.na(Active_users_p_$Reputation)] <- 0
  Active_users_p_ <-Active_users_p_[order(Active_users_p_$Reputation, decreasing = T)[1:50], ]
  Active_users_p_
}
cos <-correlation_p(The_most_active_users_P_A, The_most_active_users_P_Q, The_best_users_physics)
ggplot(correlation_p(The_most_active_users_P_A, The_most_active_users_P_Q, The_best_users_physics),
       aes(x=Question_Freq, y=Answer_Freq, size=Reputation))+
  scale_x_continuous(limits = c(0,500))+
  scale_y_continuous(limits = c(0,6000))+
  ggtitle("Correlation")+
  geom_point(alpha=0.3, color = "pink")+
  theme_ipsum()
 

### Longest Title

Longest_Title_C <- function(Posts_Cooking){
  sqldf("SELECT Title, OwnerUserId, Lenght_Of_Title, DisplayName
FROM
(SELECT Title, OwnerUserId, LENGTH(Title) as Lenght_Of_Title
      FROM Posts_Cooking)
       as long
      LEFT JOIN
      (SELECT DisplayName, Id
        FROM Users_Cooking) AS tab2
        ON long.OwnerUserId = tab2.Id
        Order By Lenght_Of_Title DESC
      LIMIT 3
      ")}
Longest_Title_C <- Longest_Title_C(Posts_Cooking)
datatable(Longest_Title_C, rownames = FALSE) %>%
  formatStyle(columns = "Title",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Lenght_Of_Title",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")
Longest_Title_G <- function(Posts_Gardening){
  sqldf("SELECT Title, OwnerUserId, Lenght_Of_Title, DisplayName
          FROM
          (SELECT Title, OwnerUserId, LENGTH(Title) as Lenght_Of_Title
          FROM Posts_Gardening)
          as long
      LEFT JOIN
      (SELECT DisplayName, Id
        FROM Users_Gardening) AS tab2
        ON long.OwnerUserId = tab2.Id
        Order By Lenght_Of_Title DESC
      LIMIT 3
      ")
}

Longest_Title_G <- Longest_Title_G(Posts_Gardening)
datatable(Longest_Title_G, rownames = FALSE) %>%
  formatStyle(columns = "Title",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Lenght_Of_Title",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")

Longest_Title_P <- function(Posts_Physics){
  sqldf("SELECT Title, OwnerUserId, Lenght_Of_Title, DisplayName
          FROM
          (SELECT Title, OwnerUserId, LENGTH(Title) as Lenght_Of_Title
          FROM Posts_Physics)
          as long
      LEFT JOIN
      (SELECT DisplayName, Id
        FROM Users_Physics) AS tab2
        ON long.OwnerUserId = tab2.Id
        Order By Lenght_Of_Title DESC
      LIMIT 3
      ")
}


Longest_Title_P <- Longest_Title_P(Posts_Physics)
datatable(Longest_Title_P, rownames = FALSE) %>%
  formatStyle(columns = "Title",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Lenght_Of_Title",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")

#mapa

Is_someone_from_Poland_G <- function(Users_Gardening){
  sqldf(
    "SELECT DisplayName, Location
    FROM Users_Gardening
    WHERE Location LIKE '%Poland%'"
  )
}
Is_someone_from_Poland_G <- Is_someone_from_Poland_G(Users_Gardening)

Is_someone_from_Poland_C <- function(Users_Cooking){
  sqldf(
    "SELECT DisplayName, Location
    FROM Users_Cooking
    WHERE Location LIKE '%Poland%'"
  )
}
Is_someone_from_Poland_C <- Is_someone_from_Poland_C(Users_Cooking)

Is_someone_from_Poland_P <- function(Users_Physics){
  sqldf(
    "SELECT DisplayName, Location
    FROM Users_Physics
    WHERE Location LIKE '%Poland%'"
  )
}
Is_someone_from_Poland_P <- Is_someone_from_Poland_P(Users_Physics)



Is_someone_from_USA <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%USA%'OR '%United States%'
        ")
}

Is_someone_from_USA_G <- Is_someone_from_USA(Users_Gardening)

Is_someone_from_USA <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%USA%'OR '%United States%'
        ")
}
Is_someone_from_USA_C <- Is_someone_from_USA(Users_Cooking)

Is_someone_from_USA <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%USA%'OR '%United States%'
        ")
}
Is_someone_from_USA_P <- Is_someone_from_USA(Users_Physics)




Is_someone_from_UK <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%UK'OR '%United Kingdom%' OR '%England%' OR '%Wales%'
        OR '%Scotland%' OR '%Northern Ireland%'
        ")
}

Is_someone_from_UK_G <- Is_someone_from_UK(Users_Gardening)

Is_someone_from_UK <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%UK'OR '%United Kingdom%' OR '%England%' OR '%Wales%'
        OR '%Scotland%' OR '%Northern Ireland%'
        ")
}
Is_someone_from_UK_C <- Is_someone_from_UK(Users_Cooking)

Is_someone_from_UK <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%UK'OR '%United Kingdom%' OR '%England%' OR '%Wales%'
        OR '%Scotland%' OR '%Northern Ireland%'
        ")
}
Is_someone_from_UK_P <- Is_someone_from_UK(Users_Physics)


###

Is_someone_from_Australia <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%Australia%'
        ")
}

Is_someone_from_Australia_G <- Is_someone_from_Australia(Users_Gardening)

Is_someone_from_Australia <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%Australia%'
        ")
}
Is_someone_from_Australia_C <- Is_someone_from_Australia(Users_Cooking)

Is_someone_from_Australia <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%Australia%'
        ")
}
Is_someone_from_Australia_P <- Is_someone_from_Australia(Users_Physics)


###
Is_someone_from_India <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%India%'
        ")
}

Is_someone_from_India_G <- Is_someone_from_India(Users_Gardening)

Is_someone_from_India <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%India%'
        ")
}
Is_someone_from_India_C <- Is_someone_from_India(Users_Cooking)

Is_someone_from_India <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%India%'
        ")
}
Is_someone_from_India_P <- Is_someone_from_India(Users_Physics)


###

Is_someone_from_Germany <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%Germany%'
        ")
}

Is_someone_from_Germany_G <- Is_someone_from_Germany(Users_Gardening)

Is_someone_from_Germany <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%Germany%'
        ")
}
Is_someone_from_Germany_C <- Is_someone_from_Germany(Users_Cooking)

Is_someone_from_Germany <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%Germany%'
        ")
}
Is_someone_from_Germany_P <- Is_someone_from_Germany(Users_Physics)

###

Is_someone_from_Canada <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%Canada%'
        ")
}

Is_someone_from_Canada_G <- Is_someone_from_Canada(Users_Gardening)

Is_someone_from_Canada <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%Canada%'
        ")
}
Is_someone_from_Canada_C <- Is_someone_from_Canada(Users_Cooking)

Is_someone_from_Canada <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%Canada%'
        ")
}
Is_someone_from_Canada_P <- Is_someone_from_Canada(Users_Physics)


###


Is_someone_from_France <- function(Users_Gardening){
  sqldf("SELECT DisplayName, Location
        FROM Users_Gardening
        WHERE Location LIKE '%France%'
        ")
}

Is_someone_from_France_G <- Is_someone_from_France(Users_Gardening)

Is_someone_from_France <- function(Users_Cooking){
  sqldf("SELECT DisplayName, Location
        FROM Users_Cooking
        WHERE Location LIKE '%France%'
        ")
}
Is_someone_from_France_C <- Is_someone_from_France(Users_Cooking)

Is_someone_from_France <- function(Users_Physics){
  sqldf("SELECT DisplayName, Location
        FROM Users_Physics
        WHERE Location LIKE '%France%'
        ")
}
Is_someone_from_France_P <- Is_someone_from_France(Users_Physics)
Users_from_P <- function(Users_Physics){
  sqldf("SELECT DISTINCT DisplayName AS Users_from_Physics
        FROM
        Users_Physics
        ")
}
Users_from_P_count <- function(Users_from_P){
  sqldf("
        SELECT COUNT(Users_from_Physics) AS Number_of_users_in_Physics
        FROM Users_from_P")
}

Users_from_P <- Users_from_P(Users_Physics)
Users_from_P_count<-Users_from_P_count(Users_from_P)

Users_from_C <- function(Users_Cooking){
  sqldf("SELECT DISTINCT DisplayName AS Users_from_Cooking
        FROM
        Users_Cooking
        ")
}
Users_from_C <- Users_from_C(Users_Cooking)
Users_from_C_count <- function(Users_from_C){
  sqldf("
        SELECT COUNT(Users_from_Cooking) AS Number_of_users_in_Cooking
        FROM Users_from_C")
}
Users_from_C_count<-Users_from_C_count(Users_from_C)


U <- function(Users_from_P, Users_from_C){
  sqldf(
    "SELECT Users_from_Physics FROM Users_from_P
    INNER JOIN Users_from_C On
    Users_from_C.Users_from_Cooking =Users_from_P.Users_from_Physics"
  )
}
u<-U(Users_from_P, Users_from_C)
Users_from_G <- function(Users_Gardening){
  sqldf("SELECT DISTINCT DisplayName AS Users_from_Gardening
        FROM
        Users_Gardening
        ")
}

Users_from_G <- Users_from_G(Users_Gardening)
Users_from_G_count <- function(Users_from_G){
  sqldf("
        SELECT COUNT(Users_from_Gardening) AS Number_of_users_in_Gardening
        FROM Users_from_G")}
Users_from_G_count<-Users_from_G_count(Users_from_G)


Users_from_G_P <- function(u, Users_from_G){
  sqldf(
    "SELECT Users_from_Physics AS Users_who_repeated_in_tables FROM u
    INNER JOIN Users_from_G On 
    Users_from_G.Users_from_Gardening =u.Users_from_Physics"
  )
}
Users_from_G_P <- Users_from_G_P(u, Users_from_G)
Users_count <- function(Users_from_G_P){
  sqldf("
        SELECT COUNT(Users_who_repeated_in_tables) AS Number_of_users
        FROM  Users_from_G_P")}

Users_counted<-Users_count(Users_from_G_P)

data <- data.frame(
  forum = c("Gardening", "Cooking", "Physics", "Same_users_in_every_forums"),
  number_of_users = c(Users_from_G_count$Number_of_users_in_Gardening,
                      Users_from_C_count$Number_of_users_in_Cooking,
                      Users_from_P_count$Number_of_users_in_Physics,
                      Users_counted$Number_of_users)
)
ggplot(data, aes(x=forum, y=number_of_users, fill =forum)) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  geom_label(aes(label = number_of_users), color = "black")
geom_text(aes(label = number_of_users),
          position = position_dodge(width = 0.9),
          vjust = -0.3)
#rysowanie mapy 

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=18.616664, lat=52.983002, 
popup="28 Users from Gardening,
                135 from Cooking,
                441 from Physics")
m <- addMarkers(m, lng=9.616664, lat=53.083002, 
popup="199 Users from Gardening,
                689 from Cooking,
                2054 from Physics")
m <- addMarkers(m, lng=-103.616664, lat=44.583002, 
popup="718 Users from Gardening,
                1662 from Cooking,
                3997 from Physics")
m <- addMarkers(m, lng=-1.616664, lat=53.583002, 
popup="185 Users from Gardening,
                414 from Cooking,
                1248 from Physics")
m <- addMarkers(m, lng=130.616664, lat=-25.583002, 
popup="232 Users from Gardening,
                621 from Cooking,
                1215 from Physics")
m <- addMarkers(m, lng=81.616664, lat=24.583002,
popup="396 Users from Gardening,
                1238 from Cooking,
                6349 from Physics")
m<- addMarkers(m, lng=-105.616664, lat=57.583002,
popup="308 Users from Gardening,
                1025 from Cooking,
                1888 from Physics")
m<- addMarkers(m, lng=2.216664, lat=45.583002, popup="83 Users from Gardening,
                302 from Cooking,
                1055 from Physics")
m


### Ile z jakiego miejsca użytkowników

Country <-function(Users_Cooking){
  sqldf("
        SELECT Location, COUNT(Id) AS People_from_this_location
        FROM Users_Cooking
        WHERE Location IS NOT NULL
        GROUP BY Location
        ORDER BY People_from_this_location DESC")
}
Country_C <- Country(Users_Cooking)

Country <-function(Users_Gardening){
  sqldf("
        SELECT Location, COUNT(Id) AS People_from_this_location
        FROM Users_Gardening
        WHERE Location IS NOT NULL
        GROUP BY Location
        ORDER BY People_from_this_location DESC")
}
Country_G <- Country(Users_Gardening)

Country <-function(Users_Physics){
  sqldf("
        SELECT Location, COUNT(Id) AS People_from_this_location
        FROM Users_Physics
        WHERE Location IS NOT NULL
        GROUP BY Location
        ORDER BY People_from_this_location DESC")
}
Country_P <- Country(Users_Physics)

ggplot(Country_C[1:10,], aes(x=Location, y=People_from_this_location, 
                             fill =People_from_this_location))+
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_distiller(palette = 7) +
  ggtitle("The most popular location in cooking")+
  theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

ggplot(Country_G[1:10,], aes(x=Location, y=People_from_this_location, 
                             fill =People_from_this_location))+
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_distiller(palette = 2) +
  ggtitle("The most popular location in gardening")+
  theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

ggplot(Country_P[1:10,], aes(x=Location, y=People_from_this_location, 
                             fill =People_from_this_location))+
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_distiller(palette = 1) +
  ggtitle("The most popular location in physics")+
  theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

The_most_popular_Tags_Cooking <- function(Tags_Cooking){
  sqldf("
SELECT TagName, Count
FROM Tags_Cooking
ORDER BY Count DESC
"
  )}
The_most_popular_Tags_Cooking <- The_most_popular_Tags_Cooking(Tags_Cooking)
wordcloud2(The_most_popular_Tags_Cooking, 
           color = "random-dark", size = 0.5, shape = "star")


The_most_popular_Tags_Physics <- function(Tags_Physics){
  sqldf("
SELECT TagName, Count
FROM Tags_Physics
ORDER BY Count DESC
"
  )}
The_most_popular_Tags_Physics <- The_most_popular_Tags_Physics(Tags_Physics)

wordcloud2(The_most_popular_Tags_Physics, color = "random-dark",
           size = 0.41, shape= "star")


The_most_popular_Tags_Gardening <- function(Tags_Gardening){
  sqldf("
SELECT TagName, Count
FROM Tags_Gardening
ORDER BY Count DESC
"
  )}
The_most_popular_Tags_Gardening <- The_most_popular_Tags_Gardening(
  Tags_Gardening)
wordcloud2(The_most_popular_Tags_Gardening,
           color = "random-dark", size = 0.7, shape="star")



### Tags 


Tags_both_in_Physics_and_cooking <- function(Tags_Cooking, Tags_Physics){
  sqldf("SELECT TagName, Count_p, Count_c,
Count_p+Count_c AS Count_sum
FROM(
  SELECT  TagName,
  Count AS Count_p
  FROM Tags_Physics
) AS Tab1
JOIN(
  SELECT  TagName AS TagName2,
  Count AS Count_c
  FROM Tags_Cooking
) AS Tab2
ON Tab1.TagName=Tab2.TagName2
ORDER BY Count_sum DESC
")
}
Tags_both_in_Physics_and_cooking<-Tags_both_in_Physics_and_cooking(Tags_Cooking,
                                                                  Tags_Physics)

wordcloud2(Tags_both_in_Physics_and_cooking, color = "random-dark", size = 0.7)

Tags_both_in_Physics_and_Gardening <- function(Tags_Gardening, Tags_Physics){
  sqldf("SELECT TagName, Count_p, Count_g,
Count_p+Count_g AS Count_sum
FROM(
  SELECT  TagName,
  Count AS Count_p
  FROM Tags_Physics
) AS Tab1
JOIN(
  SELECT  TagName AS TagName2,
  Count AS Count_g
  FROM Tags_Gardening
) AS Tab2
ON Tab1.TagName=Tab2.TagName2
ORDER BY Count_sum DESC
")
}
Tags_both_in_Physics_and_Gardening <- Tags_both_in_Physics_and_Gardening(
  Tags_Gardening, Tags_Physics)

wordcloud2(Tags_both_in_Physics_and_Gardening, color = "random-dark", size = 0.6)


Tags_both_in_Cooking_and_Gardening <- function(Tags_Gardening, Tags_Cooking){
  sqldf("SELECT TagName, Count_c, Count_g,
Count_c+Count_g AS Count_sum
FROM(
  SELECT  TagName,
  Count AS Count_c
  FROM Tags_Cooking
) AS Tab1
JOIN(
  SELECT  TagName AS TagName2,
  Count AS Count_g
  FROM Tags_Gardening
) AS Tab2
ON Tab1.TagName=Tab2.TagName2
ORDER BY Count_sum DESC
")
}
Tags_both_in_Cooking_and_Gardening <- Tags_both_in_Cooking_and_Gardening(
  Tags_Gardening, Tags_Cooking)

wordcloud2(Tags_both_in_Cooking_and_Gardening, color = "random-dark", 
           size = 0.7)

### The best comments

The_best_comment_Physics <- function(Comments_Physics, Users_Physics){
  sqldf("
SELECT Comments_Physics.Score,
Comments_Physics.Text,
Users_Physics.DisplayName
FROM Users_Physics
JOIN Comments_Physics ON Comments_Physics.UserId=Users_Physics.Id
ORDER BY Score DESC
LIMIT 10

")
}


The_best_comment_Physics <- The_best_comment_Physics(Comments_Physics, Users_Physics)
datatable(The_best_comment_Physics, rownames = FALSE) %>%
  formatStyle(columns = "Score",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Text",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")


The_best_comment_gardening <- function(Comments_Gardening, Users_Gardening){
  sqldf("
SELECT Comments_Gardening.Score,
Comments_Gardening.Text,
Users_Gardening.DisplayName
FROM Users_Gardening
JOIN Comments_Gardening ON Comments_Gardening.UserId=Users_Gardening.Id
ORDER BY Score DESC
LIMIT 10

")
}

The_best_comment_gardening <- The_best_comment_gardening(Comments_Gardening,
                                                         Users_Gardening)
datatable(The_best_comment_gardening, rownames = FALSE) %>%
  formatStyle(columns = "Score",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Text",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")



The_best_comment_cooking <- function(Comments_Cooking, Users_Cooking){
  sqldf("
SELECT Comments_Cooking.Score,
Comments_Cooking.Text,
Users_Cooking.DisplayName
FROM Users_Cooking
JOIN Comments_Cooking ON Comments_Cooking.UserId=Users_Cooking.Id
ORDER BY Score DESC
LIMIT 10

")
}

The_best_comment_cooking <- The_best_comment_cooking(
  Comments_Cooking, Users_Cooking)
datatable(The_best_comment_cooking, rownames = FALSE) %>%
  formatStyle(columns = "Score",
              backgroundColor = "yellow") %>%
  formatStyle(columns = "Text",
              backgroundColor = "lightblue") %>%
  formatStyle(columns = "DisplayName",
              backgroundColor = "lightgreen")
