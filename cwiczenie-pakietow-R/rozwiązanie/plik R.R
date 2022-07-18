library(sqldf)
library(dplyr)
library(data.table)
library(microbenchmark)
library(rmarkdown)


##Zadanie 1
install.packages("microbenchmark")
#SQL
df_sqldf_1 <- function(Tags){
                sqldf("
                SELECT TagName, Count
                FROM Tags
                ORDER BY Count DESC
                LIMIT 10"
                )}

#base
df_base_1 <- function(Tags){
  df1 <-Tags[c('TagName', 'Count')]                 
  s <-df1[order(df1$Count, decreasing = T)[1:10],]  
  row.names(s) <- NULL                              
  s
}

#dplyr
df_dplyr_1 <- function(Tags){     
   Tags[c('TagName', 'Count')]%>%     
    arrange(desc(Tags['Count']))%>%   
    slice_head(n=10)                 
}

#data.table
df_data_table_1 <- function(Tags){
  TagsDT <- data.table(Tags)              
  TagsKol <- TagsDT[,.(TagName, Count)]   
  TagsKol[order(-Count)][1:10, ]          
}


## Zadanie 2

#SQL
df_sqldf_2 <- function(Posts, Users){
  sqldf("
  SELECT Users.DisplayName, Users.Age, Users.Location,
  AVG(Posts.Score) as PostsMeanScore,
  MAX(Posts.CreationDate) AS LastPostCreationDate
  FROM Posts
  JOIN Users ON Users.AccountId=Posts.OwnerUserId
  WHERE OwnerUserId != -1
  GROUP BY OwnerUserId
  ORDER BY PostsMeanScore DESC
  LIMIT 10
")}

#base
df_base_2 <- function(Posts, Users) {
  posts_col <- merge(aggregate(x = Posts["Score"],by = Posts["OwnerUserId"], FUN = mean),         #zliczam średni Score każdego użytkownika
                     aggregate(x = Posts["CreationDate"],by = Posts["OwnerUserId"], FUN = max),   #Wyznaczam datę najnowszego postu dla danego użytkownika
                     by = "OwnerUserId")                                                          #łącze powyższe wartości za pomocą merge z wykorzysstaniem Id
  colnames(posts_col) <- c("OwnerUserId", "PostsMeanScore", "LastPostCreationDate" )              
  users_col <- Users[c("AccountId", "DisplayName", "Age", "Location")]                           
  posts_col <- posts_col[posts_col["OwnerUserId"] != -1, ]                                        
  s <- merge(users_col, posts_col, by.x = "AccountId", by.y ="OwnerUserId")                       
  s <- s[order(s["PostsMeanScore"], decreasing = TRUE)[1:10],2:6]                                 #2:6 wybiera wszystkie kolumny oprócz AccountId 
  rownames(s) <- NULL
  s
}

#dplyr

df_dplyr_2 <- function(Posts, Users) {
  Posts %>%
    inner_join(Users, by=c("OwnerUserId" = "AccountId")) %>%                             
    filter(OwnerUserId != -1) %>%                                                        
    group_by(DisplayName, Age, Location, OwnerUserId)%>%                                 
    summarize(
      PostsMeanScore = mean(Score),
      LastPostCreationDate = max(CreationDate.x)                                         #zliczam średni score i max Creation Date każdego użytkownika
    )%>%
    data.frame() %>%                                                                     
    arrange(-PostsMeanScore) %>%
    distinct()%>%
    slice_head(n=10)%>%                                                                  
    select(DisplayName, Age, Location, PostsMeanScore, LastPostCreationDate)             
}

#data.table
df_data_table_2 <- function(Posts, Users) {
  PostsDT <- data.table(Posts)                                                                                                      #tworzę data table Posts
  UsersDT <- data.table(Users)                                                                                                      #tworzę data table Users
  s <- PostsDT[UsersDT, nomatch=0, on=c("OwnerUserId"= "AccountId")]                                                                #łącze tabele Users i Posts   
  s <-s[OwnerUserId != -1]                                                                                                          #usuwam użytkowników z Id = -1
  s <- s[ ,.(DisplayName, Age, Location, PostsMeanScore = mean(Score), LastPostCreationDate = max(CreationDate)), "OwnerUserId"]    #tworzę tabele i wyliczam średni score i najnowszą PostCreatonDate
  s <- unique(s)                                                                                                                    #pozbywam się powtarzających użytkowników
  s <- s[ ,.(DisplayName, Age, Location, PostsMeanScore, LastPostCreationDate)]                                                     #tworzę tabele z danymi wymaganymi  poleceniu
  s <- s[order(-PostsMeanScore)]                                                                                                    #sortuje malejąco po PostsMeanScore
  s[1:10, ]                                                                                                                         #wybieram pierwsze 10 wierszy
}

    
## Zadanie 3

#SQL
df_sqldf_3 <- function(Users, Posts) {
sqldf("
SELECT DisplayName, QuestionsNumber, AnswersNumber
FROM
(
  SELECT COUNT(*) as AnswersNumber, Users.DisplayName, Users.Id
  FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
  WHERE Posts.PostTypeId = 1
  GROUP BY Users.Id
) AS Tab1
JOIN
(
  SELECT COUNT(*) as QuestionsNumber, Users.Id
  FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
  WHERE Posts.PostTypeId = 2
  GROUP BY Users.Id
) AS Tab2
ON Tab1.Id = Tab2.Id
WHERE QuestionsNumber < AnswersNumber
ORDER BY AnswersNumber DESC")}

#base
df_base_3_tab1 <- function(Users, Posts){                          # funkcja tworząca TAB1 
  df <- merge(Users, Posts, by.x = "Id", by.y = "OwnerUserId")     #łączę 2 tabelki: Posts i Users
  df_f <- df[df$PostTypeId==1, ]                                   #filtruję, zostawiając tylko te wiersze, które spełniają warunek w []
  df_c <- as.data.frame(table(df_f$Id))                            #Grupuję po Id i zliczam wiersze w grupach
  colnames(df_c) <- c("Id", "AnswersNumber")                       #zmieniam nazwy kolumn na porządane
  df_names <- unique.data.frame(df[,c("Id", "DisplayName")])       #tworzę tabelę z Id i nazwami użytkowników
  df_t <- merge(df_c, df_names, by.x="Id", by.y="Id")              #dodaję nazwy użytkowników do wyjściowej tabeli
  df_t[, c("AnswersNumber", "DisplayName", "Id")]                  #wybieram tylko potrzebne kolumny w odpowiedniej kolejności
}
  
df_base_3_tab2 <- function(Users, Posts){                          #funkcja tworząca TAB2
  df <- merge(Users, Posts, by.x = "Id", by.y = "OwnerUserId")     #łączę 2 tabelki: Posts i Users
  df_f <- df[df$PostTypeId==2, ]                                   #filtruję, zostawiając tylko te wiersze, które spełniają warunek w []
  df_c <- as.data.frame(table(df_f$Id))                            #Grupuję po Id i zliczam wiersze w grupach
  colnames(df_c) <- c("Id", "QuestionsNumber")                     #zmieniam nazwy kolumn na te z zadania
  df_c[, c("QuestionsNumber", "Id")]                               #wybieram potrzebne kolumny w odpowiedniej kolejności
}
  
  
df_base_3 <- function(Users, Posts){
  tab1 <- df_base_3_tab1(Users, Posts)
  tab2 <- df_base_3_tab2(Users, Posts)
  tab_c <- merge(tab1, tab2, by.x="Id", by.y="Id")                          #łączę utworzone wyżej tabelki
  tab_f <- tab_c[tab_c$QuestionsNumber < tab_c$AnswersNumber, ]             #wybieram odpowiednie wiersze spełniające warunek w []
  sort <- order(tab_f$AnswersNumber, decreasing = TRUE)                     #sortuje malejąco po AnswersNumber
  tab_f[sort, c("DisplayName", "QuestionsNumber", "AnswersNumber")]         #sortuję malejąco po AnswerNumber i wybieram potrzebne kolumny w odpowiedniej kolejności
}
  
#dplyr
df_dplyr_3.1 <- function(Users, Posts){
  Users %>%
    select(DisplayName, Id) %>%
    distinct() -> df_names
  Users %>% 
    inner_join(Posts, by=c("Id" = "OwnerUserId")) %>%     #łączę tabele Posts i Users
    filter(PostTypeId == 1) %>%                           # wybieram posty z Id = 1
    count(Id) %>% rename(AnswersNumber=n) %>%             #zmieniam nr odp na od 1 do n
    inner_join(df_names, by=c("Id" = "Id"))
}
df_dplyr_3.2 <- function(Users, Posts){
  Users%>%
    inner_join(Posts, by=c("Id" = "OwnerUserId"))%>%     #łącze posts i users po Id
    filter(PostTypeId==2) %>%                            #wybieram posty z Id = 2
    count(Id) %>% rename(QuestionsNumber=n)              #zmieniam nr pytań na od 1 do n
}
df_dplyr_3 <- function(Users, Posts){
  df_dplyr_3.1(Users,Posts) %>%
    inner_join(df_dplyr_3.2(Users,Posts), by=c("Id" = "Id"))%>%       #łącze 3.1 i 3.2 po Id
    filter(QuestionsNumber < AnswersNumber) %>%                       #wybieram wiersze spełniające warunek z polecenia WHERE
    arrange(-AnswersNumber)%>%                                        #sortuje malejąco po AnswersNumbers
    select(DisplayName, QuestionsNumber, AnswersNumber)               #wybieram niezbędne kolumny
}

#Data.table
df_data_table_3.1 <- function(Users, Posts) {
  PostsDT <- data.table(Posts)                                          #tworze data table dla posts
  UsersDT <- data.table(Users)                                          #tworzę data table dla Users
  UsersDT.j <- UsersDT[PostsDT, nomatch=0, on=c("Id"= "OwnerUserId")]   #łącze je ze sobą za pomocą Id
  UsersDT.fi <-UsersDT.j[PostTypeId == 1]                               #wybieram posty z Id = 1
  UsersDT.gr <- UsersDT.fi[ ,.(DisplayName, AnswersNumber=.N), "Id"]    #tworzę tabele z niezbędnymi kolumnami
  unique(UsersDT.gr)                                                    #usuwam powtórzenia
}
df_data_table_3.2 <- function(Users, Posts) {
  PostsDT <- data.table(Posts)                                          #tworze data table dla posts
  UsersDT <- data.table(Users)                                          #tworzę data table dla Users
  UsersDT.j <- UsersDT[PostsDT, nomatch=0, on=c("Id"= "OwnerUserId")]   #łącze je ze sobą za pomocą Id
  UsersDT.fi <-UsersDT.j[PostTypeId == 2]                               #wybieram posty z Id = 2
  UsersDT.gr <- UsersDT.fi[ ,.(DisplayName, QuestionsNumber=.N), "Id"]  #tworzę tabele z niezbędnymi kolumnami
  unique(UsersDT.gr)                                                    #usuwam powtórzenia
}
df_data_table_3 <- function(Users, Posts){
  TB1 <- df_data_table_3.1(Users, Posts)                                 
  TB2 <- df_data_table_3.2(Users, Posts)
  TB <- TB1[TB2, nomatch=0, on=c("Id"= "Id")]                                              #łącze ze sobą 2 załączone wyżej tabele
  TK <- TB[QuestionsNumber < AnswersNumber,.(DisplayName, QuestionsNumber, AnswersNumber)] #dodaje warunek z polecenia WHERE, wybieram niezbędne kolumny do tabeli
  TK[order(-AnswersNumber)]                                                                #sortuje malejąco po answers number
}


## Zadanie 4  

#SQL
df_sqldf_4 <-function(Posts, Comments){
  sqldf("SELECT
  Posts.Title, Posts.CommentCount,
  CmtTotScr.CommentsTotalScore,
  Posts.ViewCount
  FROM (
  SELECT
  PostID,
  UserID,
  SUM(Score) AS CommentsTotalScore
  FROM Comments
  GROUP BY PostID, UserID
  ) AS CmtTotScr
  JOIN Posts ON Posts.ID=CmtTotScr.PostID
  WHERE Posts.PostTypeId=1
  ORDER BY CmtTotScr.CommentsTotalScore DESC
  LIMIT 10")
}

#base
df_base_4 <- function(Posts, Comments){      
  CmtTotScr <- aggregate(Comments["Score"], 
                         by = Comments[c("PostId", "UserId")], FUN = sum) #sumuje Score każdego komentarza danego użytkownika
  colnames(CmtTotScr)[3] <- "CommentsTotalScore"
  CTS_P <- merge(Posts, CmtTotScr, by.x = "Id", by.y = "PostId")
  CTS_P <- CTS_P[CTS_P$PostTypeId == 1, ]
  TB <- data.frame(Title = CTS_P$Title, 
                    CommentCount = CTS_P$CommentCount, 
                    CommentsTotalScore = CTS_P$CommentsTotalScore,
                    ViewCount = CTS_P$ViewCount)
  ans <- TB[order(TB$CommentsTotalScore, decreasing = TRUE), ] 
  unique(ans[1:10, ])
}
  
#dplyr
df_dplyr_4_cmt_tot_scr <- function(Posts,Comments){
  Comments %>% 
    group_by(PostId, UserId) %>%                                       
    summarize(CommentsTotalScore=sum(Score), .groups = 'drop')         #drop pozwala sumować całą "grupę" tych samych Id posta
}
df_dplyr_4 <- function(Posts,Comments){
  cmt_tot_scr <- df_dplyr_4_cmt_tot_scr(Posts, Comments)
  cmt_tot_scr %>% left_join(Posts, by=c("PostId"="Id")) %>%            
    filter(PostTypeId == 1) %>%                                        
    arrange(-CommentsTotalScore) %>%                                     
    select(Title, CommentCount, CommentsTotalScore, ViewCount) %>%     
    slice_head(n=10)                                                   
}

# data.table
df_data_table_4 <- function(Posts,Comments) {
  CommentsDT <- data.table(Comments)
  PostsDT <- data.table(Posts)
  cmt_tot_scr <- CommentsDT[ , .(CommentsTotalScore = sum(Score)), .(PostId, UserId)]        #sumuje Score komentarzy każdego posta
  TB <- cmt_tot_scr[PostsDT, nomatch=0, on=c("PostId" = "Id")]                                 
  TB <- TB[PostTypeId==1,]                                                                    
  TB <- TB[order(-CommentsTotalScore),.(Title, CommentCount, CommentsTotalScore, ViewCount),]  
  TB[1:10, ]                                                                                   
}


## Zadanie 5

#SQL
df_sqldf_5 <-function(Posts){
  sqldf("SELECT
  Questions.Id,
  Questions.Title,
  BestAnswers.MaxScore,
  Posts.Score AS AcceptedScore,
  BestAnswers.MaxScore-Posts.Score AS Difference
  FROM (
  SELECT Id, ParentId, MAX(Score) AS MaxScore
  FROM Posts
  WHERE PostTypeId==2
  GROUP BY ParentId
  ) AS BestAnswers
  JOIN (
  SELECT * FROM Posts
  WHERE PostTypeId==1
  ) AS Questions
  ON Questions.Id=BestAnswers.ParentId
  JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
  ORDER BY Difference DESC
  LIMIT 10
 ")
}

#base
df_base_5 <- function(Posts){
  srt <- Posts[Posts$PostTypeId==2, ]
  BestAnswers <-aggregate(x=srt["Score"], by =srt["ParentId"], FUN = max)   #wyliczam max 
  jojn <- merge(BestAnswers, srt, by.x = "ParentId", by.y = "ParentId")     #i włączam go jako kolumne do tabeli srt
  BestAnswers <- data.frame(Id = jojn$Id, 
                   ParentId =jojn$ParentId,
                   MaxScore = jojn$Score.x)
  Questions <-Posts[Posts$PostTypeId==1, ]
  jojn1 <- merge(Questions, BestAnswers, by.x="Id", by.y="ParentId")
  jojn2 <- merge(jojn1, Posts, by.x="AcceptedAnswerId", by.y="Id")
  rama <- data.frame(Id = jojn2$Id,
                     Title = jojn2$Title.x,     
                     MaxScore = jojn2$MaxScore,
                     AcceptedScore = jojn2$Score.y,                 # .x i .y biorą się stąd że przy połączeniu posts i join1
                     Difference = (jojn2$MaxScore - jojn2$Score.y)) # niektóre kolumny się zdublowały i R automatycznie zmienił im nazwy
  rama <- unique(rama)
  fil <- rama[order(-rama$Difference), ]
  fil[1:10, ]
  }

#dplyr
df_dplyr_5 <- function(Posts){
  Posts_dplyr <- tibble::as_tibble(Posts)
  fil <- filter(Posts_dplyr, PostTypeId==2)
  po <- count(fil, ParentId, wt=max(Score), name = "MaxScore")
  ramka <- select(fil, ParentId, Id)
  BestAnswers <- inner_join(po, ramka, by="ParentId")
  Questions <- filter(Posts_dplyr, PostTypeId==1)
  jojn <- inner_join(Questions, BestAnswers, by=c("Id"="ParentId"))
  jojn2 <- inner_join(jojn, Posts_dplyr, by=c("AcceptedAnswerId"="Id"), suffix=c(".j", ".p")) #suffix pozwala zmienić nazwy zdublowanych kolumn 
  wyb <- select(jojn2, Id, Title=Title.j, MaxScore, AcceptedScore=Score.p)
  wyb<- distinct(wyb)
  dif <- count(wyb, Id, wt=sum(MaxScore, -AcceptedScore), name = "Difference") #wt pozwala mi zsumować MaxScore i -(Accepted Score) aby uzyskać różnice
  
  fin <- inner_join(wyb, dif, by=c("Id"="Id"))
  seg <- arrange(fin, desc(Difference))
  final <- slice_head(seg, n=10)
  final
}

#data.table
df_data_table_5 <- function(Posts){
  PostsDT <- data.table(Posts)
  BestAnswers <- PostsDT[PostTypeId==2 , .(ParentId, MaxScore = max(Score),Id), "ParentId"]
  BestAnswers <- BestAnswers[,.(Id, ParentId, MaxScore)]
  Questions <- PostsDT[PostTypeId==1,]
  TB_J <- Questions[BestAnswers, nomatch=0, on=c("Id" = "ParentId")]
  JOINED_2 <- TB_J[Posts, nomatch=0, on=c("AcceptedAnswerId"="Id")]
  TB <- JOINED_2[,.(Id, Title, MaxScore, AcceptedScore=i.Score, Difference=(MaxScore-i.Score))] #i.score wziął się stąd że przy połączeniu TB_J i Posts powstały zdublowane kolumny
  TB <- unique(TB)
  TB <- TB[order(-Difference)]
  TB <- TB[1:10, ]
  TB
}



