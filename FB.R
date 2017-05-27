
Token <- "EAACEdEose0cBAEfLc7Uwew1pZBXgSJcwVC0kjUWFRrY0Icb5fhbtfYThDMe92w9EPRXvMLzYiJHB4gDYsNK2E70lojqXmhvKLmCLzKx0Siz6UeIMdS1to6jKTqONce21iQT6ZC2ZAdj75hPsacqZAg0GifRZBR6HrFkIxXfi1PPbsgdZAAkssksI7BDRk1HJ0ZD"


me <- getUsers("me", token = Token, private_info = TRUE)
me$name # my name
me$hometown # my hometown
me$locale #my locale
me$category # my category
me$location # mylocation




#what is my lastest like
i <- getLikes("me", token = Token)[1,]
i

#post myself status
updateStatus("Hello from R Facebook Package", Token)



#get the number of friend on my FB (for version 2.0 up. it allows only friends who are Developer)
my_friends <- getFriends(Token, simplify = TRUE)

#my recent friends
head(my_friends$id, n = 1) # get lowest user ID

#get info of friend (data return in ID of friends)
my_friends_info <- getUsers(my_friends$id, Token, private_info = TRUE)

#friend's gender
table(my_friends_info$gender)  # gender

#Their language
table(substr(my_friends_info$locale, 1, 2))  # language
table(substr(my_friends_info$locale, 4, 5)) 


#Friends status
table(my_friends_info$relationship_status)["In relationship"] 
mat <- getNetwork(Token, format = "adj.matrix")
dim(mat)


#####################Fanpage Analysis###################################


page <- getPage("Fanpage", Token, n = 1000)

#Find the post which people like most
page[which.max(page$likes_count), ]


###Visualize
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}



# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)



# creat line chart to compare "like", "share" and "comment"
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", 
                                                                                  breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())


