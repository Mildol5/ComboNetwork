#this R notebook will focus on modeling a network of a popular tweet 
#first we load libraries and set up our twitter scraping
library(twitteR)
library(igraph)
library(stringr)
api_key <- "QZjjIh395Hi5h7B9Z1uRzl6Vl"
api_secret <- "tV902XtsGQ5PolcBQxIlJFK7c7epwOOLBzv4gVhvZES5kxZAKl"
access_token <- "824327335-RDA6jCwhL6Lgq23P3EHQ0O5jAT5BMlStGa4ZZg8e"
access_token_secret <- "EDD7rWDxnZG11wF46ITHVWsLBpsp0SIXdJpsr1LKxgCRK"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets = userTimeline("kanyewest", n=1000)
aaaa= tweets[[35]]$id
bbbb = tweets[[20]]$id
xl = searchTwitter(searchString = "to:kanyewest",sinceID = aaaa,maxID = bbbb, n=13715)
replies = data.frame(c(1:411),c(1:411),c(1:411),c(1:411),c(1:411),c(1:411))
count = 1
for(i in 1:length(xl)){
  if(length(xl[[i]]$getReplyToSID()) >0){
    if(xl[[i]]$getReplyToSID()[1] == aaaa){
      replies[count,1] = xl[[i]]$id
      replies[count,2] = xl[[i]]$screenName
      replies[count,3] = xl[[i]]$getReplyToSID()
      replies[count,4] = xl[[i]]$text
      replies[count,5] = xl[[i]]$getRetweetCount() + xl[[i]]$getFavoriteCount()
      #now look for tweets replying to this one 
      find = searchTwitter(searchString = paste0("to:",xl[[i]]$screenName),sinceID = aaaa,maxID = bbbb)
      replii <- c(0)
      count2 = 1
      for(j in 1:length(find)){
        if(length(find[[j]]$getReplyToSID()) >0){
          if(find[[j]]$getReplyToSID()[1] == xl[[i]]$id){
            replii[count2] = find[[j]]$id 
            count2 = count2+1
            }
          }
      }
      replies[count,6] = replii
      count= count+1
    }
  }
}
edgeList <- data.frame(replies,stringsAsFactors = FALSE)
edgeList['numberFavs'] <- 1:411
edgeList['numberFavs'] <- sapply(edgeList[,1], function(x) showStatus(id=x))


