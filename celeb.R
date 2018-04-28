#network_retweets
library(twitteR)
library(igraph)
library(stringr)
api_key <- "QZjjIh395Hi5h7B9Z1uRzl6Vl"
api_secret <- "tV902XtsGQ5PolcBQxIlJFK7c7epwOOLBzv4gVhvZES5kxZAKl"
access_token <- "824327335-RDA6jCwhL6Lgq23P3EHQ0O5jAT5BMlStGa4ZZg8e"
access_token_secret <- "EDD7rWDxnZG11wF46ITHVWsLBpsp0SIXdJpsr1LKxgCRK"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets = userTimeline("barackobama", n=1000,includeRts = TRUE)

tweet_txt = sapply(tweets, function(x) x$getText())
# regular expressions to find retweets
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets,
     ignore.case=TRUE, value=TRUE)
# which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)",
                   tweet_txt, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
tweet_txt[rt_patterns]



# we create a list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns)){
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  # get retweet source
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)")
  #remove ':'
  poster = gsub(":", "", unlist(poster))
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE)
  who_post[[i]] = str_extract(who_post[[i]],"([^\\s]+)")
  # name of retweeting user
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster))
}

# and we put it off the list
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)
# two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)

# generate graph
rt_graph = graph.edgelist(retweeter_poster)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# choose some layout
glay = layout.sphere(rt_graph)

# plot
par(bg="gray5", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=3,
     edge.color=hsv(h=.25, s=1, v=.7, alpha=0.5))
# add title
title("Who does Barack Obama Retweet?",
      cex.main=1, col.main="gray95")



#####NEW
tweets = userTimeline("tylerthecreator", n=1000,includeRts = TRUE)

tweet_txt = sapply(tweets, function(x) x$getText())
# regular expressions to find retweets
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets,
     ignore.case=TRUE, value=TRUE)
# which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)",
                   tweet_txt, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
tweet_txt[rt_patterns]



# we create a list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns)){
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  # get retweet source
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)")
  #remove ':'
  poster = gsub(":", "", unlist(poster))
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE)
  who_post[[i]] = str_extract(who_post[[i]],"([^\\s]+)")
  # name of retweeting user
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster))
}

# and we put it off the list
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)
# two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)

# generate graph
rt_graph = graph.edgelist(retweeter_poster)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# choose some layout
glay = layout.sphere(rt_graph)

# plot
par(bg="gray5", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=3,
     edge.color=hsv(h=.25, s=1, v=.7, alpha=0.5))
# add title
title("Who does Tyler the Creator Retweet?",
      cex.main=1, col.main="gray95")
#now from the people we find, who do they retweet? MESSY PLOT###################
post <- levels(factor(retweeter_poster[,2]))
myfunc<- function(url){
  tryCatch(
    userTimeline(user=url, n=1,includeRts = TRUE), 
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
}
for(i in 1:length(post)){
  if(is.na(myfunc(post[i]))){
    post[i] = NA
  }
}
for(i in 1:length(post)){
  if(is.na(post[i])){
    next
  }
  tweets2 = userTimeline(user=post[i], n=50,includeRts = TRUE) #look for at least 100 tweets
  
  tweet_txt2 = sapply(tweets2, function(x) x$getText())
  # regular expressions to find retweets
  rt_patterns2 = grep("(RT|via)((?:\\b\\W*@\\w+)+)",
                      tweet_txt2, ignore.case=TRUE)
  if(length(rt_patterns2)<=1){
    next
  }
  # we create a list to store user names
  who_retweet2 = as.list(1:length(rt_patterns2))
  who_post2 = as.list(1:length(rt_patterns2))
  # for loop
  for (j in 1:length(rt_patterns2)){
    # get tweet with retweet entity
    twit2 = tweets2[[rt_patterns2[j]]]
    # get retweet source
    poster2 = str_extract_all(twit2$getText(),
                              "(RT|via)((?:\\b\\W*@\\w+)+)")
    #remove ':'
    poster2 = gsub(":", "", unlist(poster2))
    # name of retweeted user
    who_post2[[j]] = gsub("(RT @|via @)", "", poster2, ignore.case=TRUE)
    who_post2[[j]] = str_extract(who_post2[[j]],"([^\\s]+)")
    # name of retweeting user
    who_retweet2[[j]] = rep(twit2$getScreenName(), length(poster2))
  }
  
  # and we put it off the list
  who_post2 = unlist(who_post2)
  who_retweet2 = unlist(who_retweet2)
  # two column matrix of edges
  retweeter_poster2 = cbind(who_retweet2, who_post2)
  retweeter_poster = rbind(retweeter_poster,retweeter_poster2)
}
# generate graph
rt_graph = graph.edgelist(retweeter_poster)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# choose some layout
glay = layout.grid(rt_graph)

# plot
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=3,
     edge.color=hsv(h=.25, s=1, v=.7, alpha=0.5),
     vertex.label=NA)
# add title
title("Full network for Tyler the Creator",
      cex.main=1, col.main="gray95")