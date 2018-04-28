network_rtweets.R 
  This the first place where you should look to undertand the code. We use the Twitter Developer API to scrape 
  Youtube's retweets (using some regular expression), and then build a graph using igraph 
celeb.R
  This is where we do analysis on Barack Obama and Tyler the Creator. Code is almost identical, except for the 
  last part where we create a full graph. 
network_tweets.R
  This is unfinished work. I was trying to see how connected twitter communities are. We would build this by seeing 
  how many twitter followers two given users have in common. This was harder to build than anticipated. Mostly because 
  of API limitations. To pick up on the work, we need to build a matrix of connections between all users in a given 
  set. 
