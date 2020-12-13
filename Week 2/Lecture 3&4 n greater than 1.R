
education = function(one)
{
  result = list();
  result$who = one;
  result$think = c("intensively", "critically")
  result$goal = "intelligence + character";
  result;
}

me = education("Connor");
me;

#Topic: n > 1
#there are many ways to do research
# install.packages("stringr", dependencies=T);
# library(stringr);	
# install.packages("rvest", dependencies=T);
# library(rvest);	


#Denzel Washington [nm0000243] vs Will Smith [nm0000226]
#https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie
#https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000226&ref_=filmo_ref_typ&sort=user_rating,desc&mode=detail&page=1&title_type=movie

grabFilmInfoFromFilmsPage = function(page) #get the film info
{
  #grab 50 elements
  #title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
  
  movies = page %>%
    html_nodes(".mode-detail");
  
  pageCount = length(movies);
  
  result = data.frame( matrix(ncol = 11, nrow = pagecount) ); #a matrix full of NA values
  
  colname(result) = c("rank", "title", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions");
  
  for(i in 1:pageCount) 
  {
    movies = movies[i];
    
    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    result$rank[i] = rank;
    
    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    result$title[i] = title;
    
    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");
    
    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    result$ttid[i] = ttid;
    
    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYear(year);
    result$year[i] = year;
    
    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;
    
    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutes(minutes);
    result$minutes[i] = minutes;		
    
    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    result$genre[i] = genre;
    
    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);	
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    result$ratings[i] = ratings;
    
    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    result$metacritic[i] = metacritic;
    
    # para ... +5 EASTER EGG ...
    
    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();
    
    votes = as.numeric(gsub(",","",info[8],fixed=T));
    result$votes[i] = votes;
    
    millions = cleanupMillions(info[11]);
    result$millions[i] = millions;			
  }
  
  result;
}

cleanupMillions = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);
  
  millions = as.numeric(millions);
  millions;
}

cleanupMinutes = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);
  
  minutes = as.numeric(minutes);
  minutes;
}

cleanupYear = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabNameFromFilmsPage = function(page) #get the actor's name
{
  name = page %>%
    html_node(".header") %>% #only looking for 1 node named header
    html_text();
  
    name = gsub("Most Rated Feature Films With", "", name, fixed=T); #fixed = true means its splitting a string and not a regular expression
    name = str_trim(name);
  
  name;
}

grabFilmCountFromFilmsPage = function(page) #get the number of films the actor played in
{
  totalCount = page %>%
    html_nodes(".desc") %>%
    html_text();
  
    temp = strsplit(totalCount, "of", fixed=T) ;
    temp2 = strsplit(temp[[1]][2], "titles", fixed=T);
  
    totalCount = str_trim(temp2[1][1]);
    totalCount = as.numeric(totalCount);
  
    temp2 = strsplit(temp[[1]][1], "to", fixed=T);
  
    pageCount = str_trim(temp2[[1]][2]);
    pageCount = as.numeric(pageCount);

  result = list();
  result$totalCount = totalCount;
  result$pageCount = pageCount;
  
  result;
}

#   nmid = "nm0000226"; Will Smith
# 	will = grabFilmsForPerson(nmid);
# 	plot(will$movies.50[,c(1,6,7:10)]);
#  	boxplot(will$movies.50$millions);

#   nmid = "nm0000243"; Denzel Washington
# 	denzel = grabFilmsForPerson(nmid);
# 	plot(denzel$movies.50[,c(1,6,7:10)]);
#  	boxplot(denzel$movies.50$millions);

grabFilmsForPerson = function(nameID)
{
  url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=", nameID, "&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie", sep="");
  
  page1 = read_html(url);
  result = list();
  
  #name of person
  result$name = grabNameFromFilmsPage(page1);
  result$countFilms = grabFilmCountFromFilmsPage(page1);
  result$movies.50 = grabFilmInfoFromFilmsPage(page1);
  
  
  #parallel format
  # ranks = page1 %>%
  #   html_nodes(".lister-item-index") %>%
  #   html_text() %>%
  #   as.numeric();
  # ranks;
  # 
  # years = page1 %>%
  #   html_nodes(".lister-item-year") %>%
  #   html_text()
  #   
  #   years = gsub('(', '', years, fixed = T);
  #   years = gsub(')', '', years, fixed = T);
  #   years = gsub('I', '', years, fixed = T);
  #   years = as.numeric(years);
  #   
  # titles = page1 %>%
  #   html_nodes(".lister-item-year") %>%
  #   html_text();
  # titles;
  
  
##### will-vs-denzel.txt #####
#      nmid = "nm0000226";
#   will = grabFilmsForPerson(nmid);
#  	plot(will$movies.50[,c(1,6,7:10)]);
#   	boxplot(will$movies.50$millions);
# 		widx =  which.max(will$movies.50$millions);
# 	will$movies.50[widx,];
# 		summary(will$movies.50$year);  # bad boys for life ... did data change?
# 
#    nmid = "nm0000243";
#  	denzel = grabFilmsForPerson(nmid);
#  	plot(denzel$movies.50[,c(1,6,7:10)]);
#   	boxplot(denzel$movies.50$millions);
# 		didx =  which.max(denzel$movies.50$millions);
# 	denzel$movies.50[didx,];
# 		summary(denzel$movies.50$year);
# 	
# 	par(mfrow=c(1,2));
# 	boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Raw Millions" );
# 	boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Raw Millions" );
# 	
# 	par(mfrow=c(1,1));
	
#https://www.in2013dollars.com/us/inflation/2000?endYear=1982&amount=100
#create variable $millions.2000 to convert all money to 2000 dollars ... based on year 
  
  result;
}

nameID = "nm0000243"; #Denzel Washington
grabFilmsForPerson(nameID)

