#We will be analyzing how presidents and their political parties impact the states and the public
#Reagan 1988-1981 (R)
#Clinton 2000-1993 (D)
#Bush 2008-2001 (R)
#Obama 2016-2009 (D)
#Source: https://millercenter.org/president


#load and unpack libraries
require(pacman)
require(dplyr)
pacman::p_load(pacman, tidycensus, viridis, tidyverse, ggplot2, rio, maps, gridExtra, scales)


#import the unemployment data from the csv
#Source: https://www.icip.iastate.edu/tables/employment/unemployment-states
employmentdf <- data.frame(read.csv("Employment_by_State.csv",header=TRUE))
view(employmentdf)


#import the poverty rate data from the csv
#Source: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html
povertydf <- data.frame(read.csv("Poverty_by_State.csv",header=TRUE))
view(povertydf)


#import the REAL disposable per capita income data from the csv
#Source: https://fred.stlouisfed.org/release/tables?rid=249&eid=259462&od=2016-01-01#
per_capita <- data.frame(read.csv("Per_Capita_Disposable_Income_by_State.csv",header=TRUE))
view(per_capita)


#import the GPD by state data from the csv
#Source: https://apps.bea.gov/itable/iTable.cfm?ReqID=70&step=1#reqid=70&step=1&isuri=1
gdp <- data.frame(read.csv("GDP_by_State.csv",header=TRUE))
view(gdp)


#calculate the change in unemployment rates by state from the beginning to end of presidencies 
remployment <- employmentdf$X1988-employmentdf$X1981
cemployment <- employmentdf$X2000-employmentdf$X1993
bemployment <- employmentdf$X2008-employmentdf$X2001
oemployment <- employmentdf$X2016-employmentdf$X2009


#calculate the change in poverty rates by state from the beginning to end of presidencies 
rpoverty <- povertydf$X1988-povertydf$X1981
cpoverty <- povertydf$X2000-povertydf$X1993
bpoverty <- povertydf$X2008-povertydf$X2001
opoverty <- povertydf$X2016-povertydf$X2009


#calculate the percent change in real disposable per capita income by state from the beginning to end of presidencies 
rpercapita <- (per_capita$X1988-per_capita$X1981)/per_capita$X1981
cpercapita <- (per_capita$X2000-per_capita$X1993)/per_capita$X1993
bpercapita <- (per_capita$X2008-per_capita$X2001)/per_capita$X2001
opercapita <- (per_capita$X2016-per_capita$X2009)/per_capita$X2009


#calculate the percent change in gdp by state from the beginning to end of presidencies 
rgdp <- (gdp$X1988-gdp$X1981)/gdp$X1981
cgdp <- (gdp$X2000-gdp$X1993)/gdp$X1993
bgdp <- (gdp$X2008-gdp$X2001)/gdp$X2001
ogdp <- (gdp$X2016-gdp$X2009)/gdp$X2009


  
#build out data frames for each president
reagan <- data.frame(employmentdf$ï..Area,remployment,rpoverty,rpercapita,rgdp)
clinton <- data.frame(employmentdf$ï..Area,cemployment,cpoverty,cpercapita,cgdp)
bush <-  data.frame(employmentdf$ï..Area,bemployment,bpoverty,bpercapita,bgdp)
obama <- data.frame(employmentdf$ï..Area,oemployment,opoverty,opercapita,ogdp)


#rename the area columns to prepare for our inner join
reagan <- reagan %>%
  rename(region=employmentdf.ï..Area)
clinton <- clinton %>%
  rename(region=employmentdf.ï..Area)
bush <- bush %>%
  rename(region=employmentdf.ï..Area)
obama <- obama %>%
  rename(region=employmentdf.ï..Area)


#turn the region column lowercase to match up with out inner join on region for the maps library
reagan$region <- tolower(reagan$region)
clinton$region <- tolower(clinton$region)
bush$region <- tolower(bush$region)
obama$region <- tolower(obama$region)


#get the map data needed to plot the map 
states = data.frame(map_data("state"))


#use the inner_join to combine data frames 
rdf <- inner_join(states,reagan,by="region")
cdf <- inner_join(states,clinton,by="region")
bdf <- inner_join(states,bush,by="region")
odf <- inner_join(states,obama,by="region")


#prep the maps background
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


#generate the plots to map out changes in poverty levels for each president across the US
rpovertymap <- ggplot(states) + 
  geom_polygon(data = rdf, aes(x=long,y=lat,group=group,fill=rpoverty), color = "white")+
  scale_fill_gradient2(limits = c(-10,10),low="green",high="red")


cpovertymap <- ggplot(states) + 
  geom_polygon(data = cdf, aes(x=long,y=lat,group=group,fill=cpoverty), color = "white")+
  scale_fill_gradient2(limits = c(-10,10),low="green",high="red")


bpovertymap <- ggplot(states) + 
  geom_polygon(data = bdf, aes(x=long,y=lat,group=group,fill=bpoverty), color = "white")+
  scale_fill_gradient2(limits = c(-10,10),low="green",high="red")


opovertymap <- ggplot(states) + 
  geom_polygon(data = odf, aes(x=long,y=lat,group=group,fill=opoverty), color = "white")+
  scale_fill_gradient2(limits = c(-10,10),low="green",high="red")


#generate the plots to map out changes in unemployment levels for each president across the US
remploymentmap <- ggplot(states) + 
  geom_polygon(data = rdf, aes(x=long,y=lat,group=group,fill=remployment), color = "white")+
  scale_fill_gradient2(low="green",high="red",limits = c(-10,5))


cemploymentmap <- ggplot(states) + 
  geom_polygon(data = cdf, aes(x=long,y=lat,group=group,fill=cemployment), color = "white")+
  scale_fill_gradient2(low="green",high="red",limits = c(-10,5))


bemploymentmap <- ggplot(states) + 
  geom_polygon(data = bdf, aes(x=long,y=lat,group=group,fill=bemployment), color = "white")+
  scale_fill_gradient2(low="green",high="red",limits = c(-10,5))


oemploymentmap <- ggplot(states) + 
  geom_polygon(data = odf, aes(x=long,y=lat,group=group,fill=oemployment), color = "white")+
  scale_fill_gradient2(low="green",high="red",limits = c(-10,5))


#generate the plots to map out changes in income per capita levels for each president across the US
rpercapitamap <- ggplot(states) + 
  geom_polygon(data = rdf, aes(x=long,y=lat,group=group,fill=rpercapita), color = "white")+
  scale_fill_gradient2(high="green", limit = c(0,1))


cpercapitamap <- ggplot(states) + 
  geom_polygon(data = cdf, aes(x=long,y=lat,group=group,fill=cpercapita), color = "white")+
  scale_fill_gradient2(high="green", limit = c(0,1))


bpercapitamap <- ggplot(states) + 
  geom_polygon(data = bdf, aes(x=long,y=lat,group=group,fill=bpercapita), color = "white")+
  scale_fill_gradient2(high="green", limit = c(0,1))


opercapitamap <- ggplot(states) + 
  geom_polygon(data = odf, aes(x=long,y=lat,group=group,fill=opercapita), color = "white")+
  scale_fill_gradient2(high="green", limit = c(0,1))


#generate the plots to map out the percent change in gdp for each president across the US
rgdpmap <- ggplot(states) + 
  geom_polygon(data = rdf, aes(x=long,y=lat,group=group,fill=rgdp), color = "white")+
  scale_fill_gradient2(high="green", limit = c(-0.5,1.5))


cgdpmap <- ggplot(states) + 
  geom_polygon(data = cdf, aes(x=long,y=lat,group=group,fill=cgdp), color = "white")+
  scale_fill_gradient2(high="green", limit = c(-0.5,1.5))


bgdpmap <- ggplot(states) + 
  geom_polygon(data = bdf, aes(x=long,y=lat,group=group,fill=bgdp), color = "white")+
  scale_fill_gradient2(high="green", limit = c(-0.5,1.5))


ogdpmap <- ggplot(states) + 
  geom_polygon(data = odf, aes(x=long,y=lat,group=group,fill=ogdp), color = "white")+
  scale_fill_gradient2(high="green", limit = c(-0.5,1.5))


#Use grid.arrange to view all 4 us poverty map plots side by side 
#With the 2 republican presidents on top and the 2 democrats on the bottom
grid.arrange(rpovertymap,bpovertymap,cpovertymap,opovertymap,nrow=2)

#Use grid.arrange to view all 4 us unemployment map plots side by side 
#With the 2 republican presidents on top and the 2 democrats on the bottom
grid.arrange(remploymentmap,bemploymentmap,cemploymentmap,oemploymentmap,nrow=2)


#Use grid.arrange to view all 4 us gdp map plots side by side 
#With the 2 republican presidents on top and the 2 democrats on the bottom
grid.arrange(rgdpmap,bgdpmap,cgdpmap,ogdpmap,nrow=2)


#Use grid.arrange to view all 4 us income per capita map plots side by side 
#With the 2 republican presidents on top and the 2 democrats on the bottom
grid.arrange(rpercapitamap,bpercapitamap,cpercapitamap,opercapitamap,nrow=2)



#create final df of all dems combined
demp = c(obama$opoverty,clinton$cpoverty)
demperc = c(obama$opercapita,clinton$cpercapita)
demgdp = c(obama$ogdp,clinton$cgdp)
dememp = c(obama$oemployment,clinton$cemployment)
demr = c(obama$region,clinton$region)

dems = data.frame(demr,demperc,demgdp,dememp,demp)
colnames(dems) <- c("region","per capita","gdp",
                    "employment","poverty")
dems$group <- "democrat"
dems

#create final df of all republican data combined
remp = c(reagan$rpoverty,bush$bpoverty)
remperc = c(reagan$rpercapita,bush$bpercapita)
remgdp = c(reagan$rgdp,bush$bgdp)
rememp = c(reagan$remployment,bush$bemployment)
remr = c(reagan$region,bush$region)

reps = data.frame(remr,remperc,remgdp,rememp,remp)
colnames(reps) <- c("region","per capita","gdp",
                    "employment","poverty")
reps$group <- "republican"
reps

#create absolute final df to run our hypothesis tests 
finaldf = rbind.data.frame(reps,dems)

view(finaldf)

#get some cursory summary information
summary(finaldf)

#run the t-test to test the null hypothesis that democratic presidents have reduced poverty levels more than republicans
t.test(poverty~group, data=finaldf, var.equal = TRUE, alternative="greater")

#create a box plot to support the the test results
boxplot(poverty~group,data=finaldf, main="Change in Poverty Level",
        xlab="Political Party", ylab="% Change in Poverty")

#run the t-test to test the null hypothesis that democratic presidents have reduced unemployment level more than republicans
t.test(employment~group, data=finaldf, var.equal = TRUE, alternative="greater")

#create a box plot to support the the test results
boxplot(employment~group,data=finaldf, main="Change in Unemployment Rate",
        xlab="Political Party", ylab="% Change in Unemployment")

#run the t-test to test the null hypothesis that democratic presidents increase the gdp less than republicans
t.test(gdp~group, data=finaldf, var.equal = TRUE, alternative="greater")

#create a box plot to support the the test results
boxplot(gdp~group,data=finaldf, main="Change in GDP",
        xlab="Political Party", ylab="% Change in GDP")

#run the t-test to test the null hypothesis that democratic presidents had lower per capita income than republicans
t.test(`per capita`~group, data=finaldf, var.equal = TRUE, alternative="greater")

#create a box plot to support the the test results
boxplot(`per capita`~group,data=finaldf, main="Change in Real Per Capita Income",
        xlab="Political Party", ylab="% Change in Income")
