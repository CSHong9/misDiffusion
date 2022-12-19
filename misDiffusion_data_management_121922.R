## Project: The Diffusion of Online Vaccine Misinformation: Fake News Publishers and the Paradox of Embeddedness 
## Purpose: Main data processing and descriptive script
## Code written by Chen-Shuo Hong
## All rights reserved
## Last revision: 12/19/2022

## clear all
rm(list=ls())
## setting working directory
setwd("C:/analyses")
## session info
sessionInfo()

#R version 4.0.5 (2021-03-31)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19044)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United States.1252  
#[2] LC_CTYPE=English_United States.1252
#[3] LC_MONETARY=English_United States.1252
#[4] LC_NUMERIC=C                           
#[5]LC_TIME=English_United States.1252    
#system code page: 950

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#loaded via a namespace (and not attached):
#[1] igraph_1.2.6 Rcpp_1.0.9 magrittr_2.0.1 hms_1.1.1 insight_0.14.4 performance_0.7.1
#[7] rlang_0.4.10 carData_3.0-4 fansi_0.5.0 car_3.0-10 tools_4.0.5 data.table_1.14.0
#[13] rio_0.5.26 utf8_1.2.1 bayestestR_0.9.0 ellipsis_0.3.2 abind_1.4-5 readxl_1.3.1     
#[19] tibble_3.1.2 lifecycle_1.0.1 crayon_1.4.2 zip_2.2.0 vctrs_0.3.8 curl_4.3         
#[25] haven_2.4.1 openxlsx_4.2.4 stringi_1.6.2 compiler_4.0.5 pillar_1.6.4 cellranger_1.1.0 
#[31] forcats_0.5.1 foreign_0.8-81 pkgconfig_2.0.3

#### LOADING PACKAGES ####
library(igraph)
library(intergraph) 
library(GGally)
library(reshape)
library(tidyverse)
library(statnet)
library(amen)
library(ade4)
library(lsa)
library(ggraph)
library(bipartite)
library(magrittr)
library(data.table)
set.seed(19)

#### READ DATA ####
rawNetData <- read.csv(file="comps_master_data_all.csv",header=TRUE, as.is=TRUE, stringsAsFactors = FALSE, encoding = "utf-8")
rawNetData$timestamp <- as.POSIXct(strptime(rawNetData$posted_at, '%a %b %d %H:%M:%S %z %Y', tz = "GMT"), tz = "GMT") #N=1056816
node_att <- read.csv(file = "node_CLN.csv", header = TRUE, as.is = TRUE, stringsAsFactors = FALSE, encoding = "utf-8")
rawNetData <- left_join(rawNetData, node_att) #1069042 obs

#### IDENTITY PUBLISHERS ####
## Identify which links come from mainstream media
ori_list <- levels(as.factor(rawNetData[rawNetData$group == 2,]$site))
add_list <- c("bbc.co.uk","bbc.in","blogs.wsj.com","cbs.com","cdc.gov","cnnespanol.cnn.com","fda.gov","go.cnn.com","nation.foxnews.com","nbc.com","nbcnews.to","nbcnews.com","radio.foxnews.com","scientificamerican.com","video.foxnews.com","whitehouse.gov","who.int","cbsnews.com")
add_name <- c("BBC", "BBC", "Washington Post", "CBS", "CDC", "CNN", "FDA", "CNN", "Fox News", "NBC", "NBC", "NBC", "Fox News", "Scientific American", "Fox News", "White House", "WHO")
expand_list <- list(orginal_list = ori_list, add_list = add_list)
NetData <- rawNetData %>% 
  mutate(expand_name = ifelse(site %in% expand_list$add_list, case_when(
    site == "bbc.co.uk" ~ "BBC",
    site == "bbc.in" ~ "BBC",
    site == "blogs.wsj.com" ~ "Washington Post",
    site == "cbs.com" ~ "CBS",
    site == "cbsnews.com" ~ "CBS",
    site == "cdc.gov" ~ "CDC",
    site == "cnnespanol.cnn.com" ~ "CNN",
    site == "fda.gov" ~ "FDA",
    site == "go.cnn.com" ~ "CNN",
    site == "nation.foxnews.com" ~ "Fox News",
    site == "nbc.com" ~ "NBC",
    site == "nbcnews.to" ~ "NBC",
    site == "nbcnews.com" ~ "NBC",
    site == "radio.foxnews.com" ~ "Fox News",
    site == "scientificamerican.com" ~ "Scientific American",
    site == "video.foxnews.com" ~ "Fox News",
    site == "whitehouse.gov" ~ "White House",
    site == "who.int" ~ "WHO"
  )
  , name))
## 40 mainstream outlets
main_name <- NetData %>% 
  filter(site %in% expand_list$orginal_list | site %in% expand_list$add_list) %>%
  filter(expand_name != "Breitbart") %>%
  filter(!is.na(expand_name)) %>%
  distinct(expand_name)

fake_list <- levels(as.factor(rawNetData[rawNetData$group == 1,]$site))
## 313 fake news publishers
fake_name <- NetData %>% 
  filter((site %in% fake_list) | (expand_name == "Breitbart")) %>%
  filter(expand_name != "Global Times"
            & expand_name != "CCTV International"
            & expand_name != "China Global Television Network"
            & expand_name != "China Daily"
            & expand_name != "BrightSide"
            & expand_name != "Swarajya"
            & expand_name != "First Post"
            & expand_name != "Indiatimes"
            & expand_name != "The Economic Times"
            & expand_name != "Mehr News Agency"
            & expand_name != "The Tehran Times"
            & expand_name != "Press TV"
            & expand_name != "Rudaw"
            & expand_name != "Behold Israel"
            & expand_name != "Al Bawaba"
            & expand_name != "The Nation Pakistan"
            & expand_name != "Al Arabiya"
            & expand_name != "Arab News"
            & expand_name != "The Majalla"
            & expand_name != "Daily Star UK"
            & expand_name != "Biologos Foundation"
            & expand_name != "WikiIslam"
            & expand_name != "Republican National Committee"
            & expand_name != "Church Militant"
            & expand_name != "The Trumpet"
            & expand_name != "The Conservative Papers"
            & expand_name != "Prevention Magazine"
            & expand_name != "iHarare"
            & expand_name != "Tasnim News Agency") %>%
  filter(!is.na(expand_name)) %>%
  distinct(expand_name)

NetData_core <- NetData %>%
  filter(expand_name %in% as.list(main_name)[[1]] | expand_name %in% as.list(fake_name)[[1]]) %>%
  mutate(expand_group = ifelse(expand_name %in% as.list(main_name)[[1]], "trusted media", "fake news"))
## 551,183 tweets

user_list <- NetData_core %>%
  select(user_id, user_name, verified) %>%
  distinct() %>%
  group_by(user_id, user_name, verified) %>%
  mutate(n= n())
## 199,230 users

## We should remove duplicates happened when left join node_att
NetData_core_bp_upd <- NetData_core %>% 
  select(posted_at, tweet_id, site, user_id, user_name, location, verified, 
         followers_count, friends_count, created_at, possibly_sensitive, retweet_count,
         favorite_count,expand_name,expand_group, retweet, quote) %>%
  distinct()
## 538883 tweets

#### DIFFUSION PLOT ####
##This is for Figure 4
NetData_core_bp_upd$timestamp <- as.POSIXct(strptime(NetData_core_bp_upd$posted_at, '%a %b %d %H:%M:%S %z %Y', tz = "GMT"), tz = "GMT")
NetData_core_bp_upd$date <- as.Date(NetData_core_bp_upd$timestamp)
NetData_core_bp_upd$share <- 1
setDT(NetData_core_bp_upd)
NetData_core_bp_upd[ , site_ID := .GRP, by = expand_name] # create ID by group
NetData_core_bp_upd[ , user_ID_raw := .GRP, by = user_name]
NetData_core_bp_upd[ , user_ID := user_ID_raw + max(site_ID)]
active_site <- NetData_core_bp_upd[ , .(site_count = length(unique(site_ID))), by = c("date","expand_group")] # unique publishers by date
site_count <- reshape(active_site, idvar = "date", timevar = "expand_group", direction = "wide")
colnames(site_count) <- c("date","count_fake","count_trusted")
NetData_core_bp_upd <- left_join(NetData_core_bp_upd, site_count, by = "date")
require(plyr)
dd_share_fake <- ddply(NetData_core_bp_upd[expand_group == "fake news" 
                                      & retweet == 0 & quote == 0,], .(date), 
                  summarise, val_f = sum(share)/unique(count_fake))
dd_rt_fake <- ddply(NetData_core_bp_upd[expand_group == "fake news" 
                                   & retweet == 0 & quote == 0,], .(date), 
               summarise, val_f = sum(retweet_count)/unique(count_fake))
dd_fav_fake <- ddply(NetData_core_bp_upd[expand_group == "fake news" 
                                   & retweet == 0 & quote == 0,], .(date), 
               summarise, val_f = sum(favorite_count)/unique(count_fake))

a <- ggplot(data = dd_share_fake) + 
  geom_line(aes(x = date, y = val_f #cumsum(val_f)
                  , linetype="solid")) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  xlab("") +
  theme_classic() +
  theme(legend.position="bottom")+
  scale_linetype_discrete(name = "Outlets",
                          labels = c("Fakenews"))+
  ggtitle("A. Posts")+
  ylab("")
b <- ggplot(data = dd_rt_fake) + 
  geom_line(aes(x = date, y = val_f #cumsum(val_f)
                  , linetype="solid")) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  xlab("") +
  theme_classic() +
  theme(legend.position="bottom")+
  scale_linetype_discrete(name = "Outlets",
                          labels = c("Fakenews"))+
  ggtitle("B. Retweets")+
  ylab("")
c <- ggplot(data = dd_fav_fake) + 
  geom_line(aes(x = date, y = val_f #cumsum(val_f)
                , linetype="solid")) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  xlab("") +
  theme_classic() +
  theme(legend.position="bottom")+
  scale_linetype_discrete(name = "Outlets",
                          labels = c("Fakenews"))+
  ggtitle("C. Likes")+
  ylab("")

library(grid)
library(gridExtra)
library(lemon)
grid_arrange_shared_legend(a,b,c,nrow = 3,ncol=1, position='top')

#### BIPARTITE NETWORKS ####
##This is for Figure 5
sample <- NetData_core_bp_upd[expand_group == "fake news",] ### 333059 tweets (analytic)
length(unique(sample$site_ID)) #unique publishers: 313
length(unique(sample$user_ID)) #unique users: 103117

##create three 
sample_date <- NetData_core_bp_upd[date == "2021-06-20" & #"2021-04-19 #2021-01-04
                              expand_name != user_name &
                              user_name != "GMWatch" &
                              expand_group == "fake news",c("expand_name","user_name")]
mode1 <- unique(sample_date$user_name)
mode2 <- unique(sample_date$expand_name)
length(mode1)
length(mode2)
net <- as.network(sample_date, 
                  bipartite=T,   # Number of nodes in the second mode (events)
                  directed=FALSE,
                  multiple=T)
inet<- graph.data.frame(sample_date, directed=FALSE)
V(inet)$type <- bipartite_mapping(inet)$type
bilayout <- layout.bipartite(inet)
V(inet)$color <- ifelse(V(inet)$type, "gray", "black")
deg <- igraph::degree(inet, mode="all")
V(inet)$size <- log(deg)*2

#pdf(file = "bipartite.pdf")
gplot(net, 
      gmode="twomode", 
      usearrows = FALSE, 
      displaylabels = FALSE, 
      edge.col="gray")
#dev.off()

# projection
bipartite_matrix <- as.matrix(table(sample_date))
bipartite_matrix_binary <- ifelse(test=bipartite_matrix>0,yes=1,no=0)
unipartite_matrix <- tcrossprod(bipartite_matrix_binary)
net_uni <- as.network(unipartite_matrix, 
                      bipartite=F,
                      directed=FALSE,
                      multiple=F,
                      ignore.eval=FALSE,
                      names.eval='weight')
net_uni %v% "size" <- diag(unipartite_matrix)

#pdf(file = "onemode.pdf")
gplot(net_uni, 
    usearrows = FALSE, 
    displaylabels = FALSE, 
    edge.col="black",
    vertex.cex=log(net_uni %v% "size")/2+0.2)
#dev.off()


#### ONE-MODE PROJECTION LOOP ####
date_list <- unique(NetData_core_bp_upd$date)
gr.list <- list()
pop.list <- list()
out.list <- list()
igr.list <- list()
for (i in 1:168) {
  date_id <- date_list[i]
  #date_id_end <- date_list[i+2]
  test <- NetData_core_bp_upd[date == date_id &
                              #date <= date_id_end &
                              expand_name != user_name &
                              user_name != "GMWatch" &
                              expand_group == "fake news",
                              c("site_ID","user_ID")]
  # follower counts
  pop.list[[i]] <- NetData_core_bp_upd %>%
    filter(date == date_id & expand_name != user_name & 
             user_name != "GMWatch" & expand_group == "fake news") %>%
    mutate(sensitive_1 = ifelse(possibly_sensitive == "True", 1, 0)) %>%
    group_by(site_ID) %>% 
    dplyr::summarise(followers = sum(followers_count), sensitive = sum(sensitive_1), total = n())
  # contagion
  out.list[[i]] <- NetData_core_bp_upd %>%
    filter(date == date_id & expand_name != user_name & 
             user_name != "GMWatch" & expand_group == "fake news" & 
             retweet == 0 & quote == 0) %>%
    group_by(site_ID) %>% 
    dplyr::summarise(post = n(), rt = sum(retweet_count), like = sum(favorite_count))
  # projection
  bipartite_matrix <- as.matrix(table(test))
  bipartite_matrix_binary <- ifelse(test=bipartite_matrix>0,yes=1,no=0)
  unipartite_matrix <- tcrossprod(bipartite_matrix_binary)
  #diag(unipartite_matrix)
  net_uni <- as.network(unipartite_matrix, 
                        bipartite=F,
                        directed=FALSE,
                        multiple=F,
                        ignore.eval=FALSE,
                        names.eval='weight')
  net_uni %v% "size" <- diag(unipartite_matrix)
  gr.list[[i]] <- net_uni
  inet_uni <- asIgraph(net_uni)
  igr.list[[i]] <- inet_uni
}

#### NETWORK CHARACTERISTICS ####
##### Global Structure #####
#purrr::map_dbl(gr.list, network.size)
sgb_net <- data.frame(time_id = 1:168,
                     nsize = map_dbl(gr.list, network.size) %>% enframe() %>% select(value),
                     density = map_dbl(gr.list, gden, mode ="graph") %>% enframe() %>% select(value),
                     trans = map_dbl(igr.list, transitivity, type="global") %>% enframe() %>% select(value))
colnames(sgb_net) <- c("time_id","nsize","density","trans")
sgb_net$time_id <- as.character(sgb_net$time_id)
igb_net <- igr.list %>%
  map_dfr(~ tibble(comp = igraph::components(.x)$no,
                   iso = sum(igraph::degree(.x)==0),
                   cen = centr_degree(.x)$centralization),
          .id = "time_id")
gb_net <- left_join(sgb_net,igb_net)
gb_net$isop <- gb_net$iso/gb_net$comp
res <- cor(gb_net[,-1])
round(res, 2)

##### EGO Structure #####
ego_list <- list()
for (i in 1:168) {
  g <- igr.list[[i]]
  id <- V(g)$vertex.names
  audience <- V(g)$size
  degeego <- igraph::degree(g)
  cluster <- transitivity(g, type="weighted")
  ego_data <- data.frame(time_ID = date_list[i],
                         site_ID = id,
                         aud = audience,
                         deg = degeego,
                         isoego = ifelse(degeego==0, 1, 0),
                         trans_local = ifelse(is.na(cluster),0,cluster))
  ego_follower <- pop.list[[i]]
  ego_follower$site_ID <- as.character(ego_follower$site_ID)
  temp_f <- left_join(ego_data, ego_follower, by=c("site_ID"))
  ego_contagion <- out.list[[i]]
  ego_contagion$site_ID <- as.character(ego_contagion$site_ID)
  temp <- left_join(temp_f, ego_contagion, by=c("site_ID"))
  temp$meanfw <- temp$followers/temp$aud
  temp$logmeanfw <- log(temp$meanfw + 1)
  temp$propsen <- temp$sensitive/temp$total
  temp$logpost <- log(temp$post)
  temp$logrt <- log(temp$rt + 1)
  temp$loglike <- log(temp$like + 1)
  temp$avgrt <- temp$rt/temp$post
  temp$avglike <- temp$like/temp$post
  ego_list[[i]] <- temp
}

##### Panel Structure #####
ego.data <- bind_rows(ego_list, .id = "time_id")
model.data <- left_join(ego.data, gb_net, by = "time_id")
summary(model.data)
res <- cor(model.data[complete.cases(model.data),-(1:3)]) ##12751
round(res, 2)
model.data %<>%
  mutate(# audience size centered around its mean and scaled by 10 (1 unit = 100 users)
    aud.cen = scale(as.numeric(aud), scale= 100),
    trans_localsq = trans_local^2,
    nsize.cen = scale(nsize, scale = FALSE),
    transsq = trans^2)
model.data <- as.data.table(model.data)

cols = c("aud","deg","isoego","trans_local","logmeanfw","trans_localsq","aud.cen","propsen")
anscols = paste("lag", cols, sep="_")
model.data[, (anscols) := shift(.SD, 1, NA, "lag"), .SDcols=cols, by = c("site_ID")]
time_feature <- unique(model.data[,c("time_id","nsize","density","trans","comp","iso","cen","isop","transsq")])
cols_global = c("nsize","density","trans","comp","iso","cen","isop","transsq")
anscols_global = paste("lag", cols_global, sep="_")
time_feature[, (anscols_global) := shift(.SD, 1, NA, "lag"), .SDcols=cols_global]
site.panel <- left_join(model.data, time_feature[,c("time_id","lag_nsize",
                                                    "lag_density","lag_trans",
                                                    "lag_comp","lag_iso",
                                                    "lag_cen","lag_isop","lag_transsq")])
site.panel$post_fill <- ifelse(is.na(site.panel$post), 0, site.panel$post)
site_list <- distinct(NetData_core_bp_upd[ , c("site_ID", "expand_name")])
site_list$site_ID <- as.character(site_list$site_ID)
site.panel.newid <- left_join(site.panel,site_list, by = "site_ID")
site_meta <- read.csv("site_metadata.csv")
site_meta$site_ID <- as.character(site_meta$site_ID)
site.panel.ana <- left_join(site.panel.newid,site_meta, by = "expand_name")

message <- NetData_core_bp_upd[expand_group == "fake news" 
                    & retweet == 0 & quote == 0, c("site_ID","user_ID",
                                                   "possibly_sensitive",
                                                   "retweet_count","favorite_count",
                                                   "date","followers_count")]
message$time_ID <- message$date
message$site_ID <- as.character(message$site_ID)
site.panel.ana$site_ID <- site.panel.ana$site_ID.x
message.panel <- left_join(message, site.panel.ana, by = c("site_ID","time_ID"))
message.panel <- as.data.table(message.panel)
message.panel$logfollower <- log(message.panel$followers_count+1)
message.panel.ana <- message.panel[time_id>1,c("site_ID","user_ID",
                                               "possibly_sensitive",
                                               "retweet_count","favorite_count","followers_count",
                                               "logfollower",
                                               "date","time_id",
                                               "lag_aud","lag_deg","lag_isoego","lag_trans_local",
                                               "lag_logmeanfw","lag_trans_localsq","lag_aud.cen",
                                               "lag_nsize",
                                               "lag_density","lag_trans",
                                               "lag_comp","lag_iso",
                                               "lag_cen","lag_isop","lag_transsq",
                                               "location","sponsor","revenue","owner",
                                               "year","traffic","Media.Type")]
message.panel.ana$cgn <- message.panel.ana$retweet_count + message.panel.ana$favorite_count

##descriptive statistics
library(vtable)
st(site.panel.ana[!is.na(site.panel.ana$lag_isoego),])#, out="csv", file = "descriptive_site_upd.csv")
st(message.panel.ana[!is.na(message.panel.ana$lag_isoego),])#, out="csv", file = "descriptive_message_upd.csv")
write.csv(site.panel.ana, "sitepanel_upd.csv")
write.csv(message.panel.ana, "messagepanel_upd.csv")