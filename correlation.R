require(tidyverse)
require(openxlsx)

setwd("/Users/Yuki/Dropbox/TokyoBay_eDNA/nishijima2019")
edna = read.table("countOTUv2.fish.txt", header = T) 
edna = edna %>% gather(key = OTU, value = reads, 4:ncol(edna))

sp = read.xlsx("OTU_Species.xlsx", sheet = 1, colNames = F)
colnames(sp) = c("OTU", "s_name", "j_name")

edna = right_join(edna, sp, by = "OTU") %>% mutate(year = as.numeric(str_sub(Date, 1, 2))+2000, month = as.numeric(str_sub(Date, 3, 4)), day = as.numeric(str_sub(Date, 5, 6)))

site = read.table("sampling_points.txt", header = T) %>% dplyr::rename(Point = pop)
length(unique(edna$Point))
head(site)
head(edna)
# edna = edna %>% right_join(edna, site, by = "Point")
edna = merge(edna, site, by = "Point")

cpue = c("マコガレイ", "イシガレイ", "マアナゴ", "コノシロ", "スズキ", "カマス", "アカカマス", "クロダイ", "シログチ", "トラフグ")
head(edna)
edna2 = edna %>% filter(j_name == cpue)
unique(edna2$j_name)


# CPUE data -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/TokyoBay_CPUE/nominalCPUE")
old = read.csv("new_chiba3.csv")
new = read.csv("tent_data.csv", fileEncoding = "CP932")

head(old)
head(new)

old2 = old %>% select(Y,M,Lon,Lat,FISH,CATCH,NUM,CPUE,GEAR)
mode(old2$M)
new2 = new %>% select(year,month,lon,lat,回数,CPUE,全銘柄,species,gear) %>% rename(Y = year, Lon = lon, Lat = lat, NUM = 回数, CATCH = 全銘柄, FISH = species, GEAR= gear)
month =  data.frame(month = unique(new2$month))
month
month$M = c(9,10,7,8,6,12,11,4,5,2,3,1)
new2 = left_join(new2, month, by = "month") %>% dplyr::select(-month)

data = rbind(old2, new2) %>% filter(Y == 2018)
fish = data.frame(FISH = unique(data$FISH))
fish
fish$j_name = c("マコガレイ", "イシガレイ", "コノシロ", "コウイカ", "マアナゴ", "スズキ", "クルマエビ", "クロダイ", "トラフグ", "アカカマス")
# data = data %>% right_join(data, fish, by = "FISH") %>% filter(j_name == cpue)
data = merge(data, fish, by = "FISH") %>% filter(j_name == cpue)
unique(data$j_name)


# eDNA-catch ----------------------------------------------------
df_c = data %>% group_by(M, FISH, j_name) %>% summarize(mean_c = mean(CATCH)) %>% dplyr::rename(month = M)
df_d = edna2 %>% group_by(month, j_name, Depth) %>% summarize(mean_d = mean(reads))

head(df_c, 2)
head(df_d, 2)
df = merge(df_c, df_d, by = c("month", "j_name")) %>% mutate(l_catch = log(mean_c), l_dna = log(mean_d))
summary(df)

g = ggplot(df, aes(x = l_catch, y = l_dna, colour = FISH))
p = geom_point()
labs = labs(x = "Log(catch)", y = "Log(# of reads)", title = "All data")
g+p+labs+theme_bw()

g = ggplot(df, aes(x = l_catch, y = l_dna, colour = FISH))
p = geom_point()
f = facet_wrap(~ Depth, ncol = 2)
labs = labs(x = "Log(catch)", y = "Log(# of reads)", title = "Depth")
g+p+f+labs+theme_bw()

