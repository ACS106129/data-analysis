#整理資料
Adf <- read.csv('res/E3甲.csv', stringsAsFactor = F)
Bdf <- read.csv('res/E3乙.csv', stringsAsFactor = F)
Cdf <- read.csv('res/E3丙.csv', stringsAsFactor = F)
Ddf <- read.csv('res/E3丁.csv', stringsAsFactor = F)
colnames(Adf) <- c("艦娘", "艦種", "A數量", "AS勝", "AA勝", "AB勝", "A提督等級", "A掉落率");
colnames(Bdf) <- c("艦娘", "艦種", "B數量", "BS勝", "BA勝", "BB勝", "B提督等級", "B掉落率");
colnames(Cdf) <- c("艦娘", "艦種", "C數量", "CS勝", "CA勝", "CB勝", "C提督等級", "C掉落率");
colnames(Ddf) <- c("艦娘", "艦種", "D數量", "DS勝", "DA勝", "DB勝", "D提督等級", "D掉落率");
Adf <- ddply(Adf, .(A數量), transform, A平均提督等級 = round(mean(as.numeric(strsplit(A提督等級, " ~ ")[[1]]))), 艦種 = ifelse(is.na(艦種), "無", 艦種))
Bdf <- ddply(Bdf, .(B數量), transform, B平均提督等級 = round(mean(as.numeric(strsplit(B提督等級, " ~ ")[[1]]))), 艦種 = ifelse(is.na(艦種), "無", 艦種))
Cdf <- ddply(Cdf, .(C數量), transform, C平均提督等級 = round(mean(as.numeric(strsplit(C提督等級, " ~ ")[[1]]))), 艦種 = ifelse(is.na(艦種), "無", 艦種))
Ddf <- ddply(Ddf, .(D數量), transform, D平均提督等級 = round(mean(as.numeric(strsplit(D提督等級, " ~ ")[[1]]))), 艦種 = ifelse(is.na(艦種), "無", 艦種))
mergedf <- Reduce(function(...) merge(..., by = c("艦娘", "艦種"), all = T), list(Adf, Bdf, Cdf, Ddf))

#長條圖(艦種-數量)
g_bar <- ggplot(mergedf[order(mergedf$艦娘),], aes(艦種, mapply(sum, A數量, B數量, C數量, D數量, na.rm = TRUE), fill = 艦娘)) + geom_bar(stat = "identity") + labs(y = "總數量") + scale_y_continuous(labels = scales::comma)

#圓餅圖(艦種-掉落率)
genePie <- function(piedf, droprate, name) {
  piedf$艦種 <- factor(piedf$艦種, levels = rev(piedf$艦種))
  lab <- aaply(droprate, 1, function(element) paste0(element, "%"))
  breakpoint <- cumsum(droprate) - droprate / 2
  g <- ggplot(piedf, aes(x = "", y = droprate, fill = 艦種), title = name) +
    geom_bar(stat = "identity", color = "black") +
    geom_label_repel(aes(label = lab, y = breakpoint)) +
    coord_polar(theta = "y") +
    theme(axis.ticks = element_blank()
          , axis.text = element_blank()
         , axis.title = element_text(colour = 'black', size = 20)
          , panel.grid = element_blank()) +
    labs(x = NULL, y = name)
  return(g)
}
dropdf <- ddply(mergedf, .(艦種), summarize
                , 綜合掉落率 = round(sum(A數量, B數量, C數量, D數量, na.rm = T) / sum(mergedf$A數量, mergedf$B數量, mergedf$C數量, mergedf$D數量, na.rm = T), 4) * 100
                , S勝掉落率 = round(sum(AS勝, BS勝, CS勝, DS勝, na.rm = T) / sum(mergedf$AS勝, mergedf$BS勝, mergedf$CS勝, mergedf$DS勝, na.rm = T), 4) * 100
                , A勝掉落率 = round(sum(AA勝, BA勝, CA勝, DA勝, na.rm = T) / sum(mergedf$AA勝, mergedf$BA勝, mergedf$CA勝, mergedf$DA勝, na.rm = T), 4) * 100
                , B勝掉落率 = round(sum(AB勝, BB勝, CB勝, DB勝, na.rm = T) / sum(mergedf$AB勝, mergedf$BB勝, mergedf$CB勝, mergedf$DB勝, na.rm = T), 4) * 100)
totalDropdf <- dropdf[dropdf$綜合掉落率 != 0,]
SDropdf <- dropdf[dropdf$S勝掉落率 != 0,]
ADropdf <- dropdf[dropdf$A勝掉落率 != 0,]
BDropdf <- dropdf[dropdf$B勝掉落率 != 0,]

g_total_pie <- genePie(totalDropdf, sort(totalDropdf$綜合掉落率, decreasing = T), "綜合掉落率")
g_s_pie <- genePie(SDropdf, sort(SDropdf$S勝掉落率, decreasing = T), "S勝掉落率")
g_a_pie <- genePie(ADropdf, sort(ADropdf$A勝掉落率, decreasing = T), "A勝掉落率")
g_b_pie <- genePie(BDropdf, sort(BDropdf$B勝掉落率, decreasing = T), "B勝掉落率")

grid.arrange(g_total_pie, g_s_pie, g_a_pie, g_b_pie)