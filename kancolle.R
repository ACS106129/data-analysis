
#處理資料
#file<-read.table("./EX.csv",header = TRUE,sep = ",")
#甲難度csv
file <- read.table("./HARD.csv", header = TRUE, sep = ",")
#乙難度csv
#file<-read.table("./NORMAL.csv",header = TRUE,sep = ",")
#丙難度csv
#file<-read.table("./EASY.csv",header = TRUE,sep = ",")
#丁難度csv
file <- cbind(file, "最低提督等級" = substr(file$提督等級, 1, 3), "最高提督等級" = substr(file$提督等級, 7, 9))
file <- file[, c(1, 2, 3, 4, 5, 6, 9, 10, 8)]
temp <- cbind(file, gsub("%", "", file$掉落率))
temp <- cbind(file, 掉落率num = as.numeric(gsub("%", "", file$掉落率)))
drop <- ddply(temp, "艦種", summarise, 綜合掉落率 = sum(數量), S勝掉落率 = sum(S勝), A勝掉落率 = sum(A勝), B勝掉落率 = sum(B勝), 掉落率 = sum(掉落率num))
drop$艦種 <- factor(drop$艦種, levels = rev(drop$艦種))

#勝率/掉落率分析
掉落率S <- sprintf("%.2f", c(drop$S勝掉落率 / sum(drop$S勝掉落率)) * 100)
掉落率A <- sprintf("%.2f", c(drop$A勝掉落率 / sum(drop$A勝掉落率)) * 100)
掉落率B <- sprintf("%.2f", c(drop$B勝掉落率 / sum(drop$B勝掉落率)) * 100)
posMIX <- c((cumsum(drop$綜合掉落率[1:8]) + cumsum(c(0, drop$綜合掉落率[-c(8, 9)]))) / 2, 0)
posS <- c((cumsum(drop$S勝掉落率[1:8]) + cumsum(c(0, drop$S勝掉落率[-c(8, 9)]))) / 2, 0)
posA <- c((cumsum(drop$A勝掉落率[1:8]) + cumsum(c(0, drop$A勝掉落率[-c(8, 9)]))) / 2, 0)
posB <- c((cumsum(drop$B勝掉落率[1:8]) + cumsum(c(0, drop$B勝掉落率[-c(8, 9)]))) / 2, 0)
drop <- cbind(drop, 掉落率S, 掉落率A, 掉落率B, posMIX, posS, posA, posB)
ggplot(drop[c(1:8),], aes("", 綜合掉落率, fill = 艦種, label = paste(掉落率, "%", sep = ""))) + geom_col(color = "black") + coord_polar("y") + geom_label_repel(aes(y = posMIX)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(drop[c(1:8),], aes("", S勝掉落率, fill = 艦種, label = paste(掉落率S, "%", sep = ""))) + geom_col(color = "black") + coord_polar("y") + geom_label_repel(aes(y = posS)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(drop[c(1:8),], aes("", A勝掉落率, fill = 艦種, label = paste(掉落率A, "%", sep = ""))) + geom_col(color = "black") + coord_polar("y") + geom_label_repel(aes(y = posA)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(drop[c(1:8),], aes("", B勝掉落率, fill = 艦種, label = paste(掉落率B, "%", sep = ""))) + geom_col(color = "black") + coord_polar("y") + geom_label_repel(aes(y = posB)) + theme(panel.grid = element_blank(), axis.text = element_blank())

#新船綜合/各難度掉落機率
newship <- c(rep("old", length(file$S勝)))
newship[c(which(file$艦娘 == "Johnston"), which(file$艦娘 == "Gotland"), which(file$艦娘 == "福江"))] = "new"
temp <- cbind(temp, newship)
new <- ddply(temp, c("newship", "艦種"), summarise, 綜合掉落率 = sum(數量), S勝掉落率 = sum(S勝), A勝掉落率 = sum(A勝), B勝掉落率 = sum(B勝))
rate <- sprintf("%.2f", new$綜合掉落率 / sum(new$綜合掉落率) * 100)
rateS <- sprintf("%.2f", new$S勝掉落率 / sum(new$S勝掉落率) * 100)
rateA <- sprintf("%.2f", new$A勝掉落率 / sum(new$A勝掉落率) * 100)
rateB <- sprintf("%.2f", new$B勝掉落率 / sum(new$B勝掉落率) * 100)
new <- cbind(new, rate, rateS, rateA, rateB)
new$艦種 <- as.factor(c("福江", "Gotland", "Johnston", rep(NA, 9)))
posN <- c(((sum(new$綜合掉落率) - cumsum(new$綜合掉落率)) * 2 + new$綜合掉落率) / 2)
posNS <- c(((sum(new$S勝掉落率) - cumsum(new$S勝掉落率)) * 2 + new$S勝掉落率) / 2)
posNA <- c(((sum(new$A勝掉落率) - cumsum(new$A勝掉落率)) * 2 + new$A勝掉落率) / 2)
posNB <- c(((sum(new$B勝掉落率) - cumsum(new$B勝掉落率)) * 2 + new$B勝掉落率) / 2)
new <- cbind(new, posN, posNS, posNA, posNB)
new$posN[which(new$newship == "old")] <- NA
new$posNS[which(new$newship == "old")] <- NA
new$posNA[which(new$newship == "old")] <- NA
new$posNB[which(new$newship == "old")] <- NA
new$艦種 <- factor(new$艦種, levels = c("福江", "Gotland", "Johnston", NA))
new$rate <- as.numeric(as.character(new$rate))
new$rateS <- as.numeric(as.character(new$rateS))
new$rateA <- as.numeric(as.character(new$rateA))
new$rateB <- as.numeric(as.character(new$rateB))
new <- ddply(new, c("posN", "posNS", "posNA", "posNB", "艦種"), summarise, 綜合掉落率 = sum(綜合掉落率), S勝掉落率 = sum(S勝掉落率), A勝掉落率 = sum(A勝掉落率), B勝掉落率 = sum(B勝掉落率), rate = sum(rate), rateS = sum(rateS), rateA = sum(rateA), rateB = sum(rateB))
ggplot(new, aes("", 綜合掉落率, fill = 艦種, label = rate)) + geom_col(color = "black") + coord_polar("y", direction = -1) + geom_label_repel(aes(y = posN)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(new, aes("", S勝掉落率, fill = 艦種, label = S勝掉落率)) + geom_col(color = "black") + coord_polar("y", direction = -1) + geom_label_repel(aes(y = posNS)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(new, aes("", A勝掉落率, fill = 艦種, label = A勝掉落率)) + geom_col(color = "black") + coord_polar("y", direction = -1) + geom_label_repel(aes(y = posNA)) + theme(panel.grid = element_blank(), axis.text = element_blank())
ggplot(new, aes("", B勝掉落率, fill = 艦種, label = B勝掉落率)) + geom_col(color = "black") + coord_polar("y", direction = -1) + geom_label_repel(aes(y = posNB)) + theme(panel.grid = element_blank(), axis.text = element_blank())

