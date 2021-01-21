## Chapter 4
rm(list = ls())

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA)
plot(dose, drugA, type = "b")

plot(dose, drugA, type = "b", lty = 1, pch = 15)
lines(dose, drugB, type = "b", lty = 2, pch = 17)
legend("topleft", title = "Drug Type",
       legend = c("A", "B"), 
       lty = c(1, 2), 
       pch = c(15, 17))

library(MASS)
data(anorexia)
str(anorexia)
attach(anorexia)
par(mfrow = c(1, 3))
hist(Prewt)
plot(density(Prewt))
hist(Prewt, freq = FALSE, col = "red",
     xlab = "体重(lbs)", 
     main = "治疗前体重分布直方图",
     las = 1)
lines(density(Prewt), col = "blue", lwd = 2)
rug(Prewt)
detach(anorexia)
par(mfrow = c(1, 2))

library(vcd)
data(Arthritis)
attach(Arthritis)
counts <- table(Improved)
counts 
barplot(counts, xlab = "Improvement", ylab = "Freqency", las = 1)

counts <- table(Improved, Treatment)
barplot(counts, 
        col = c("red", "yellow", "green"),
        xlab = "Improvement", 
        ylab = "Freqency", 
        beside = TRUE, las = 1)
legend("top", legend = rownames(counts), 
       fill = c("red", "yellow", "green"))
par(mfrow = c(1, 1))

library(epiDisplay)
aggregate.plot(anorexia$Postwt, by = list(anorexia$Treat), 
               error = "sd", legend = FALSE, 
               bar.col = c("red", "yellow", "green"),
               ylim = c(0,100), las = 1,
               main = "")

percent <- c(5.8, 27.0, 0.5, 20.8, 12.8, 33.1)
disease <- c("上感", "中风", "外伤", "昏厥", "食物中毒", "其他")
lbs <- paste0(disease, percent, "%")
pie(percent, labels = lbs, col = rainbow(6))

anorexia$wt.change <- anorexia$Postwt - anorexia$Prewt
b <- boxplot(anorexia$wt.change, ylab = "Weight change (lbs)", las = 1)

boxplot(wt.change ~ Treat, data = anorexia,
        ylab = "Weight change (lbs)", las = 1)

library(vioplot)
vioplot(wt.change ~ Treat, data = anorexia, 
        ylab = "Weight change (lbs)",
        col = "gold", las = 1)

dotchart(VADeaths)
dotchart(t(VADeaths),pch = 19)

pdf("mygraph.pdf")
boxplot(wt.change ~ Treat, 
        data = anorexia, 
        ylab = "Weight change (lbs)",
        las = 1)
dev.off()

tiff(filename = "mygraph.tiff",
     width = 15, height = 12, units = "cm", res = 300)
boxplot(wt.change ~ Treat, data = anorexia, ylab = "Weight change (lbs)")
dev.off()

library(ggplot2)
p <- ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) 
p + geom_point()

library(gridExtra)
mtcars$am <- factor(mtcars$am)
p1 <- ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + geom_point()
p2 <- ggplot(data = mtcars, aes(x = wt, y = mpg, shape = am)) + geom_point()
grid.arrange(p1, p2, nrow=1)
 
ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + stat_smooth()           

ggplot(data = mtcars, aes(x = wt, y = mpg, color = am)) + 
        stat_smooth(method = "lm")

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
        geom_point(aes(color = am)) +
        stat_smooth() 

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
        geom_point(aes(color = am)) +
        scale_color_manual(values = c("blue", "red")) +
        stat_smooth() 

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
        geom_point() +
        stat_smooth() +
        facet_grid(~ am)

ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
        geom_point(aes(color = am)) +
        stat_smooth() +
        theme_bw()

library(ggplot2)
data(anorexia, package = "MASS")
anorexia$wt.change <- anorexia$Postwt - anorexia$Prewt
p1 <- ggplot(anorexia, aes(x = wt.change)) +
        geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
        labs(x = "Weight change (lbs)") +
        theme_bw()
p1

p2 <- ggplot(anorexia, aes(x = wt.change, y = ..density..)) +
        geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
        stat_density(geom = "line",linetype = "dashed", size = 1) +
        labs(x = "Weight change (lbs)") +
        theme_bw()
p2

p3 <- ggplot(anorexia, aes(x = wt.change, color = Treat, linetype = Treat)) +
        stat_density(geom = "line", size = 1) +
        labs(x = "Weight change (lbs)") +
        theme_bw()
p3

p4 <- ggplot(anorexia, aes(x= Treat, y = wt.change)) +
        geom_boxplot() +
        theme_bw()
p4

library(ggpubr)
my_comparisons <- list(c("CBT", "Cont"), c("CBT", "FT"), c("Cont", "FT"))
p5 <- ggplot(anorexia, aes(x= Treat, y = wt.change)) +
        geom_boxplot() +
        stat_compare_means(comparisons = my_comparisons,
                           method = "t.test",
                           color = "blue") +
        theme_bw()
p5

p6 <- ggplot(anorexia, aes(x= Treat, y = wt.change)) +
        geom_violin() +
        geom_point(position = position_jitter(0.1), alpha = 0.5) +
        theme_bw()
p6

library(vcd)
data(Arthritis)
ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
        geom_bar(color = "black") +
        scale_fill_brewer() +
        theme_bw()

ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
        geom_bar(color = "black", position = "fill") +
        scale_fill_brewer() +
        theme_bw()

ggplot(Arthritis, aes(x = Treatment, fill = Improved)) +
        geom_bar(color = "black", position = "dodge") +
        scale_fill_brewer() +
        theme_bw()

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
ggsave("myplot.png", p)
ggsave("myplot.pdf", p)

ggsave("myplot.tiff", width = 15, height = 12, units = "cm", dpi = 500)

library(epiDisplay)
data(Oswego)
pyramid(Oswego$age, Oswego$sex, col.gender = c(2, 4), bar.label = TRUE)

library(sjPlot)
data(efc)
names(efc)
view_df(efc)    # 查看数据信息
qdata <- dplyr::select(efc, c82cop1:c90cop9)
plot_stackfrq(qdata)

data(mtcars)
dat <- scale(mtcars)
class(dat)
heatmap(dat)

library(scatterplot3d)
data(trees)
scatterplot3d(trees, type = "h", highlight.3d = TRUE, angle = 55, pch = 16)

library(rgl)
plot3d(trees)

library(wordcloud2)
head(demoFreqC)
wordcloud2(demoFreqC)

for (i in 1:10) {
        png(file = paste0(i, ".png"))
        plot.new()
        text(0.5, 0.5, 11 - i, cex = 6)
        dev.off()
}

library(ggplot2)
library(gganimate)
airquality$date <-
        as.Date(paste(1973, airquality$Month, airquality$Day, sep = "-"))

g <- ggplot(airquality, aes(date, Temp)) +
        geom_line() +
        transition_time(Month)+
        ease_aes('sine-in-out')
anim_save(g, filename = "animation.gif")
