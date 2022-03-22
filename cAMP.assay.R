attach(cell_viability_2)
hist(cell_viability, `que`,`Cell Viability %`, main = "",
     xlim = 100, ylim = 120, 
     xlab = "Quercetin(µm)" , ylab = "cell Viability", 
     col = "dark blue", border = "orange")

cor.test(Quercetin,`Cell Viability %`)
regz.cv<- lm(`Cell Viability %`~Quercetin)
summary(regz.cv)
plot(Quercetin,`Cell Viability %`)
que <- as.integer(`Quercetin(µm)`)
require(ggplot2)
p <- ggplot( cell_viability, aes(que, `Cell Viability %`))

p + geom_histogram()

attach(Book1)
d <- ggplot(Book1, aes(log10(`GLP-1(7-37)`),cAMP))
d + geom_smooth(se = F, ylim = 120, ylab = "cAMP pmol/mg protein",
                xlab = "Log GLP-1(7-37)", col = "black")
plot.default(log10(`GLP-1(7-37)`),cAMP, type = "o", 
             ylab = "cAMP pmol/mg protein",
             xlab = "Log GLP-1(7-37)", pch = 19)
attach(Book2)
attach(Book3)
plot.default(log10(`Exendin 4`),cAMP., type,
             ylab = "cAMP pmol/mg protein",
             xlab = "Log Exendin 4")
plot.def

attach()
