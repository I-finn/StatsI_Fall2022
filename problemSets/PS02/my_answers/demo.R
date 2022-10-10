y<- rnorm(100, 0, 1)
x<- rnorm(100, 0, 1)

pdf("test.pdf")
plot(x,y)
dev.off()


