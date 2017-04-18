# Manipulate plots and save them as different formats
# some random data
x <- rnorm(100)
y <- 0.5 + 0.3*x+rnorm(100)/5

# here is the standard plot
plot(x,y)

# I would like to write something else for the labels on the axes
# use xlab= and ylab=
plot(x,y, xlab="My own x label", ylab="my own y label")

# a title, use main=
plot(x,y, xlab="My own x label", ylab="my own y label", main="My title")

# Change the symbols, I don't like circles, use pch
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2)
#see
?pch # you can use many different symbols

# Change the colour
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="red")
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue")

# Make the symbols bigger, use cex=
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2)
# make fatter lines use lwd=
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3)

# change the size of the labels and axis,
# use cex.lab and cex.axis, the value "1.5" is just a scaling factor
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3,
     cex.axis=1.5, cex.lab=1.5)

# Change the font of the labels and axis
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3,
     cex.axis=1.5, cex.lab=1.5,
     font.axis=2, font.lab=3)

# insert a legend, use legend()
y2 <- 0.5 - 0.3*x+rnorm(100)/5 
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3,
     cex.axis=1.5, cex.lab=1.5,
     font.axis=2, font.lab=3)
points(x,y2,pch=6,col="red",cex=2,lwd=3)
legend("bottom",c("my y data","my y data 2"), col=c("blue", "red"),
       pch=c(2,6), lwd=3, cex=1.5, lty=NA)
# lty means "linetype" and you have no line
# see
?legend

# if you would like to display your plot in different window
# use X11() and dev.off() to close
X11(height=10,width=15)
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3,
     cex.axis=1.5, cex.lab=1.5,
     font.axis=2, font.lab=3)
# now close
dev.off()

# Or straight away save as a jpeg or tiff or png
jpeg(filename="z:/willem/teaching/envx2001/SaveMyplot.jpg", width=720, height=720)
plot(x,y, xlab="My own x label", ylab="my own y label", main="plot with triangles",
     pch=2, col="blue", cex=2, lwd=3,
     cex.axis=1.5, cex.lab=1.5,
     font.axis=2, font.lab=3)
dev.off()
# This should now be in the directory indicated
# see
?jpeg
