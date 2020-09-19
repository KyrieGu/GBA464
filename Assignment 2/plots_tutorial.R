# plots_tutorial
# Yufeng Huang
# this is designed to help Assignment 2

x <- 1:10
y <- x^2
z <- 2*x + 20

# simply use plot() again will replace original
plot(x, y)  # single plot
plot(x, z, type = 'l')  # replaces the original plot, type gives the type of plots

# can set par() to overlay two plots
plot(x, y)
par(new = T)    # start accepting new plots directly on the old one
plot(x, z, type = 'l')
par(new = F)    # turn it off

# but we found that the axis are weird, so we can do the following
plot(x, y)
points(x, z, type = 'l')    # points will not generate new plot but will instead add to new plots (using orignal axis)

# or we can do
plot(x, y)
par(new = T)
plot(x, z, type = 'l', xlab = "", ylab = "", axes=FALSE)
axis(side = 4)  # so we suppressed the axis on the second plot, but add it back on the 4th side (right)
par(new = F)

 