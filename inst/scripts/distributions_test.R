# serial distribution to calculate exposure time
inc_samp <- 30
x1 = sn::rsn(n = 5000,xi = inc_samp,omega = 3,alpha = 4)
plot(density(x1))
length(x1[x1<inc_samp])/5000*100

x2 = sn::dsn(xi = inc_samp,omega = 2,alpha = 0)
plot(density(x2))

# onset distribution, dist_shape, dist_scale
x=rweibull(20000,3,11)
plot(density(x))

# delay distribution, delay_shape, delay_scale
y=rweibull(5000,0.7,4)
plot(density(y))


# distribution for selecting new cases from isolated infectors
# size=disp.iso, mu=r0isolated
z1=plot(density(rnbinom(5000,size=0.1,mu=0)))

pnbinom(4, size=100, mu=0.8, lower.tail = FALSE)

data.frame(x=0:10,P=rnbinom(0:10,size=100,mu=0.8))
sum(xx$P[xx$P>5])

# distribution for selecting new cases from non-isolated infectors
# size=disp.com, mu=r0community
z2=plot(density(rnbinom(5000,size=100,mu=0.8)))

