

n <- 100
m <- 10000
num <- double (n)
ver <- double (n)
ver_sum <- double (n)
mas <- double (m)
m300 <- double (300)
m30 <- double (30)

myRand = function(number)
{
	for(i in 1:n)
	{
		if (number <= ver_sum[i])
		{
			return (num[i])
		}
	}
	return (num[n - 1])
}

for (i in 1:n)
{
	num[i] <- i + 1
	ver_sum[i] <- 0
}

step <- 1
ver[1] <- 0.2

for (i in 2:10)
{
	ver[i] <- 0.2 / 9
}

k <- 0.30
step <- 11

while (k > 0)
{
	for (i in (0 + step):(30 + step))
	{
		ver[i] <- k / 30
	}
	step <- step + 30
	k <- k - 0.1
}

for (i in 1:n)
{
	for (j in 1:(i))
	{
		ver_sum[i] <- ver_sum[i] + ver[j]
	}
}

for (i in 1:m){
	mas[i] <- myRand(runif(1, 0, 1))
}

for (i in 1:300)
{
	m300[i] <- mas[sample(1:10000, 1)]
}

m30[1] <- mas[sample(1:10000, 1)]
i <- 2
k <- 1
while (i < 31)
{
	m30[i] <- mas[sample(1:10000, 1)]
	while(k < i)
	{
		if (m30[i] == m30[k])
		{
			m30[i] <- mas[sample(1:10000, 1)]
			k <- 1
		}
		else
		{
			k <- k + 1
		}
	}
	k <- 1
	i <- i + 1
}

Mx <- 0
Dx <- 0
for(i in 1:n)
{
	Mx <- Mx + num[i] * ver[i] 
	Dx <- Dx + num[i] * num[i] * ver[i]
}
Dx <- Dx - Mx * Mx
cat("Mx_d = ", Mx, "\n")
cat("Dx_d = ", Dx, "\n")

cat("Mx_300 = ", mean(m300), "\n")
cat("Dx_300 = ", var(m300), "\n")
cat("Mx_30 = ", mean(m30), "\n")
cat("Dx_30 = ", var(m30), "\n")


par (mfrow = c(3,1))
hist(mas, breaks = 100)
hist(m300, breaks = 100)
hist(m30, breaks = 100)
