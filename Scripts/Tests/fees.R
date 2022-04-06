fee <- 0.00075 # fee percentage
c0 <- 10 # dollars
p0 <- 1.6 # Initial coin price

(r <- 2*fee / (1 - fee)) # win yield
(r <- -0.01) # win yield # 2*fee / (1 - fee) tie

# buy
(mount <- c0/p0) # coins
# fee in dollars
feeCapitalBuy <- mount*p0*fee

# sale
p1 <- p0*(1+r) # new price
c1 <- mount*p1 - feeCapitalBuy
feeCapitalSale <- mount*p1*fee
# mount <- 0

(c2 <- c1 - feeCapitalSale)
(realYield <- (c2 - c0) / c0)



# Total
(c2 <- c1 - feeCapitalSale)
(mount*p1 - feeCapitalBuy) - mount*p1*fee
(mount*p1 - mount*p0*fee) - mount*p1*fee
(mount*p1 - mount*p0*fee) - mount*p1*fee
((c0/p0)*p1 - (c0/p0)*p0*fee) - (c0/p0)*p1*fee
(c0/p0)*(p1 - p0*fee - p1*fee)
(c0/p0)*(p1 - fee*(p0 + p1)) # ...(1)
c0*(p1 - fee*p0 - p1*fee)/p0 
c0*((1 - fee)*p1 - (1 - 1 + fee)*p0)/p0
c0*((1 - fee)*p1 - (- 1 + fee)*p0 - p0)/p0
c0*((1 - fee)*p1 + (1 - fee)*p0 - p0)/p0
c0*((1 - fee)*(p1 + p0) - p0)/p0
c0*((1 - fee)*(p1 + p0)/p0 - 1)

# De (1)
c0*((p1/p0) - fee*(1 + (p1/p0)))
c0*((1+r) - fee*(1 + (1+r)))
c0*(1 + r - fee*(2 + r))
c0*(1 + r - fee*(2 + r))
c0*(1 + r - 2*fee - r*fee)
c0*(1 - 2*fee + r*(1 - fee))


# Despejando mínimo r para no perder:
c0*(1 - 2*fee + r*(1 - fee)) >= c0
1 - 2*fee + r*(1 - fee) >= 1
- 2*fee + r*(1 - fee) >= 0
r*(1 - fee) >= 2*fee
r >= 2*fee / (1 - fee)


# Real Yield
(realYield <- (c2 - c0) / c0)
((c2 - c0) / c0)
((c1 - feeCapitalSale - c0) / c0)
((mount*p1 - feeCapitalBuy - feeCapitalSale - c0) / c0)
(((c0/p0)*p1 - (c0/p0)*p0*fee - (c0/p0)*p1*fee - c0) / c0)
((1/p0)*p1 - (1/p0)*p0*fee - (1/p0)*p1*fee - 1)
((p1/p0) - fee - fee*p1/p0 - 1)
((p1/p0)*(1 - fee) - fee - 1)
((1 + r)*(1 - fee) - fee - 1)
(1 - fee + r - r*fee - fee - 1)
(r - r*fee - 2*fee)
(r*(1 - fee) - 2*fee)
