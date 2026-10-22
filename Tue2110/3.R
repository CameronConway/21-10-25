install.packages('DSAIDE')
library('DSAIDE')
dsaidemenu()

#R0 [b*S(0)]/g
(0.002*1000)/1 #2, should double
#declines as recovery rate is greater than 
#infection rate, so recover before infecting
#no infected hosts left, as recovered
#Not every host was infected (S still >0)
#epidemic final size is 798

#still initial increase and peak
#higher b means shorter and higher I
#final r changes

#b 0.001: I is 1 around time 1, R is 43
#b 0.01: I is 668 around 1, R is 1000

#g 0.5: I 400 around 5, R 981
#g 1.5: I is 35 around 10, R is 458


