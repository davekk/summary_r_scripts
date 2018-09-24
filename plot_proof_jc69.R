n_A = function(alpha,t) { (n_A_0 - N/4) * exp (-4 * alpha * t) +N/4}
N=1000 ; n_A_0 =300

n_A(alpha=50,t=10.3)

curve(n_A(50,x), from = 0, to = 10, col="blue") # plot of the function from 0-10

curve(n_A(50,x), from = 0, to = 1, n=1000, col="blue") # plot of the function from 0-1, with 1000 points of reference
curve(n_A(50,x), from = 0, to = 1, n=1000, col="blue", ylim=c(200,400)) # plot of the function from 0-1, with 1000 points of reference, between 200 and 400
curve(n_A(0.5,x),  n=1000, col="red", add=T) # add a second plot colored red with a thusand points and with much less alpha of 0.5

abline(h=0.25*N, col="green") # add abline at 0.25*N colored green
# what about C nucleotide
# initially are 200
n_C = function(alpha,t) { (n_C_0 - N/4) * exp (-4 * alpha * t) +N/4}
n_C_0 =200
# plotting the fate of these with instatenoius =0.5
curve(n_C(0.5,x),  n=1000, col="brown", add=T) # add plot of C nucleotide to A nucleotide

# ploting d(estimated distance) as a function of p (observed/edit distance)
JC69_distance= function(p){-3/4 * log(1 - 4/3 *p)}

curve(JC69_distance(x), xlim = c(0,1), xlab ='p distance', ylab='inferred distance under JC69')
abline(a=0, b=1, lty = 2) # 

curve(JC69_distance(x), xlim = c(0,1), ylim = c(0,1), xlab = 'p distance', ylab='inferred distance under JC69')
abline(a=0, b=1, lty = 2)

curve(JC69_distance(x), xlim = c(0,1), n=100, xlab = 'p distance', ylab='inferred distance under JC69')
abline(a=0, b=1, lty = 2)

abline(v=.75, col="grey")

# List the library paths
# The issue is likely to be in the first directory
paths = .libPaths()

## Try and detect bad files
list.files(paths, 
           pattern = "^00LOCK*|*\\.rds$|*\\.RDS$",
           full.names = TRUE)

## List files of size 0
l = list.files(paths, full.names = TRUE)
l[sapply(l, file.size) == 0]
