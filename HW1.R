# vectors for G
x_g = c(seq(-2,0,length.out = 500), seq(-2,0,length.out = 500), rep(-2,500), rep(0,250), seq(-1,0,length.out = 250))
y_g = c(rep(1,500), rep(-1,500), seq(-1,1,length.out = 500), seq(-1,0,length.out = 250), rep(0,250))

# vectors for S
x_s = c(seq(1,3,length.out = 500), rep(1,250), seq(1,3,length.out = 500), rep(3,250), seq(1,3,length.out = 500))
y_s = c(rep(1,500), seq(0,1,length.out = 250), rep(0,500), seq(-1,0,length.out = 250), rep(-1,500))

#combining the G and S vectors
x_gs = c(x_g, x_s)
y_gs = c(y_g, y_s)

#creating the GS matrix and plotting
z = rbind(x_gs,y_gs)
plot(y_gs~x_gs, xlim = c(-4,4), ylim = c(-4,4))

library(animation)
dev.control('enable')
myani = ani.record(reset = TRUE, replay.cur = FALSE)

x11()

#shear - over x-axis (vertically fixed)
t = diag(2)
for (k in seq(-4,4, length.out = 100)) {
  t[1,2] = k
  newmat = apply(z, 2, function(x) t %*% x)
  plot(newmat[2, ] ~ newmat[1, ], xlim = c(-4, 4), ylim = c(-4, 4))
  ani.record()
}
  
#scaling
t = diag(2)
for (k in seq(0,4, length.out = 100)) {
  t[1,1] = k 
  t[2,2] = k
    
  newmat = apply(z, 2, function(x) t %*% x)
  plot(newmat[2, ] ~ newmat[1, ], xlim = c(-4, 4), ylim = c(-4, 4))
  ani.record()
  }

#rotation
x11()

for (k in seq(0, 2*pi, length.out = 100)) {
  
  t = matrix(c(cos(k), sin(k), -sin(k), cos(k)), nrow = 2)
  
  newmat = apply(z, 2, function(x) t %*% x)
  plot(newmat[2, ] ~ newmat[1, ], xlim = c(-4, 4), ylim = c(-4, 4))
  ani.record()
}


#projection attempt - from class: projection is squishing
#goal: start in 2D, and squish to line
#I tried what feels like everything...
#I'm not sure why I keep getting this error: non-conformable arguments
#The matrix is still a 2x2, I just zeroed out the final row

for (k in seq(0, 4, length.out = 100)) {
  
  t = matrix(c(cos(k), sin(k), 0, 0), nrow = 2, ncol = 2)
 
  newmat = apply(z, 1, function(x) t %*% x)
  plot(newmat[2, ] ~ newmat[1, ], xlim = c(-4, 4), ylim = c(-4, 4))
  ani.record()
}