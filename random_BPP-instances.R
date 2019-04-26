#Class constrained Bin Packing Problem Instance Generator
#A modified version for the class constrained BPP
#and simplified from the original BPPGen by Petra Schwerin (1997)


#Initialize random seed
set.seed(2019)


#----- Class Constrained Bin Packing Problem Generator -----
#Generate number of instances (m) for defined parameters (n, C, w_min, w_max)
#with randomly selected number of colors per bin (k)
CCBPPG <- function(n, c, wm, wx, q, m) {
        if (m == 0) return("No problems generated")
        dir <- sprintf("Random_Instances_%s_%s_%s_%s", c, n, c*wm, c*wx)
        dir.create(dir)
        for (i in c(1:m)) {
                #how many colors are available for assignment to each element
                q1 <- sample(c((ceiling((100*n/(2*c))*q)):q), 1) 
                x <- cbppgen(n, c, wm, wx, q1)
                filepath <- file.path(dir, sprintf("Random_Instance_%s_%s.bpp", 
                                                                     c, i))
                write.table(x, file = filepath, quote = F, row.names = F, col.names = F)
        }
}


#Instance Generator
cbppgen <- function(n, c, wm, wx, q) {
        v <- list()
        if (wm <= 0 || wm >= wx || wx >= 1) return()
        for (i in c(1:n)) {
                z <- rnorm(n = 1, mean = 0, sd = 1/3)
                wi <- abs(ceiling(wm+(wx-wm)*z+c*z))
                if (wi > c) wi = c
                ki <- sample(c(1:q), 1)
                v[[i]] <- c(wi,ki)
                #maximum number of colors allowed in each bin
                k <- ceiling(q/((wx-wm)*c/1000)) 
        }
        inst <- cbind(c(n,c,k), "")
        inst <- rbind(inst, matrix(unlist(v), ncol = 2, byrow = T))
        return(inst)
}



#------ Definition of parameters -----
n <- c(90, 300, 1200)   #number of items
C <- c(7500, 15000)     #capacity of the bin
w_min <- c(0.05, 0.15)  #minimum weight of the items
w_max <- c(0.75, 0.9)   #maximum weight of the items
p <- rnorm(1)           #random proportion
q <- ceiling(abs(p*min(n))) #maximum different colors


#----- Generation of instances -----
CCBPPG(n = n[1], c = C[1], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[1], c = C[1], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[1], c = C[1], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[1], c = C[1], wm = w_min[2], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[1], c = C[2], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[1], c = C[2], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[1], c = C[2], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[1], c = C[2], wm = w_min[2], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[2], c = C[1], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[2], c = C[1], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[2], c = C[1], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[2], c = C[1], wm = w_min[2], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[2], c = C[2], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[2], c = C[2], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[2], c = C[2], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[2], c = C[2], wm = w_min[2], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[3], c = C[1], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[3], c = C[1], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[3], c = C[1], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[3], c = C[1], wm = w_min[2], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[3], c = C[2], wm = w_min[1], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[3], c = C[2], wm = w_min[1], wx = w_max[2], q = q, m = 25)
CCBPPG(n = n[3], c = C[2], wm = w_min[2], wx = w_max[1], q = q, m = 25)
CCBPPG(n = n[3], c = C[2], wm = w_min[2], wx = w_max[2], q = q, m = 25)
