#Modification of Falkenauer U instances 

#arbitrary seed
set.seed(2019)

#Retrieve the instances
fnames <- list.files(path = "Falkenauer/Falkenauer U", pattern = ".txt", 
                          full.names = T)

finst <- lapply(fnames, read.csv, header = F)

#Define the maximum number of colors (q), 
#and the number of colors allowed in each bin (k)
q <- 10
k <- sample(x = c(2:q), size = length(finst), replace = T)
k_inst <- list()
for (i in c(1:length(finst))) {
        n <- finst[[i]][1,1]
        ki <- sample(x = c(1:k[i]), size = n, replace = T)
        k_inst[[i]] <- ki
}

#Combine the new information with the corresponding instance
inst <- lapply(finst, function (x) x[-c(1,2),])
for (i in c(1:length(inst))){
        inst[[i]] <- cbind(inst[[i]], k_inst[[i]])
        inst[[i]] <- inst[[i]][sample(1:nrow(inst[[i]])), ] #randomize order of instances
}

for (i in c(1:length(finst))) {
        finst[[i]] <- cbind(c(finst[[i]][c(1,2),"V1"], k[i]),"")
        finst[[i]] <- rbind(finst[[i]],inst[[i]])
}

dir.create("Falkenauer_U_mod")
for (i in c(1:length(finst))) {
        filepath <- file.path("Falkenauer_U_mod", 
                              sprintf("Falkenauer_U_mod_%s_%s.bpp", 
                                      finst[[i]][1,1],i))
        write.table(x=finst[[i]], file = filepath, quote = F, row.names = F, 
                    col.names = F)
}
