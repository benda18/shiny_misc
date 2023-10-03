#collatz conjecture
library(dplyr)
library(ggplot2)
library(crayon)
library(glue)
library(fractional)
library(primes)
library(igraph)

#cat('\f')
rm(list=ls())

setwd("~/R/play/shiny_misc/shiny_collatz_conjecture")
# Functions----
# sum_down_chars <- function(x, stop.at = 9){
#   out <- x
#   while(out > stop.at){
#     out <- as.character(out) %>%
#       strsplit(., "") %>%
#       unlist() %>%
#       as.numeric() %>%
#       sum()
#   }
#   return(out)
# }


# is_even <- function(x){
#   if(x != floor(x)) {
#     stop("n must be an integer")
#   }
#   return(floor(x/2)*2 == x)
# }

# divisible_by <- function(x, divby = 2){
#   if(x != floor(x)) {
#     stop("n must be an integer")
#   }
#   return(floor(x/divby)*divby == x)
# }


# collatz_seq_summary <- function(n, odd.opers = c(3,1),
#                         even.opers = c(2)){
#   #n_loops <- 0
#   # while(n_loops < 1000){
#   #   n_loops <- n_loops + 1
#     n.mem <- n
#     if(n != floor(n)) {
#       stop("n must be an integer")
#     }
#     nx <- 0
#     while(n > 1){
#       nx <- nx + 1
#       
#       if(floor(n/even.opers)*even.opers == n){
#         #if even
#         n <- n/even.opers
#       }else{
#         #if odd
#         n <- odd.opers[1]*n+odd.opers[2]
#       }
#       if(nx > 100000000){
#         nx <- NA
#         print("ERROR: too many loops")
#         break
#         #stop("too many loops")
#       }
#     }
#   #   if(n_loops >= 1000){
#   #     n.mem = NA
#   #     nx = NA
#   #   # }
#   # }
#   
#   return(data.frame(n = n.mem, 
#            nx = nx))
# }



"An+B | n/D"

buildaseq <- function(n = 7, A = 3, B = 1, D = 2,
                         max.loops = 1000, 
                      print.formula = F){
  if(print.formula){
    cat(inverse(bold(glue("{A}n+{B} | n/{D}  ||  (n={n})\n\n"))))
  }
  
  out <- n
  loopn <- 0
  while(loopn < max.loops & 
        !1 %in% out){
    if((n/(D)) == floor(n/(D))){
      n <- n / (D)
    }else{
      n <- A*n+B
      
    }
    if(!n %in% out){
      out <- c(out, n)
      loopn <- loopn + 1
    }else{
      #out <- c(out, n)
      #loopn <- max.loops * 10
      loopn <- loopn + 1
    }
  }
  if(loopn > max.loops ){
    cat(bgRed("overflow\n\n"))
  }
  return(out)
}




collatz_seq <- function(x, type.a.divby = 2, 
                        type.b.multiply = 3, 
                        type.b.plus = 1, 
                        max.loops = 1000){
  out <- x
  n.stop <- 0
  while(n.stop < max.loops){
    n.stop <- n.stop + 1
    # determine type
    if( (x/type.a.divby) == floor(x/type.a.divby) ){
      # if type.a
      x <- x / type.a.divby
    }else{
      # if type.b
      x <- (x * type.b.multiply) + type.b.plus
      #x <- abs((x*type.b.multiply) + type.b.plus)
    }
    out <- c(out, x)
    if(x == 1){
      break
    }
    if(n.stop == max.loops){
      print( "max loops reached")
    }
    #print(x)
    #Sys.sleep(0.1)
  }
  
  return(out)
}

collatz_seq(15)

# lapply(1:2500, 
#        collatz_seq) %>%
#   lapply(., length) %>%
#   unlist() %>% table() %>%
#   unname() %>% 
#   unlist() %>% 
#   as.vector() %>%
#   plot()
# 
# A1 <- sample(2:10,1)
# B1 <- sample(-10:10,1)
# D1 <- 2

# replicate(n = 25, 
#           last(collatz_seq2(x = sample(1:900,1), 
#              A = A1, 
#              B = B1, 
#              D = D1,
#              max.loops = 1000))) %>%
#   scales::comma(., accuracy = 1)




# working equations
"3n-3 | n/3"
"4n-2 | n/2"
"3n-1 | n/9" # everything plots the same
"3n+1 | n/9" # everything plots the same
"3n-4 | n/3" # lots resolve to 1, but the rest resolve to very specific patterns
"3n-9 | n/3"
"3n-1 | n/2"
"3n-6 | n/3" # when n = 445; resolves to 1



# type.a.val  <- 3#sample(1:10,1)
# type.b1.val <- 3#sample(1:10,1)
# type.b2.val <- sample(-10:10,1)
# # analysis----
# (x1 <- sample(1:500,1))
# some.outputs <- collatz_seq(x=x1, 
#             type.a.divby = type.a.val, 
#             type.b.multiply = type.b1.val, 
#             type.b.plus = type.b2.val, 
#             max.loops = 2000)
# #plot(some.outputs, type = "b")
# range(some.outputs) %>% scales::comma(., accuracy = 1)
# 
# ggplot(data = data.frame(n = 1:length(some.outputs), 
#                          ot = some.outputs), 
#        aes(x = n, y = ot)) +
#   geom_line(alpha = 0.3)+
#   geom_point(alpha = 0.5, size = 1)+
#   geom_line(aes(x = n, y = cummean(ot)),
#             color = "red")+
#   scale_y_continuous(labels = scales::comma)+
#   scale_x_continuous(labels = scales::comma)+
#   labs(title = glue("FUN = {type.b1.val}n + {type.b2.val} | n/{type.a.val}"), 
#        subtitle = glue("n = {x1} \t [sequence ends at {last(some.outputs)}]"))+
#   theme(plot.background = element_rect(fill = c("cyan", NULL)[c(last(some.outputs) ==1, 
#                                                                 last(some.outputs) !=1)]))
#  

# # old try----
# library(tictoc)
# x <- 1:100
# 
# out <- NULL
# 
# tic();for(i in x){
#   out <- rbind(out, 
#                collatz_seq_summary(i))
# };toc()
# 
# 
# out$rat <- out$nx / out$n
# 
# out <- out %>%
#   .[order(.$n,decreasing=T),] %>%
#   as_tibble()
# 
# ggplot() + 
#   geom_point(data = out,
#              aes(x = n, y = rat, color = factor(nx))) +
#   # geom_smooth(data = out, 
#   #             aes(x = n, y = rat, group = factor(nx)))+
#   scale_y_log10()+
#   scale_x_log10()+
#   theme(legend.position = "none")
# 
# ggplot() + 
#   geom_density(data = out, 
#                aes(x = nx))
# 

# 
# 
# out$frac_rat <- fractional(out$rat, eps = 1e-04) %>%
#   as.character()
# 
# out$prime <- primes::is_prime(out$n)
# 
# out
# 
# out %>% 
#   as_tibble()
# 
# out$sqrt_int <- sqrt(out$n) == floor(sqrt(out$n))
# out$sumdownchars <- lapply(X = out$n, FUN = sum_down_chars) %>%
#   unlist()
# 
# ggplot() + 
#   geom_point(data = out, 
#              aes(x = rat, y = sumdownchars))
# 
# 
# x <- sample(1:1000000, size = 50000)
# sdcx <- unlist(lapply(x, sum_down_chars, stop.at = 100))
# 
# sdcx %>% table() %>% prop.table() %>% plot()
# 
# out %>%
#   group_by(nx) %>%
#   summarise(count = n(), 
#             pct_prime = sum(prime)/count, 
#             pct_sqrt_int = sum(sqrt_int)/count) %>%
#   .[order(.$count,decreasing = T),]
# 
# slice_max(out, order_by = rat, n = 1)
# slice_min(out, order_by = rat, n = 2)
# 
# 
# #x == (x %% y) + y * (x %/% y) # %%  is the same as "the remainder after division"
# 
# 
# x <- data.frame(n = 1:10,
#                 fact = NA, 
#                 lfact = NA,
#                 gamma = NA, 
#                 lgamma = NA, 
#                 choose = NA,
#                 mod = NA,
#                 nx = NA)
# 
# 
# x$fact <- factorial(x$n)
# x$lfact <- lfactorial(x$n)
# x$gamma <- gamma(x$n)
# x$lgamma <- lgamma(x$n)
# x$choose <- choose(x$n, k = 2)
# x$mod <- x$n %% 2
# 
# ggplot(data = x, 
#        aes(x = fact, y = lfact))+
#   geom_line() + 
#   geom_point()+
#   scale_x_log10()
# 

# igraph----

rm_collatz_evens <- function(v = collatz_seq(15)){
  v[(v %% 2)==1]
}

c2df <- function(cs = collatz_seq(15)){
  require(igraph)
  if(length(cs) == 1){
    out <- NA
  }else{
    temp.df <- NULL
    for(i in 2:length(cs)){
      temp.df <- rbind(temp.df, 
                       data.frame(from = cs[i-1], 
                                  to = cs[i]))
    }
    
    #out <- graph_from_data_frame(temp.df, directed = T)
    out <- temp.df
  }
  
  return(out)
}


rm(list.of.dfs)
list.of.dfs <- lapply(X = 1:5, 
       FUN = collatz_seq) %>%
  # lapply(., 
  #        rm_collatz_evens) %>%
  lapply(., 
         c2df)

bind.dfs <- NULL
for(i in 1:length(list.of.dfs)){
  
  if(i == 1){
    bind.dfs <- list.of.dfs[[i]]
  }else{
    bind.dfs <- rbind(bind.dfs, 
                      list.of.dfs[[i]])
  }
}

a.gr <- bind.dfs %>%
  .[complete.cases(.),] %>%
  group_by_all() %>%
  summarise() %>%
  graph_from_data_frame(., directed = T) 

a.gr <- igraph::distances(graph = a.gr, to = V(a.gr)[V(a.gr)==1]) %>%
  as.data.frame()

a.gr$n <- as.integer(rownames(a.gr))
a.gr <- a.gr %>% as_tibble()
colnames(a.gr) <- c("dist_to_1", "n")


ggplot(data = a.gr,#[a.gr$n <= 16000 & a.gr$n >= 15000,], 
       aes(x = n, y = dist_to_1)) + 
  geom_col()+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(limits = c(0,NA))

hist(a.gr$dist_to_1)
