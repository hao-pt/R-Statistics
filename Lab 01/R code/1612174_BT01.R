#Cau 1:
tung_xuc_sac <- function(yeu_cau, count = 0){
  n <- 1:6
  xs <- sample(n, 2, replace = TRUE)
  if(yeu_cau == 1){ #a) Tong so cham khong qua 5
    return (sum(xs) < 5)
  }
  else if(yeu_cau == 2){ #b) So cham cua 2 lan nhu nhau
    return (xs[1] == xs[2])  
  }
  else if(yeu_cau == 3){ #c) So cham lan I la chan, lan II la le
    return (xs[1] %% 2 == 0 && xs[2] %% 2 != 0)
  }
  return (FALSE)
}

tan_suat <- function(n, yeu_cau){
  if(yeu_cau < 4){
    kq <- replicate(n, tung_xuc_sac(yeu_cau))
    return (sum(kq)/n)
  }
}

#Test
#a) Tong so cham khong qua 5
6/36
tan_suat(100000, 1)
#b) So cham cua 2 lan nhu nhau
6/36
tan_suat(300000, 2)
#c) So cham lan I la chan, lan II la le
9/36
tan_suat(50000, 3)

#d) So cham lan thu I > so cham lan II khi lan I < 4
tn_tung_xuc_xac_co_dk <- function(n){
  count <- 0
  count1 <- 0
  for(i in 1:n){
    range <- 1:6
    xs <- sample(range, 2, replace = TRUE)
    if(xs[1] < 4){
      count <- count + 1
      if(xs[1] > xs[2])
        count1 <- count1 + 1
    }
  }
  return (count1/count)
}
1/6
tn_tung_xuc_xac_co_dk(100000)
#e) Tong so cham > 9 khi biet it nhat 1 lan duoc so cham > 3
tung_xuc_sac_co_tong_so_cham_lon_hon_9 <- function(n){
  count <- 0
  count1 <- 0
  for(i in 1:n){
    range <- 1:6
    xs <- sample(range, 2, replace = TRUE)
    if(xs[1] > 3 || xs[2] > 3){
      count <- count + 1
      if(sum(xs) > 9)
        count1 <- count1 + 1
    }
  }
  return (count1/count)
}
2/9
tung_xuc_sac_co_tong_so_cham_lon_hon_9(5000)

#Cau 2:
lay_bi <- function(yeuCau){
  hop <- c(1,2,3)
  hop1 <- c(rep('X', 5), rep('D', 4))
  hop2 <- c(rep('X', 8), rep('D', 4), rep('V', 5))
  hop3 <- c(rep('X', 6), rep('D', 2), rep('V', 4))
  
  #Lay ngau nhien 1 hop
  idx_hop_chon <- sample(hop, 1)
  hop_chon <- c()
  if(idx_hop_chon == 1)
    hop_chon <- hop1
  else if(idx_hop_chon == 2)
    hop_chon <- hop2
  else
    hop_chon <- hop3
  #Lay ngau nhien 1 vien bi tu hop chon
  bi_chon <- sample(hop_chon, 1)
  
  if(yeuCau == 1){ #a) Lay duoc bi xanh
    return (bi_chon == 'X')
  }
  else if(yeuCau == 2){ #b) Lay duoc bi xanh cua hop I
    return (bi_chon == 'X' && idx_hop_chon == 1)
  }
  return (FALSE)
}

tan_suat_bi <- function(n, yeu_cau){
  kq <- replicate(n, lay_bi(yeu_cau))
  return (sum(kq)/n)
}
#Test
#a) Lay duoc bi xanh
467/918
tan_suat_bi(50000, 1)
#b) Lay duoc bi xanh cua hop I
5/27
tan_suat_bi(100000, 2)

#c) N???u bi???t r???ng bi l???y du???c là bi vàng, kh??? nang bi dó thu???c v??? h???p nào là l???n nh???t
lay_bi_vang <- function(n){
  hop <- c(1,2,3)
  hop1 <- c(rep('X', 5), rep('D', 4))
  hop2 <- c(rep('X', 8), rep('D', 4), rep('V', 5))
  hop3 <- c(rep('X', 6), rep('D', 2), rep('V', 4))
  
  count <- c(0,0,0) #Dem so TH co bi vang trong tung hop
  countBiVang <- 0 #Dem so TH co bi vang 
  
  for (i in 1:n){
    #Chon ngau nhien 1 hop
    idx_hop_chon <- sample(hop, 1)
    # Chon ngau nhien 1 vien bi tu hop da chon
    hop_chon <- c()
    if(idx_hop_chon == 1)
      hop_chon <- hop1
    else if(idx_hop_chon == 2)
      hop_chon <- hop2
    else
      hop_chon <- hop3
    bi_chon <- sample(hop_chon, 1)
    
    if(bi_chon == 'V'){
      countBiVang <- countBiVang + 1
      if(idx_hop_chon == 1)
        count[1] <- count[1] + 1
      else if(idx_hop_chon == 2)
        count[2] <- count[2] + 1
      else if(idx_hop_chon == 3)
        count[3] <- count[3] + 1
    }
  }
  
  #Tim bi vang thuoc ve hop co xs cao nhat
  idx_max <- 0
  maxCount <- max(count)
  if(maxCount == count[1])
    idx_max <- 1
  else if(maxCount == count[2])
    idx_max <- 2
  else if(maxCount == count[3])
    idx_max <- 3
  
  return (c(idx_max, maxCount / countBiVang))
}

#c) N???u bi???t r???ng bi l???y du???c là bi vàng, kh??? nang bi dó thu???c v??? h???p nào là?
c(3, 17/32)
lay_bi_vang(50000)

