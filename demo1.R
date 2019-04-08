#Hàm th???c hi???n thí nghi???m b???c bi 
#và ki???m tra bi???n c??? "b???c du???c c??? 3 bi D???" có x???y ra hay khôn
bocbi <- function(){
  hopbi <- c(rep('R', 10), rep('B',20))
  babi <- sample(hopbi, 3, replace = TRUE)
  return (sum(babi == 'R') == 3)
}

#Hàm th???c hi???n N l???n thí nghi???m b???c bi
#và tính t???n su???t c???a bi???n c??? c???n tính
tansuat <- function(n){
  kq <- replicate(n, bocbi())
  return (sum(kq)/n)
}

#BTVD
#a XS de lay 3 chai cam
chonNuoc <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (sum(baChai == 'Cam') == 3)
}
tanSuat <- function(n){
  kq <- replicate(n, chonNuoc())
  return (sum(kq)/n)
}

#b XS de khong lay duoc chai cam nao
choose(6,3)/choose(10,3)
chonNuocChanh <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (sum(baChai == 'Chanh') == 3)
}
tanSuat2 <- function(n){
  kq <- replicate(n, chonNuocChanh())
  return (sum(kq)/n)
}

#c XS de lay dung 1 chai cam
(4 * choose(6,2)) / choose(10,3)
chonDung1Cam <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (sum(baChai == 'Cam') == 1)
}
tanSuat3 <- function(n){
  kq <- replicate(n, chonDung1Cam())
  return (sum(kq)/n)
}

#d XS de lay it nhat 1 chai cam
1 - choose(6,3)/choose(10,3)
tanSuat4 <- function(n){
  kq <- replicate(n, chonNuocChanh())
  return (1 - sum(kq)/n)
}

#e Toi da 2 chai cam
1 - choose(4,3)/choose(10,3)
tanSuat5 <- function(n){
  kq <- replicate(n, chonNuoc())
  return (1 - sum(kq)/n)
}
tanSuat5(50000)

#f Co ca 2 loai Cam & Chanh
1 - (choose(6,3) + choose(4,3))/choose(10,3)
Co2Loai <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (sum(baChai == 'Cam') == 0 || sum(baChai == 'Chanh') == 0)
}
tanSuat6 <- function(n){
  kq <- replicate(n, Co2Loai())
  return (1 - sum(kq)/n)
}
tanSuat6(50000)

#g Lay 2 loai nay va 1 loai kia
(choose(4,2)*6 + choose(6,2)*4) / choose(10,3)
HaiLoaiNayMotLoaiKia <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (sum(baChai == 'Cam') == 1 || sum(baChai == 'Chanh') == 1)
}
tanSuat7 <- function(n){
  kq <- replicate(n, HaiLoaiNayMotLoaiKia())
  return (sum(kq)/n)
}
tanSuat7(50000)

#h Lay 2 chai nuoc chanh lien tiep
th1 <- 6/10 * 5/9 * 4/8
th2 <- 4/10 * 6/9 * 5/8
(th1+th2)
Lay2ChaiChanhLienTiep <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (baChai[1] == baChai[2] && baChai[1] == 'Chanh') || (baChai[2] == baChai[3] && baChai[2] == 'Chanh')
}
tanSuat8 <- function(n){
  kq <- replicate(n, Lay2ChaiChanhLienTiep())
  return (sum(kq)/n)
}
tanSuat8(50000)

#i Lay chai dau tien la chai nuoc cam
4/10
LayChaiDauTienLaNuocCam <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (baChai[1] == 'Cam')
}
tanSuat9 <- function(n){
  kq <- replicate(n, LayChaiDauTienLaNuocCam())
  return (sum(kq)/n)
}
tanSuat9(50000)

#j Lay cac chai xen ke nhau biet chai dau tien la nuoc cam
4/10 *6/9 *3/8
LayChaiXenKeBietChaiDauLaNuocCam <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (baChai[1] == 'Cam' && baChai[2] == 'Chanh' && baChai[3] == 'Cam')
}
tanSuat10 <- function(n){
  kq <- replicate(n, LayChaiXenKeBietChaiDauLaNuocCam())
  return (sum(kq)/n)
}
tanSuat10(50000)

#k Lay duoc nhieu chai cam hon biet chai dau tien la cam
1 - 4/10 * 6/9 * 5/8
lay_nhieu_chai_cam_biet_chai_dau_tien_la_cam <- function(){
  quaynuoc <- c(rep('Cam', 4), rep('Chanh', 6))
  baChai <- sample(quaynuoc, 3)
  return (baChai[1] == 'Cam' && sum(baChai == "Chanh") == 2)
}
tanSuat11 <- function(n){
  kq <- replicate(n, lay_nhieu_chai_cam_biet_chai_dau_tien_la_cam())
  return (1 - sum(kq)/n)
}
tanSuat11(50000)

