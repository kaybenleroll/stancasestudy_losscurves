# this is to read data

------------------Code Begin----------------------------------------------
a=read.csv(choose.files(),header=TRUE)

grp.code=unique(a$GRCODE)
#
# function to get Schedule P triangle data given ins group
ins.line.data=function(g.code){
  b=subset(a,a$GRCODE==g.code)
  name=b$GRNAME
  grpcode=b$GRCODE
  ay=b$AccidentYear
  dev=b$DevelopmentLag

  cum_incloss=b[,6]
  cum_pdloss=b[,7]
  bulk_loss=b[,8]
  dir_premium=b[,9]
  ced_premium=b[,10]
  net_premium=b[,11]
  single=b[,12]
  posted_reserve97=b[,13]

  # get incremental paid losses - assume data is sorted by ay and dev
  inc_pdloss=numeric(0)
  for (i in unique(ay)){
    s=(ay==i)
    pl=c(0,cum_pdloss[s])
    ndev=length(pl)-1
    il=rep(0,ndev)
    for (j in 1:ndev){            
      il[j]=pl[j+1]-pl[j]
      }
    inc_pdloss=c(inc_pdloss,il)
    }
  data.out=data.frame(name,grpcode,ay,dev,net_premium,dir_premium,ced_premium,
     cum_pdloss,cum_incloss,bulk_loss,inc_pdloss,single,posted_reserve97)
  return(data.out)
  }
#


# Example

  comauto=ins.line.data(grp.code[1])

# upper triangle
com.insample=subset(comauto,ay+dev<=1998)
# lower triangle
com.outsample=subset(comauto,ay+dev>1998)
