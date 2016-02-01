check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages(c("gdata","utils","lpSolve"))

path = '/Users/luohaosheng/Desktop/Question/Tios/Test/company-production.xlsx'
Usage = read.xls(path,sheet=1)
Usage[,1]=NULL
Usage = t(as.matrix(Usage))
Limit = read.xls(path,sheet=2)
Limit[,1]=NULL
Limit = t(as.matrix(Limit))
Return = read.xls(path,sheet=3)
Return[,1]=NULL
Return = t(as.matrix(Return))

n_col = ncol(Usage)
n_row = nrow(Usage)

f.obj = Return
f.con = rbind(Usage,diag(n_col))
f.dir = c(rep("<=",n_row),rep(">=",n_col))
f.rhs = c(Limit,rep(0,n_col))

Sol = lp ("max", f.obj, f.con, f.dir, f.rhs)
Amount = as.matrix(Sol$solution)

#Q2 revise

f.obj = Return
f.con = diag(n_col)
f.dir = rep(">=",n_col)
f.rhs = rep(0,n_col)

Sol = lp ("max", f.obj, f.con, f.dir, f.rhs)
Amount = as.matrix(Sol$solution)
which(Amount==0)


#非零解需要




