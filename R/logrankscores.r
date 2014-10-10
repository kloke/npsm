logrankscores = new("scores",phi=function (u){ -1 - log(1-u)},Dphi=function (u) { 1/(1-u) })
