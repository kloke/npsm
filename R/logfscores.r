logfscores = new("scores",phi=function (u, param)
{
    m1 = param[1]
    m2 = param[2]
    f1 = qf(u,2*m1,2*m2)
    top = m1*m2*(f1-1)
    bot = m2+m1*f1
    top/bot
}
,Dphi=function (u, param)
{
    m1 = param[1]
    m2 = param[2]
    f1 = qf(u,2*m1,2*m2)
    top = m1*m2*(m1+m2)
    bot = ((m2+m1*f1)^2)*df(f1,2*m1,2*m2)
    top/bot
},param=c(1,1))


