`PosBinFilter` <-
function(x,y,steps)
{
        rg <- range(x)
        delta = (rg[2]-rg[1])/steps
        res = array(dim=steps)
        for (i in 1:steps) {
                res[i] = median(y[(x >= (rg[1]+(i-1)*delta)) & (x < (rg[1]+i*delta))])
        }
        res[is.na(res)] <- median(res[!is.na(res)])
        res
}

