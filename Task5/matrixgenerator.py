import math
sign = lambda x: math.copysign(1, x)

def signToType (sign):
    if sign == 0:
        return "Zero"
    if sign == 1:
        return "Positive"
    if sign == -1:
        return "Negative"



res = []
for i in range(-1, 2):
    ls = []

    for j in range(-1, 2):
        prod =(i * j) 
        theSign = 0
        if (prod != 0):
            theSign = sign(prod)
        
        ls.append([signToType(theSign)])
    
    res.append(ls)
print(res)


#[[[Positive]; [Zero]; [Negetive]]; [[Zero]; [Zero]; [Zero]]; [[Negetive]; [Zero]; [Positive]]]