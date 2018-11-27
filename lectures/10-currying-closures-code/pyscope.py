# demonstration of python's lexical scoping. 

def makeinc(init):
  total = init
  def incfunc():
    nonlocal total              # required to label as nonlocal 
    total = total+1             # to get lexical scoping
    return total
  return incfunc

inc1 = makeinc(0)               # inc1{ret=0}
inc1()                          # inc1{ret=1}
inc1()                          # inc1{ret=2}
ret = inc1()                    # inc1{ret=3}
print(ret)

inc2 = makeinc(10)              # inc2{ret=10}
ret = inc2()                    # inc2{ret=11}
ret = inc2()                    # inc2{ret=12}
print(ret)

ret = inc1()                    # inc1{ret=4}
print(ret)

