import inspect
import numpy

# Attempt to get the signature for `f`.
# Return `False` when no signature is available.
def sign(f):
   try:
     return inspect.signature(f)
   except:
     return False

mod = numpy.random
 
print("Numpy functions where inspect.signature fails")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if inspect.isfunction(f):
        if not sign(f):
            print(x)

print("")
print("Numpy functions where inspect.signature works")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if inspect.isfunction(f):
        if sign(f):
            print(x)

print("")
print("Non functions")
print("---")
print("---")

print("Ufuncs")
print("---")

for x in dir(mod):
    f = getattr(mod,x)
    if not inspect.isfunction(f):
        if type(f)==type(numpy.absolute):    # a <ufunc>
          print(x)

print("")
print("Builtins")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if (not inspect.isfunction(f)) and inspect.isbuiltin(f):
        print(x)

print("")
print("Non functions besides ufuncs and builtins")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if not inspect.isfunction(f):
        if not type(f)==type(numpy.absolute): # a <ufunc>
            if not inspect.isbuiltin(f):
                print(x)
