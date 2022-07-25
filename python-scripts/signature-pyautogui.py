import inspect
import pyautogui

###
### This script finds the functions in `numpy.pyautogui` that have signatures.
###


# Attempt to get the signature for `f`.
# Return `False` when no signature is available.
def sign(f):
   try:
     return inspect.signature(f)
   except:
     return False

mod = pyautogui
 
print("Functions where inspect.signature fails")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if inspect.isfunction(f):
       if not sign(f):
          print(x)

print("Functions where inspect.signature works")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if inspect.isfunction(f):
        if sign(f):
            print(x)

print("")
print("Builtins (where inspect.signature fails)")
print("---")
for x in dir(mod):
    f = getattr(mod,x)
    if (not inspect.isfunction(f)) and inspect.isbuiltin(f):
       if not sign(f):
          print(x)

