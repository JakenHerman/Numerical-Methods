from sympy import *

x = Symbol('x')
e = x**7 - 28*x**6 + 322*x**5 - 1960*x**4 + 6769*x**3 - 13132*x**2 + 13068*x - 5040
e_prime = e.diff(x)

pprint(e_prime)