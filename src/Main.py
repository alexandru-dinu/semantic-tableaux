from ModalTableaux import *

phi = eval(input())

print("\nSemantic Tableaux\n")

t = tableaux(phi)

print(t)
print("Is SAT:", isSat(t))
