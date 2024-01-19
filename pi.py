import numpy as np

# Definindo a matriz A e o vetor b
A = np.array([[1, 2, 4, 1],
              [2, 0, 1, 0],
              [4, 2, 3, 1],
              [3, 1, 2, 1]], dtype=float)
b = np.array([16, 13, 27, 7], dtype=float)

# Função para fatoração LU
def lu_decomposition(A):
    n = len(A)
    L = np.eye(n)  # Inicializando a matriz L como uma matriz identidade
    U = A.copy()   # Inicializando a matriz U como uma cópia de A
    
    for i in range(n):
        pivot = U[i, i]  # Pivô
        
        # Realizando a eliminação gaussiana para calcular L e U
        for j in range(i + 1, n):
            factor = U[j, i] / pivot
            L[j, i] = factor
            U[j, :] -= factor * U[i, :]
    
    return L, U

# Executando a fatoração LU
L, U = lu_decomposition(A)

print("Matriz L:")
print(L)
print("\nMatriz U:")
print(U)

# Resolvendo o sistema Ax = b usando a fatoração LU
# Ly = b (resolvendo para y)
y = np.linalg.solve(L, b)

# Ux = y (resolvendo para x)
x = np.linalg.solve(U, y)

print("\nSolução para x:")
print(x)
