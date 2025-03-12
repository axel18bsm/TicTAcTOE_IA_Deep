import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models
import random

# Fonction pour générer des données d'entraînement (simplifiée)
def generate_training_data(num_games=100000):
    X = []  # Entrées : états du plateau
    y = []  # Sorties : coups choisis
    for _ in range(num_games):
        board = np.zeros((3, 3), dtype=int)  # 0: vide, 1: X, -1: O
        current_player = 1  # Commence avec X
        while True:
            # Liste des cases libres
            available_moves = [(i, j) for i in range(3) for j in range(3) if board[i, j] == 0]
            if not available_moves:
                break  # Match nul
            # Choisir un coup au hasard (simulé, pour l'exemple)
            move = random.choice(available_moves)
            i, j = move
            board[i, j] = current_player
            # Ajouter l'état du plateau et le coup comme donnée d'entraînement
            X.append(board.flatten())
            y.append(i * 3 + j)  # Convertir (i, j) en un index de 0 à 8
            # Vérifier si le joueur a gagné
            if (abs(board[i].sum()) == 3 or abs(board[:, j].sum()) == 3 or
                abs(np.diag(board).sum()) == 3 or abs(np.diag(np.fliplr(board)).sum()) == 3):
                break
            current_player *= -1  # Changer de joueur
    return np.array(X), np.array(y)

# Générer des données d'entraînement
X_train, y_train = generate_training_data()

# Créer le modèle de réseau de neurones
model = models.Sequential([
    layers.Input(shape=(9,)),  # 9 cases pour un plateau 3x3
    layers.Dense(128, activation='relu'),
    layers.Dense(64, activation='relu'),
    layers.Dense(9, activation='softmax')  # 9 sorties pour les 9 cases possibles
])

# Compiler le modèle
model.compile(optimizer='adam', loss='sparse_categorical_crossentropy', metrics=['accuracy'])

# Entraîner le modèle
model.fit(X_train, y_train, epochs=10, batch_size=32, validation_split=0.2)

# Sauvegarder le modèle
model.save('tictactoe_model.h5')
print("Modèle sauvegardé sous 'tictactoe_model.h5'")