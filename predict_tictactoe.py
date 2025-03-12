import numpy as np
import tensorflow as tf
import sys
import json

# Charger le modèle
model = tf.keras.models.load_model('tictactoe_model.h5')

# Recevoir les données du plateau depuis Free Pascal (passées en JSON via stdin)
input_data = sys.stdin.read()
data = json.loads(input_data)

# Convertir le plateau en un tableau numpy (0: vide, 1: X, -1: O)
board = np.array(data['board']).reshape(3, 3)
board = board.astype(np.float32)

# Prédire les probabilités pour chaque case
flat_board = board.flatten()
prediction = model.predict(np.array([flat_board]), verbose=0)
probabilities = prediction[0]

# Masquer les cases déjà occupées
for i in range(9):
    if flat_board[i] != 0:
        probabilities[i] = -1  # Ignorer les cases occupées

# Choisir le coup avec la plus haute probabilité
best_move = np.argmax(probabilities)

# Retourner le résultat à Free Pascal (via stdout)
result = {'move': int(best_move)}
print(json.dumps(result))