unit TicTacToeGame;



{$mode ObjFPC}{$H+} {$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, raylib, raygui;

type
  TPlayer = (plNone, plX, plO);  // Vide, X, O
  TGameMode = (gmHumanVsHuman, gmHumanVsAI, gmHumanVsDeepAI); // Modes de jeu

  TPlateau = record
    Id: Integer;
    lenom: PChar;
    NbreLigne: Integer;
    NbreColonne: Integer;
    LargCase: Integer;
    HautCase: Integer;
    Couleur1Case: TColor;
    Couleur2Case: TColor;
    ClearBackground: TColor;
    boardWidth, boardHeight: Integer;
    offsetX, offsetY: Integer;
    nbrecase: Integer;
  end;

  TCase = record
    id: Integer;
    x, y, width, height: Integer;
    color: TColor;
    player: TPlayer;
    milieuX, milieuY: Integer;
  end;

const
  SCREEN_WIDTH = 1024;
  SCREEN_HEIGHT = 728;

var
  Leboard: TPlateau;
  board: array of TCase;
  statustext: PChar = 'Chargement initial';
  modeButtons, quitButton, restartButton: TRectangle;
  boardTexture: TTexture2D;
  toggleSliderActiveHaut: Integer = 0;  // 0 = Humain vs Humain, 1 = Humain vs IA Simple, 2 = Humain vs IA Deep
  gameMode: TGameMode = gmHumanVsHuman;
  currentPlayer: TPlayer = plX;  // Variable pour suivre le joueur actuel
  clickedId: Integer = -1;  // ID de la case cliquée
  clickPosX: Single = 0;    // Coordonnée X du clic
  clickPosY: Single = 0;    // Coordonnée Y du clic
  lastPlayedIndex: Integer = -1;  // Index de la dernière case jouée
  untext: string;
  gameOver: Boolean = False; // État pour indiquer la fin du jeu
  player1Symbol: TPlayer = plNone; // Symbole de Joueur 1
  player2Symbol: TPlayer = plNone; // Symbole de Joueur 2
  player1Score: Double = 0; // Score global pour Joueur 1
  player2Score: Double = 0; // Score global pour Joueur 2
  scoreToWinRound: Integer = 5; // Score cible pour gagner globalement
  player1Name: string = 'Joueur Humain 1'; // Nom du Joueur 1
  player2Name: string = 'Joueur Humain 2'; // Nom du Joueur 2
  symbolsInitialized: Boolean;          // Indique si les symboles ont été attribués
  nouvelleMancheButton: TRectangle;     // Bouton pour démarrer une nouvelle manche


procedure CheckDraw(var isDraw: Boolean);
procedure CheckVictory(var hasWon: Boolean; var winner: TPlayer);
procedure InitBoard();
procedure LoadBoardFromCSV(const FileName: string);
procedure DrawBoard();
procedure gui();
procedure UnloadResources();
procedure RestartGame();
procedure InitializeGame();
procedure ResetManche();


implementation

procedure InitializePlayerNames();
begin
  case gameMode of
    gmHumanVsHuman:
      begin
        player1Name := 'Joueur Humain 1';
        player2Name := 'Joueur Humain 2';
      end;
    gmHumanVsAI:
      begin
        player1Name := 'Joueur Humain';
        player2Name := 'Joueur AI';
      end;
    gmHumanVsDeepAI:
      begin
        player1Name := 'Joueur Humain';
        player2Name := 'Joueur Deep';
      end;
  end;
end;
procedure InitializeGame();
var
  i: Integer;
  tickCount: DWORD;
  remainder: Integer;
begin
  // Initialiser les symboles seulement si ce n'est pas encore fait
  if not symbolsInitialized then
  begin
    tickCount := GetTickCount64;
    remainder := tickCount mod 2;
    if remainder = 0 then
    begin
      player1Symbol := plO;
      player2Symbol := plX;
    end
    else
    begin
      player1Symbol := plX;
      player2Symbol := plO;
    end;
    symbolsInitialized := True; // Marquer comme initialisé
  end;

  // Choisir le joueur initial aléatoirement pour chaque partie
  tickCount := GetTickCount64;
  remainder := tickCount mod 2;
  if remainder = 0 then
    currentPlayer := player2Symbol
  else
    currentPlayer := player1Symbol;

  // Réinitialiser le plateau
  for i := 0 to Leboard.nbrecase - 1 do
    board[i].player := plNone;

  // Réinitialiser d'autres variables si nécessaire
  clickedId := -1;
  gameOver := False;
  statustext := 'En attente';
end;

procedure RestartGame();
begin
  InitializeGame();  // Réinitialise le plateau pour une nouvelle partie
  gameOver := False; // Relance une partie
end;

procedure LoadBoardFromCSV(const filename: string);
var
  i, j: Integer;
  fileText: TStringList;
  line: string;
  values: TStringArray;
begin
  fileText := TStringList.Create;
  try
    fileText.LoadFromFile(filename);
    Leboard.nbrecase := fileText.Count - 1; // Supposant une ligne d'en-tête
    SetLength(board, Leboard.nbrecase);

    for i := 1 to fileText.Count - 1 do
    begin
      line := fileText[i];
      values := line.Split(',');
      // Débogage : Afficher le nombre de colonnes et leur contenu
      WriteLn('Ligne ', i, ': ', Length(values), ' colonnes');
      for j := 0 to Length(values) - 1 do
        WriteLn('  Colonne ', j, ': ', values[j]);

      if Length(values) >= 24 then // Vérifier toutes les 24 colonnes
      begin
        board[i - 1].id := StrToIntDef(values[0], 0);
        board[i - 1].x := StrToIntDef(values[1], 0);
        board[i - 1].y := StrToIntDef(values[2], 0);
        board[i - 1].width := StrToIntDef(values[3], 0);
        board[i - 1].height := StrToIntDef(values[4], 0);
        board[i - 1].milieuX := StrToIntDef(values[15], 0); // Colonne 16 (MilieuX)
        board[i - 1].milieuY := StrToIntDef(values[16], 0); // Colonne 17 (MilieuY)

        // Débogage : Afficher les valeurs assignées
        WriteLn('Case ', i - 1, ': ID=', board[i - 1].id,
                ', X=', board[i - 1].x, ', Y=', board[i - 1].y,
                ', MilieuX=', board[i - 1].milieuX, ', MilieuY=', board[i - 1].milieuY);
      end
      else
        WriteLn('Erreur : Ligne ', i, ' n''a que ', Length(values), ' colonnes au lieu de 24.');
    end;
  finally
    fileText.Free;
  end;
end;

procedure InitBoard();
begin
  LoadBoardFromCSV('echiquier.csv');  // Charger les données du CSV
  boardTexture := LoadTexture('screenshot.png');
  if not IsTextureValid(boardTexture) then
    WriteLn('Erreur : Impossible de charger screenshot.png');
  statustext := 'Plateau chargé depuis image';
end;

procedure DrawBoard();
var
  i: Integer;
  playerInfoText: string;
  scoreText: string;
  player1Symbole: string;
  player2Symbole: string;
  currentPlayerText: string;
  statustext: PChar;
begin
  ClearBackground(Leboard.ClearBackground);
  DrawTexture(boardTexture, Leboard.offsetX, Leboard.offsetY, WHITE);

  // Dessiner les symboles sur le plateau
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plX then
      DrawText('X', board[i].milieuX + Leboard.offsetX - 25, board[i].milieuY + Leboard.offsetY - 27, 40, RED)
    else if board[i].player = plO then
      DrawText('O', board[i].milieuX + Leboard.offsetX - 25, board[i].milieuY + Leboard.offsetY - 27, 40, BLUE);
  end;

  // Déterminer les symboles des joueurs
  if player1Symbol = plX then
    player1Symbole := 'X'
  else
    player1Symbole := 'O';
  if player2Symbol = plX then
    player2Symbole := 'X'
  else
    player2Symbole := 'O';

  // Texte du joueur actuel (si la manche n’est pas terminée)
  if not ((player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound)) then
  begin
    if currentPlayer = player1Symbol then
      currentPlayerText := 'À ' + player1Name + ' de jouer'
    else
      currentPlayerText := 'À ' + player2Name + ' de jouer';
  end
  else
    currentPlayerText := '';

  // Afficher le bouton "Rejouer" pour une nouvelle partie (seulement si la manche n’est pas finie)
  if gameOver and not ((player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound)) then
  begin
    restartButton := RectangleCreate((SCREEN_WIDTH - 120) div 2, 200, 120, 40);
    if GuiButton(restartButton, 'Rejouer') > 0 then
    begin
      RestartGame();
    end;
  end;

  // Afficher le gagnant et le bouton "Nouvelle Manche" si la manche est terminée
  if (player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound) then
  begin
    if player1Score >= scoreToWinRound then
      statustext := PChar(player1Name + ' a gagné la manche !')
    else
      statustext := PChar(player2Name + ' a gagné la manche !');
    DrawText(statustext, (SCREEN_WIDTH - MeasureText(statustext, 30)) div 2, 250, 30, WHITE);

    // Bouton "Nouvelle Manche"
    nouvelleMancheButton := RectangleCreate((SCREEN_WIDTH - 180) div 2, 300, 180, 40);
    if GuiButton(nouvelleMancheButton, 'Nouvelle Manche') > 0 then
    begin
      ResetManche();
    end;
  end;

  // Afficher le joueur actuel
  if currentPlayerText <> '' then
    DrawText(PChar(currentPlayerText), 10, 400, 20, WHITE);

  // Afficher les symboles des joueurs
  playerInfoText := player1Name + ' : ' + player1Symbole + ', ' +
                    player2Name + ' : ' + player2Symbole;
  DrawText(PChar(playerInfoText), 10, 450, 20, WHITE);

  // Afficher les scores globaux
  scoreText := 'Score - ' + player1Name + ' : ' + FloatToStr(player1Score) + ', ' +
               player2Name + ' : ' + FloatToStr(player2Score);
  DrawText(PChar(scoreText), 10, 500, 20, WHITE);
end;


procedure gui();
begin
  GuiSetStyle(SLIDER, SLIDER_PADDING, 2);
  modeButtons := RectangleCreate(750, 20, 240, 25);
  GuiToggleSlider(modeButtons, 'Humain vs Humain;IA Simple;IA Deep', @toggleSliderActiveHaut);
  case toggleSliderActiveHaut of
    0: gameMode := gmHumanVsHuman;
    1: gameMode := gmHumanVsAI;
    2: gameMode := gmHumanVsDeepAI;
  end;
  GuiStatusBar(RectangleCreate(0, SCREEN_HEIGHT - 20, SCREEN_WIDTH, 20), statustext);
end;

procedure UnloadResources();
begin
  if IsTextureValid(boardTexture) then
    UnloadTexture(boardTexture);
  SetLength(board, 0);
end;
procedure CheckVictory(var hasWon: Boolean; var winner: TPlayer);
var
  i: Integer;
begin
  hasWon := False;
  winner := plNone;

  // Vérifier les lignes horizontales
  for i := 0 to 2 do
  begin
    if (board[i * 3].player <> plNone) and
       (board[i * 3].player = board[i * 3 + 1].player) and
       (board[i * 3].player = board[i * 3 + 2].player) then
    begin
      winner := board[i * 3].player;
      hasWon := True;
      Break;
    end;
  end;

  // Vérifier les colonnes verticales
  if not hasWon then
  begin
    for i := 0 to 2 do
    begin
      if (board[i].player <> plNone) and
         (board[i].player = board[i + 3].player) and
         (board[i].player = board[i + 6].player) then
      begin
        winner := board[i].player;
        hasWon := True;
        Break;
      end;
    end;
  end;

  // Vérifier la diagonale descendante
  if not hasWon then
  begin
    if (board[0].player <> plNone) and (board[0].player = board[4].player) and (board[0].player = board[8].player) then
    begin
      winner := board[0].player;
      hasWon := True;
    end;
  end;

  // Vérifier la diagonale montante
  if not hasWon then
  begin
    if (board[2].player <> plNone) and (board[2].player = board[4].player) and (board[2].player = board[6].player) then
    begin
      winner := board[2].player;
      hasWon := True;
    end;
  end;

  // Mettre à jour les scores si une victoire est détectée
  if hasWon then
  begin
    if winner = plX then
    begin
      statustext := 'Gagnant : X';
      if player1Symbol = plX then
        player1Score := player1Score + 1
      else
        player2Score := player2Score + 1;
    end
    else
    begin
      statustext := 'Gagnant : O';
      if player1Symbol = plO then
        player1Score := player1Score + 1
      else
        player2Score := player2Score + 1;
    end;
  end;
end;

procedure CheckDraw(var isDraw: Boolean);
var
  i: Integer;
begin
  isDraw := True;
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plNone then
    begin
      isDraw := False;
      Exit;
    end;
  end;
  if isDraw then
  begin
    statustext := 'Match nul';
    player1Score := player1Score + 0.5;
    player2Score := player2Score + 0.5;
  end;
end;
procedure ResetManche();
begin
  player1Score := 0;         // Réinitialise les scores
  player2Score := 0;
  symbolsInitialized := False; // Permet une nouvelle attribution des symboles
  InitializeGame();           // Relance une nouvelle partie
  gameOver := False;          // Réinitialise l’état du jeu
end;

end.

