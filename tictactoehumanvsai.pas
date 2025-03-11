unit TicTacToeHumanVsAI;

{$mode ObjFPC}{$H+}

interface

uses
  raylib, TicTacToeGame, SysUtils, windows;

procedure HumanVsAIPlay();

implementation

function maxDouble(a, b: Double): Double;
begin
  if a > b then Result := a else Result := b;
end;

function minDouble(a, b: Double): Double;
begin
  if a < b then Result := a else Result := b;
end;

procedure StartNewGame();
begin
  aiDepth := 2;
  WriteLn('--- Nouvelle partie ---');
  WriteLn('Profondeur Alpha-Beta fixée à : ', aiDepth);
end;

procedure HandleHumanClicks(var selectedIndex: Integer; var mousePos: TVector2);
var
  i: Integer;
  clicked: Boolean;
  caseRect: TRectangle;
begin
  mousePos := GetMousePosition();
  clicked := IsMouseButtonPressed(MOUSE_LEFT_BUTTON);
  selectedIndex := -1;

  if clicked then
  begin
    for i := 0 to Leboard.nbrecase - 1 do
    begin
      caseRect := RectangleCreate(board[i].x + Leboard.offsetX, board[i].y + Leboard.offsetY, board[i].width, board[i].height);
      if CheckCollisionPointRec(mousePos, caseRect) and (board[i].player = plNone) then
      begin
        selectedIndex := i;
        WriteLn('Humain a cliqué sur la case : ', i);
        Break;
      end;
    end;
  end;
end;

procedure SwitchPlayer();
begin
  if currentPlayer = plX then
    currentPlayer := plO
  else
    currentPlayer := plX;
  WriteLn('Changement de joueur. Nouveau joueur : ', currentPlayer);
end;

function EvaluateBoard(hasWon: Boolean; winner: TPlayer; isDraw: Boolean): Double;
begin
  if hasWon then
  begin
    if winner = player2Symbol then
      Result := 1
    else
      Result := -1;
  end
  else if isDraw then
    Result := 0
  else
    Result := 0;
end;

function AlphaBeta(depth: Integer; maximizingPlayer: Boolean; alpha: Double; beta: Double): Double;
var
  i: Integer;
  eval: Double;
  hasWon: Boolean;
  isDraw: Boolean;
  winner: TPlayer;
begin
  CheckVictory(hasWon, winner);
  CheckDraw(isDraw);
  if (depth = 0) or (hasWon) or (isDraw) then
  begin
    Result := EvaluateBoard(hasWon, winner, isDraw);
    Exit;
  end;

  if maximizingPlayer then
  begin
    Result := -MaxInt;
    for i := 0 to Leboard.nbrecase - 1 do
    begin
      if board[i].player = plNone then
      begin
        board[i].player := player2Symbol;
        eval := AlphaBeta(depth - 1, False, alpha, beta);
        board[i].player := plNone;
        Result := maxDouble(Result, eval);
        alpha := maxDouble(alpha, Result);
        if beta <= alpha then Break;
      end;
    end;
  end
  else
  begin
    Result := MaxInt;
    for i := 0 to Leboard.nbrecase - 1 do
    begin
      if board[i].player = plNone then
      begin
        board[i].player := player1Symbol;
        eval := AlphaBeta(depth - 1, True, alpha, beta);
        board[i].player := plNone;
        Result := minDouble(Result, eval);
        beta := minDouble(beta, Result);
        if beta <= alpha then Break;
      end;
    end;
  end;
end;

function FindWinningMove(): Integer;
var
  i: Integer;
  hasWon: Boolean;
  winner: TPlayer;
begin
  Result := -1;
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plNone then
    begin
      board[i].player := player2Symbol;
      CheckVictory(hasWon, winner);
      board[i].player := plNone;
      if hasWon and (winner = player2Symbol) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function FindBlockingMove(): Integer;
var
  i: Integer;
  hasWon: Boolean;
  winner: TPlayer;
begin
  Result := -1;
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plNone then
    begin
      board[i].player := player1Symbol;
      CheckVictory(hasWon, winner);
      board[i].player := plNone;
      if hasWon and (winner = player1Symbol) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function ChooseMoveWithAlphaBeta(): Integer;
var
  i: Integer;
  bestValue: Double;
  bestMove: Integer;
  eval: Double;
begin
  bestValue := -MaxInt;
  bestMove := -1;
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plNone then
    begin
      board[i].player := player2Symbol;
      eval := AlphaBeta(aiDepth - 1, False, -MaxInt, MaxInt);
      board[i].player := plNone;
      if eval > bestValue then
      begin
        bestValue := eval;
        bestMove := i;
      end;
    end;
  end;
  Result := bestMove;
end;

procedure AIPlay();
var
  move: Integer;
begin
  move := FindWinningMove();
  if move < 0 then move := FindBlockingMove();
  if move < 0 then move := ChooseMoveWithAlphaBeta();
  if move >= 0 then
  begin
    board[move].player := player2Symbol;
    WriteLn('L''IA joue sur la case ', move);
  end;
end;

procedure HumanVsAIPlay();
var
  selectedIndex: Integer;
  mousePos: TVector2;
  hasWon: Boolean;
  isDraw: Boolean;
  winner: TPlayer;
begin
  case gameState of
    gsHumanTurn:
    begin
      if (currentPlayer = player1Symbol) then
      begin
        HandleHumanClicks(selectedIndex, mousePos);
        if selectedIndex >= 0 then
        begin
          board[selectedIndex].player := currentPlayer;
          clickedId := board[selectedIndex].id;
          clickPosX := mousePos.x;
          clickPosY := mousePos.y;
          gameState := gsCheckResult;        // joueur humain,  on va verifier s'il y a un gain
        end;
      end
      else if (gameMode = gmHumanVsAI) and (currentPlayer = player2Symbol) then
        gameState := gsIAThinking;       // donne la main à  l ia
    end;

    gsIAThinking:
    begin
      if (gameMode = gmHumanVsAI) and (currentPlayer = player2Symbol) then
      begin
        aiMoveInProgress := True;           // l ia initalise pour dire qu'elle va bosser
        gameState := gsIAPlaying;
      end
      else
        gameState := gsHumanTurn;          // le code  ne vient jamais ici
    end;

    gsIAPlaying:
    begin
      if (gameMode = gmHumanVsAI) and aiMoveInProgress then
      begin
        AIPlay();                           // l ia rentre en action
        aiMoveInProgress := False;
        gameState := gsCheckResult;         // on va verifier s'il y a un gain
      end;
    end;

    gsCheckResult:
    begin
      CheckVictory(hasWon, winner);
      if hasWon then
      begin
        gameOver := True;
        if winner = player1Symbol then
          player1Score := player1Score + 1
        else if winner = player2Symbol then
          player2Score := player2Score + 1;
        WriteLn('Score mis à jour - Humain: ', player1Score:0:2, ' IA: ', player2Score:0:2);
      end
      else
      begin
        CheckDraw(isDraw);
        if isDraw then
        begin
          gameOver := True;
          player1Score := player1Score + 0.5;
          player2Score := player2Score + 0.5;
          WriteLn('Match nul - Scores: ', player1Score:0:2, ' / ', player2Score:0:2);
        end;
      end;

      if gameOver then
      begin
        if (player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound) then
          ResetManche() // Réinitialise les scores uniquement après une manche gagnée
        else
          RestartGame(); // Relance une nouvelle partie sans toucher aux scores
        gameState := gsGameOver;
      end
      else
      begin
        SwitchPlayer();
        if (gameMode = gmHumanVsAI) and (currentPlayer = player2Symbol) then
          gameState := gsIAThinking
        else
          gameState := gsHumanTurn;
      end;
    end;

    gsGameOver:
    begin
      if not ((player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound)) then
        gameState := gsHumanTurn;
    end;
  end;
end;

initialization
  Randomize;
  aiTurn := False;

end.
