unit TicTacToeHumanVsDeepAI;

{$mode ObjFPC}{$H+}

interface

uses
  raylib, TicTacToeGame, SysUtils, windows, process,classes;

procedure HumanVsDeepAIPlay();

implementation

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

// Fonction pour appeler le script Python et obtenir le meilleur coup
function GetAIMoveFromPython(): Integer;

var
  AProcess: TProcess;
  OutputStream: TMemoryStream;
  Buffer: array[0..1023] of Byte;
  BytesRead: LongInt;
  InputData: string;
  OutputData: string;
  i: Integer;
  BoardArray: array[0..8] of Integer;
  JsonData: string;
  JsonResult: string;
  LogFile: TextFile;
begin
  Result := -1;

  // Préparer les données du plateau pour Python
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    if board[i].player = plNone then
      BoardArray[i] := 0
    else if board[i].player = plX then
      BoardArray[i] := 1
    else
      BoardArray[i] := -1;
  end;
  JsonData := '{"board": [' + IntToStr(BoardArray[0]);
  for i := 1 to Leboard.nbrecase - 1 do
    JsonData := JsonData + ',' + IntToStr(BoardArray[i]);
  JsonData := JsonData + ']}';
  WriteLn('Données envoyées à Python : ', JsonData);

  // Sauvegarder dans un fichier
  AssignFile(LogFile, 'tictactoe_log_fp.txt');
  if FileExists('tictactoe_log_fp.txt') then
    Append(LogFile)
  else
    Rewrite(LogFile);
  WriteLn(LogFile, 'Entrée JSON : ', JsonData);
  CloseFile(LogFile);

  // Configurer le processus pour appeler Python
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'python'; // Assure-toi que python est dans le PATH
    AProcess.Parameters.Add('C:\\Users\\Axel\\PycharmProjects\\PythonProject4\\predict_tictactoe.py');
    AProcess.Options := AProcess.Options + [poUsePipes];
    AProcess.Execute;

    // Envoyer les données JSON au script Python
    AProcess.Input.Write(JsonData[1], Length(JsonData));
    AProcess.CloseInput;
    WriteLn('Données envoyées, attente de la réponse...');

    // Lire la sortie du script Python
    OutputStream := TMemoryStream.Create;
    try
      repeat
        BytesRead := AProcess.Output.Read(Buffer, 1024);
        OutputStream.Write(Buffer, BytesRead);
      until BytesRead = 0;

      SetLength(OutputData, OutputStream.Size);
      OutputStream.Position := 0;
      OutputStream.Read(OutputData[1], OutputStream.Size);
      WriteLn('Réponse reçue de Python : ', OutputData);

      // Sauvegarder dans un fichier
      AssignFile(LogFile, 'tictactoe_log_fp.txt');
      Append(LogFile);
      WriteLn(LogFile, 'Sortie JSON : ', OutputData);
      CloseFile(LogFile);

      JsonResult := Copy(OutputData, 1, Pos('}', OutputData) + 1);
      Result := StrToIntDef(Copy(JsonResult, Pos('"move":', JsonResult) + 7, Pos('}', JsonResult) - Pos('"move":', JsonResult) - 7), -1);
      WriteLn('Coup retourné par Python : ', Result);
    finally
      OutputStream.Free;
    end;
  finally
    AProcess.Free;
  end;
end;

procedure AIPlay();
var
  move: Integer;
begin
  WriteLn('--- Tour de l''IA Deep ---');
  move := GetAIMoveFromPython();
  if move >= 0 then
  begin
    board[move].player := player2Symbol;
    WriteLn('L''IA Deep joue sur la case ', move);
    aiMoveInProgress := False;
  end;
end;

procedure HumanVsDeepAIPlay();
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
          gameState := gsCheckResult;
        end;
      end
      else if (gameMode = gmHumanVsDeepAI) and (currentPlayer = player2Symbol) then
        gameState := gsIAThinking;
    end;

    gsIAThinking:
    begin
      if (gameMode = gmHumanVsDeepAI) and (currentPlayer = player2Symbol) then
      begin
        aiMoveInProgress := True;
        gameState := gsIAPlaying;
      end
      else
        gameState := gsHumanTurn;
    end;

    gsIAPlaying:
    begin
      if (gameMode = gmHumanVsDeepAI) and aiMoveInProgress then
      begin
        AIPlay();
        gameState := gsCheckResult;
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
        WriteLn('Score mis à jour - Humain: ', player1Score:0:2, ' IA Deep: ', player2Score:0:2);
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
          ResetManche()
        else
          RestartGame();
        gameState := gsGameOver;
      end
      else
      begin
        SwitchPlayer();
        if (gameMode = gmHumanVsDeepAI) and (currentPlayer = player2Symbol) then
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

end.unit Unit1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

implementation

end.

