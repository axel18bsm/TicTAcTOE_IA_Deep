unit TicTacToeHumanVsHuman;

{$mode ObjFPC}{$H+}

interface

uses
  raylib, TicTacToeGame, SysUtils, windows;

procedure HumanVsHumanPlay();

var
  clickedId: Integer = -1;
  clickPosX: Single = 0;
  clickPosY: Single = 0;

implementation

procedure HandleClicks(var selectedIndex: Integer; var mousePos: TVector2);
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
end;

procedure HumanVsHumanPlay();
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
      HandleClicks(selectedIndex, mousePos);
      if selectedIndex >= 0 then
      begin
        board[selectedIndex].player := currentPlayer;
        clickedId := board[selectedIndex].id;
        clickPosX := mousePos.x;
        clickPosY := mousePos.y;
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
        WriteLn('Score mis à jour - Joueur 1: ', player1Score:0:2, ' Joueur 2: ', player2Score:0:2);
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
          ResetManche() // Réinitialise les scores après une manche gagnée
        else
          RestartGame(); // Relance une nouvelle partie sans toucher aux scores
        gameState := gsGameOver;
      end
      else
      begin
        SwitchPlayer();
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

end.
