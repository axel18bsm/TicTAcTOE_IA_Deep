unit TicTacToeHumanVsHuman;

{$mode ObjFPC}{$H+}

interface

uses
  raylib,  // Pour CheckCollisionPointRec et autres fonctions
  TicTacToeGame,SysUtils,windows;  // Pour accéder aux variables globales

procedure HumanVsHumanPlay();

var
  clickedId: Integer = -1;  // ID de la case cliquée
  clickPosX: Single = 0;    // Coordonnée X du clic
  clickPosY: Single = 0;    // Coordonnée Y du clic

implementation



procedure HandleClicks(var selectedIndex: Integer; var mousePos: TVector2);
var
  i: Integer;
  clicked: Boolean;
  caseRect: TRectangle;
begin
  mousePos := GetMousePosition();
  clicked := IsMouseButtonPressed(MOUSE_LEFT_BUTTON);
  selectedIndex := -1; // Initialise à -1 (aucune case sélectionnée)

  if clicked then
  begin
    for i := 0 to Leboard.nbrecase - 1 do
    begin
      caseRect := RectangleCreate(board[i].x + Leboard.offsetX, board[i].y + Leboard.offsetY, board[i].width, board[i].height);
      if CheckCollisionPointRec(mousePos, caseRect) and (board[i].player = plNone) then
      begin
        selectedIndex := i; // Enregistre l'index de la case cliquée
        Break; // Sortir après avoir trouvé une case valide
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
  HandleClicks(selectedIndex, mousePos);

  if selectedIndex >= 0 then
  begin
    board[selectedIndex].player := currentPlayer;
    clickedId := board[selectedIndex].id;  // Stocker l'ID de la case cliquée
    clickPosX := mousePos.x;  // Stocker les coordonnées du clic
    clickPosY := mousePos.y;
    SwitchPlayer();

    // Vérifier la victoire ou le match nul
    CheckVictory(hasWon, winner);
    if hasWon then
    begin
      if (player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound) then
        gameOver := True
      else
        RestartGame(); // Relancer une nouvelle partie après une victoire si < 5
      Exit;
    end;

    CheckDraw(isDraw);
    if isDraw then
    begin
      if (player1Score >= scoreToWinRound) or (player2Score >= scoreToWinRound) then
        gameOver := True
      else
        RestartGame(); // Relancer une nouvelle partie après un nul si < 5
      Exit;
    end;
  end;
end;

end.

