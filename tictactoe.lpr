program TicTacToe;

{$mode objfpc}{$H+}

uses
  cmem,
  raylib,
  SysUtils,
  TicTacToeGame,
  TicTacToeGameManager,
  raygui, TicTacToeHumanVsHuman;

begin
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, 'Tic-Tac-Toe Raylib Raygui Free Pascal');
  SetTargetFPS(60);
  GuiLoadStyle(PChar(GetApplicationDirectory + 'gui_styles/style_amber.rgs'));

  InitBoard();

  InitializeGame();

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    DrawBoard();
    gui();
    quitButton := RectangleCreate(SCREEN_WIDTH - 150, 50, 120, 40);
    if GuiButton(quitButton, 'Quitter') > 0 then
      break;
    // Gérer la logique du jeu via GameManager
    ManageGame();
    EndDrawing();
  end;

  UnloadResources();
  CloseWindow();
end.

