program TicTacToe;

{$mode objfpc}{$H+}

uses
  cmem,
  raylib,
  SysUtils,
  TicTacToeGame,
  raygui;

begin
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, 'Tic-Tac-Toe Raylib Raygui Free Pascal');
  SetTargetFPS(60);
  GuiLoadStyle(PChar(GetApplicationDirectory + 'gui_styles/style_amber.rgs'));

  InitBoard();

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    // DrawBoard avant gui (point 2)
    DrawBoard();
    gui();
    // Bouton Quitter dans la boucle principale (point 2 : garder votre version)
    quitButton := RectangleCreate(SCREEN_WIDTH - 150, 50, 120, 40);
    if GuiButton(quitButton, 'Quitter') > 0 then
      break;
    EndDrawing();
  end;

  UnloadResources();
  CloseWindow();
end.

