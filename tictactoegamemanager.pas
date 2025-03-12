unit TicTacToeGameManager;

{$mode ObjFPC}{$H+}

interface

uses
  TicTacToeGame, TicTacToeHumanVsHuman, TicTacToeHumanVsAI, TicTacToeHumanVsDeepAI;

procedure ManageGame();

implementation

procedure ManageGame();
begin
  if gameMode <> lastGameMode then
  begin
    if gameMode = gmHumanVsAI then
      InitializeHumanVsAI()
    else if gameMode = gmHumanVsDeepAI then
      InitializeHumanVsAI()  // On peut réutiliser la même initialisation pour l'instant
    else if gameMode = gmHumanVsHuman then
      InitializeGame();
    lastGameMode := gameMode;
  end;

  case gameMode of
    gmHumanVsHuman: HumanVsHumanPlay();
    gmHumanVsAI: HumanVsAIPlay();
    gmHumanVsDeepAI: HumanVsDeepAIPlay();
  end;
end;

end.

