unit TicTacToeGameManager;

{$mode ObjFPC}{$H+}

interface

uses
  TicTacToeGame, TicTacToeHumanVsHuman, TicTacToeHumanVsAI;

procedure ManageGame();

implementation

procedure ManageGame();
begin
  if gameMode <> lastGameMode then
  begin
    if gameMode = gmHumanVsAI then
      InitializeHumanVsAI()
    else if gameMode = gmHumanVsHuman then
      InitializeGame();
    lastGameMode := gameMode;
  end;

  case gameMode of
    gmHumanVsHuman: HumanVsHumanPlay();
    gmHumanVsAI: HumanVsAIPlay();
  end;
end;

end.

