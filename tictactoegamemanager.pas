unit TicTacToeGameManager;

{$mode ObjFPC}{$H+}

interface

uses
  TicTacToegame,TicTacToehumanvshuman;
 // TicTacToeHumanVsHuman,
 // TicTacToeHumanVsAI,
 // TicTacToeHumanVsDeepAI;

procedure ManageGame();

implementation

procedure ManageGame();
begin
  // Gérer la logique du jeu selon le mode sélectionné
  case gameMode of
    gmHumanVsHuman: HumanVsHumanPlay();
    //gmHumanVsAI: HumanVsAIPlay();
    //gmHumanVsDeepAI: HumanVsDeepAIPlay();
  end;
end;

end.

