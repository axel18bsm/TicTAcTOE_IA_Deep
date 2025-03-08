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
  modeButtons, quitButton: TRectangle;
  boardTexture: TTexture2D;
  toggleSliderActiveHaut: Integer = 0;  // 0 = Humain vs Humain, 1 = Humain vs IA Simple, 2 = Humain vs IA Deep
  gameMode: TGameMode = gmHumanVsHuman;

procedure InitBoard();
procedure LoadBoardFromCSV(const FileName: string);
procedure DrawBoard();
procedure gui();
procedure UnloadResources();

implementation

procedure LoadBoardFromCSV(const FileName: string);
var
  F: TextFile;
  Line: string;
  Fields: TStringList;
  i: Integer;
begin
  Fields := TStringList.Create;
  Fields.Delimiter := ',';
  Fields.StrictDelimiter := True;

  AssignFile(F, FileName);
  try
    Reset(F);

    // Ignorer l'en-tête
    if not EOF(F) then
      ReadLn(F, Line);

    // Lire la première ligne pour initialiser Leboard
    if not EOF(F) then
    begin
      ReadLn(F, Line);
      Fields.DelimitedText := Line;

      Leboard.Id := StrToInt(Fields[17]);           // PlateauID
      Leboard.NbreLigne := StrToInt(Fields[18]);    // PlateauNbreLigne
      Leboard.NbreColonne := StrToInt(Fields[19]);  // PlateauNbreColonne
      Leboard.nbrecase := Leboard.NbreLigne * Leboard.NbreColonne;

      // Charger les couleurs
      Leboard.Couleur1Case.r := StrToInt(Fields[5]);  // Color1R
      Leboard.Couleur1Case.g := StrToInt(Fields[6]);  // Color1G
      Leboard.Couleur1Case.b := StrToInt(Fields[7]);  // Color1B
      Leboard.Couleur1Case.a := StrToInt(Fields[8]);  // Color1A
      Leboard.Couleur2Case.r := StrToInt(Fields[9]);  // Color2R
      Leboard.Couleur2Case.g := StrToInt(Fields[10]); // Color2G
      Leboard.Couleur2Case.b := StrToInt(Fields[11]); // Color2B
      Leboard.Couleur2Case.a := StrToInt(Fields[12]); // Color2A
      Leboard.ClearBackground.r := StrToInt(Fields[20]); // ClearBgR
      Leboard.ClearBackground.g := StrToInt(Fields[21]); // ClearBgG
      Leboard.ClearBackground.b := StrToInt(Fields[22]); // ClearBgB
      Leboard.ClearBackground.a := StrToInt(Fields[23]); // ClearBgA

      // Initialiser les dimensions à partir de la première case
      Leboard.LargCase := StrToInt(Fields[3]);  // Width
      Leboard.HautCase := StrToInt(Fields[4]);  // Height
      Leboard.offsetX := 60;
      Leboard.offsetY := 60;
      Leboard.boardWidth := Leboard.NbreColonne * Leboard.LargCase;
      Leboard.boardHeight := Leboard.NbreLigne * Leboard.HautCase;

      // Redimensionner et charger board
      SetLength(board, Leboard.nbrecase);

      i := 0;
      board[i].id := StrToInt(Fields[0]);
      board[i].x := StrToInt(Fields[1]);
      board[i].y := StrToInt(Fields[2]);
      board[i].width := StrToInt(Fields[3]);
      board[i].height := StrToInt(Fields[4]);
      board[i].color.r := StrToInt(Fields[5]);
      board[i].color.g := StrToInt(Fields[6]);
      board[i].color.b := StrToInt(Fields[7]);
      board[i].color.a := StrToInt(Fields[8]);
      board[i].player := plNone;  // Pas de joueur au départ
      board[i].milieuX := StrToInt(Fields[16]);
      board[i].milieuY := StrToInt(Fields[17]);
      Inc(i);
    end;

    // Charger les autres cases
    while not EOF(F) and (i < Leboard.nbrecase) do
    begin
      ReadLn(F, Line);
      Fields.DelimitedText := Line;
      board[i].id := StrToInt(Fields[0]);
      board[i].x := StrToInt(Fields[1]);
      board[i].y := StrToInt(Fields[2]);
      board[i].width := StrToInt(Fields[3]);
      board[i].height := StrToInt(Fields[4]);
      board[i].color.r := StrToInt(Fields[5]);
      board[i].color.g := StrToInt(Fields[6]);
      board[i].color.b := StrToInt(Fields[7]);
      board[i].color.a := StrToInt(Fields[8]);
      board[i].player := plNone;
      board[i].milieuX := StrToInt(Fields[16]);
      board[i].milieuY := StrToInt(Fields[17]);
      Inc(i);
    end;

    CloseFile(F);
  except
    on E: Exception do
    begin
      CloseFile(F);
      WriteLn('Erreur lors du chargement CSV : ', E.Message);
    end;
  end;
  Fields.Free;
end;

procedure InitBoard();
begin
  LoadBoardFromCSV('echiquier.csv');  // Charger les données du CSV
  // Charger l'image directement avec LoadTexture (point 4)
  boardTexture := LoadTexture('screenshot.png');
  if not IsTextureValid(boardTexture) then
    WriteLn('Erreur : Impossible de charger screenshot.png');
  statustext := 'Plateau chargé depuis image';
end;

procedure DrawBoard();
var
  i: Integer;
begin
  ClearBackground(Leboard.ClearBackground);
  // Utiliser les offsets calculés dans LoadBoardFromCSV (point 1)
  DrawTexture(boardTexture, Leboard.offsetX, Leboard.offsetY, WHITE);
  // Surcharge pour afficher X ou O si un joueur est présent
  for i := 0 to Leboard.nbrecase - 1 do
  begin
    case board[i].player of
      plX: DrawText('X', board[i].milieuX - 10, board[i].milieuY - 20, 40, RED);
      plO: DrawText('O', board[i].milieuX - 10, board[i].milieuY - 20, 40, BLUE);
    end;
  end;
end;

procedure gui();
begin
  // Configurer le style du slider
  GuiSetStyle(SLIDER, SLIDER_PADDING, 2);

  // Slider pour choisir le mode (point 6 : garder votre position)
  modeButtons := RectangleCreate(750, 20, 240, 25);
  GuiToggleSlider(modeButtons, 'Humain vs Humain;IA Simple;IA Deep', @toggleSliderActiveHaut);

  // Mettre à jour le mode de jeu selon la sélection
  case toggleSliderActiveHaut of
    0: gameMode := gmHumanVsHuman;
    1: gameMode := gmHumanVsAI;
    2: gameMode := gmHumanVsDeepAI;
  end;

  // Réactiver GuiStatusBar (point 5)
  GuiStatusBar(RectangleCreate(0, SCREEN_HEIGHT - 20, SCREEN_WIDTH, 20), statustext);
end;

procedure UnloadResources();
begin
  if IsTextureValid(boardTexture) then
    UnloadTexture(boardTexture);
  // Pas de Leboard.lenom à libérer car non initialisé
  SetLength(board, 0);
end;

end.

