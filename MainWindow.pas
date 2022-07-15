unit MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.Menus, Vcl.StdCtrls,
  DataStructs,
  System.Generics.Collections, System.DateUtils;

type
  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewFile: TMenuItem;
    LoadFile: TMenuItem;
    SaveFile: TMenuItem;
    TextBox: TMemo;
    SeedLabel: TLabel;
    SeedTextBox: TEdit;
    WidthLabel: TLabel;
    WidthTextBox: TEdit;
    HeightLabel: TLabel;
    HeightTextBox: TEdit;
    MinSizeLabel: TLabel;
    MinRoomSizeTextBox: TEdit;
    DepthLabel: TLabel;
    DepthTextBox: TEdit;
    ExitProgram: TMenuItem;
    NewSeedButton: TButton;
    SplitVarLabel: TLabel;
    SizeVarLabel: TLabel;
    DoorVarLabel: TLabel;
    SplitVarTextBox: TEdit;
    SizeVarTextBox: TEdit;
    DoorVarTextBox: TEdit;
    GenerateButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure NewSeedButtonClick(Sender: TObject);
    procedure SeedTextBoxChange(Sender: TObject);
    procedure WidthTextBoxChange(Sender: TObject);
    procedure HeightTextBoxChange(Sender: TObject);
    procedure MinRoomSizeTextBoxChange(Sender: TObject);
    procedure DepthTextBoxChange(Sender: TObject);
    procedure SplitVarTextBoxChange(Sender: TObject);
    procedure SizeVarTextBoxChange(Sender: TObject);
    procedure DoorVarTextBoxChange(Sender: TObject);
    procedure ExitProgramClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
  private
    function GenerateSeed: Int64;

    procedure BSP(room: TRoom; levelsToGo: Integer);
    function Split(min, max, variance: Extended): Extended;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  seed: Int64;

  minSize: Integer;
  depth: Integer;

  dungeonWidth: Integer;
  dungeonHeight: Integer;

  splitVariance: Extended;
  sizeVariance: Extended;
  doorVariance: Extended;

  rooms: TList<TRoom>;

  currID: Char;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //set all variables to the correct value
  minSize := 4;
  depth := 4;

  dungeonWidth := 80;
  dungeonHeight := 80;

  splitVariance := 0.5;
  sizeVariance := 0.5;
  doorVariance := 0.5;

  rooms := TList<TRoom>.Create;

  currID := 'A';

  seed := GenerateSeed;

  //set up the text box
  TextBox.Text := '';
  TextBox.ReadOnly := true;
  TextBox.ScrollBars := ssBoth;
end;

//------------------------------------------------------------------------------
//GUI Handling
//------------------------------------------------------------------------------

procedure TForm1.NewSeedButtonClick(Sender: TObject);
begin
  seed := GenerateSeed;
end;

procedure TForm1.SeedTextBoxChange(Sender: TObject);
begin
  seed := StrToInt64(SeedTextBox.Text);
end;

procedure TForm1.SizeVarTextBoxChange(Sender: TObject);
begin
  if SizeVarTextBox.Text <> '' then sizeVariance := StrToFloat(SizeVarTextBox.Text);
end;

procedure TForm1.SplitVarTextBoxChange(Sender: TObject);
begin
  if SplitVarTextBox.Text <> '' then splitVariance := StrToFloat(SplitVarTextBox.Text);
end;

procedure TForm1.DoorVarTextBoxChange(Sender: TObject);
begin
  if DoorVarTextBox.Text <> '' then doorVariance := StrToFloat(DoorVarTextBox.Text);
end;

procedure TForm1.WidthTextBoxChange(Sender: TObject);
begin
  dungeonWidth := StrToInt(WidthTextBox.Text);
end;

procedure TForm1.HeightTextBoxChange(Sender: TObject);
begin
  dungeonHeight := StrToInt(HeightTextBox.Text);
end;

procedure TForm1.MinRoomSizeTextBoxChange(Sender: TObject);
begin
  minSize := StrToInt(MinRoomSizeTextBox.Text);
end;

procedure TForm1.DepthTextBoxChange(Sender: TObject);
begin
  depth := StrToInt(DepthTextBox.Text);
end;

procedure TForm1.GenerateButtonClick(Sender: TObject);
var
  startRoom: TRoom;
begin
  //check all the values to make sure they conform to the proper ranges
  if (splitVariance < 0) or (splitVariance > 1) then
  begin
    splitVariance := 0.5;
    SplitVarTextBox.Text := FloatToStr(splitVariance);
  end;

  if (sizeVariance < 0) or (sizeVariance > 1) then
  begin
    sizeVariance := 0.5;
    SizeVarTextBox.Text := FloatToStr(sizeVariance);
  end;

  if (doorVariance < 0) or (doorVariance > 1) then
  begin
    doorVariance := 0.5;
    DoorVarTextBox.Text := FloatToStr(doorVariance);
  end;

  if seed < 0 then seed := GenerateSeed;

  System.RandSeed := seed;

  startRoom := TRoom.Create(0, dungeonWidth - 1, 0, dungeonHeight - 1);
  BSP(startRoom, depth);
end;

//Menubar stuff

procedure TForm1.ExitProgramClick(Sender: TObject);
begin
  Application.MainForm.Close;
end;

//------------------------------------------------------------------------------
//OTHER FUNCTIONS
//------------------------------------------------------------------------------

//function to get the current system time as a seed for the random number
//generator
function TForm1.GenerateSeed: Int64;
var
  st: SystemTime;
  dt: TDateTime;
begin
  Winapi.Windows.GetSystemTime(st);

  dt := System.SysUtils.EncodeDate(st.wYear, st.WMonth, st.WDay) +
        System.SysUtils.EncodeTime(st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);

  Result := System.DateUtils.MillisecondsBetween(dt, UnixDateDelta);

  SeedTextBox.Text := IntToStr(Result);
end;

//procedure to generate list of rooms
procedure TForm1.BSP(room: TRoom; levelsToGo: Integer);
var
  splitPoint: Extended;
  leftRoom, rightRoom: TRoom;
begin
  //check base cases first
  //stop if we reach the desired depth, or if the current room has reached the
  //minimum size
  if (levelsToGo = 0) or (room.rightWall - room.leftWall < minSize) or
      (room.topWall - room.bottomWall < minSize) then
  begin
    room.id := Succ(currID);
    rooms.Add(room);
    Exit;
  end;

  Dec(levelsToGo);

  //determine direction upon which we will split. whichever wall is longer is
  //the one that is split
  if (room.rightWall - room.leftWall > room.topWall - room.bottomWall) then
  begin
    splitPoint := Split(room.leftWall, room.rightWall, splitVariance);

    //using splitPoint, make two new rooms split down the line along that point
    leftRoom := TRoom.Create(room.leftWall, splitPoint, room.topWall, room.bottomWall);
    rightRoom := TRoom.Create(splitPoint, room.rightWall, room.topWall, room.bottomWall);


  end;

end;

function TForm1.Split(min, max, variance: Extended): Extended;
var
  randomVariance, midPoint: Extended;
begin
  //get our random variance value, the amount from which we're deviating from
  //the center. with no variance, we will always split right down the middle of
  //a room. this variance value will always be between -1 and 1, inclusive.
  randomVariance := Random * 2 - 1;

  midPoint := (min + max) / 2;

  midPoint := midPoint + (randomVariance * variance * (max - min) / 2);

  Result := midPoint;
end;

end.
