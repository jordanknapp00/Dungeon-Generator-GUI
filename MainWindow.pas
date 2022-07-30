unit MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.Menus, Vcl.StdCtrls,
  DataStructs,
  System.Generics.Collections, System.DateUtils, System.UITypes, Vcl.ComCtrls;

type
  TMap = Array of Array of Char;

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewFile: TMenuItem;
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
    SaveFileAs: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    procedure NewFileClick(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure SaveFileAsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    function GenerateSeed: Int64;

    procedure BSP(room: TRoom; levelsToGo: Integer);
    function Split(min, max, variance: Extended): Extended;
    procedure Shrink;
    function Print: TStringList;
    procedure ConnectDoors(map: TMap; doorAt: TDoor);

    function ArrayToString(const arr: Array of Char): String;

    function HandleSave(): Boolean;
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

  rooms: TObjectList<TRoom>;

  currID: Char;

  generated: Boolean;

  saveText: TStringList;
  fileName: String;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // set all variables to the correct value
  minSize := 4;
  depth := 4;

  dungeonWidth := 80;
  dungeonHeight := 80;

  splitVariance := 0.5;
  sizeVariance := 0.5;
  doorVariance := 0.5;

  rooms := TObjectList<TRoom>.Create(true);
  rooms.OwnsObjects := true;

  currID := 'A';

  seed := GenerateSeed;

  generated := false;

  saveText := TStringList.Create;
  fileName := '';

  // set up the text box
  TextBox.Font.Name := 'Courier New';
  TextBox.Text := '';
  TextBox.ReadOnly := true;
  TextBox.ScrollBars := ssBoth;

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  roomAt: TRoom;
begin
  CanClose := HandleSave;

  // free up allocated memory
  if CanClose then
  begin
    for roomAt in rooms do
    begin
      roomAt.doors.Clear;
      roomAt.doors.Free;
    end;
    rooms.Clear;
    rooms.Free;

    saveText.Free;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Form1 := nil;
end;

// -----------------------------------------------------------------------------
// GUI Handling
// -----------------------------------------------------------------------------

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
  if SizeVarTextBox.Text <> '' then
    sizeVariance := StrToFloat(SizeVarTextBox.Text);
end;

procedure TForm1.SplitVarTextBoxChange(Sender: TObject);
begin
  if SplitVarTextBox.Text <> '' then
    splitVariance := StrToFloat(SplitVarTextBox.Text);
end;

procedure TForm1.DoorVarTextBoxChange(Sender: TObject);
begin
  if DoorVarTextBox.Text <> '' then
    doorVariance := StrToFloat(DoorVarTextBox.Text);
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
  roomAt: TRoom;

  output: TStringList;
  index: Integer;
begin
  // check all the values to make sure they conform to the proper ranges
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

  // if splitVariance is higher than about .8, it can cause problems
  if splitVariance > 0.8 then
  begin
    splitVariance := 0.8;
    SplitVarTextBox.Text := FloatToStr(splitVariance);
  end;

  if seed < 0 then
    seed := GenerateSeed;

  System.RandSeed := seed;

  currID := 'A';

  // free up the current list of rooms
  for roomAt in rooms do
  begin
    roomAt.doors.Clear;
    roomAt.doors.Free;
  end;
  rooms.Clear;

  startRoom := TRoom.Create(0, dungeonWidth - 1, 0, dungeonHeight - 1);
  BSP(startRoom, depth);

  Shrink;

  output := Print;

  // set up the save file
  saveText.Clear;
  saveText.Add('Parameters (copy these parameters to save your dungeon)');
  saveText.Add('Size: ' + IntToStr(dungeonWidth) + 'x' +
    IntToStr(dungeonHeight));
  saveText.Add('Depth: ' + IntToStr(depth));
  saveText.Add('Seed: ' + IntToStr(seed));
  saveText.Add('Minsize: ' + IntToStr(minSize));
  saveText.Add('Split variance: ' + FloatToStr(splitVariance));
  saveText.Add('Size variance: ' + FloatToStr(sizeVariance));
  saveText.Add('Door variance: ' + FloatToStr(doorVariance));
  saveText.Add('');
  saveText.Add
    ('For the command line version of the program, copy the parameters directly:');
  saveText.Add('-size ' + IntToStr(dungeonWidth) + ' ' + IntToStr(dungeonHeight)
    + ' -depth ' + IntToStr(depth) + ' -minsize ' + IntToStr(minSize) +
    ' -splitvar ' + FloatToStr(splitVariance) + ' -sizevar ' +
    FloatToStr(sizeVariance) + ' -doorvar ' + FloatToStr(doorVariance) +
    ' -seed ' + IntToStr(seed));
  saveText.Add('');
  saveText.Add('Here''s your dungeon:');
  saveText.Add('');

  // now add the output to saveText
  for index := 0 to output.Count - 1 do
    saveText.Add(output[index]);

  generated := true;

  startRoom.Free;
  output.Free;
end;

// Menubar stuff

procedure TForm1.NewFileClick(Sender: TObject);
var
  roomAt: TRoom;
begin
  if not HandleSave then
    Exit;

  // just reset everything to the default
  minSize := 4;
  MinRoomSizeTextBox.Text := IntToStr(4);
  depth := 4;
  DepthTextBox.Text := IntToStr(4);

  dungeonWidth := 80;
  WidthTextBox.Text := IntToStr(80);
  dungeonHeight := 80;
  HeightTextBox.Text := IntToStr(80);

  splitVariance := 0.5;
  SplitVarTextBox.Text := FloatToStr(0.5);
  sizeVariance := 0.5;
  SizeVarTextBox.Text := FloatToStr(0.5);
  doorVariance := 0.5;
  DoorVarTextBox.Text := FloatToStr(0.5);

  for roomAt in rooms do
  begin
    roomAt.doors.Clear;
    roomAt.doors.Free;
  end;
  rooms.Clear;

  currID := 'A';

  seed := GenerateSeed;

  generated := false;

  saveText.Clear;
  fileName := '';

  // set up the text box
  TextBox.Font.Name := 'Courier New';
  TextBox.Text := '';
  TextBox.ReadOnly := true;
  TextBox.ScrollBars := ssBoth;
end;

procedure TForm1.SaveFileClick(Sender: TObject);
begin
  // if no dungeon has been generated, show warning dialog and exit
  if not generated then
  begin
    messageDlg('You must generate a dungeon in order to save it.', mtWarning,
      mbOKCancel, 0);
    Exit;
  end;

  // if we haven't already loaded a file, then follow the procedure for creating
  // and saving to a new file
  if fileName = '' then
  begin
    SaveFileAsClick(nil);
    Exit;
  end;

  // otherwise, simply overwrite the existing file
  saveText.SaveToFile(fileName);
end;

procedure TForm1.SaveFileAsClick(Sender: TObject);
var
  dialog: TSaveDialog;
begin
  if not generated then
  begin
    messageDlg('You must generate a dungeon in order to save it.', mtWarning,
      mbOKCancel, 0);
    Exit;
  end;

  // initialize the save dialog, which should create a new file, i think
  dialog := TSaveDialog.Create(self);
  dialog.InitialDir := GetCurrentDir;
  dialog.Filter := 'Text Documents (*.txt)|*.txt';
  dialog.DefaultExt := 'txt';
  dialog.FilterIndex := 1;

  if dialog.Execute then
  begin
    fileName := dialog.Files[0];
    saveText.SaveToFile(fileName);
  end;

  dialog.Free;
end;

procedure TForm1.ExitProgramClick(Sender: TObject);
begin
  Application.MainForm.Close;
end;

// -----------------------------------------------------------------------------
// OTHER FUNCTIONS
// -----------------------------------------------------------------------------

// function that takes care of saving whenever the program tries to exit. return
// true if the program can exit, return false if not.
function TForm1.HandleSave(): Boolean;
var
  optionSelected: Integer;
  dialog: TSaveDialog;
begin
  Result := false;

  if generated then
  begin
    optionSelected := messageDlg('Would you like to save the current dungeon?',
      mtConfirmation, mbYesNoCancel, 0);

    if optionSelected = mrYes then
    // if yes is hit, we need to handle the save dialog
    begin
      // if there is no current file, we need to create one with a save dialog
      if fileName = '' then
      begin
        // initialize the save dialog
        dialog := TSaveDialog.Create(self);
        dialog.InitialDir := GetCurrentDir;
        dialog.Filter := 'Text Documents (*.txt)|*.txt';
        dialog.DefaultExt := 'txt';
        dialog.FilterIndex := 1;

        if dialog.Execute then
        begin
          // if a file was picked, set it to be saved
          if dialog.Files.Count > 0 then
          begin
            fileName := dialog.Files[0];
            saveText.SaveToFile(fileName);
            Result := true;
          end
          else
            Result := false;
        end;

        dialog.Free;
      end
      // otherwise, simple save the current file
      else
      begin
        saveText.SaveToFile(fileName);
        Result := true;
      end;
    end
    // if no is hit, the program SHOULD exit
    else if optionSelected = mrNo then
      Result := true
      // if anything else is hit, the program SHOULD NOT exit. hopefully this
      // will account for the user hitting the red X on the dialog.
    else
      Result := false;
  end
  else
    Result := true;
end;

// function to get the current system time as a seed for the random number
// generator
function TForm1.GenerateSeed: Int64;
var
  st: SystemTime;
  dt: TDateTime;
begin
  Winapi.Windows.GetSystemTime(st);

  dt := System.SysUtils.EncodeDate(st.wYear, st.WMonth, st.WDay) +
    System.SysUtils.EncodeTime(st.wHour, st.wMinute, st.wSecond,
    st.wMilliseconds);

  Result := System.DateUtils.MillisecondsBetween(dt, UnixDateDelta);

  SeedTextBox.Text := IntToStr(Result);
end;

// procedure to recursively create rooms
procedure TForm1.BSP(room: TRoom; levelsToGo: Integer);
var
  splitPoint: Extended;
  leftRoom, rightRoom, bottomRoom, topRoom: TRoom;

  doorAt: TDoor;
  toGetDoor: TRoom;

  leftDoor, rightDoor, topDoor, bottomDoor: TDoor;
begin
  // check base cases first
  // stop if we reach the desired depth, or if the current room has reached the
  // minimum size
  if (levelsToGo = 0) or (room.rightWall - room.leftWall < minSize) or
    (room.topWall - room.bottomWall < minSize) then
  begin
    room.id := currID;
    currID := Succ(currID);
    rooms.Add(room);
    Exit;
  end;

  Dec(levelsToGo);

  // determine direction upon which we will split. whichever wall is longer is
  // the one that is split
  if (room.rightWall - room.leftWall > room.topWall - room.bottomWall) then
  begin
    splitPoint := Split(room.leftWall, room.rightWall, splitVariance);

    // using splitPoint, make two new rooms split down the line along that point
    leftRoom := TRoom.Create(room.leftWall, splitPoint, room.bottomWall,
      room.topWall);
    rightRoom := TRoom.Create(splitPoint, room.rightWall, room.bottomWall,
      room.topWall);

    // since this started as one big room, we need to make sure the doors end up
    // in the appropriate room. any doors that were on the left side of the split
    // should end up in the left room, and vice versa
    for doorAt in room.doors do
    begin
      if doorAt.x < splitPoint then
        toGetDoor := leftRoom
      else
        toGetDoor := rightRoom;

      toGetDoor.AddDoor(doorAt);

      // since we're splitting vertically, and there is a specific allowed
      // variance for door locations, we want to relocate the door so it is
      // within that variance. for example, with 0 variance, doors are always in
      // the center of a room. vertical doors, however, would no longer be in the
      // center becase we've split along the vertical axis. so we need to use the
      // split function to move the door within the appropriate bounds
      if not doorAt.isHorizontal then
        doorAt.x := Split(toGetDoor.leftWall, toGetDoor.rightWall,
          doorVariance);
    end;

    // after any doors have been added and relocated, we need to add a new set of
    // doors between the rooms we just created. we've split vertically, so the
    // doors will be horizontal.
    leftDoor := TDoor.Create(splitPoint, Split(room.bottomWall, room.topWall,
      doorVariance), true);
    rightDoor := TDoor.Create(splitPoint, Split(room.bottomWall, room.topWall,
      doorVariance), true);

    leftDoor.other := rightDoor;
    rightDoor.other := leftDoor;

    leftRoom.doors.Add(leftDoor);
    rightRoom.doors.Add(rightDoor);

    // now recursively split these subrooms
    BSP(leftRoom, levelsToGo);
    BSP(rightRoom, levelsToGo);
  end
  else
  begin
    // split across the opposite dimension. the code here is largely the same as
    // above, just doing things in the x direction rather than y.
    splitPoint := Split(room.bottomWall, room.topWall, splitVariance);

    bottomRoom := TRoom.Create(room.leftWall, room.rightWall, room.bottomWall,
      splitPoint);
    topRoom := TRoom.Create(room.leftWall, room.rightWall, splitPoint,
      room.topWall);

    for doorAt in room.doors do
    begin
      if doorAt.y < splitPoint then
        toGetDoor := bottomRoom
      else
        toGetDoor := topRoom;

      toGetDoor.AddDoor(doorAt);

      if doorAt.isHorizontal then
        doorAt.y := Split(toGetDoor.bottomWall, toGetDoor.topWall,
          doorVariance);
    end;

    bottomDoor := TDoor.Create(Split(room.leftWall, room.rightWall,
      doorVariance), splitPoint, false);
    topDoor := TDoor.Create(Split(room.leftWall, room.rightWall, doorVariance),
      splitPoint, false);

    bottomDoor.other := topDoor;
    topDoor.other := bottomDoor;

    bottomRoom.AddDoor(bottomDoor);
    topRoom.AddDoor(topDoor);

    BSP(bottomRoom, levelsToGo);
    BSP(topRoom, levelsToGo);
  end;

end;

function TForm1.Split(min, max, variance: Extended): Extended;
var
  randomVariance, midPoint: Extended;
begin
  // get our random variance value, the amount from which we're deviating from
  // the center. with no variance, we will always split right down the middle of
  // a room. this variance value will always be between -1 and 1, inclusive.
  randomVariance := Random * 2 - 1;

  midPoint := (min + max) / 2;

  midPoint := midPoint + (randomVariance * variance * (max - min) / 2);

  Result := midPoint;
end;

// shrink down each room some amount based on sizeVariance
procedure TForm1.Shrink;
var
  roomAt: TRoom;
  doorAt: TDoor;

  horizScale, vertScale, currWidth, newWidth, currHeight, newHeight,
    centerWidth, centerHeight, doorOffsetX, doorOffsetY: Extended;
begin
  for roomAt in rooms do
  begin
    // shrink horizontally first
    horizScale := (1 - sizeVariance * Random);
    currWidth := roomAt.rightWall - roomAt.leftWall;
    newWidth := currWidth * horizScale;

    // at minimum, each room must be 2 units smaller to create some space in
    // between each room
    if currWidth - newWidth < 2 then
      newWidth := currWidth - 2;

    // but rooms also must conform to the minimum size
    if newWidth < minSize then
      newWidth := minSize;

    centerWidth := (roomAt.rightWall + roomAt.leftWall) / 2;

    // adjust vertical walls according to new width
    roomAt.rightWall := centerWidth + newWidth / 2;
    roomAt.leftWall := centerWidth - newWidth / 2;

    // now do the same process, but vertically
    vertScale := (1 - sizeVariance * Random);
    currHeight := roomAt.topWall - roomAt.bottomWall;
    newHeight := currHeight * vertScale;

    if currHeight - newHeight < 2 then
      newHeight := currHeight - 2;

    if newHeight < minSize then
      newHeight := minSize;

    centerHeight := (roomAt.bottomWall + roomAt.topWall) / 2;

    roomAt.bottomWall := centerHeight - newHeight / 2;
    roomAt.topWall := centerHeight + newHeight / 2;

    horizScale := newWidth / currWidth;
    vertScale := newHeight / currHeight;

    // also need to adjust the doors so they're always inside a wall
    for doorAt in roomAt.doors do
    begin
      doorOffsetX := doorAt.x - centerWidth;
      doorAt.x := horizScale * doorOffsetX + centerWidth;

      doorOffsetY := doorAt.y - centerHeight;
      doorAt.y := vertScale * doorOffsetY + centerHeight;
    end;
  end;
end;

// actually print the map to the textbox
//
// '.' = void space
// ' ' = open space (in a room)
// '+' = room corner
// 'O' = tunnel
// 'H' = vertical door
// 'I' = horizontal door
// '|' = vertical wall
// '-' = horizontal wall
function TForm1.Print: TStringList;
var
  map: TMap;

  colAt, rowAt: Integer;

  roomAt: TRoom;
  doorAt: TDoor;

  left, right, top, bottom: Integer;

  output: TStringList;
begin
  // set size of 2d char array
  SetLength(map, dungeonHeight, dungeonWidth);

  // begin by filling entire space with void. it will be drawn over later.
  for colAt := 0 to dungeonWidth - 1 do
    for rowAt := 0 to dungeonHeight - 1 do
      map[rowAt, colAt] := '.';

  for roomAt in rooms do
  begin
    left := Trunc(roomAt.leftWall);
    right := Trunc(roomAt.rightWall);
    top := Trunc(roomAt.topWall);
    bottom := Trunc(roomAt.bottomWall);

    // place vertical lines for sides of rooms, ignoring corners
    for colAt := left + 1 to right - 1 do
    begin
      map[colAt][bottom] := '|';
      map[colAt][top] := '|';
    end;

    // place horizontal lines for top and bottom of rooms, ignoring corners
    for rowAt := bottom + 1 to top - 1 do
    begin
      map[left][rowAt] := '-';
      map[right][rowAt] := '-';
    end;

    // fill rooms with empty space
    for colAt := left + 1 to right - 1 do
      for rowAt := bottom + 1 to top - 1 do
        map[colAt, rowAt] := ' ';

    // apply corner tiles for each room
    map[left, bottom] := '+';
    map[right, bottom] := '+';
    map[left, top] := '+';
    map[right, top] := '+';

    // apply room id
    map[Trunc((left + right) / 2)][Trunc((bottom + top) / 2)] := roomAt.id;

    // handle doors
    for doorAt in roomAt.doors do
    begin
      colAt := Trunc(doorAt.x);
      rowAt := Trunc(doorAt.y);

      if doorAt.isHorizontal then
        map[colAt, rowAt] := 'H'
      else
        map[colAt][rowAt] := 'I';

      ConnectDoors(map, doorAt);
    end;
  end;

  // now need to actually print it
  output := TStringList.Create;
  for colAt := 0 to dungeonHeight - 1 do
  begin
    output.Add(ArrayToString(map[colAt]));
  end;

  TextBox.Lines := output;

  Result := output;
end;

// create connections between each door
procedure TForm1.ConnectDoors(map: TMap; doorAt: TDoor);
var
  midPoint, fromCol, toCol, fromRow, toRow, temp, colAt, rowAt: Integer;
begin
  if doorAt.isHorizontal then
  begin
    midPoint := Trunc(doorAt.divDim);

    fromCol := Trunc(doorAt.x);
    toCol := Trunc(doorAt.other.x);

    fromRow := Trunc(doorAt.y);
    toRow := Trunc(doorAt.other.y);

    if fromCol > toCol then
    begin
      temp := fromCol;
      fromCol := toCol;
      toCol := temp;

      temp := fromRow;
      fromRow := toRow;
      toRow := temp;
    end;

    for colAt := fromCol + 1 to midPoint do
      map[colAt, fromRow] := 'O';

    for colAt := midPoint to toCol - 1 do
      map[colAt, toRow] := 'O';

    if fromRow > toRow then
    begin
      temp := fromRow;
      fromRow := toRow;
      toRow := temp;
    end;

    for rowAt := fromRow + 1 to toRow - 1 do
      map[midPoint, rowAt] := 'O';
  end
  else
  begin
    midPoint := Trunc(doorAt.divDim);

    fromCol := Trunc(doorAt.x);
    toCol := Trunc(doorAt.other.x);

    fromRow := Trunc(doorAt.y);
    toRow := Trunc(doorAt.other.y);

    if fromRow > toRow then
    begin
      temp := fromRow;
      fromRow := toRow;
      toRow := temp;

      temp := fromCol;
      fromCol := toCol;
      toCol := temp;
    end;

    for rowAt := fromRow + 1 to midPoint do
      map[fromCol][rowAt] := 'O';

    for rowAt := midPoint to toRow - 1 do
      map[toCol][rowAt] := 'O';

    if fromCol > toCol then
    begin
      temp := fromCol;
      fromCol := toCol;
      toCol := temp;
    end;

    for colAt := fromCol + 1 to toCol - 1 do
      map[colAt][midPoint] := 'O';
  end;
end;

// helper function to get a string from an array of chars
function TForm1.ArrayToString(const arr: array of Char): string;
begin
  if Length(arr) > 0 then
    SetString(Result, PChar(@arr[0]), Length(arr))
  else
    Result := '';
end;

end.
