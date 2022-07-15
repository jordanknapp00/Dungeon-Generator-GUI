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
  private
    function GenerateSeed: Int64;
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

end.
