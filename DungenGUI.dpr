program DungenGUI;

uses
  Vcl.Forms,
  MainWindow in 'MainWindow.pas' {Form1},
  DataStructs in 'DataStructs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
