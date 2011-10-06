program BitmapCopyAndPasteTest;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  CCR.BitmapCopyAndPaste in 'CCR.BitmapCopyAndPaste.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
