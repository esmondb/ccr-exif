unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts;

type
  TForm1 = class(TForm)
    ImageControl1: TImageControl;
    btnCopy: TButton;
    btnPaste: TButton;
    Layout1: TLayout;
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses CCR.BitmapCopyAndPaste;

procedure TForm1.btnCopyClick(Sender: TObject);
begin
  CopyBitmapToClipboard(ImageControl1.Bitmap);
end;

procedure TForm1.btnPasteClick(Sender: TObject);
begin
  if not PasteBitmapFromClipboard(ImageControl1.Bitmap) then
    MessageDlg('No valid bitmap available to paste.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

end.
