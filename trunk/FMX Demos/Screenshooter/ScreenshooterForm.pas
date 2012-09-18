unit ScreenshooterForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit,
  FMX.ListBox;

type
  TfrmScreenshooter = class(TForm)
    lblAuthor: TLabel;
    edtAuthor: TEdit;
    edtTitle: TEdit;
    lblTitle: TLabel;
    edtSubject: TEdit;
    edtComments: TEdit;
    lblComments: TLabel;
    lblSubject: TLabel;
    edtKeywords: TEdit;
    lblKeywords: TLabel;
    lblRating: TLabel;
    cboRating: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    chkWriteThumbnail: TCheckBox;
    chkWriteXMP: TCheckBox;
    btnCreate: TButton;
    btnClose: TButton;
    dlgSave: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmScreenshooter: TfrmScreenshooter;

implementation

{$R *.fmx}

uses CCR.Exif, CCR.Exif.Consts, ScreenshooterUtils, CCR.Exif.FMXUtils;

procedure TfrmScreenshooter.FormCreate(Sender: TObject);
begin
  edtAuthor.Text := GetUserDisplayName;
  edtComments.Text := edtComments.Text + CCRExifVersion;
end;

procedure TfrmScreenshooter.btnCreateClick(Sender: TObject);
var
  ExifData: TExifData;
  Stream: TMemoryStream;
  TempBitmap: TBitmap;
begin
  Stream := TMemoryStream.Create;
  try
    Hide;
    Sleep(1000);
    Beep;
    TakeJpegScreenshot(Stream);
    Show;
    ExifData := TExifData.Create;
    try
      if chkWriteXMP.IsChecked then //default is to only update an XMP property if it already exists
        ExifData.XMPWritePolicy := xwAlwaysUpdate;
      ExifData.SetAllDateTimeValues(Now);
      ExifData.Author := edtAuthor.Text;
      ExifData.CameraMake := 'n/a';
      ExifData.CameraModel := 'n/a';
      ExifData.Comments := edtComments.Text;
      ExifData.Keywords := edtKeywords.Text;
      ExifData.Software := Application.Title;
      ExifData.Subject := edtSubject.Text;
      ExifData.Title := edtTitle.Text;
      ExifData.UserRating := TWindowsStarRating(cboRating.ItemIndex);
      if chkWriteThumbnail.IsChecked then
      begin
        Stream.Position := 0;
        TempBitmap := TBitmap.CreateFromStream(Stream);
        try
          ExifData.CreateThumbnail(TempBitmap);
        finally
          TempBitmap.Free;
        end;
      end;
      ExifData.SaveToGraphic(Stream);
    finally
      ExifData.Free;
    end;
    if not dlgSave.Execute then Exit;
    Stream.SaveToFile(dlgSave.FileName);
  finally
    Stream.Free;
  end;
  if MessageDlg('Created screenshot. Open it now?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    TFileManager.ShellExecute(dlgSave.FileName);
end;

procedure TfrmScreenshooter.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
