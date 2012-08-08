program ResavingTestXE;

uses
  Forms,
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas',
  CCR.Exif.Demos in '..\CCR.Exif.Demos.pas',
  ResaveForm in 'ResaveForm.pas' {frmSaveTest},
  ResaveCompleteDlg in 'ResaveCompleteDlg.pas' {frmOpenFile};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSaveTest, frmSaveTest);
  Application.CreateForm(TfrmOpenFile, frmOpenFile);
  Application.Run;
end.
