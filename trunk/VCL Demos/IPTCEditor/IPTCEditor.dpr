program IPTCEditor;

uses
  Forms,
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.Demos in '..\CCR.Exif.Demos.pas',
  CCR.SizeGripCtrl in 'CCR.SizeGripCtrl.pas',
  IPTCEditForm in 'IPTCEditForm.pas' {frmIPTC};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmIPTC, frmIPTC);
  Application.Run;
end.
