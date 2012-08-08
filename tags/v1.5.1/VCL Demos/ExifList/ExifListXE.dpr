program ExifListXE;

{$R *.dres}

uses
  Forms,
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas',
  CCR.Exif.Demos in '..\CCR.Exif.Demos.pas' {frmRoundtripOptions},
  ExifListForm in 'ExifListForm.pas' {frmExifList},
  ExifListFrame in 'ExifListFrame.pas' {OutputFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExifList, frmExifList);
  Application.Run;
end.
