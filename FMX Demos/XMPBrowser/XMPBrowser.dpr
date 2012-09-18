program XMPBrowser;

uses
  FMX.Forms,
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas',
  CCR.Exif.FMXUtils in '..\CCR.Exif.FMXUtils.pas',
  XMPController in 'XMPController.pas' {dtmController: TDataModule},
  XMPForm in 'XMPForm.pas' {frmXMPBrowser};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdtmController, dtmController);
  Application.Run;
end.
