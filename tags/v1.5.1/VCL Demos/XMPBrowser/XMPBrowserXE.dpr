program XMPBrowserXE;

uses
  Forms,
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas',
  CCR.Exif.Demos in '..\CCR.Exif.Demos.pas',
  XMPBrowserForm in 'XMPBrowserForm.pas' {frmXMPBrowser},
  XMPBrowserFrame in 'XMPBrowserFrame.pas' {OutputFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmXMPBrowser, frmXMPBrowser);
  Application.Run;
end.
