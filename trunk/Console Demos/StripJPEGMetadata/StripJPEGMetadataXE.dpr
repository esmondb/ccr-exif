program StripJPEGMetadataXE;

{$APPTYPE CONSOLE}

{$R *.dres}

uses
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas',
  StripJPEGMetadataImpl in 'StripJPEGMetadataImpl.pas';

begin
end.
