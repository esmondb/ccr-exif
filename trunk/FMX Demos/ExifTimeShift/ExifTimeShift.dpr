program ExifTimeShift;

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
  CCR.Exif.FMXUtils in '..\CCR.Exif.FMXUtils.pas' {$R *.res},
  TimeShiftOptions in 'TimeShiftOptions.pas',
  TimeShiftForm in 'TimeShiftForm.pas' {frmExifTimeShift},
  TimeShiftOptionsForm in 'TimeShiftOptionsForm.pas' {frmOptions},
  ImageForm in 'ImageForm.pas' {frmImage};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmExifTimeShift, frmExifTimeShift);
  Application.Run;
end.
