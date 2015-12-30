CCR Exif is a Delphi library to read and write Exif, IPTC and XMP metadata from JPEG, TIFF and PSD images. Requires Delphi 2006 or later to compile, preferably Delphi 2007 or later. Also works with Win64 or OS X targets in Delphi XE2+, though not XE2/iOS because FPC is not supported.

**Important** - due to difficulties in supporting both the VCL `TJpegImage` and the FMX `TBitmap` in the same unit, use in a FireMonkey project requires manually setting a `FMX` conditional define at the project level. Further, for mobile, only XE5 is supported, and only with Andreas Hausladen's [string types patch](http://andy.jgknet.de/blog/2013/10/the-return-of-the-byte-strings/) applied.

**_Example 1: reading the camera make and model tags from an image's Exif metadata:_**
```
uses
  CCR.Exif;

procedure ReadCameraMakeAndModel(const FileName: string; out Make, Model: string);
var
  ExifData: TExifData;
begin
  ExifData := TExifData.Create;
  try
    ExifData.LoadFromGraphic(FileName); //LoadFromJPEG before v1.5.0   
    Make  := ExifData.CameraMake;  //returns an empty string if tag doesn't exist
    Model := ExifData.CameraModel; //ditto
  finally
    ExifData.Free;
  end;
end;
```

**_Example 2: setting some Exif tags:_**
```
uses
  CCR.Exif;

procedure SetSomeExifTags(const FileName: string);
var
  ExifData: TExifData;
begin
  ExifData := TExifData.Create;
  try
    ExifData.LoadFromGraphic(FileName);
    ExifData.Subject := 'Wimbledon Tennis';
    ExifData.SetKeyWords(['tennis', 'Wimbledon', 'match', 'SW19']);
    //Exif uses the degrees/minutes/seconds GPS format
    ExifData.GPSLatitude.Assign(51, 26, 1.48, ltNorth);
    ExifData.GPSLongitude.Assign(0, 12, 50.63, lnWest);
    ExifData.SaveToGraphic(FileName); 
  finally
    ExifData.Free;
  end;
end;
```

**_Example 3: setting the 'headline' and 'credit' IPTC tags:_**
```
uses
  CCR.Exif.IPTC;

procedure SetIPTCHeadlineAndCredit(const FileName,
  NewHeadline, NewCredit: string);
var
  IPTCData: TIPTCData;
begin
  IPTCData := TIPTCData.Create;
  try
    IPTCData.LoadFromGraphic(FileName);
    IPTCData.Headline := NewHeadline;
    IPTCData.Credit := NewCredit;
    IPTCData.SaveToGraphic(FileName);
  finally
    IPTCData.Free;
  end;
end;
```

For more information about the API, please refer to the included PDF, available [here](http://ccr-exif.googlecode.com/svn/trunk/Documentation.pdf). A small set of console and GUI demos is also included with the main source.