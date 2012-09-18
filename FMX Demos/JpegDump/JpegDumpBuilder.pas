unit JpegDumpBuilder;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, CCR.Exif.BaseUtils;

type
  TJpegDumpBuilder = class(TStringBuilder)
  strict private
    FExifThumbnail: TBitmap;
    procedure DoDefault(const Segment: IFoundJPEGSegment;const BlockName: string;
      LeaveBlankLine: Boolean = True; const Comment: string = '');
    procedure LoadAdobeApp13(const Segment: IFoundJPEGSegment);
    procedure LoadExif(const Segment: IFoundJPEGSegment);
    procedure LoadJFIF(const Segment: IFoundJPEGSegment);
    procedure LoadSOF(const Segment: IFoundJPEGSegment);
    procedure LoadSOS(const Segment: IFoundJPEGSegment);
    procedure LoadXMP(const Segment: IFoundJPEGSegment);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Load(const AFileName: string);
    function AppendLine(const Value: string; const Args: array of const): TStringBuilder; overload;
    property ExifThumbnail: TBitmap read FExifThumbnail;
  end;

implementation

uses CCR.Exif, CCR.Exif.IPTC, CCR.Exif.TagIDs, CCR.Exif.XMPUtils;

const
  SEndianness: array[TEndianness] of string = ('Small endian', 'Big endian');
  SectionNames: array[TExifSectionKind] of string = ('Main IFD', 'Exif sub-IFD',
    'Interop sub-IFD', 'GPS sub-IFD', 'Thumbnail IFD', 'Maker note');
  SDataType: array[TExifDataType] of string = ('Byte', 'ASCII string', 'Word',
    'LongWord', 'Fraction', 'ShortInt', 'Undefined', 'SmallInt', 'LongInt',
    'Signed fraction', 'Single', 'Double', 'SubDirOffset');
  SDataOffsetsType: array[TExifDataOffsetsType] of string = ('From start of Exif data',
    'From MakerNote start', 'From MakerNote IFD start', 'Not a standard IFD format');

function TagIDToStr(const Tag: TExifTag): string;
begin
  if Tag.IsPadding then
  begin
    Result := 'ttWindowsPadding';
    Exit;
  end;
  FmtStr(Result, '$%.4x', [Tag.ID]);
  case Tag.Section.Kind of
    esGeneral, esThumbnail:
      case Tag.ID of
        ttImageDescription          : Result := 'ttImageDescription';
        ttMake                      : Result := 'ttMake';
        ttModel                     : Result := 'ttModel';
        ttOrientation               : Result := 'ttOrientation';
        ttXResolution               : Result := 'ttXResolution';
        ttYResolution               : Result := 'ttYResolution';
        ttResolutionUnit            : Result := 'ttResolutionUnit';
        ttSoftware                  : Result := 'ttSoftware';
        ttDateTime                  : Result := 'ttDateTime';
        ttArtist                    : Result := 'ttArtist';
        ttWhitePoint                : Result := 'ttWhitePoint';
        ttPrimaryChromaticities     : Result := 'ttPrimaryChromaticities';
        ttYCbCrCoefficients         : Result := 'ttYCbCrCoefficients';
        ttYCbCrPositioning          : Result := 'ttYCbCrPositioning';
        ttReferenceBlackWhite       : Result := 'ttReferenceBlackWhite';
        ttCopyright                 : Result := 'ttCopyright';
        ttExifOffset                : Result := 'ttExifOffset';
        ttGPSOffset                 : Result := 'ttGPSOffset';
        ttPrintIM                   : Result := 'ttPrintIM';
        ttWindowsTitle              : Result := 'ttWindowsTitle';
        ttWindowsComments           : Result := 'ttWindowsComments';
        ttWindowsAuthor             : Result := 'ttWindowsAuthor';
        ttWindowsKeywords           : Result := 'ttWindowsKeywords';
        ttWindowsSubject            : Result := 'ttWindowsSubject';
        ttWindowsRating             : Result := 'ttWindowsRating';
      else
        if Tag.Section.Kind = esThumbnail then
          case Tag.ID of
            ttImageWidth                : Result := 'ttImageWidth';
            ttImageHeight               : Result := 'ttImageHeight';
            ttBitsPerSample             : Result := 'ttBitsPerSample';
            ttCompression               : Result := 'ttCompression';
            ttPhotometricInterp         : Result := 'ttPhotometricInterp';
            ttStripOffsets              : Result := 'ttStripOffsets';
            ttSamplesPerPixel           : Result := 'ttSamplesPerPixel';
            ttRowsPerStrip              : Result := 'ttRowsPerStrip';
            ttStripByteCounts           : Result := 'ttStripByteCounts';
            ttPlanarConfiguration       : Result := 'ttPlanarConfiguration';
            ttJpegIFOffset              : Result := 'ttJpegIFOffset';
            ttJpegIFByteCount           : Result := 'ttJpegIFByteCount';
          end;
      end;
    esDetails:
      case Tag.ID of
        ttExposureTime              : Result := 'ttExposureTime';
        ttFNumber                   : Result := 'ttFNumber';
        ttExposureProgram           : Result := 'ttExposureProgram';
        ttSpectralSensitivity       : Result := 'ttSpectralSensitivity';
        ttISOSpeedRatings           : Result := 'ttISOSpeedRatings';
        ttExifVersion               : Result := 'ttExifVersion';
        ttDateTimeOriginal          : Result := 'ttDateTimeOriginal';
        ttDateTimeDigitized         : Result := 'ttDateTimeDigitized';
        ttComponentsConfiguration   : Result := 'ttComponentsConfiguration';
        ttCompressedBitsPerPixel    : Result := 'ttCompressedBitsPerPixel';
        ttShutterSpeedValue         : Result := 'ttShutterSpeedValue';
        ttApertureValue             : Result := 'ttApertureValue';
        ttBrightnessValue           : Result := 'ttBrightnessValue';
        ttExposureBiasValue         : Result := 'ttExposureBiasValue';
        ttMaxApertureValue          : Result := 'ttMaxApertureValue';
        ttSubjectDistance           : Result := 'ttSubjectDistance';
        ttMeteringMode              : Result := 'ttMeteringMode';
        ttLightSource               : Result := 'ttLightSource';
        ttFlash                     : Result := 'ttFlash';
        ttFocalLength               : Result := 'ttFocalLength';
        ttMakerNote                 : Result := 'ttMakerNote';
        ttUserComment               : Result := 'ttUserComment';
        ttSubsecTime                : Result := 'ttSubsecTime';
        ttSubsecTimeOriginal        : Result := 'ttSubsecTimeOriginal';
        ttSubsecTimeDigitized       : Result := 'ttSubsecTimeDigitized';
        ttFlashPixVersion           : Result := 'ttFlashPixVersion';
        ttColorSpace                : Result := 'ttColorSpace';
        ttExifImageWidth            : Result := 'ttExifImageWidth';
        ttExifImageHeight           : Result := 'ttExifImageHeight';
        ttRelatedSoundFile          : Result := 'ttRelatedSoundFile';
        ttInteropOffset             : Result := 'ttInteropOffset';
        ttFlashEnergy               : Result := 'ttFlashEnergy';
        ttSpatialFrequencyResponse  : Result := 'ttSpatialFrequencyResponse';
        ttFocalPlaneXResolution     : Result := 'ttFocalPlaneXResolution';
        ttFocalPlaneYResolution     : Result := 'ttFocalPlaneYResolution';
        ttFocalPlaneResolutionUnit  : Result := 'ttFocalPlaneResolutionUnit';
        ttSubjectLocation           : Result := 'ttSubjectLocation';
        ttExposureIndex             : Result := 'ttExposureIndex';
        ttSensingMethod             : Result := 'ttSensingMethod';
        ttFileSource                : Result := 'ttFileSource';
        ttSceneType                 : Result := 'ttSceneType';
        ttCFAPattern                : Result := 'ttCFAPattern';
        ttCustomRendered            : Result := 'ttCustomRendered';
        ttExposureMode              : Result := 'ttExposureMode';
        ttWhiteBalance              : Result := 'ttWhiteBalance';
        ttDigitalZoomRatio          : Result := 'ttDigitalZoomRatio';
        ttFocalLengthIn35mmFilm     : Result := 'ttFocalLengthIn35mmFilm';
        ttSceneCaptureType          : Result := 'ttSceneCaptureType';
        ttGainControl               : Result := 'ttGainControl';
        ttContrast                  : Result := 'ttContrast';
        ttSaturation                : Result := 'ttSaturation';
        ttSharpness                 : Result := 'ttSharpness';
        ttDeviceSettingDescription  : Result := 'ttDeviceSettingDescription';
        ttSubjectDistanceRange      : Result := 'ttSubjectDistanceRange';
        ttImageUniqueID             : Result := 'ttImageUniqueID';
        ttOffsetSchema              : Result := 'ttOffsetSchema';
        ttCameraOwnerName           : Result := 'ttCameraOwnerName';
        ttBodySerialNumber          : Result := 'ttBodySerialNumber';
        ttLensSpecification         : Result := 'ttLensSpecification';
        ttLensMake                  : Result := 'ttLensMake';
        ttLensModel                 : Result := 'ttLensModel';
        ttLensSerialNumber          : Result := 'ttLensSerialNumber';
      end;
    esInterop:
      case Tag.ID of
        ttInteropIndex              : Result := 'ttInteropIndex';
        ttInteropVersion            : Result := 'ttInteropVersion';
        ttRelatedImageFileFormat    : Result := 'ttRelatedImageFileFormat';
        ttRelatedImageWidth         : Result := 'ttRelatedImageWidth';
        ttRelatedImageLength        : Result := 'ttRelatedImageLength';
      end;
    esGPS:
      case Tag.ID of
        ttGPSVersionID              : Result := 'ttGPSVersionID';
        ttGPSLatitudeRef            : Result := 'ttGPSLatitudeRef';
        ttGPSLatitude               : Result := 'ttGPSLatitude';
        ttGPSLongitudeRef           : Result := 'ttGPSLongitudeRef';
        ttGPSLongitude              : Result := 'ttGPSLongitude';
        ttGPSAltitudeRef            : Result := 'ttGPSAltitudeRef';
        ttGPSAltitude               : Result := 'ttGPSAltitude';
        ttGPSTimeStamp              : Result := 'ttGPSTimeStamp';
        ttGPSSatellites             : Result := 'ttGPSSatellites';
        ttGPSStatus                 : Result := 'ttGPSStatus';
        ttGPSMeasureMode            : Result := 'ttGPSMeasureMode';
        ttGPSDOP                    : Result := 'ttGPSDOP';
        ttGPSSpeedRef               : Result := 'ttGPSSpeedRef';
        ttGPSSpeed                  : Result := 'ttGPSSpeed';
        ttGPSTrackRef               : Result := 'ttGPSTrackRef';
        ttGPSTrack                  : Result := 'ttGPSTrack';
        ttGPSImgDirectionRef        : Result := 'ttGPSImgDirectionRef';
        ttGPSImgDirection           : Result := 'ttGPSImgDirection';
        ttGPSMapDatum               : Result := 'ttGPSMapDatum';
        ttGPSDestLatitudeRef        : Result := 'ttGPSDestLatitudeRef';
        ttGPSDestLatitude           : Result := 'ttGPSDestLatitude';
        ttGPSDestLongitudeRef       : Result := 'ttGPSDestLongitudeRef';
        ttGPSDestLongitude          : Result := 'ttGPSDestLongitude';
        ttGPSDestBearingRef         : Result := 'ttGPSDestBearingRef';
        ttGPSDestBearing            : Result := 'ttGPSDestBearing';
        ttGPSDestDistanceRef        : Result := 'ttGPSDestDistanceRef';
        ttGPSDestDistance           : Result := 'ttGPSDestDistance';
        ttGPSProcessingMethod       : Result := 'ttGPSProcessingMethod';
        ttGPSAreaInformation        : Result := 'ttGPSAreaInformation';
        ttGPSDateStamp              : Result := 'ttGPSDateStamp';
        ttGPSDifferential           : Result := 'ttGPSDifferential';
      end;
  end;
end;

{ TJpegDumpBuilder }

procedure TJpegDumpBuilder.AfterConstruction;
begin
  FExifThumbnail := TBitmap.Create(0, 0);
  inherited;
end;

destructor TJpegDumpBuilder.Destroy;
begin
  FExifThumbnail.Free;
  inherited;
end;

procedure TJpegDumpBuilder.Load(const AFileName: string);
var
  Segment: IFoundJPEGSegment;

  procedure DoUnrecognised;
  begin
    if Segment.MarkerNum in [jmAppSpecificFirst..jmAppSpecificLast] then
      DoDefault(Segment, Format('APP%d', [Segment.MarkerNum - jmAppSpecificFirst]))
    else
      DoDefault(Segment, Format('Unrecognised marker ($%.2x)', [Segment.MarkerNum]));
  end;
var
  AnsiStr: AnsiString;
  Len: Integer;
begin
  for Segment in JPEGHeader(AFileName) do
    case Segment.MarkerNum of
      jmStartOfScan:
        LoadSOS(Segment);
      jmEndOfImage: DoDefault(Segment, 'End of image (EOI)');
      jmJFIF:
        LoadJFIF(Segment);
      jmStartOfFrame0..jmStartOfFrame3, jmStartOfFrame5..jmStartOfFrame7:
        LoadSOF(Segment);
      jmRestartInternal:
      begin
        DoDefault(Segment, 'Restart interval', False);
        if Segment.Data.Size >= 2 then
          AppendLine('Value'#9'%d', [PWord(Segment.Data.Memory)^]);
        AppendLine;
      end;
      jmComment:
      begin
        DoDefault(Segment, 'Comment', False);
        //*should* be null terminated, but we'll play safe
        Len := Segment.Data.Size;
        if (Len > 0) and (PAnsiChar(Segment.Data.Memory)[Len - 1] = #0) then
          Dec(Len);
        SetString(AnsiStr, PAnsiChar(Segment.Data.Memory), Len);
        AppendLine('Value'#9'%s', [string(AnsiStr)]);
        AppendLine;
      end;
      jmApp1:
        if Segment.IsExifBlock then
          LoadExif(Segment)
        else if Segment.IsXMPBlock then
          LoadXMP(Segment)
        else
          DoUnrecognised;
      jmApp13:
        if Segment.IsAdobeApp13 then
          LoadAdobeApp13(Segment)
        else
          DoUnrecognised;
      jmQuantizationTable: DoDefault(Segment, 'Quantisation table definition(s)');
      jmHuffmanTable: DoDefault(Segment, 'Huffman table definition(s)');
    else
      DoUnrecognised;
    end;
end;

function TJpegDumpBuilder.AppendLine(const Value: string; const Args: array of const): TStringBuilder;
begin
  Result := AppendLine(Format(Value, Args));
end;

procedure TJpegDumpBuilder.DoDefault(const Segment: IFoundJPEGSegment;
  const BlockName: string; LeaveBlankLine: Boolean = True; const Comment: string = '');
begin
  Append('--- ' + BlockName);
  if Comment <> '' then Append(' (' + Comment + ') ');
  AppendLine(' ---');
  AppendLine('Segment offset'#9'$%.4x', [Segment.Offset]);
  if (Segment.MarkerNum in TJPEGSegment.MarkersWithNoData) and (Segment.Data.Size = 0) then
    AppendLine('Segment has no data')
  else
  begin
    AppendLine('Total size of segment'#9'%d bytes', [Segment.TotalSize]);
    if LeaveBlankLine and (Segment.Data.Size <= 128) then
      AppendLine('Data'#9'%s', [BinToHexStr(Segment.Data)]);
  end;
  if LeaveBlankLine then AppendLine;
end;

procedure TJpegDumpBuilder.LoadAdobeApp13(const Segment: IFoundJPEGSegment);
const
  SYesNo: array[Boolean] of string = ('No', 'Yes');
var
  Block: IAdobeResBlock;
  Counter: Integer;
  FoundIPTC: Boolean;
  IPTCData: TIPTCData;
  S: string;
  Section: TIPTCSection;
  Tag: TIPTCTag;
begin
  DoDefault(Segment, 'Adobe Photoshop (APP13)', False);
  Counter := 0;
  FoundIPTC := False;
  for Block in Segment do
  begin
    Inc(Counter);
    FoundIPTC := FoundIPTC or Block.IsIPTCBlock;
  end;
  AppendLine('Contains IPTC data'#9'%s', [SYesNo[FoundIPTC]]);
  AppendLine('Number of data blocks'#9'%d', [Counter]);
  AppendLine;
  IPTCData := TIPTCData.Create;
  try
    Counter := 0;
    for Block in Segment do
    begin
      Inc(Counter);
      if Block.IsIPTCBlock then
        AppendLine('Data block %d (IPTC):', [Counter])
      else
        AppendLine('Data block %d:', [Counter]);
      AppendLine('Signature'#9'%s', [string(Block.Signature)]);
      AppendLine('Type ID'#9'$%.4x', [Block.TypeID]);
      if Block.Name <> '' then
        S := string(Block.Name)
      else
        S := '(none)';
      AppendLine('Name'#9'%s', [S]);
      AppendLine('Data size'#9'%d bytes', [Block.Data.Size]);
      if Block.IsIPTCBlock then
      begin
        IPTCData.LoadFromStream(Block.Data);
        for Section in IPTCData do
          for Tag in Section do
            AppendLine('%d:%d'#9'%s', [Tag.Section.ID, Tag.ID, Tag.AsString]);
      end
      else if Block.Data.Size <= 128 then
        AppendLine('Data'#9'%s', [BinToHexStr(Block.Data.Memory, Block.Data.Size)]);
      AppendLine;
    end;
  finally
    IPTCData.Free;
  end;
end;

procedure TJpegDumpBuilder.LoadExif(const Segment: IFoundJPEGSegment);
var
  ExifData: TExifData;
  S: string;
  Section: TExifSection;
  Tag: TExifTag;
begin
  DoDefault(Segment, 'Exif', False);
  ExifData := TExifData.Create;
  try
    ExifData.LoadFromStream(Segment.Data);
    AppendLine('Byte order of main structure'#9'' + SEndianness[ExifData.Endianness]);
    AppendLine('');
    for Section in ExifData do
      if Section.Count > 0 then
      begin
        AppendLine('%s (%d tags):', [SectionNames[Section.Kind], Section.Count]);
        if leBadOffset in Section.LoadErrors then
          AppendLine('*** Bad offset ***');
        if leBadTagCount in Section.LoadErrors then
          AppendLine('*** Claims to contain more tags than could be parsed ***');
        if leBadTagHeader in Section.LoadErrors then
          AppendLine('*** Contains one or more malformed tag headers ***');
        if Section.Kind = esMakerNote then
          if ExifData.MakerNote is TUnrecognizedMakerNote then
            AppendLine('Unrecognised type - couldn''t parse tag structure')
          else
          begin
            S := Copy(ExifData.MakerNote.ClassName, 2, MaxInt);
            AppendLine('Type'#9'' + Copy(S, 1, System.Length(S) - 9));
            AppendLine('Byte order'#9'' + SEndianness[ExifData.MakerNote.Endianness]);
            AppendLine('Data offsets'#9'' + SDataOffsetsType[ExifData.MakerNote.DataOffsetsType]);
            //AppendLine;
          end;
        for Tag in Section do
        begin
          if not Tag.WellFormed then
            S := '*** Malformed tag header ***'
          else if (Tag.ID = ttMakerNote) and (Section.Kind = esDetails) and
             (ExifData.MakerNote.Tags.Count <> 0) then
            S := '[Recognised - see below]'
          else
            S := Tag.AsString;
          AppendLine('%s'#9'%s (%d)'#9'%s', [TagIDToStr(Tag), SDataType[Tag.DataType],
            Tag.ElementCount, S]);
        end;
        AppendLine;
      end;
    if ExifData.HasThumbnail then
      FExifThumbnail.Assign(ExifData.Thumbnail);
  finally
    ExifData.Free;
  end;
end;

procedure TJpegDumpBuilder.LoadJFIF(const Segment: IFoundJPEGSegment);
var
  JFIFHeader: TJFIFData;
begin
  Segment.Data.ReadBuffer(JFIFHeader, SizeOf(JFIFHeader));
  DoDefault(Segment, 'JFIF header', False);
  AppendLine('Ident'#9'' + JFIFHeader.Ident, []);
  AppendLine('Version'#9'%d.%d', [JFIFHeader.MajorVersion, JFIFHeader.MinorVersion]);
  AppendLine('Density units'#9'%d', [Ord(JFIFHeader.DensityUnits)]);
  AppendLine('Horz density'#9'%d', [JFIFHeader.HorzDensity]);
  AppendLine('Vert density'#9'%d', [JFIFHeader.VertDensity]);
  { The JFIF thumbnail is not the same thing as the Exif thumbnail, so don't
    be surprised at the next two values being 0. }
  if (JFIFHeader.ThumbnailWidth <> 0) or (JFIFHeader.ThumbnailHeight <> 0) then
  begin
    AppendLine('Thumbnail width'#9'%d', [JFIFHeader.ThumbnailWidth]);
    AppendLine('Thumbnail height'#9'%d', [JFIFHeader.ThumbnailHeight]);
  end;
  AppendLine;
end;

procedure TJpegDumpBuilder.LoadSOF(const Segment: IFoundJPEGSegment);
var
  SOFData: PJPEGStartOfFrameData;
  Str: string;
begin
  case Segment.MarkerNum of
    jmStartOfFrame0: Str := '(baseline DCT)';
    jmStartOfFrame1: Str := '(extended sequential DCT)';
    jmStartOfFrame2: Str := '(progressive DCT)';
    jmStartOfFrame3: Str := '(lossless sequential)';
    jmStartOfFrame5: Str := '(differential sequential DCT)';
    jmStartOfFrame6: Str := '(differential progressive DCT)';
    jmStartOfFrame7: Str := '(differential lossless sequential)';
  else Str := IntToStr(Segment.MarkerNum - jmStartOfFrame0);
  end;
  DoDefault(Segment, 'Start of frame ' + Str, False);
  SOFData := Segment.Data.Memory;
  if Segment.Data.Size >= SizeOf(TJPEGStartOfFrameData) then
  begin
    AppendLine('Sample precision'#9'%d', [SOFData.SamplePrecision]);
    AppendLine('Width'#9'%d', [SOFData.ImageWidth]);
    AppendLine('Height'#9'%d', [SOFData.ImageHeight]);
    AppendLine('Component count'#9'%d', [SOFData.ComponentCount]);
  end;
  AppendLine;
end;

procedure TJpegDumpBuilder.LoadSOS(const Segment: IFoundJPEGSegment);
begin
  DoDefault(Segment, 'Start of scan (SOS)');
end;

function GetXMPNamespaceTitle(Schema: TXMPSchema): string;
begin
  case Schema.NamespaceInfo.Kind of
    xsCameraRaw: Result := 'Camera Raw';
    xsColorant: Result :='Dublin Core';
    xsDimensions: Result := 'Dimensions';
    xsDublinCore: Result := 'Dublin Core';
    xsExif: Result := 'Exif';
    xsExifAux: Result := 'Exif Auxiliary';
    xsMicrosoftPhoto: Result := 'Microsoft Photo';
    xsPDF: Result := 'PDF';
    xsPhotoshop: Result := 'Photoshop';
    xsResourceEvent: Result := 'Resource Event';
    xsResourceRef: Result := 'Resource Reference';
    xsTIFF: Result := 'TIFF';
    xsXMPBasic: Result := 'XMP Basic';
    xsXMPBasicJobTicket: Result := 'XMP Basic Job Ticket';
    xsXMPDynamicMedia: Result := 'XMP Dynamic Media';
    xsXMPMediaManagement: Result := 'XMP Media Management';
    xsXMPPagedText: Result := 'XMP Paged Text';
    xsXMPRights: Result := 'XMP Rights';
  else Result := Schema.NamespaceInfo.Prefix;
  end;
end;

procedure TJpegDumpBuilder.LoadXMP(const Segment: IFoundJPEGSegment);
  procedure DoIt(Level: Integer; const Name: string; const Props: IXMPPropertyCollection);
  const
    SPropKinds: array[TXMPPropertyKind] of string =
      ('simple', 'structure', 'alternative (''alt'') array', 'unordered (''bag'') array',
       'ordered (''seq'') array');
  var
    Counter: Integer;
    Prop: TXMPProperty;
    S: string;
  begin
    if Level <= 1 then
      AppendLine(Name)
    else
    begin
      S := StringOfChar(' ', Pred(Level) * 3) + Name;
      AppendLine(S);
    end;
    Counter := 0;
    for Prop in Props do
    begin
      Inc(Counter);
      if (Prop.ParentProperty = nil) or not (Prop.ParentProperty.Kind in [xpBagArray, xpSeqArray]) then
        if Prop.Kind = xpSimple then
          S := Prop.Name
        else
          S := Format('%s'#9'[%s]', [Prop.Name, SPropKinds[Prop.Kind]])
      else
        S := Format('<item %d>', [Counter]);
      if Prop.SubPropertyCount = 0 then
        S := S + #9 + Prop.ReadValue;
      DoIt(Level + 1, S, Prop);
    end;
  end;
var
  I: Integer;
  Schema: TXMPSchema;
  S: string;
  SeekPtr: PAnsiChar;
  StartPos: Integer;
  UTF8Str: UTF8String;
  XMPPacket: TXMPPacket;
begin
  DoDefault(Segment, 'XMP', False);
  XMPPacket := TXMPPacket.Create;
  try
    StartPos := Segment.Data.Position;
    if not XMPPacket.TryLoadFromStream(Segment.Data) then
    begin
      AppendLine;
      AppendLine('*** Failed to parse packet ***');
      AppendLine;
      SeekPtr := Segment.Data.Memory;
      for I := 0 to Segment.Data.Size - 1 do
      begin
        if SeekPtr^ in [#0..#8, #11..#12, #14..#31] then SeekPtr^ := ' ';
        Inc(SeekPtr);
      end;
      SeekPtr := Segment.Data.Memory;
      Inc(SeekPtr, StartPos);
      SetString(UTF8Str, SeekPtr, Segment.Data.Size - StartPos);
      AppendLine(string(UTF8Str));
      AppendLine;
      Exit;
    end;
    S := XMPPacket.AboutAttributeValue;
    if S = '' then S := '(empty or not set)';
    AppendLine('About attribute value'#9'%s', [S]);
    for Schema in XMPPacket do
    begin
      AppendLine;
      DoIt(0, GetXMPNamespaceTitle(Schema) + ':', Schema);
    end;
  finally
    XMPPacket.Free;
  end;
  AppendLine;
end;

end.
