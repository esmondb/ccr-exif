{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.0 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is ExifListFrame.pas.                                              }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ExifListFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
  ComCtrls, StdCtrls, ExtCtrls, Tabs, CCR.Exif;

type
  TTabSet = class(Tabs.TTabSet) //work around a bug that is actually TPageControl's
  protected
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  end;

  TOutputFrame = class(TFrame)
    grpThumbnail: TGroupBox;
    imgThumbnail: TImage;
    grpTags: TGroupBox;
    TabSet: TTabSet;
    panListViewHost: TPanel;
    shpTop: TShape;
    lsvStandard: TListView;
    shpLeft: TShape;
    shpRight: TShape;
    procedure TabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
  private
    lsvMakerNote: TListView;
    procedure AddValue(ListView: TListView; const Name, Value: string;
      AddToTop: Boolean = False);
    procedure AddLoadErrorsValue(ListView: TListView; Section: TExifSection;
      AddToTop: Boolean = False);
    procedure LoadStandardValues(ExifData: TExifData);
    procedure LoadMakerNoteValues(MakerNote: TExifMakerNote; ValueMap: TCustomIniFile);
  protected
    function ActiveListView: TListView;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const FileName: string; MakerNoteValueMap: TCustomIniFile);
    function CanCopyToClipboard: Boolean;
    procedure CopyToClipboard;
    procedure SelectAll;
  end;

implementation

uses ClipBrd, DateUtils, StrUtils, Themes, CCR.Exif.TiffUtils;

{$R *.dfm}

{$IF CompilerVersion <= 18.0}
  {$DEFINE BUGGYCOMPILER}
{$IFEND}

procedure TTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  Ancestor: TWinControl;
begin
  Ancestor := Parent;
  while Ancestor <> nil do
    if (Ancestor is TTabSheet) then
      if TTabSheet(Ancestor).PageControl.ActivePage = Ancestor then
        Break
      else
        Exit
    else
      Ancestor := Ancestor.Parent;
  inherited;
end;

const
  SGeneral = 'General';
  SEndiannessCaption = 'Byte order';
  SEndianness: array[TEndianness] of string = ('Small endian', 'Big endian');
  SNoYes: array[Boolean] of string = ('No', 'Yes');
  SUnrecognized = '[Unrecognised value (%d)]';

function ColorSpaceToStr(Value: TExifColorSpace): string;
begin
  case Value of
    csTagMissing: Result := '';
    csRGB: Result := 'sRGB';
    csAdobeRGB: Result := 'Adobe RGB';
    csWideGamutRGB: Result := 'Wide gamut RGB (Sony)';
    csICCProfile: Result := 'ICC profile (Sony)';
    csUncalibrated: Result := 'Uncalibrated (maybe Adobe RGB)';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DirectionRefToStr(Value: TGPSDirectionRef): string;
begin
  case Value of
    drMissingOrInvalid, drTrueNorth: Result := 'true north';
    drMagneticNorth: Result := 'magnetic north';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DistanceRefToStr(Value: TGPSDistanceRef): string;
begin
  case Value of //KM is the default according to the Exif spec
    dsMissingOrInvalid, dsKilometres: Result := 'kilometres';
    dsMiles: Result := 'miles';
    dsKnots: Result := 'knots';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function ExposureProgramToStr(Value: TExifExposureProgram): string;
begin
  case Value of
    eeTagMissing: Result := '';
    eeUndefined: Result := 'Undefined';
    eeManual: Result := 'Manual';
    eeNormal: Result := 'Normal';
    eeAperturePriority: Result := 'Aperture priority';
    eeShutterPriority: Result := 'Shutter priority';
    eeCreative: Result := 'Creative';
    eeAction: Result := 'Action';
    eePortrait: Result := 'Portrait';
    eeLandscape: Result := 'Landscape';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function FileSourceToStr(Value: TExifFileSource): string;
begin
  case Value of
    fsUnknown: Result := '';
    fsFilmScanner: Result := 'Film scanner';
    fsReflectionPrintScanner: Result := 'Reflection print scanner';
    fsDigitalCamera: Result := 'Digital camera';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function FlashModeToStr(Value: TExifFlashMode): string;
begin
  case Value of
    efUnknown: Result := '';
    efCompulsoryFire: Result := 'Compulsory fire';
    efCompulsorySuppression: Result := 'Compulsory suppression';
    efAuto: Result := 'Auto';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GainControlToStr(Value: TExifGainControl): string;
begin
  case Value of
    egTagMissing: Result := '';
    egNone: Result := 'None';
    egLowGainUp: Result := 'Low gain up';
    egHighGainUp: Result := 'High gain up';
    egLowGainDown: Result := 'Low gain down';
    egHighGainDown: Result := 'High gain down';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSAltitudeRefToStr(Value: TGPSAltitudeRef): string;
begin
  case Value of
    alTagMissing: Result := '';
    alAboveSeaLevel: Result := 'above sea level';
    alBelowSeaLevel: Result := 'below sea level';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSDifferentialToStr(Value: TGPSDifferential): string;
begin
  case Value of
    dfTagMissing: Result := '';
    dfWithoutCorrection: Result := 'Without correction';
    dfCorrectionApplied: Result := 'Correction applied';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSMeasureModeToStr(Value: TGPSMeasureMode): string;
begin
  case Value of
    mmUnknown: Result := '';
    mm2D: Result := '2D';
    mm3D: Result := '3D';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSSpeedRefToStr(Value: TGPSSpeedRef): string;
begin
  case Value of
    srMissingOrInvalid, srKilometresPerHour: Result := 'km/h'; //km/h is the default according to the Exit spec
    srMilesPerHour: Result := 'mph';
    srKnots: Result := 'knots';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSStatusToStr(Value: TGPSStatus): string;
begin
  case Value of
    stMissingOrInvalid: Result := '';
    stMeasurementActive: Result := 'Measurement active/in progress';
    stMeasurementVoid: Result := 'Measurement void/in interop';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function LightSourceToStr(Value: TExifLightSource): string;
begin
  case Value of
    elTagMissing: Result := '';
    elUnknown: Result := 'Unknown';
    elDaylight: Result := 'Daylight';
    elFluorescent: Result := 'Fluorescent';
    elTungsten: Result := 'Tungsten';
    elFlash: Result := 'Flash';
    elFineWeather: Result := 'Fine weather';
    elCloudyWeather: Result := 'Cloudy weather';
    elShade: Result := 'Shade';
    elDaylightFluorescent: Result := 'Daylight fluorescent';
    elDayWhiteFluorescent: Result := 'DayWhite fluorescent';
    elCoolWhiteFluorescent: Result := 'Cool white fluorescent';
    elWhiteFluorescent: Result := 'White fluorescent';
    elStandardLightA: Result := 'Standard light A';
    elStandardLightB: Result := 'Standard light B';
    elStandardLightC: Result := 'Standard light C';
    elD55: Result := 'D55';
    elD65: Result := 'D65';
    elD75: Result := 'D75';
    elD50: Result := 'D50';
    elISOStudioTungsten: Result := 'ISO studio tungsten';
    elOther: Result := 'Other';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function MeteringModeToStr(Value: TExifMeteringMode): string;
begin
  case Value of
    emTagMissing: Result := '';
    emUnknown: Result := 'Unknown';
    emAverage: Result := 'Average';
    emCenterWeightedAverage: Result := 'Centre weighted average';
    emSpot: Result := 'Spot';
    emMultiSpot: Result := 'Multiple spot';
    emPattern: Result := 'Pattern';
    emPartial: Result := 'Partial';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function OrientationToStr(Value: TTiffOrientation): string;
begin
  case Value of
    toUndefined: Result := '';
    toTopLeft: Result := 'Normal';
    toTopRight: Result := 'Mirror horizontal';
    toBottomRight: Result := 'Rotate 180°';
    toBottomLeft: Result := 'Mirror vertical';
    toLeftTop: Result := 'Mirrow horizontal and rotate 270°';
    toRightTop: Result := 'Rotate 90°';
    toRightBottom: Result := 'Mirror hotizontal and rotate 90°';
    toLeftBottom: Result := 'Rotate 270°';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function RenderingToStr(Value: TExifRendering): string;
begin
  case Value of
    erTagMissing: Result := '';
    erNormal: Result := 'Normal';
    erCustom: Result := 'Custom';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function ResolutionUnitsToStr(Value: TTiffResolutionUnit): string;
begin
  case Value of
    trNone: Result := '';
    trInch: Result := 'inches';
    trCentimetre: Result := 'centimetres';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SceneCaptureTypeToStr(Value: TExifSceneCaptureType): string;
begin
  case Value of
    ecTagMissing: Result := '';
    ecStandard: Result := 'Standard';
    ecLandscape: Result := 'Landscape';
    ecPortrait: Result := 'Portrait';
    ecNightScene: Result := 'NightScene';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SceneTypeToStr(Value: TExifSceneType): string;
begin
  case Value of
    esUnknown: Result := '';
    esDirectlyPhotographed: Result := 'Directly photographed';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SensingMethodToStr(Value: TExifSensingMethod): string;
begin
  case Value of
    esTagMissing: Result := '';
    esMonochrome: Result := 'Monochrome';
    esOneChip: Result := 'One chip';
    esTwoChip: Result := 'Two chip';
    esThreeChip: Result := 'ThreeChip';
    esColorSequential: Result := 'Colour sequential';
    esTrilinear: Result := 'Trilinear';
    esColorSequentialLinear: Result := 'Colour sequential linear';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function StrobeLightToStr(Value: TExifStrobeLight): string;
begin
  case Value of
    esNoDetectionFunction: Result := 'No detection function';
    esUndetected: Result := 'Undetected';
    esDetected: Result := 'Detected';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SubjectDistanceRangeToStr(Value: TExifSubjectDistanceRange): string;
begin
  case Value of
    edTagMissing: Result := '';
    edUnknown: Result := 'Unknown';
    edMacro: Result := 'Macro';
    edClose: Result := 'Close';
    edDistant: Result := 'Distant';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function WhiteBalanceModeToStr(Value: TExifWhiteBalanceMode): string;
begin
  case Value of
    ewTagMissing: Result := '';
    ewAuto: Result := 'Auto';
    ewManual: Result := 'Manual';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DateTimeToStr(const Value: TDateTime): string;
begin
  DateTimeToString(Result, 'dddddd "at" tt', Value);
end;

constructor TOutputFrame.Create(AOwner: TComponent);
begin
  inherited;
  lsvStandard.DoubleBuffered := True;
  lsvMakerNote := TListView.Create(Self);
  lsvMakerNote.Align := lsvStandard.Align;
  lsvMakerNote.AlignWithMargins := lsvStandard.AlignWithMargins;
  lsvMakerNote.BorderStyle := lsvStandard.BorderStyle;
  lsvMakerNote.ColumnClick := lsvStandard.ColumnClick;
  lsvMakerNote.Columns := lsvStandard.Columns;
  lsvMakerNote.DoubleBuffered := True;
  lsvMakerNote.Margins := lsvStandard.Margins;
  lsvMakerNote.MultiSelect := lsvStandard.MultiSelect;
  lsvMakerNote.ReadOnly := lsvStandard.ReadOnly;
  lsvMakerNote.RowSelect := lsvStandard.RowSelect;
  lsvMakerNote.ViewStyle := lsvStandard.ViewStyle;
  lsvMakerNote.Visible := False;
  lsvMakerNote.Parent := lsvStandard.Parent;
  TabSet.Tabs.Objects[0] := lsvStandard;
  TabSet.Tabs.Objects[1] := lsvMakerNote;
end;

function TOutputFrame.ActiveListView: TListView;
begin
  Result := (TabSet.Tabs.Objects[TabSet.TabIndex] as TListView);
end;

function TOutputFrame.CanCopyToClipboard: Boolean;
begin
  Result := (ActiveListView.SelCount > 0);
end;

procedure TOutputFrame.CopyToClipboard;
var
  Item: TListItem;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    for Item in ActiveListView.Items do
      if Item.Selected then
        if Item.SubItems.Count = 0 then
          Strings.Add(Item.Caption)
        else
          Strings.Add(Item.Caption + #9 + Item.SubItems[0]);
    Clipboard.AsText := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TOutputFrame.SelectAll;
begin
  ActiveListView.SelectAll;
end;

procedure TOutputFrame.TabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  NewListView: TWinControl;
begin
  if lsvMakerNote.Tag = 0 then //list view buglet - horz scroll bar initially shown
  begin
    lsvMakerNote.Tag := 1;
    lsvMakerNote.Show;
    lsvMakerNote.Hide;
  end;
  NewListView := (TabSet.Tabs.Objects[NewTab] as TWinControl);
  NewListView.Show;
  NewListView.SetFocus;
  (TabSet.Tabs.Objects[TabSet.TabIndex] as TControl).Hide;
end;

procedure TOutputFrame.AddValue(ListView: TListView; const Name, Value: string;
  AddToTop: Boolean = False);
var
  NewItem: TListItem;
begin
  if AddToTop then
    NewItem := ListView.Items.Insert(0)
  else
    NewItem := ListView.Items.Add;
  NewItem.Caption := '   ' + Name;
  NewItem.SubItems.Add(Value);
end;

procedure TOutputFrame.AddLoadErrorsValue(ListView: TListView; Section: TExifSection;
  AddToTop: Boolean = False);
var
  Error: TExifSectionLoadError;
  S: string;
begin
  S := '';
  for Error in Section.LoadErrors do
    case Error of
      leBadOffset: S := S + ', bad IFD offset';
      leBadTagCount: S := S + ', bad tag count';
      leBadTagHeader: S := S + ', one or more bad tag headers';
    end;
  if (Section.Kind = esThumbnail) and (Section.Owner as TExifData).Thumbnail.Empty then
    S := S + ', bad image offset';
  if S = '' then
    S := 'Yes'
  else
    S := 'No:' + Copy(S, 2, MaxInt);
  AddValue(ListView, 'Loaded cleanly', S, AddToTop);
end;

procedure TOutputFrame.LoadFromFile(const FileName: string; MakerNoteValueMap: TCustomIniFile);
var
  ExifData: TExifData;
begin
  grpThumbnail.Hide;
  imgThumbnail.Picture.Assign(nil);
  ExifData := nil;
  lsvStandard.Items.BeginUpdate;
  lsvMakerNote.Items.BeginUpdate;
  try
    lsvStandard.Items.Clear;
    lsvMakerNote.Items.Clear;
    ExifData := TExifData.Create;
    ExifData.EnsureEnumsInRange := False; //as we use case statements rather than array constants, no need to keep this property set to True
    ExifData.LoadFromGraphic(FileName);
    if ExifData.Empty then
      lsvStandard.Items.Add.Caption := 'No Exif metadata found'
    else
      LoadStandardValues(ExifData);
    if ExifData.HasMakerNote then
      LoadMakerNoteValues(ExifData.MakerNote, MakerNoteValueMap)
    else
      lsvMakerNote.Items.Add.Caption := 'No MakerNote found';
  finally
    lsvStandard.Items.EndUpdate;
    lsvMakerNote.Items.EndUpdate;
    ExifData.Free;
  end;
  if imgThumbnail.Picture.Graphic <> nil then
  begin
    grpThumbnail.Width := (grpThumbnail.Width - imgThumbnail.Width) +
      imgThumbnail.Picture.Width;
    grpThumbnail.Visible := True;
  end;
end;

procedure TOutputFrame.LoadStandardValues(ExifData: TExifData);

  procedure AddValue(const Name, Value: string); overload;
  begin
    if Value <> '' then Self.AddValue(lsvStandard, Name, Value);
  end;

  procedure AddValue(const Name, Value: string; const Args: array of const); overload;
  begin
    AddValue(Name, Format(Value, Args));
  end;

  procedure AddValue(const Name: string; const Value: Int64); overload;
  begin
    if Value <> 0 then
      AddValue(Name, IntToStr(Value));
  end;

  procedure AddValue(const Name: string; YesNoValue: Boolean); overload;
  begin
    AddValue(Name, SNoYes[YesNoValue])
  end;

  procedure AddValue(const Name: string; const DateTime: TDateTime); overload;
  begin
    if DateTime <> 0 then
      AddValue(Name, DateTimeToStr(DateTime));
  end;

  procedure AddValue(const Name: string; const Fraction: TExifFraction;
    const Units: string = ''); overload;
  begin
    if not Fraction.MissingOrInvalid then
      AddValue(Name, '%g %s', [Fraction.Quotient, Units]);
  end;

  procedure AddValue(const Name: string; const Fraction: TExifSignedFraction); overload;
  begin
    if not Fraction.MissingOrInvalid then
      AddValue(Name, '%g', [Fraction.Quotient]);
  end;

  procedure AddValue(const Name: string; const Value: TSmallPoint); overload;
  begin
    if not InvalidPoint(Value) then
      AddValue(Name, '(%d, %d)', [Value.x, Value.y]);
  end;

  procedure AddValue(const Name: string; Coord: TGPSCoordinate); overload;
  var
    DirectionStr: string;
  {$IFDEF BUGGYCOMPILER}
    Degrees, Minutes, Seconds: TExifFraction; //work around D2006 compiler bug with intermediate vars
  {$ENDIF}
  begin
    if Coord.MissingOrInvalid then Exit;
    case Coord.Direction of
      'N': DirectionStr := 'north';
      'S': DirectionStr := 'south';
      'W': DirectionStr := 'west';
      'E': DirectionStr := 'east';
    else DirectionStr := '';
    end;
  {$IFDEF BUGGYCOMPILER}
    Degrees := Coord.Degrees;
    Minutes := Coord.Minutes;
    Seconds := Coord.Seconds;
    AddValue(Name, '%g°, %g minutes and %g seconds %s', [Degrees.Quotient,
      Minutes.Quotient, Seconds.Quotient, DirectionStr]);
  {$ELSE}
    AddValue(Name, '%g°, %g minutes and %g seconds %s', [Coord.Degrees.Quotient,
      Coord.Minutes.Quotient, Coord.Seconds.Quotient, DirectionStr]);
  {$ENDIF}
  end;

  procedure AddValue(const Name: string; const Direction: TExifFraction;
    Ref: TGPSDirectionRef); overload;
  begin
    if not Direction.MissingOrInvalid then
      AddValue(Name, '%g %s', [Direction.Quotient, DirectionRefToStr(Ref)]);
  end;

  procedure AddValue(const Name: string; const Distance: TExifFraction;
    Ref: TGPSDistanceRef); overload;
  begin
    if not Distance.MissingOrInvalid then
      AddValue(Name, '%g %s', [Distance.Quotient, DistanceRefToStr(Ref)]);
  end;

  procedure AddValue(const Name: string; Resolution: TCustomExifResolution); overload;
  {$IFDEF BUGGYCOMPILER}
  var
    X, Y: TExifFraction; //work around D2006 compiler bug with intermediate vars
  begin
    if Resolution.MissingOrInvalid then Exit;
    X := Resolution.X;
    Y := Resolution.Y;
    AddValue(Name, '%g x %g %s', [X.Quotient, Y.Quotient, ResolutionUnitsToStr(Resolution.Units)]);
  end;
  {$ELSE}
  begin
    if not Resolution.MissingOrInvalid then
      AddValue(Name, '%g x %g %s', [Resolution.X.Quotient, Resolution.Y.Quotient,
        ResolutionUnitsToStr(Resolution.Units)]);
  end;
  {$ENDIF}

  function DoSection(Kind: TExifSectionKind; const Name: string): Boolean;
  begin
    Result := (ExifData[Kind].Count > 0) or (ExifData[Kind].LoadErrors <> []);
    if not Result then Exit;
    lsvStandard.Items.Add.Caption := Name;
    AddLoadErrorsValue(lsvStandard, ExifData[Kind]);
  end;
begin
  lsvStandard.Items.Add.Caption := SGeneral;
  AddValue(SEndiannessCaption, SEndianness[ExifData.Endianness]);
  if DoSection(esGeneral, 'Main IFD') then
  begin
    AddValue('Camera make', ExifData.CameraMake);
    AddValue('Camera model', ExifData.CameraModel);
    AddValue('Software', ExifData.Software);
    AddValue('Date/time', ExifData.DateTime);
    AddValue('Image description', ExifData.ImageDescription);
    AddValue('Copyright', ExifData.Copyright);
    AddValue('Orientation', OrientationToStr(ExifData.Orientation));
    AddValue('Resolution', ExifData.Resolution);
    AddValue('Author', ExifData.Author);
    AddValue('Comments', ExifData.Comments);
    AddValue('Keywords', ExifData.Keywords);
    AddValue('Subject', ExifData.Subject);
    AddValue('Title', ExifData.Title);
  end;
  if DoSection(esDetails, 'Exif sub-IFD') then
  begin
    AddValue('Exif version', ExifData.ExifVersion.AsString);
    AddValue('Aperture value', ExifData.ApertureValue);
    AddValue('Brightness value', ExifData.BrightnessValue);
    AddValue('Colour space', ColorSpaceToStr(ExifData.ColorSpace));
    AddValue('Compressed bits per pixel', ExifData.CompressedBitsPerPixel);
    AddValue('Date/time original', ExifData.DateTimeOriginal);
    AddValue('Date/time digitised', ExifData.DateTimeDigitized);
    AddValue('Digital zoom ratio', ExifData.DigitalZoomRatio);
    AddValue('Exif image width', ExifData.ExifImageWidth);
    AddValue('Exif image height', ExifData.ExifImageHeight);
    AddValue('Exposure programme', ExposureProgramToStr(ExifData.ExposureProgram));
    AddValue('Exposure time', ExifData.ExposureTime, 'seconds');
    AddValue('Exposure index', ExifData.ExposureIndex);
    AddValue('Exposure bias value', ExifData.ExposureBiasValue);
    AddValue('File source', FileSourceToStr(ExifData.FileSource));
    if not ExifData.Flash.MissingOrInvalid then
    begin
      AddValue('Flash present', ExifData.Flash.Present);
      AddValue('Flash mode', FlashModeToStr(ExifData.Flash.Mode));
      AddValue('Flash fired', ExifData.Flash.Fired);
      AddValue('Flash red eye reduction', ExifData.Flash.RedEyeReduction);
      AddValue('Flash strobe energy', ExifData.Flash.StrobeEnergy);
      AddValue('Flash strobe light', StrobeLightToStr(ExifData.Flash.StrobeLight));
    end;
    AddValue('F number', ExifData.FNumber);
    AddValue('Focal length', ExifData.FocalLength);
    AddValue('Focal length in 35mm film', ExifData.FocalLengthIn35mmFilm);
    AddValue('Focal plane resolution', ExifData.FocalPlaneResolution);
    AddValue('Gain control', GainControlToStr(ExifData.GainControl));
    AddValue('Image unique ID', ExifData.ImageUniqueID);
    if not ExifData.ISOSpeedRatings.MissingOrInvalid then
      AddValue('ISO speed rating(s)', ExifData.ISOSpeedRatings.AsString);
    AddValue('Light source', LightSourceToStr(ExifData.LightSource));
    AddValue('MakerNote data offset', ExifData.OffsetSchema);
    AddValue('Max aperture value', ExifData.MaxApertureValue);
    AddValue('Metering mode', MeteringModeToStr(ExifData.MeteringMode));
    AddValue('Related sound file', ExifData.RelatedSoundFile);
    AddValue('Rendering', RenderingToStr(ExifData.Rendering));
    AddValue('Scene capture type', SceneCaptureTypeToStr(ExifData.SceneCaptureType));
    AddValue('Scene type', SceneTypeToStr(ExifData.SceneType));
    AddValue('Sensing method', SensingMethodToStr(ExifData.SensingMethod));
    if ExifData.ShutterSpeedInMSecs <> 0 then
      AddValue('Shutter speed', '%.4g milliseconds', [ExifData.ShutterSpeedInMSecs]);
    AddValue('Subject distance', ExifData.SubjectDistance);
    AddValue('Spectral sensitivity', ExifData.SpectralSensitivity);
    AddValue('Subject distance', ExifData.SubjectDistance);
    AddValue('Subject distance range', SubjectDistanceRangeToStr(ExifData.SubjectDistanceRange));
    AddValue('Subject location', ExifData.SubjectLocation);
    AddValue('White balance mode', WhiteBalanceModeToStr(ExifData.WhiteBalanceMode));
    { don't do sub sec tags as their values are rolled into the date/times by the
      latters' property getters }
  end;
  if DoSection(esInterop, 'Interoperability sub-IFD') then
  begin
    AddValue('Interoperability type', ExifData.InteropTypeName);
    AddValue('Interoperability version', ExifData.InteropVersion.AsString);
  end;
  if DoSection(esGPS, 'GPS sub-IFD') then
  begin
    AddValue('GPS version', ExifData.GPSVersion.AsString);
    AddValue('GPS date/time (UTC)', ExifData.GPSDateTimeUTC);
    AddValue('GPS latitude', ExifData.GPSLatitude);
    AddValue('GPS longitude', ExifData.GPSLongitude);
    AddValue('GPS altitude', ExifData.GPSAltitude, 'metres ' +
      GPSAltitudeRefToStr(ExifData.GPSAltitudeRef));
    AddValue('GPS satellites', ExifData.GPSSatellites);
    AddValue('GPS status', GPSStatusToStr(ExifData.GPSStatus));
    AddValue('GPS measure mode', GPSMeasureModeToStr(ExifData.GPSMeasureMode));
    AddValue('GPS DOP', ExifData.GPSDOP);
    AddValue('GPS speed', ExifData.GPSSpeed, GPSSpeedRefToStr(ExifData.GPSSpeedRef));
    AddValue('GPS track', ExifData.GPSTrack, ExifData.GPSTrackRef);
    AddValue('GPS image direction', ExifData.GPSImgDirection,
      ExifData.GPSImgDirectionRef);
    AddValue('GPS map datum', ExifData.GPSMapDatum);
    AddValue('GPS destination latitude', ExifData.GPSDestLatitude);
    AddValue('GPS destination longitude', ExifData.GPSDestLongitude);
    AddValue('GPS destination bearing', ExifData.GPSDestBearing,
      ExifData.GPSDestBearingRef);
    AddValue('GPS destination distance', ExifData.GPSDestDistance,
      ExifData.GPSDestDistanceRef);
    AddValue('GPS differential', GPSDifferentialToStr(ExifData.GPSDifferential));
  end;
  if DoSection(esThumbnail, 'Thumbnail IFD') then
  begin
    AddValue('Thumbnail orientation', OrientationToStr(ExifData.Orientation));
    AddValue('Thumbnail resolution', ExifData.Resolution);
    if not ExifData.Thumbnail.Empty then imgThumbnail.Picture.Assign(ExifData.Thumbnail);
  end;
end;

procedure TOutputFrame.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent <> nil then
  begin
    TabSet.ParentFont := True;
    TabSet.Height := TabSet.Canvas.TextHeight('X') + 10;
    if (AParent is TTabSheet) and ThemeServices.ThemesEnabled then
      TabSet.BackgroundColor := clWindow;
  end;
end;

procedure TOutputFrame.LoadMakerNoteValues(MakerNote: TExifMakerNote;
  ValueMap: TCustomIniFile);

  procedure LoadValue(const Section, Ident, DefDescription, DefValue: string);
  begin
    AddValue(lsvMakerNote, ValueMap.ReadString(Section, 'TagDescription',
      DefDescription), ValueMap.ReadString(Section, Ident, DefValue));
  end;
var
  I: Integer;
  S, Section, TypeName, ValueStr: string;
  Tag: TExifTag;
begin
  if MakerNote is TUnrecognizedMakerNote then
  begin
    lsvMakerNote.Items.Add.Caption := 'Unrecognised format';
    Exit;
  end;
  TypeName := ValueMap.ReadString(MakerNote.ClassName, 'UseTagsFrom', '');
  if TypeName = '' then TypeName := MakerNote.ClassName;
  for Tag in MakerNote.Tags do
  begin
    FmtStr(Section, '%s.$%.4x', [TypeName, Tag.ID]);
    if Tag.WellFormed and ValueMap.ReadBool(Section, 'TreatAsTagGroup', False) then
      for I := 0 to Tag.ElementCount - 1 do
      begin
        S := ValueMap.ReadString(Section, 'TagDescription', '');
        if S <> '' then
          S := Format('%s (%d)', [S, I])
        else
          S := Format('Unknown ($%.4x, %d)', [Tag.ID, I]);
        ValueStr := Tag.ElementAsString[I];
        LoadValue(Format('%s(%d)', [Section, I]), ValueStr, S, ValueStr);
      end
    else
    begin
      if not Tag.WellFormed then
        ValueStr := '[Badly formed tag header]'
      else if Tag.DataType = tdUndefined then
        if Tag.ElementCount > 50 then
          ValueStr := Format('{%s...}', [Copy(Tag.AsString, 1, 100)])
        else
          ValueStr := Format('{%s}', [Tag.AsString])
//      else if (Tag.ID = ttPanasonicTimeSincePowerOn) and (ExifData.MakerNoteType = TPanasonicMakerNote) then
//        ValueStr := SecsToStr(Tag.ReadLongWord(0, 0) div 100)
      else
        ValueStr := Tag.AsString;
      LoadValue(Section, Tag.AsString, Format('Unknown ($%.4x)', [Tag.ID]), ValueStr);
    end;
  end;
  lsvMakerNote.AlphaSort;
  lsvMakerNote.Items.Insert(0).Caption := 'Tags';
  AddLoadErrorsValue(lsvMakerNote, MakerNote.Tags, True);
  AddValue(lsvMakerNote, SEndiannessCaption, SEndianness[MakerNote.Endianness], True);
  TypeName := MakerNote.ClassName;
  AddValue(lsvMakerNote, 'Type', Copy(TypeName, 2, Length(TypeName) - 10), True);
  lsvMakerNote.Items.Insert(0).Caption := SGeneral;
end;

end.
