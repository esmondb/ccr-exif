{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.2                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ExifListForm;
{
  FMX/XE3 port of the original VCL Exif List demo. Targets both Windows and OS X; on Windows
  is an SDI application like the VCL original, on OS X Mac-style MDI a la Preview.app.
  Note: the latter means the program opens with no form showing on OS X. This is not a bug,
  but intentional.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Generics.Collections, System.Actions, System.IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.TabControl,
  FMX.TreeView, FMX.Platform, FMX.Menus,
  CCR.Exif, CCR.Exif.FMXUtils, ExifListController;

type
  TfrmExifList = class(TForm, IExifDocumentForm)
    TabControl: TTabControl;
    tabStandard: TTabItem;
    tabMakerNote: TTabItem;
    tabThumbnail: TTabItem;
    imgThumbnail: TImageControl;
    trvStandard: TTreeView;
    tviEndiannes: TTreeViewItem;
    tviGeneralSection: TTreeViewItem;
    tviDetailsSection: TTreeViewItem;
    tbiInteropSection: TTreeViewItem;
    tviGPSSection: TTreeViewItem;
    tviThumbnailSection: TTreeViewItem;
    lblEndianness: TLabel;
    trvMakerNote: TTreeView;
    tviMakerNoteType: TTreeViewItem;
    tviMakerNoteEndianness: TTreeViewItem;
    tviMakerNoteTags: TTreeViewItem;
    lblMakerNoteType: TLabel;
    lblMakerNoteEndianness: TLabel;
    lyoButtonBar: TLayout;
    btnCopyTags: TButton;
    btnOpenInDefProgram: TButton;
    btnOpenFile: TButton;
    btnClose: TButton;
    lblNoExifData: TLabel;
    lblNoMakerNote: TLabel;
    tviExampleChild: TTreeViewItem;
    tabSource: TTabItem;
    imgSource: TImageControl;
    lblNoThumbnail: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateValueHighlightColor(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tviTopLevelDblClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    class var FMakerNoteMap: TMemIniFile;
    class constructor InitializeClass;
    class destructor FinalizeClass;
  strict private
    FDoneDragEnter, FHandleDragDirectly: Boolean;
    FFileName: string;
    FHighlightedLabels: TDictionary<TObject,TLabel>;
    FLoadedSource: Boolean;
    FRotateSourceBy: Single;
    FSelectedFontColor, FUnselectedFontColor: TAlphaColor;
    FValueLabelPaddingLeft: Single;
    procedure AddValue(const Parent: TTreeViewItem; const Name, Value: string);
    procedure AddLoadErrorsValue(const Parent: TTreeViewItem; Section: TExifSection);
    procedure LoadStandardValues(ExifData: TExifData);
    procedure LoadMakerNoteValues(MakerNote: TExifMakerNote);
    { IExifDocumentForm }
    function GetFileName: string;
    function CanCopyTags: Boolean;
    procedure CopyTags(const AService: IFMXClipboardService);
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    procedure OpenFile(const AFileName: string);
  protected
    procedure CreateHandle; override;
  public
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: Boolean); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DragLeave; override;
  end;

implementation

{$R *.fmx}

uses CCR.Exif.BaseUtils, CCR.Exif.TiffUtils, ExifListUtils;

class constructor TfrmExifList.InitializeClass;
var
  Stream: TResourceStream;
  Strings: TStringList;
begin
  { Load the maker note map - the INI file resource is added in the first place via
    Project|Resources and Images... }
  FMakerNoteMap := TMemIniFile.Create('');
  if FindResource(HInstance, 'MakerNotes', RT_RCDATA) <> 0 then
  begin
    Strings := nil;
    Stream := TResourceStream.Create(HInstance, 'MakerNotes', RT_RCDATA);
    try
      Strings := TStringList.Create;
      Strings.LoadFromStream(Stream, TEncoding.ANSI);
      FMakerNoteMap.SetStrings(Strings);
    finally
      Stream.Free;
      Strings.Free;
    end;
  end;
end;

class destructor TfrmExifList.FinalizeClass;
begin
  FMakerNoteMap.Free;
end;

procedure TfrmExifList.FormCreate(Sender: TObject);
begin
  lyoButtonBar.Visible := (TFileManager.DocumentMode = dmOnePerAppInst);
  { On OS X, the text of a selected tree view item is white. While the item text
    proper is covered, our additional labels are not, so set up handling it ourselves
    (the other part is the shared OnChange handler for the two tree views). }
  FHighlightedLabels := TDictionary<TObject,TLabel>.Create;
  tviEndiannes.TagObject := lblEndianness;
  tviMakerNoteType.TagObject := lblMakerNoteType;
  tviMakerNoteEndianness.TagObject := lblMakerNoteEndianness;

  tviExampleChild.ApplyStyleLookup;
  FUnselectedFontColor := tviExampleChild.FontColor;
  FSelectedFontColor := LookupSelectedFontColor(tviExampleChild);

  FValueLabelPaddingLeft := lblEndianness.Padding.Left - tviExampleChild.Position.X;
end;

procedure TfrmExifList.FormDestroy(Sender: TObject);
begin
  FHighlightedLabels.Free;
end;

procedure TfrmExifList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmExifList.FormShow(Sender: TObject);
begin
  OnShow := nil;
  { Oh joy - longstanding VCL action list bugs are copied over to FMX... }
  if lyoButtonBar.Visible then
  begin
    btnCopyTags.Action := dtmController.actCopyTags;
    btnOpenInDefProgram.Action := dtmController.actOpenInDefProg;
    btnOpenFile.Action := dtmController.actOpenFile;
    btnClose.Action := dtmController.actClose;
  end;
end;

procedure TfrmExifList.CreateHandle;
begin
  inherited;
  TMacCommands.EnableFullScreen(Self);
end;

procedure TfrmExifList.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  { The default behaviour is to pass the event on to the control being hovered
    over. Handling the drag operation at the form level just means it doesn't
    matter what control files are dropped onto. }
  FDoneDragEnter := True;
  FHandleDragDirectly := (Data.Files <> nil);
  if not FHandleDragDirectly then inherited;
end;

procedure TfrmExifList.DragOver(const Data: TDragObject; const Point: TPointF;
  var Accept: Boolean);
begin
  if not FDoneDragEnter then //!!!FMX bug - DragEnter never called on Windows
    DragEnter(Data, Point);
  if FHandleDragDirectly then
    Accept := True
  else
    inherited;
end;

procedure TfrmExifList.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  if FHandleDragDirectly then
    TFileManager.OpenDroppedFiles(Data.Files)
  else
    inherited;
  FDoneDragEnter := False;
end;

procedure TfrmExifList.DragLeave;
begin
  FDoneDragEnter := False;
  inherited;
end;

procedure TfrmExifList.tviTopLevelDblClick(Sender: TObject);
begin
  TTreeViewItem(Sender).IsExpanded := not (Sender as TTreeViewItem).IsExpanded;
end;

procedure TfrmExifList.AddValue(const Parent: TTreeViewItem; const Name, Value: string);
var
  Child: TTreeViewItem;
  ValueLabel: TLabel;
begin
  Child := TTreeViewItem.Create(Self);
  Child.Text := Name;
  if Value <> '' then
  begin
    ValueLabel := TLabel.Create(Child);
    ValueLabel.Align := lblEndianness.Align;
    ValueLabel.Padding.Left := FValueLabelPaddingLeft;
    ValueLabel.StyledSettings := lblEndianness.StyledSettings;
    ValueLabel.Text := Value;
    ValueLabel.Width := lblEndianness.Width;
    ValueLabel.WordWrap := False;
    Child.AddObject(ValueLabel);
    Child.TagObject := ValueLabel;
  end;
  Parent.AddObject(Child);
end;

procedure TfrmExifList.UpdateValueHighlightColor(Sender: TObject);
var
  ValueLabel: TLabel;
  Selected: TTreeViewItem;
begin
  Selected := (Sender as TTreeView).Selected;
  if (Selected = nil) or (FHighlightedLabels = nil) then Exit;
  if FHighlightedLabels.TryGetValue(Sender, ValueLabel) then
    ValueLabel.FontColor := FUnselectedFontColor;
  if Selected.TagObject is TLabel then
  begin
    ValueLabel := TLabel(Selected.TagObject);
    ValueLabel.FontColor := FSelectedFontColor;
    FHighlightedLabels.AddOrSetValue(Sender, ValueLabel);
  end;
end;

procedure TfrmExifList.AddLoadErrorsValue(const Parent: TTreeViewItem; Section: TExifSection);
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
  AddValue(Parent, 'Loaded cleanly', S);
end;

procedure TfrmExifList.LoadMakerNoteValues(MakerNote: TExifMakerNote);

  procedure LoadValue(const Section, Ident, DefDescription, DefValue: string);
  begin
    AddValue(tviMakerNoteTags, FMakerNoteMap.ReadString(Section, 'TagDescription',
      DefDescription), FMakerNoteMap.ReadString(Section, Ident, DefValue));
  end;
var
  I: Integer;
  S, Section, TypeName, ValueStr: string;
  Tag: TExifTag;
begin
  for I := tviMakerNoteTags.Count - 1 downto 0 do
    tviMakerNoteTags[I].Free;
  if MakerNote is TUnrecognizedMakerNote then
  begin
    lblMakerNoteType.Text := 'Unrecognised format';
    lblMakerNoteEndianness.Text := 'Unknown';
    Exit;
  end;
  TypeName := MakerNote.ClassName;
  lblMakerNoteType.Text := TypeName.Substring(1, TypeName.Length - 10);
  lblMakerNoteEndianness.Text := SEndianness[MakerNote.Endianness];
  AddLoadErrorsValue(tviMakerNoteTags, MakerNote.Tags);
  TypeName := FMakerNoteMap.ReadString(TypeName, 'UseTagsFrom', TypeName);
  for Tag in MakerNote.Tags do
  begin
    FmtStr(Section, '%s.$%.4x', [TypeName, Tag.ID]);
    if Tag.WellFormed and FMakerNoteMap.ReadBool(Section, 'TreatAsTagGroup', False) then
      for I := 0 to Tag.ElementCount - 1 do
      begin
        S := FMakerNoteMap.ReadString(Section, 'TagDescription', '');
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
end;

procedure TfrmExifList.LoadStandardValues(ExifData: TExifData);

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name, Value: string); overload;
  begin
    if Value <> '' then AddValue(Parent, Name, Value);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name, Value: string;
    const Args: array of const); overload;
  begin
    CheckAddValue(Parent, Name, Format(Value, Args));
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Value: Int64); overload;
  begin
    if Value <> 0 then
      CheckAddValue(Parent, Name, IntToStr(Value));
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    YesNoValue: Boolean); overload;
  begin
    CheckAddValue(Parent, Name, SNoYes[YesNoValue])
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const DateTime: TDateTimeTagValue); overload;
  begin
    if not DateTime.MissingOrInvalid then
      CheckAddValue(Parent, Name, DateTimeToStr(DateTime));
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Fraction: TExifFraction; const Units: string = ''); overload;
  begin
    if not Fraction.MissingOrInvalid then
      CheckAddValue(Parent, Name, '%g %s', [Fraction.Quotient, Units]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Fraction: TExifSignedFraction); overload;
  begin
    if not Fraction.MissingOrInvalid then
      CheckAddValue(Parent, Name, '%g', [Fraction.Quotient]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Value: TSmallPoint); overload;
  begin
    if not InvalidPoint(Value) then
      CheckAddValue(Parent, Name, '(%d, %d)', [Value.x, Value.y]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    Coord: TGPSCoordinate); overload;
  var
    DirectionStr: string;
  begin
    if Coord.MissingOrInvalid then Exit;
    case Coord.Direction of
      'N': DirectionStr := 'north';
      'S': DirectionStr := 'south';
      'W': DirectionStr := 'west';
      'E': DirectionStr := 'east';
    else DirectionStr := '';
    end;
    CheckAddValue(Parent, Name, '%g°, %g minutes and %g seconds %s',
      [Coord.Degrees.Quotient, Coord.Minutes.Quotient, Coord.Seconds.Quotient, DirectionStr]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Direction: TExifFraction; Ref: TGPSDirectionRef); overload;
  begin
    if not Direction.MissingOrInvalid then
      CheckAddValue(Parent, Name, '%g %s', [Direction.Quotient, DirectionRefToStr(Ref)]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    const Distance: TExifFraction; Ref: TGPSDistanceRef); overload;
  begin
    if not Distance.MissingOrInvalid then
      CheckAddValue(Parent, Name, '%g %s', [Distance.Quotient, DistanceRefToStr(Ref)]);
  end;

  procedure CheckAddValue(const Parent: TTreeViewItem; const Name: string;
    Resolution: TCustomExifResolution); overload;
  begin
    if not Resolution.MissingOrInvalid then
      CheckAddValue(Parent, Name, '%g x %g %s', [Resolution.X.Quotient,
        Resolution.Y.Quotient, ResolutionUnitsToStr(Resolution.Units)]);
  end;

  function DoSection(const Parent: TTreeViewItem; Kind: TExifSectionKind): Boolean;
  var
    I: Integer;
  begin
    for I := Parent.Count - 1 downto 0 do
      Parent[I].Free;
    Result := (ExifData[Kind].Count > 0) or (ExifData[Kind].LoadErrors <> []);
    if Result then
      AddLoadErrorsValue(Parent, ExifData[Kind])
    else
      AddValue(Parent, '(Not found)', '');
  end;
begin
  lblEndianness.Text := SEndianness[ExifData.Endianness];
  if DoSection(tviGeneralSection, esGeneral) then
  begin
    CheckAddValue(tviGeneralSection, 'Camera make', ExifData.CameraMake);
    CheckAddValue(tviGeneralSection, 'Camera model', ExifData.CameraModel);
    CheckAddValue(tviGeneralSection, 'Software', ExifData.Software);
    CheckAddValue(tviGeneralSection, 'Date/time', ExifData.DateTime);
    CheckAddValue(tviGeneralSection, 'Image description', ExifData.ImageDescription);
    CheckAddValue(tviGeneralSection, 'Copyright', ExifData.Copyright);
    CheckAddValue(tviGeneralSection, 'Orientation', OrientationToStr(ExifData.Orientation));
    CheckAddValue(tviGeneralSection, 'Resolution', ExifData.Resolution);
    CheckAddValue(tviGeneralSection, 'Author', ExifData.Author);
    CheckAddValue(tviGeneralSection, 'Comments', ExifData.Comments);
    CheckAddValue(tviGeneralSection, 'Keywords', ExifData.Keywords);
    CheckAddValue(tviGeneralSection, 'Subject', ExifData.Subject);
    CheckAddValue(tviGeneralSection, 'Title', ExifData.Title);
  end;
  if DoSection(tviDetailsSection, esDetails) then
  begin
    CheckAddValue(tviDetailsSection, 'Exif version', ExifData.ExifVersion.AsString);
    CheckAddValue(tviDetailsSection, 'Aperture value', ExifData.ApertureValue);
    CheckAddValue(tviDetailsSection, 'Body serial number', ExifData.BodySerialNumber);
    CheckAddValue(tviDetailsSection, 'Brightness value', ExifData.BrightnessValue);
    CheckAddValue(tviDetailsSection, 'Camera owner', ExifData.CameraOwnerName);
    CheckAddValue(tviDetailsSection, 'Colour space', ColorSpaceToStr(ExifData.ColorSpace));
    CheckAddValue(tviDetailsSection, 'Compressed bits per pixel', ExifData.CompressedBitsPerPixel);
    CheckAddValue(tviDetailsSection, 'Date/time original', ExifData.DateTimeOriginal);
    CheckAddValue(tviDetailsSection, 'Date/time digitised', ExifData.DateTimeDigitized);
    CheckAddValue(tviDetailsSection, 'Digital zoom ratio', ExifData.DigitalZoomRatio);
    CheckAddValue(tviDetailsSection, 'Exif image width', ExifData.ExifImageWidth);
    CheckAddValue(tviDetailsSection, 'Exif image height', ExifData.ExifImageHeight);
    CheckAddValue(tviDetailsSection, 'Exposure programme', ExposureProgramToStr(ExifData.ExposureProgram));
    CheckAddValue(tviDetailsSection, 'Exposure time', ExifData.ExposureTime, 'seconds');
    CheckAddValue(tviDetailsSection, 'Exposure index', ExifData.ExposureIndex);
    CheckAddValue(tviDetailsSection, 'Exposure bias value', ExifData.ExposureBiasValue);
    CheckAddValue(tviDetailsSection, 'File source', FileSourceToStr(ExifData.FileSource));
    if not ExifData.Flash.MissingOrInvalid then
    begin
      CheckAddValue(tviDetailsSection, 'Flash present', ExifData.Flash.Present);
      CheckAddValue(tviDetailsSection, 'Flash mode', FlashModeToStr(ExifData.Flash.Mode));
      CheckAddValue(tviDetailsSection, 'Flash fired', ExifData.Flash.Fired);
      CheckAddValue(tviDetailsSection, 'Flash red eye reduction', ExifData.Flash.RedEyeReduction);
      CheckAddValue(tviDetailsSection, 'Flash strobe energy', ExifData.Flash.StrobeEnergy);
      CheckAddValue(tviDetailsSection, 'Flash strobe light', StrobeLightToStr(ExifData.Flash.StrobeLight));
    end;
    CheckAddValue(tviDetailsSection, 'F number', ExifData.FNumber);
    CheckAddValue(tviDetailsSection, 'Focal length', ExifData.FocalLength);
    CheckAddValue(tviDetailsSection, 'Focal length in 35mm film', ExifData.FocalLengthIn35mmFilm);
    CheckAddValue(tviDetailsSection, 'Focal plane resolution', ExifData.FocalPlaneResolution);
    CheckAddValue(tviDetailsSection, 'Gain control', GainControlToStr(ExifData.GainControl));
    CheckAddValue(tviDetailsSection, 'Image unique ID', ExifData.ImageUniqueID);
    if not ExifData.ISOSpeedRatings.MissingOrInvalid then
      CheckAddValue(tviDetailsSection, 'ISO speed rating(s)', ExifData.ISOSpeedRatings.AsString);
    CheckAddValue(tviDetailsSection, 'Lens make', ExifData.LensMake);
    CheckAddValue(tviDetailsSection, 'Lens model', ExifData.LensModel);
    CheckAddValue(tviDetailsSection, 'Lens serial number', ExifData.LensSerialNumber);
    CheckAddValue(tviDetailsSection, 'Light source', LightSourceToStr(ExifData.LightSource));
    CheckAddValue(tviDetailsSection, 'MakerNote data offset', ExifData.OffsetSchema);
    CheckAddValue(tviDetailsSection, 'Max aperture value', ExifData.MaxApertureValue);
    CheckAddValue(tviDetailsSection, 'Metering mode', MeteringModeToStr(ExifData.MeteringMode));
    CheckAddValue(tviDetailsSection, 'Related sound file', ExifData.RelatedSoundFile);
    CheckAddValue(tviDetailsSection, 'Rendering', RenderingToStr(ExifData.Rendering));
    CheckAddValue(tviDetailsSection, 'Scene capture type', SceneCaptureTypeToStr(ExifData.SceneCaptureType));
    CheckAddValue(tviDetailsSection, 'Scene type', SceneTypeToStr(ExifData.SceneType));
    CheckAddValue(tviDetailsSection, 'Sensing method', SensingMethodToStr(ExifData.SensingMethod));
    if ExifData.ShutterSpeedInMSecs <> 0 then
      CheckAddValue(tviDetailsSection, 'Shutter speed', '%.4g milliseconds', [ExifData.ShutterSpeedInMSecs]);
    CheckAddValue(tviDetailsSection, 'Subject distance', ExifData.SubjectDistance);
    CheckAddValue(tviDetailsSection, 'Spectral sensitivity', ExifData.SpectralSensitivity);
    CheckAddValue(tviDetailsSection, 'Subject distance', ExifData.SubjectDistance);
    CheckAddValue(tviDetailsSection, 'Subject distance range', SubjectDistanceRangeToStr(ExifData.SubjectDistanceRange));
    CheckAddValue(tviDetailsSection, 'Subject location', ExifData.SubjectLocation);
    CheckAddValue(tviDetailsSection, 'White balance mode', WhiteBalanceModeToStr(ExifData.WhiteBalanceMode));
    { don't do sub sec tags as their values are rolled into the date/times by the
      latters' property getters }
  end;
  if DoSection(tbiInteropSection, esInterop) then
  begin
    CheckAddValue(tbiInteropSection, 'Interoperability type', ExifData.InteropTypeName);
    CheckAddValue(tbiInteropSection, 'Interoperability version', ExifData.InteropVersion.AsString);
  end;
  if DoSection(tviGPSSection, esGPS) then
  begin
    CheckAddValue(tviGPSSection, 'GPS version', ExifData.GPSVersion.AsString);
    CheckAddValue(tviGPSSection, 'GPS date/time (UTC)', ExifData.GPSDateTimeUTC);
    CheckAddValue(tviGPSSection, 'GPS latitude', ExifData.GPSLatitude);
    CheckAddValue(tviGPSSection, 'GPS longitude', ExifData.GPSLongitude);
    CheckAddValue(tviGPSSection, 'GPS altitude', ExifData.GPSAltitude, 'metres ' +
      GPSAltitudeRefToStr(ExifData.GPSAltitudeRef));
    CheckAddValue(tviGPSSection, 'GPS satellites', ExifData.GPSSatellites);
    CheckAddValue(tviGPSSection, 'GPS status', GPSStatusToStr(ExifData.GPSStatus));
    CheckAddValue(tviGPSSection, 'GPS measure mode', GPSMeasureModeToStr(ExifData.GPSMeasureMode));
    CheckAddValue(tviGPSSection, 'GPS DOP', ExifData.GPSDOP);
    CheckAddValue(tviGPSSection, 'GPS speed', ExifData.GPSSpeed, GPSSpeedRefToStr(ExifData.GPSSpeedRef));
    CheckAddValue(tviGPSSection, 'GPS track', ExifData.GPSTrack, ExifData.GPSTrackRef);
    CheckAddValue(tviGPSSection, 'GPS image direction', ExifData.GPSImgDirection,
      ExifData.GPSImgDirectionRef);
    CheckAddValue(tviGPSSection, 'GPS map datum', ExifData.GPSMapDatum);
    CheckAddValue(tviGPSSection, 'GPS destination latitude', ExifData.GPSDestLatitude);
    CheckAddValue(tviGPSSection, 'GPS destination longitude', ExifData.GPSDestLongitude);
    CheckAddValue(tviGPSSection, 'GPS destination bearing', ExifData.GPSDestBearing,
      ExifData.GPSDestBearingRef);
    CheckAddValue(tviGPSSection, 'GPS destination distance', ExifData.GPSDestDistance,
      ExifData.GPSDestDistanceRef);
    CheckAddValue(tviGPSSection, 'GPS differential', GPSDifferentialToStr(ExifData.GPSDifferential));
  end;
  if DoSection(tviThumbnailSection, esThumbnail) then
  begin
    CheckAddValue(tviThumbnailSection, 'Thumbnail orientation', OrientationToStr(ExifData.ThumbnailOrientation));
    CheckAddValue(tviThumbnailSection, 'Thumbnail resolution', ExifData.ThumbnailResolution);
    if ExifData.HasThumbnail then imgThumbnail.Bitmap.Assign(ExifData.Thumbnail);
  end;
  lblNoThumbnail.Visible := not ExifData.HasThumbnail;
end;

function TfrmExifList.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TfrmExifList.TabControlChange(Sender: TObject);
begin
  if tabSource.IsSelected and not FLoadedSource and (FFileName <> '') then
  begin
    imgSource.Bitmap.LoadFromFile(FFileName);
    if FRotateSourceBy <> 0 then imgSource.Bitmap.Rotate(FRotateSourceBy);
    FLoadedSource := True;
  end;
end;

procedure TfrmExifList.GotoNextTab;
begin
  if TabControl.TabIndex = TabControl.TabCount - 1 then
    TabControl.TabIndex := 0
  else
    TabControl.TabIndex := TabControl.TabIndex + 1;
end;

procedure TfrmExifList.GotoPreviousTab;
begin
  if TabControl.TabIndex = 0 then
    TabControl.TabIndex := TabControl.TabCount - 1
  else
    TabControl.TabIndex := TabControl.TabIndex - 1;
end;

procedure TfrmExifList.OpenFile(const AFileName: string);
var
  ExifData: TExifData;
begin
  ExifData := nil;
  trvStandard.BeginUpdate;
  trvMakerNote.BeginUpdate;
  try
    ExifData := TExifData.Create;
    ExifData.LoadFromGraphic(AFileName);
    LoadStandardValues(ExifData);
    LoadMakerNoteValues(ExifData.MakerNote);
    trvStandard.Visible := not ExifData.Empty;
    trvMakerNote.Visible := ExifData.HasMakerNote;
    lblNoExifData.Opacity := 1;
    lblNoMakerNote.Opacity := 1;
    case ExifData.Orientation of
      toBottomRight: FRotateSourceBy := 180;
      toRightTop: FRotateSourceBy := 90;
      toLeftBottom: FRotateSourceBy := 270;
    else FRotateSourceBy := 0;
    end;
    FLoadedSource := False;
    if tabSource.IsSelected then TabControlChange(nil);
  finally
    trvStandard.EndUpdate;
    trvMakerNote.EndUpdate;
    ExifData.Free;
  end;
  FFileName := AFileName;
end;

function TfrmExifList.CanCopyTags: Boolean;
begin
  case TabControl.TabIndex of
    0: Result := trvStandard.Visible;
    1: Result := trvMakerNote.Visible;
  else Result := False;
  end;
end;

procedure TfrmExifList.CopyTags(const AService: IFMXClipboardService);
var
  Data: TStringBuilder;

  procedure ProcessItems(const Container: IItemsContainer; const Indent: string = '');
  var
    I: Integer;
    Item: TTreeViewItem;
  begin
    for I := 0 to Container.GetItemsCount - 1 do
    begin
      Item := Container.GetItem(I) as TTreeViewItem;
      Data.Append(Indent + Item.Text);
      if Item.TagObject is TLabel then
        Data.Append(Tabulator + TLabel(Item.TagObject).Text);
      Data.AppendLine;
      ProcessItems(Item, Indent + Tabulator);
    end;
  end;
begin
  Data := TStringBuilder.Create;
  try
    case TabControl.TabIndex of
      0: ProcessItems(trvStandard);
      1: ProcessItems(trvMakerNote);
    else Exit;
    end;
    AService.SetClipboard(Data.ToString);
  finally
    Data.Free;
  end;
  MessageDlg('Copied tags to clipboard', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0)
end;

initialization
  TfrmExifList.ClassName; //ensure class is linked in!
end.
