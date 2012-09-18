unit IPTCEditorForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes, System.Generics.Collections,
  System.Actions, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.Edit, FMX.Objects, FMX.ActnList, FMX.ListBox, FMX.ExtCtrls,
  CCR.Exif.IPTC, CCR.Exif.FMXUtils, IPTCEditorController, FMX.Memo, FMX.Grid,
  FMX.Menus;

type
  TCalendarEdit = class(FMX.ExtCtrls.TCalendarEdit)        //small interposer to support a 'missing' or 'empty' state
  protected
    procedure SetText(const Value: string); override;
  public
    procedure DropDown; override;
  end;

  TGrid = class(FMX.Grid.TGrid, IIPTCRepeatingPairControl) //as I don't want to force installing a custom ctrl for a demo of something else...
  strict private
    FData: TList<TIPTCRepeatablePair>;
    procedure AddNew;
    function CanRemoveSelected: Boolean;
    procedure RemoveSelected;
  protected
    function GetValue(Col, Row: Integer): TValue; override;
    procedure SetValue(Col, Row: Integer; const Value: TValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadDataFromArray(const NewData: TIPTCRepeatablePairs);
    function SaveDataToArray: TIPTCRepeatablePairs;
  end;

  TVertScrollBox = class(FMX.Layouts.TVertScrollBox)       //interposer to expose the the content ctrl
    property Content: TScrollContent read FContent;
  end;

  TfrmIPTCEditor = class(TForm, IIPTCDocumentForm)
    TabControl: TTabControl;
    tabEnvelopeRec: TTabItem;
    tabApplicationRec: TTabItem;
    edtModelVersion: TEdit;
    Label1: TLabel;
    edtDestination: TEdit;
    Label2: TLabel;
    Image1: TImage;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    Image2: TImage;
    Line1: TLine;
    StyleBook: TStyleBook;
    lyoTabs: TLayout;
    rctEnvelopeRecord: TRectangle;
    lblEnvelopeRec: TLabel;
    rctApplicationRec: TRectangle;
    lblApplicationRec: TLabel;
    lblNoTagsFound: TLabel;
    btnClearAll: TSpeedButton;
    Image3: TImage;
    ToolBar: TToolBar;
    btnReload: TSpeedButton;
    Image4: TImage;
    Line2: TLine;
    btnClose: TSpeedButton;
    edtFileFormat: TEdit;
    Label3: TLabel;
    edtFileFormatVersion: TEdit;
    Label4: TLabel;
    edtServiceIdentifier: TEdit;
    Label5: TLabel;
    edtEnvelopeNumber: TEdit;
    Label6: TLabel;
    edtProductID: TEdit;
    Label7: TLabel;
    cboEnvelopePriority: TComboBox;
    Label8: TLabel;
    edtDateSent: TCalendarEdit;
    Label9: TLabel;
    edtUNOCode: TEdit;
    Label10: TLabel;
    edtARMIdentifier: TEdit;
    Label11: TLabel;
    edtARMVersion: TEdit;
    Label12: TLabel;
    edtRecordVersion: TEdit;
    Label13: TLabel;
    edtObjectTypeRef: TEdit;
    Label14: TLabel;
    edtObjectAttributeRef: TEdit;
    Label15: TLabel;
    edtObjectName: TEdit;
    Label16: TLabel;
    edtEditStatus: TEdit;
    Label17: TLabel;
    cboUrgency: TComboBox;
    Label18: TLabel;
    memSubjectRefs: TMemo;
    Label19: TLabel;
    edtCategoryCode: TEdit;
    Label20: TLabel;
    memSupplementaryCategories: TMemo;
    Label21: TLabel;
    edtFixtureIdentifier: TEdit;
    Label22: TLabel;
    memKeywords: TMemo;
    lblKeywords: TLabel;
    vsbApplicationRec: TVertScrollBox;
    vsbEnvelopeRec: TVertScrollBox;
    Label24: TLabel;
    stgContentLocations: TGrid;
    colCode: TStringColumn;
    colName: TStringColumn;
    btnClearDateSent: TButton;
    edtReleaseDate: TCalendarEdit;
    Button1: TButton;
    Label25: TLabel;
    edtExpirationDate: TCalendarEdit;
    Button2: TButton;
    Label26: TLabel;
    edtOriginatingProgram: TEdit;
    Label27: TLabel;
    edtProgramVersion: TEdit;
    Label28: TLabel;
    edtObjectCycleCode: TEdit;
    Label29: TLabel;
    stgBylines: TGrid;
    colBylineName: TStringColumn;
    colBylineTitle: TStringColumn;
    Label30: TLabel;
    edtCity: TEdit;
    Label31: TLabel;
    edtSubLocation: TEdit;
    Label32: TLabel;
    edtProvinceOrState: TEdit;
    Label33: TLabel;
    edtCountryCode: TEdit;
    Label34: TLabel;
    edtCountryName: TEdit;
    Label35: TLabel;
    edtOriginalTransmissionRef: TEdit;
    Label36: TLabel;
    edtHeadline: TEdit;
    Label37: TLabel;
    edtCredit: TEdit;
    Label38: TLabel;
    edtSource: TEdit;
    Label39: TLabel;
    edtCopyrightNotice: TEdit;
    Label40: TLabel;
    Label41: TLabel;
    memContacts: TMemo;
    Label42: TLabel;
    memCaptionOrAbstract: TMemo;
    Label43: TLabel;
    memWritersOrEditors: TMemo;
    lyoSpacer: TLayout;
    Label23: TLabel;
    rdoUnspecifiedImageOrientation: TRadioButton;
    rdoLandscape: TRadioButton;
    rdoPortrait: TRadioButton;
    rdoSquare: TRadioButton;
    procedure TabControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rctTabClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure NumberEditKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure btnClearDateClick(Sender: TObject);
    procedure GridEditingDone(Sender: TObject; const Col, Row: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FData: TIPTCData;
    FFileName: string;
    FLastTabIndex: Integer;
    FModified: Boolean;
    FTabFillColor: array [Boolean] of TAlphaColor;
    FTabPadding: array [Boolean] of TRectF;
    FTabSides: array [Boolean] of TSides;
    procedure ReloadControls;
    { IIPTCDocumentForm }
    function GetFileName: string;
    procedure ClearAll;
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    function FileIsModified: Boolean;
    procedure OpenFile(const AFileName: string);
    procedure SaveFile;
  end;

var
  frmIPTCEditor: TfrmIPTCEditor;

implementation

{$R *.fmx}

uses System.Math, CCR.Exif.BaseUtils;

resourcestring
  SConfirmSave = 'Current file has been changed. Save?';
  SUnrecognisedFileType = 'This program only works with JPEG or Adobe Photoshop (PSD) ' +
    'images, and ''%s'' is neither.';

{ TCalendarEdit }

procedure TCalendarEdit.DropDown;
begin
  if TDateTimeTagValue(Date).MissingOrInvalid then
    Date := Trunc(Now); //avoid exception later...
  inherited;
end;

procedure TCalendarEdit.SetText(const Value: string);
begin
  if TDateTimeTagValue(Date).MissingOrInvalid then
    inherited SetText('')
  else
    inherited SetText(Value);
end;

{ TGrid }

constructor TGrid.Create(AOwner: TComponent);
begin
  inherited;
  FData := TList<TIPTCRepeatablePair>.Create;
end;

destructor TGrid.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TGrid.AddNew;
var
  NewPair: TIPTCRepeatablePair;
begin
  RowCount := FData.Add(NewPair) + 1;
  Selected := RowCount - 1;
end;

function TGrid.CanRemoveSelected: Boolean;
begin
  Result := (RowCount >= 0);
end;

procedure TGrid.RemoveSelected;
var
  Index: Integer;
begin
  Index := Selected;
  FData.Delete(Index);
  RowCount := FData.Count;
  if RowCount > 0 then Selected := Max(0, Index - 1);
end;

function TGrid.GetValue(Col, Row: Integer): TValue;
var
  Pair: TIPTCRepeatablePair;
begin
  Pair := FData[Row];
  case Col of
    0: Result := Pair.Key;
    1: Result := Pair.Value;
  else Assert(False);
  end;
end;

procedure TGrid.SetValue(Col, Row: Integer; const Value: TValue);
var
  Pair: TIPTCRepeatablePair;
begin
  Pair := FData[Row];
  case Col of
    0: Pair.Key := Value.ToString;
    1: Pair.Value := Value.ToString;
  else Assert(False);
  end;
  FData[Row] := Pair;
end;

procedure TGrid.LoadDataFromArray(const NewData: TIPTCRepeatablePairs);
begin
  FData.Clear;
  FData.AddRange(NewData);
  RowCount := Length(NewData);
end;

function TGrid.SaveDataToArray: TIPTCRepeatablePairs;
var
  I: Integer;
begin
  Result := FData.ToArray;
  for I := High(Result) downto Low(Result) do
    if (Result[I].Key = '') and (Result[I].Value = '') then
      SetLength(Result, I);
end;

{ TfrmIPTCEditor }

procedure TfrmIPTCEditor.FormCreate(Sender: TObject);
var
  Comp: TComponent;
  Control, SBChild: TControl;
  MaxWidth: Single;
  I: Integer;
  Priorities: TArray<string>;
  ScrollBox: TVertScrollBox;
  SetAnchorsFor: TList<TControl>;
begin
  { Fix up anchors and control widths (can't do it at design time perfectly due to the
    fact the scroll bar width in the OS X theme is thinner than in the Windows one).
    A slight annoyance is that the size of a tab's client area is only propertly determined
    once the tab has been selected, so we take account of that here. }
  SetAnchorsFor := TList<TControl>.Create;
  try
    for I := TabControl.TabCount - 1 downto 0 do
    begin
      TabControl.TabIndex := I;
      for Control in TabControl.ActiveTab.Controls do
        if Control is TContent then
        begin
          ScrollBox := Control.Controls[0] as TVertScrollBox;
          ScrollBox.ApplyStyleLookup;
          ScrollBox.ShowScrollBars := True;
          MaxWidth := ScrollBox.ClientWidth - edtModelVersion.Position.Y;
          ScrollBox.BeginUpdate;
          try
            for SBChild in ScrollBox.Content.Controls do
              if SBChild.Width > 200 then
              begin
                SBChild.Width := MaxWidth - SBChild.Position.X;
                SetAnchorsFor.Add(SBChild);
              end;
          finally
            ScrollBox.EndUpdate;
          end;
          Break;
        end;
    end;
    for Control in SetAnchorsFor do
      Control.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
  finally
    SetAnchorsFor.Free;
  end;
  { Initialise combo boxes }
  Priorities := TArray<string>.Create('<none>', 'Lowest', 'Very low', 'Low', 'Normal',
    'Normal-high', 'High', 'Very high', 'Highest', 'User defined', 'Reserved');
  for Comp in Self do
    if (Comp is TCustomComboBox) and (TCustomComboBox(Comp).Items.Count = 0) then
      TCustomComboBox(Comp).Items.AddStrings(Priorities);
  { TTabControl.Position = tpBottom doesn't work properly, so we implement our own
    bottom aligned tab style. I'm also not keen on the TTabControl style for OS X
    either, so this kills two birds with one stone... }
  tabEnvelopeRec.TagObject := rctEnvelopeRecord;
  tabApplicationRec.TagObject := rctApplicationRec;
  FTabFillColor[True] := rctEnvelopeRecord.Fill.Color;
  FTabPadding[True] := rctEnvelopeRecord.Padding.Rect;
  FTabSides[True] := rctEnvelopeRecord.Sides;
  FTabFillColor[False] := rctApplicationRec.Fill.Color;
  FTabPadding[False] := rctApplicationRec.Padding.Rect;
  FTabSides[False] := rctApplicationRec.Sides;
  { Other }
  FData := TIPTCData.Create(Self);
  if TOSVersion.Platform = pfMacOS then
    ToolBar.Visible := False;
end;

procedure TfrmIPTCEditor.FormShow(Sender: TObject);
var
  Comp: TComponent;
begin
  OnShow := nil;
  { Set up actions (actions may be new to FMX, but this is an ancient IDE bug!) }
  btnOpen.Action := dtmController.actOpen;
  btnSave.Action := dtmController.actSave;
  btnClearAll.Action := dtmController.actClearAll;
  btnReload.Action := dtmController.actReload;
  btnClose.Action := dtmController.actClose;
  for Comp in Self do
    if Comp is TGrid then
      TGrid(Comp).PopupMenu := dtmController.mnuRepeatingValue;
end;

procedure TfrmIPTCEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmIPTCEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FModified then
  begin
    case MessageDlg(SConfirmSave, TMsgDlgType.mtConfirmation, mbYesNoCancel, 0) of
      mrYes:
        try
          SaveFile;
        except
          CanClose := False;
          Application.HandleException(ExceptObject);
        end;
      mrNo: {OK};
    else CanClose := False;
    end;
  end;
end;

procedure TfrmIPTCEditor.ClearAll;
var
  Comp: TComponent;
begin
  for Comp in Self do
    if Comp is TCustomEdit then
      TCustomEdit(Comp).Text := ''
    else
      if Comp is TCheckBox then
        TCheckBox(Comp).IsChecked := False
      else
        if Comp is TGrid then
          TGrid(Comp).LoadDataFromArray(nil)
        else
          if Comp is TMemo then
            TMemo(Comp).Text := '';
end;

function TfrmIPTCEditor.FileIsModified: Boolean;
begin
  Result := FModified;
end;

function TfrmIPTCEditor.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TfrmIPTCEditor.GotoNextTab;
begin
  if TabControl.TabIndex = TabControl.TabCount - 1 then
    TabControl.TabIndex := 0
  else
    TabControl.TabIndex := TabControl.TabIndex + 1;
end;

procedure TfrmIPTCEditor.GotoPreviousTab;
begin
  if TabControl.TabIndex = 0 then
    TabControl.TabIndex := TabControl.TabCount - 1
  else
    TabControl.TabIndex := TabControl.TabIndex - 1;
end;

procedure TfrmIPTCEditor.OpenFile(const AFileName: string);
var
  NewData: TIPTCData;
begin
  { Use a temporary TIPTCData in case of an exception on load, whether raised in
    LoadFromGraphic or otherwise }
  NewData := TIPTCData.Create;
  try
    if not NewData.LoadFromGraphic(AFileName) then
      raise EInvalidImage.CreateFmt(SUnrecognisedFileType, [ExtractFileName(AFileName)]);
    FData.Assign(NewData);
  finally
    NewData.Free;
  end;
  ReloadControls;
  FModified := False;
  lyoTabs.Visible := True;
  TabControl.Visible := True;
  FFileName := AFileName;
end;

procedure TfrmIPTCEditor.ReloadControls;
  procedure LoadRepeatablePairs(const AGrid: TStringGrid; const APairs: TIPTCRepeatablePairs);
  var
    I: Integer;
  begin
    AGrid.RowCount := Length(APairs);
    for I := 0 to High(APairs) do
    begin
      AGrid.Cells[0, I] := APairs[I].Key;
      AGrid.Cells[1, I] := APairs[I].Value;
    end;
  end;
begin
  lblNoTagsFound.Visible := FData.Empty;
  //envelope record
  edtModelVersion.Text := FData.ModelVersion.ToString;
  edtDestination.Text := FData.Destination;
  edtFileFormat.Text := FData.FileFormat.ToString;
  edtFileFormatVersion.Text := FData.FileFormatVersion.ToString;
  edtServiceIdentifier.Text := FData.ServiceIdentifier;
  edtEnvelopeNumber.Text := FData.EnvelopeNumberString;
  edtProductID.Text := FData.ProductID;
  cboEnvelopePriority.ItemIndex := Ord(FData.EnvelopePriority);
  edtDateSent.Date := FData.DateSent;
  edtUNOCode.Text := FData.UNOCode;
  edtARMIdentifier.Text := FData.ARMIdentifier.ToString;
  edtARMVersion.Text := FData.ARMVersion.ToString;
  //application record
  edtRecordVersion.Text := FData.RecordVersion.ToString;
  edtObjectTypeRef.Text := FData.ObjectTypeRef;
  edtObjectAttributeRef.Text := FData.ObjectAttributeRef;
  edtObjectName.Text := FData.ObjectName;
  edtEditStatus.Text := FData.EditStatus;
  cboUrgency.ItemIndex := Ord(FData.Urgency);
  memSubjectRefs.Text := FData.SubjectRefs.Join(SLineBreak);
  edtCategoryCode.Text := FData.CategoryCode;
  memSupplementaryCategories.Text := FData.SupplementaryCategories.Join(SLineBreak);
  edtFixtureIdentifier.Text := FData.FixtureIdentifier;
  memKeywords.Text := FData.Keywords.Join(SLineBreak);
  stgContentLocations.LoadDataFromArray(FData.ContentLocations);
  edtReleaseDate.Date := FData.ReleaseDate;
  edtExpirationDate.Date := FData.ExpirationDate;
  edtOriginatingProgram.Text := FData.OriginatingProgram;
  edtProgramVersion.Text := FData.ProgramVersion;
  edtObjectCycleCode.Text := FData.ObjectCycleCode;
  stgBylines.LoadDataFromArray(FData.BylineDetails);
  edtCity.Text := FData.City;
  edtSubLocation.Text := FData.SubLocation;
  edtProvinceOrState.Text := FData.ProvinceOrState;
  edtCountryCode.Text := FData.CountryCode;
  edtCountryName.Text := FData.CountryName;
  edtOriginalTransmissionRef.Text := FData.OriginalTransmissionRef;
  edtHeadline.Text := FData.Headline;
  edtCredit.Text := FData.Credit;
  edtSource.Text := FData.Source;
  edtCopyrightNotice.Text := FData.CopyrightNotice;
  memContacts.Text := FData.Contacts.Join(SLineBreak);
  memCaptionOrAbstract.Text := FData.CaptionOrAbstract;
  memWritersOrEditors.Text := FData.WritersOrEditors.Join(SLineBreak);
  case FData.ImageOrientation of
    ioLandscape: rdoLandscape.IsChecked := True;
    ioPortrait: rdoPortrait.IsChecked := True;
    ioSquare: rdoSquare.IsChecked := True;
  else rdoUnspecifiedImageOrientation.IsChecked := True;
  end;
end;

procedure TfrmIPTCEditor.SaveFile;
begin
  //envelope record
  FData.ModelVersion := TWordTagValue.CreateFromString(edtModelVersion.Text);
  FData.Destination := edtDestination.Text;
  FData.FileFormat := TWordTagValue.CreateFromString(edtFileFormat.Text);
  FData.FileFormatVersion := TWordTagValue.CreateFromString(edtFileFormatVersion.Text);
  FData.ServiceIdentifier := edtServiceIdentifier.Text;
  FData.EnvelopeNumberString := edtEnvelopeNumber.Text;
  FData.ProductID := edtProductID.Text;
  FData.EnvelopePriority := TIPTCPriority(cboEnvelopePriority.ItemIndex);
  FData.DateSent := edtDateSent.Date;
  FData.UNOCode := edtUNOCode.Text;
  FData.ARMIdentifier := TWordTagValue.CreateFromString(edtARMIdentifier.Text);
  FData.ARMVersion := TWordTagValue.CreateFromString(edtARMVersion.Text);
  //application record
  FData.RecordVersion := TWordTagValue.CreateFromString(edtRecordVersion.Text);
  FData.ObjectTypeRef := edtObjectTypeRef.Text;
  FData.ObjectAttributeRef := edtObjectAttributeRef.Text;
  FData.ObjectName := edtObjectName.Text;
  FData.EditStatus := edtEditStatus.Text;
  FData.Urgency := TIPTCPriority(cboUrgency.ItemIndex);
  FData.SubjectRefs := TIPTCStringArray.CreateFromStrings(memSubjectRefs.Lines);
  FData.CategoryCode := edtCategoryCode.Text;
  FData.SupplementaryCategories := TIPTCStringArray.CreateFromStrings(memSupplementaryCategories.Lines);
  FData.FixtureIdentifier := edtFixtureIdentifier.Text;
  FData.Keywords := TIPTCStringArray.CreateFromStrings(memKeywords.Lines);
  FData.ContentLocations := stgContentLocations.SaveDataToArray;
  FData.ReleaseDate := edtReleaseDate.Date;
  FData.ExpirationDate := edtExpirationDate.Date;
  FData.OriginatingProgram := edtOriginatingProgram.Text;
  FData.ProgramVersion := edtProgramVersion.Text;
  FData.ObjectCycleCode := edtObjectCycleCode.Text;
  FData.BylineDetails := stgBylines.SaveDataToArray;
  FData.City := edtCity.Text;
  FData.SubLocation := edtSubLocation.Text;
  FData.ProvinceOrState := edtProvinceOrState.Text;
  FData.CountryCode := edtCountryCode.Text;
  FData.CountryName := edtCountryName.Text;
  FData.OriginalTransmissionRef := edtOriginalTransmissionRef.Text;
  FData.Headline := edtHeadline.Text;
  FData.Credit := edtCredit.Text;
  FData.Source := edtSource.Text;
  FData.CopyrightNotice := edtCopyrightNotice.Text;
  FData.Contacts := TIPTCStringArray.CreateFromStrings(memContacts.Lines);
  FData.CaptionOrAbstract := memCaptionOrAbstract.Text;
  FData.WritersOrEditors := TIPTCStringArray.CreateFromStrings(memWritersOrEditors.Lines);
  if rdoLandscape.IsChecked then
    FData.ImageOrientation := ioLandscape
  else if rdoPortrait.IsChecked then
    FData.ImageOrientation := ioPortrait
  else if rdoSquare.IsChecked then
    FData.ImageOrientation := ioSquare;
  //write the changes
  FData.SaveToGraphic(FFileName);
end;

procedure TfrmIPTCEditor.GridEditingDone(Sender: TObject; const Col,
  Row: Integer);
begin
  ControlChanged(Sender);
end;

procedure TfrmIPTCEditor.ControlChanged(Sender: TObject);
begin
  FModified := True;
end;

procedure TfrmIPTCEditor.btnClearDateClick(Sender: TObject);
var
  Obj: TFmxObject;
begin
  Obj := Sender as TFmxObject;
  while not (Obj is TCalendarEdit) do
  begin
    Assert(Obj <> nil);
    Obj := Obj.Parent;
  end;
  TCalendarEdit(Obj).Date := TDateTimeTagValue.CreateMissingOrInvalid;
end;

procedure TfrmIPTCEditor.NumberEditKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Shift - [ssAlt, ssCtrl, ssCommand] <> [] then
    case KeyChar of
      '0'..'9': {OK}
    else
      KeyChar := #0;
      Beep;
    end;
end;

procedure TfrmIPTCEditor.rctTabClick(Sender: TObject);
begin
  TabControl.TabIndex := (Sender as TRectangle).Tag;
end;

procedure TfrmIPTCEditor.TabControlChange(Sender: TObject);
  procedure UpdateTabRectangle(TabIndex: Integer; Selected: Boolean);
  var
    RectCtrl: TRectangle;
  begin
    RectCtrl := TabControl.Tabs[TabIndex].TagObject as TRectangle;
    RectCtrl.Fill.Color := FTabFillColor[Selected];
    RectCtrl.Padding.Rect := FTabPadding[Selected];
    RectCtrl.Sides := FTabSides[Selected];
  end;
begin
  if not Visible then Exit;
  UpdateTabRectangle(FLastTabIndex, False);
  FLastTabIndex := TabControl.TabIndex;
  UpdateTabRectangle(FLastTabIndex, True);
end;

initialization
  TfrmIPTCEditor.ClassName;
end.
