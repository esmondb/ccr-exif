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
{ The Original Code is IPTCEditForm.pas.                                               }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit IPTCEditForm;
{
  Simple editor for IPTC data stored in JPEG files. You'll need to edit this
  source file slightly if you want it to work 100% correctly when compiling
  under D2006 (see where the BUGGYCOMPILER define is used - basically,
  intermediate variables are needed, like what I've done for ExifListFrame.pas).
}
interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs, Buttons,   
  ActnList, StdActns, ComCtrls, StdCtrls, ExtCtrls, Grids, ValEdit, CCR.Exif.Demos;

type
  TValueListEditor = class(ValEdit.TValueListEditor)
  protected
    procedure DrawCell(ACol: Integer; ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override; //use gray text if ctrl disabled
    procedure Loaded; override;
  end;

  TfrmIPTC = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ActionList: TActionList;
    EditSelectAll1: TEditSelectAll;
    actOpen: TAction;
    dlgOpen: TOpenPictureDialog;
    scbEditorial: TScrollBox;
    Label23: TLabel;
    edtFixtureIdentifier: TEdit;
    edtObjectTypeRef: TEdit;
    Label21: TLabel;
    Label19: TLabel;
    edtObjectAttributeRef: TEdit;
    edtCategoryCode: TEdit;
    edtRecordVersion: TEdit;
    edtObjectName: TEdit;
    memSubjectRefs: TMemo;
    Label17: TLabel;
    Label18: TLabel;
    Label13: TLabel;
    Label22: TLabel;
    Label14: TLabel;
    edtEditStatus: TEdit;
    Label20: TLabel;
    Label15: TLabel;
    cboUrgency: TComboBox;
    memSupplementaryCats: TMemo;
    scbEnvelope: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    cboEnvelopePriority: TComboBox;
    edtEnvelopeNumber: TEdit;
    edtModelVersion: TEdit;
    edtDestination: TEdit;
    edtFileFormatVersion: TEdit;
    edtServiceIdentifier: TEdit;
    edtARMVersion: TEdit;
    edtProductID: TEdit;
    dtpDateSent: TDateTimePicker;
    edtFileFormat: TEdit;
    edtUNOCode: TEdit;
    edtARMIdentifier: TEdit;
    Label16: TLabel;
    memKeywords: TMemo;
    Label24: TLabel;
    vleContentLocations: TValueListEditor;
    Label25: TLabel;
    dtpReleaseDate: TDateTimePicker;
    Label26: TLabel;
    dtpExpirationDate: TDateTimePicker;
    Label27: TLabel;
    edtSpecialInstructions: TEdit;
    Label28: TLabel;
    Label29: TLabel;
    dtpDateCreated: TDateTimePicker;
    Label30: TLabel;
    dtpDigitalCreationDate: TDateTimePicker;
    Label31: TLabel;
    edtOriginatingProgram: TEdit;
    Label32: TLabel;
    edtProgramVersion: TEdit;
    Label33: TLabel;
    edtObjectCycleCode: TEdit;
    Label34: TLabel;
    vleBylines: TValueListEditor;
    Label35: TLabel;
    edtCity: TEdit;
    edtSubLocation: TEdit;
    Label36: TLabel;
    Label37: TLabel;
    edtProvinceOrState: TEdit;
    edtCountryCode: TEdit;
    Label38: TLabel;
    edtCountryName: TEdit;
    Label39: TLabel;
    Label40: TLabel;
    edtOriginalTransmissionRef: TEdit;
    Label41: TLabel;
    edtHeadline: TEdit;
    edtCredit: TEdit;
    Label42: TLabel;
    edtSource: TEdit;
    Label43: TLabel;
    edtCopyrightNotice: TEdit;
    Label44: TLabel;
    Label45: TLabel;
    memContacts: TMemo;
    Label46: TLabel;
    memCaptionOrAbstract: TMemo;
    Label47: TLabel;
    memWritersOrEditors: TMemo;
    actSaveOrReload: TAction;
    panActions: TPanel;
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    panFooter: TPanel;
    lblTagsWith: TLabel;
    lblHighlighted: TLabel;
    lblLabelsHaveEtc: TLabel;
    lblNoTagsFound: TLabel;
    btnClear: TBitBtn;
    btnReload: TBitBtn;
    cboActionAdvised: TComboBox;
    Label48: TLabel;
    cboImageOrientation: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure ControlChange(Sender: TObject);
    procedure actSaveOrReloadExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnClearClick(Sender: TObject);
  private
    FSizeGrip: TWinControl;
    FTagControlLabels: array of TLabel;
    procedure UpdateLabelHighlighting(DueToSaveNotLoad: Boolean);
  protected
    procedure DoFileOpen(const FileName: string; const FileToCompare: string = ''); override;
    procedure Resizing(State: TWindowState); override;
  end;

var
  frmIPTC: TfrmIPTC;

implementation

uses Math, CCR.Exif.BaseUtils, CCR.Exif.IPTC, CCR.SizeGripCtrl;

{$R *.dfm}

{$IF CompilerVersion <= 18.0}
  {$DEFINE BUGGYCOMPILER}
{$IFEND}

{$IF NOT Declared(NativeInt)}
type
  NativeInt = Integer;
{$IFEND}

resourcestring
  SConfirmSave = 'Current file has been changed. Save?';
  SConfirmReload = 'Are you sure you want to reload? Any changes made will be lost if you confirm.';
  SUnrecognisedFileType = 'This program only works with JPEG or Adobe Photoshop (PSD) ' +
    'images, and ''%s'' is neither.';

{ TValueListEditor }

procedure TValueListEditor.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  if not Enabled then Canvas.Font.Color := clGrayText;
  inherited;
end;

procedure TValueListEditor.Loaded;
begin
  inherited;
  if Screen.PixelsPerInch <> 96 then
    DefaultRowHeight := Abs(Font.Height) + 11;
end;

{ TfrmIPTC }

procedure TfrmIPTC.FormCreate(Sender: TObject);
var
  Comp: TComponent;
  CompAsCtrl: TControl absolute Comp;
  Counter: Integer;
  W, H: Integer;
begin
  W := GetSystemMetrics(SM_CXVSCROLL);
  Width := Width + W;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  SetLength(FTagControlLabels, ComponentCount);
  Counter := 0;
  for Comp in Self do
    if (Comp is TControl) and (CompAsCtrl.Parent is TScrollBox) then
    begin
      CompAsCtrl.Enabled := False;
      if Comp is TLabel then
      begin
        Assert(TLabel(Comp).FocusControl <> nil);
        FTagControlLabels[Counter] := TLabel(Comp);
        TLabel(Comp).FocusControl.Tag := NativeInt(Comp);
        Inc(Counter);
      end
      else if (Comp is TComboBox) and (TComboBox(Comp).Items.Count = 0) then
        with TComboBox(Comp).Items do
        begin
          Add('<none>'); Add('Lowest'); Add('Very low'); Add('Low'); Add('Normal');
          Add('Normal-high'); Add('High'); Add('Very high'); Add('Highest');
          Add('User defined'); Add('Reserved');
        end;
      if (CompAsCtrl.Parent = scbEditorial) and (akRight in CompAsCtrl.Anchors) then
        CompAsCtrl.Width := CompAsCtrl.Width - W;
    end;
  SetLength(FTagControlLabels, Counter);
  with cboActionAdvised.Items do
  begin
    Objects[1] := TObject(iaObjectKill);
    Objects[2] := TObject(iaObjectReplace);
    Objects[3] := TObject(iaObjectAppend);
    Objects[4] := TObject(iaObjectAppend);
  end;
  with cboImageOrientation.Items do
  begin
    Objects[1] := TObject(ioLandscape);
    Objects[2] := TObject(ioPortrait);
    Objects[3] := TObject(ioSquare);
  end;
  FSizeGrip := TSizeGrip.Create(Self);
  FSizeGrip.Parent := Self;
  W := MulDiv(Width, 3, 2);
  H := MulDiv(Height, 3, 2);
  with Screen.WorkAreaRect do
  begin
    if Self.Left + W <= Right then Self.Width := W;
    if Self.Top + H <= Bottom then Self.Height := H;
  end;
  SupportOpeningFiles := True;
end;

procedure TfrmIPTC.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not actSaveOrReload.Enabled then Exit;
  case MessageDlg(SConfirmSave, mtConfirmation, mbYesNoCancel, 0) of
    mrYes: actSaveOrReload.Execute;
    mrNo: ;
  else CanClose := False;
  end;
end;

procedure TfrmIPTC.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  function IsDroppedDown: Boolean;
  begin
    if ActiveControl is TCustomCombo then
      Result := TCustomCombo(ActiveControl).DroppedDown
    else if ActiveControl is TDateTimePicker then
      Result := TDateTimePicker(ActiveControl).DroppedDown
    else
      Result := False;
  end;

  procedure DoScrollBox;
  var
    ScrollBox: TScrollBox;
  begin
    Handled := True;
    ScrollBox := PageControl.ActivePage.Controls[0] as TScrollBox;
    ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position - WheelDelta div 10;
  end;
begin
  if (ActiveControl = nil) or (ActiveControl = PageControl) or
    (ActiveControl.ClassType = TEdit) or (ActiveControl.Parent = panActions) then
    DoScrollBox
  else if (FindVCLWindow(Mouse.CursorPos) <> ActiveControl) and not IsDroppedDown then
    DoScrollBox;
end;

procedure TfrmIPTC.Resizing(State: TWindowState);
begin
  inherited;
  if FSizeGrip <> nil then FSizeGrip.Visible := (State <> wsMaximized);
end;

procedure TfrmIPTC.ControlChange(Sender: TObject);
begin
  actSaveOrReload.Enabled := True;
end;

procedure TfrmIPTC.DoFileOpen(const FileName: string; const FileToCompare: string = '');
  procedure DoDate(Control: TDateTimePicker; const Date: TDateTimeTagValue);
  begin
    if Date.MissingOrInvalid then
      Control.Date := Now
    else
      Control.Date := Date;
    Control.Checked := not Date.MissingOrInvalid;
  end;

  procedure DoEnum(Control: TComboBox; const OrdValue: NativeInt);
  begin
    with Control do
      ItemIndex := Max(0, Items.IndexOfObject(TObject(OrdValue)));
  end;

  procedure DoPriority(Control: TComboBox; const Value: TIPTCPriority);
  begin
    Control.ItemIndex := Ord(Value);
  end;

  procedure DoRepeatable(Control: TMemo; const Value: TStringDynArray);
  var
    S: string;
  begin
    Control.Lines.BeginUpdate;
    try
      Control.Lines.Clear;
      for S in Value do
        Control.Lines.Add(S);
    finally
      Control.Lines.EndUpdate;
    end;
  end;

var
  Comp: TComponent;
  IPTCData: TIPTCData;
begin
  IPTCData := TIPTCData.Create;
  try
    if not IPTCData.LoadFromGraphic(FileName) then
      raise EInvalidGraphic.CreateFmt(SUnrecognisedFileType, [ExtractFileName(FileName)]);
    //envelope record
    {$IFNDEF BUGGYCOMPILER}
    edtModelVersion.Text := IPTCData.ModelVersion.AsString;
    {$ENDIF}
    edtDestination.Text := IPTCData.Destination;
    {$IFNDEF BUGGYCOMPILER}
    edtFileFormat.Text := IPTCData.FileFormat.AsString;
    edtFileFormatVersion.Text := IPTCData.FileFormatVersion.AsString;
    {$ENDIF}
    edtServiceIdentifier.Text := IPTCData.ServiceIdentifier;
    edtEnvelopeNumber.Text := IPTCData.EnvelopeNumberString;
    edtProductID.Text := IPTCData.ProductID;
    DoPriority(cboEnvelopePriority, IPTCData.EnvelopePriority);
    DoDate(dtpDateSent, IPTCData.DateSent);
    edtUNOCode.Text := IPTCData.UNOCode;
    {$IFNDEF BUGGYCOMPILER}
    edtARMIdentifier.Text := IPTCData.ARMIdentifier.AsString;
    edtARMVersion.Text := IPTCData.ARMVersion.AsString;
    {$ENDIF}
    //editorial record
    {$IFNDEF BUGGYCOMPILER}
    edtRecordVersion.Text := IPTCData.RecordVersion.AsString;
    {$ENDIF}
    edtObjectTypeRef.Text := IPTCData.ObjectTypeRef;
    edtObjectAttributeRef.Text := IPTCData.ObjectAttributeRef;
    edtObjectName.Text := IPTCData.ObjectName;
    edtEditStatus.Text := IPTCData.EditStatus;
    DoPriority(cboUrgency, IPTCData.Urgency);
    DoRepeatable(memSubjectRefs, IPTCData.SubjectRefs);
    edtCategoryCode.Text := IPTCData.CategoryCode;
    DoRepeatable(memSupplementaryCats, IPTCData.SupplementaryCategories);
    edtFixtureIdentifier.Text := IPTCData.FixtureIdentifier;
    DoRepeatable(memKeywords, IPTCData.Keywords);
    IPTCData.GetContentLocationValues(vleContentLocations.Strings);
    DoDate(dtpReleaseDate, IPTCData.ReleaseDate);
    DoDate(dtpExpirationDate, IPTCData.ExpirationDate);
    edtSpecialInstructions.Text := IPTCData.SpecialInstructions;
    DoEnum(cboActionAdvised, Ord(IPTCData.ActionAdvised));
    DoDate(dtpDateCreated, IPTCData.DateCreated);
    DoDate(dtpDigitalCreationDate, IPTCData.DigitalCreationDate);
    edtOriginatingProgram.Text := IPTCData.OriginatingProgram;
    edtProgramVersion.Text := IPTCData.ProgramVersion;
    edtObjectCycleCode.Text := IPTCData.ObjectCycleCode;
    IPTCData.GetBylineValues(vleBylines.Strings);
    edtCity.Text := IPTCData.City;
    edtSubLocation.Text := IPTCData.SubLocation;
    edtProvinceOrState.Text := IPTCData.ProvinceOrState;
    edtCountryCode.Text := IPTCData.CountryCode;
    edtCountryName.Text := IPTCData.CountryName;
    edtOriginalTransmissionRef.Text := IPTCData.OriginalTransmissionRef;
    edtHeadline.Text := IPTCData.Headline;
    edtCredit.Text := IPTCData.Credit;
    edtSource.Text := IPTCData.Source;
    edtCopyrightNotice.Text := IPTCData.CopyrightNotice;
    DoRepeatable(memContacts, IPTCData.Contacts);
    memCaptionOrAbstract.Text := IPTCData.CaptionOrAbstract;
    DoRepeatable(memWritersOrEditors, IPTCData.WritersOrEditors);
    DoEnum(cboImageOrientation, Ord(IPTCData.ImageOrientation));
  finally
    IPTCData.Free;
  end;
  actSaveOrReload.Enabled := False;
  btnClear.Enabled := True;
  for Comp in Self do
    if (Comp is TControl) and (TControl(Comp).Parent is TScrollBox) then
      TControl(Comp).Enabled := True;
  UpdateLabelHighlighting(False);
  panFooter.Show;
  if FileToCompare <> '' then CreateNewExeInstance(FileToCompare);
end;

procedure TfrmIPTC.UpdateLabelHighlighting(DueToSaveNotLoad: Boolean);
var
  Ctrl: TLabel;
  DoneAtLeastOne: Boolean;

  procedure TryIt(DoIt: Boolean);
  begin
    if DoIt then
    begin
      DoneAtLeastOne := True;
      Ctrl.Font.Color := clHotLight;
    end
    else
      Ctrl.ParentFont := True;
  end;
begin
  DoneAtLeastOne := DueToSaveNotLoad;
  for Ctrl in FTagControlLabels do
    if Ctrl.FocusControl is TCustomEdit then
      TryIt(TCustomEdit(Ctrl.FocusControl).GetTextLen > 0)
    else if Ctrl.FocusControl is TDateTimePicker then
      TryIt(TDateTimePicker(Ctrl.FocusControl).Checked)
    else if Ctrl.FocusControl is TComboBox then
      TryIt(TComboBox(Ctrl.FocusControl).ItemIndex > 0)
    else if Ctrl.FocusControl is TValueListEditor then
      TryIt(TValueListEditor(Ctrl.FocusControl).Strings.Count > 0)
    else
      Assert(False);
  lblNoTagsFound.Visible := not DoneAtLeastOne;
end;

procedure TfrmIPTC.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

procedure TfrmIPTC.actSaveOrReloadExecute(Sender: TObject);
  function GetDateValue(Control: TDateTimePicker): TDateTimeTagValue;
  begin
    if Control.Checked then
      Result := Control.Date
    else
      Result := TDateTimeTagValue.CreateMissingOrInvalid;
  end;

  function GetEnumValue(Control: TComboBox): NativeInt;
  begin
    Result := NativeInt(Control.Items.Objects[Control.ItemIndex]);
  end;

  function GetPriorityValue(Combo: TComboBox): TIPTCPriority;
  begin
    Result := TIPTCPriority(Combo.ItemIndex);
  end;

  function GetRepeatableValue(Control: TMemo): TStringDynArray;
  var
    I: Integer;
  begin
    SetLength(Result, Control.Lines.Count);
    for I := 0 to High(Result) do
      Result[I] := Control.Lines[I];
  end;

  function GetWordValue(Control: TEdit): TWordTagValue;
  begin
    Result := TWordTagValue.CreateFromString(Control.Text);
  end;
var
  IPTCData: TIPTCData;
begin
  if actSaveOrReload.ActionComponent = btnReload then
  begin
    if IsPositiveResult(MessageDlg(SConfirmReload, mtConfirmation, mbYesNo, 0))  then
      OpenFile(FileName);
    actSaveOrReload.ActionComponent := nil;
    Exit;
  end;
  IPTCData := TIPTCData.Create;
  try
    IPTCData.LoadFromGraphic(FileName);
    //envelope record
    IPTCData.ModelVersion := GetWordValue(edtModelVersion);
    IPTCData.Destination := edtDestination.Text;
    IPTCData.FileFormat := GetWordValue(edtFileFormat);
    IPTCData.FileFormatVersion := GetWordValue(edtFileFormatVersion);
    IPTCData.ServiceIdentifier := edtServiceIdentifier.Text;
    IPTCData.EnvelopeNumberString := edtEnvelopeNumber.Text;
    IPTCData.ProductID := edtProductID.Text;
    IPTCData.EnvelopePriority := GetPriorityValue(cboEnvelopePriority);
    IPTCData.DateSent := GetDateValue(dtpDateSent);
    IPTCData.UNOCode := edtUNOCode.Text;
    IPTCData.ARMIdentifier := GetWordValue(edtARMIdentifier);
    IPTCData.ARMVersion := GetWordValue(edtARMVersion);
    //editorial record
    IPTCData.RecordVersion := GetWordValue(edtRecordVersion);
    IPTCData.ObjectTypeRef := edtObjectTypeRef.Text;
    IPTCData.ObjectAttributeRef := edtObjectAttributeRef.Text;
    IPTCData.ObjectName := edtObjectName.Text;
    IPTCData.EditStatus := edtEditStatus.Text;
    IPTCData.Urgency := GetPriorityValue(cboUrgency);
    IPTCData.SubjectRefs := GetRepeatableValue(memSubjectRefs);
    IPTCData.CategoryCode := edtCategoryCode.Text;
    IPTCData.SupplementaryCategories := GetRepeatableValue(memSupplementaryCats);
    IPTCData.FixtureIdentifier := edtFixtureIdentifier.Text;
    IPTCData.Keywords := GetRepeatableValue(memKeywords);
    IPTCData.SetContentLocationValues(vleContentLocations.Strings);
    IPTCData.ReleaseDate := GetDateValue(dtpReleaseDate);
    IPTCData.ExpirationDate := GetDateValue(dtpExpirationDate);
    IPTCData.SpecialInstructions := edtSpecialInstructions.Text;
    IPTCData.ActionAdvised := TIPTCActionAdvised(GetEnumValue(cboActionAdvised));
    IPTCData.DateCreated := GetDateValue(dtpDateCreated);
    IPTCData.DigitalCreationDate := GetDateValue(dtpDigitalCreationDate);
    IPTCData.OriginatingProgram := edtOriginatingProgram.Text;
    IPTCData.ProgramVersion := edtProgramVersion.Text;
    IPTCData.ObjectCycleCode := edtObjectCycleCode.Text;
    IPTCData.SetBylineValues(vleBylines.Strings);
    IPTCData.City := edtCity.Text;
    IPTCData.SubLocation := edtSubLocation.Text;
    IPTCData.ProvinceOrState := edtProvinceOrState.Text;
    IPTCData.CountryCode := edtCountryCode.Text;
    IPTCData.CountryName := edtCountryName.Text;
    IPTCData.OriginalTransmissionRef := edtOriginalTransmissionRef.Text;
    IPTCData.Headline := edtHeadline.Text;
    IPTCData.Credit := edtCredit.Text;
    IPTCData.Source := edtSource.Text;
    IPTCData.CopyrightNotice := edtCopyrightNotice.Text;
    IPTCData.Contacts := GetRepeatableValue(memContacts);
    IPTCData.CaptionOrAbstract := memCaptionOrAbstract.Text;
    IPTCData.WritersOrEditors := GetRepeatableValue(memWritersOrEditors);
    IPTCData.ImageOrientation := TIPTCImageOrientation(GetEnumValue(cboImageOrientation));
    //finish up...
    IPTCData.SaveToGraphic(FileName);
  finally
    IPTCData.Free;
  end;
  UpdateLabelHighlighting(True);
  actSaveOrReload.Enabled := False;
end;

procedure TfrmIPTC.btnClearClick(Sender: TObject);
var
  LabelCtrl: TLabel;
begin
  for LabelCtrl in FTagControlLabels do
    if LabelCtrl.FocusControl is TCustomEdit then
      TCustomEdit(LabelCtrl.FocusControl).Clear
    else if LabelCtrl.FocusControl is TDateTimePicker then
      TDateTimePicker(LabelCtrl.FocusControl).Checked := False
    else if LabelCtrl.FocusControl is TComboBox then
      TComboBox(LabelCtrl.FocusControl).ItemIndex := 0
    else if LabelCtrl.FocusControl is TValueListEditor then
      TValueListEditor(LabelCtrl.FocusControl).Strings.Clear
    else
      Assert(False);
end;

procedure TfrmIPTC.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
