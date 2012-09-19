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

unit TimeShiftForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  FMX.Effects, FMX.Edit, FMX.Grid, FMX.Layouts, FMX.Menus, FMX.StdActns, System.Actions,
  FMX.ActnList, CCR.Exif, CCR.Exif.FMXUtils, ImageForm;

type
  TfrmExifTimeShift = class(TForm)
    ToolBar1: TToolBar;
    btnAdd: TSpeedButton;
    Image1: TImage;
    btnProcess: TSpeedButton;
    Image2: TImage;
    Line1: TLine;
    Label1: TLabel;
    BevelEffect1: TBevelEffect;
    spbMinsToShift: TSpinBox;
    btnRemove: TSpeedButton;
    Image3: TImage;
    Line2: TLine;
    BevelEffect2: TBevelEffect;
    btnOptions: TSpeedButton;
    Image4: TImage;
    grdFiles: TGrid;
    colFileName: TColumn;
    colFileDateTIme: TColumn;
    colExifDateTime: TColumn;
    colDateTimeOriginal: TColumn;
    colDateTimeDigitized: TColumn;
    dlgOpen: TOpenDialog;
    MacMenu: TMainMenu;
    itmApp: TMenuItem;
    itmAbout: TMenuItem;
    itmFile: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    itmProcess: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ActionList: TActionList;
    actAdd: TAction;
    actAppQuit: TFileExit;
    actAppHide: TFileHideApp;
    actAppHideOthers: TFileHideAppOthers;
    actMinimizeWindow: TAction;
    actZoomWindow: TAction;
    actAppShowAll: TAction;
    actBringAllToFront: TAction;
    actAppAbout: TAction;
    actAppPreferences: TAction;
    MenuItem2: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    actRemove: TAction;
    actProcess: TAction;
    actShowImage: TAction;
    itmView: TMenuItem;
    itmShowImage: TMenuItem;
    Line3: TLine;
    BevelEffect3: TBevelEffect;
    btnShowImage: TSpeedButton;
    Image5: TImage;
    actToggleFullScreen: TAction;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    itmWindow: TMenuItem;
    itmMinimize: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    itmBringAllToFront: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure grdFilesGetValue(Sender: TObject; const Col, Row: Integer;
      var Value: TValue);
    procedure FormDestroy(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure spbMinsToShiftKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure actProcessExecute(Sender: TObject);
    procedure actProcessUpdate(Sender: TObject);
    procedure actRemoveUpdate(Sender: TObject);
    procedure actAppPreferencesExecute(Sender: TObject);
    procedure grdFilesKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure actShowImageExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
    procedure actShowImageUpdate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  strict private
    FData: TObjectList<TExifDataPatcher>;
    procedure ImageFormActivate(Sender: TObject);
  end;

var
  frmExifTimeShift: TfrmExifTimeShift;

implementation

{$R *.fmx}

uses System.DateUtils, CCR.Exif.BaseUtils, TimeShiftOptions, TimeShiftOptionsForm;

function DateTimeToText(const ADateTime: TDateTimeTagValue): string;
begin
  if ADateTime.MissingOrInvalid then
    Result := '<missing>'
  else
    Result := DateTimeToStr(ADateTime);
end;

procedure TfrmExifTimeShift.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  { Only assign OnOpenFile here to avoid an error dialog being shown before the
    main form itself is. }
  TFileManager.OnOpenFile :=
    procedure (const AFileName: string)
    var
      EDP: TExifDataPatcher;
    begin
      if FileIsReadOnly(AFileName) then
      begin
        MessageDlg(Format('Cannot patch a read-only file (%s)', [AFileName]),
          TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
        Exit;
      end;
      EDP := TExifDataPatcher.Create(AFileName);
      try
        if EDP.Empty then
          MessageDlg(Format('Cannot process image because it does not contain any Exif metadata (%s)', [AFileName]),
            TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
        else
        begin
          FData.Add(EDP);
          grdFiles.RowCount := grdFiles.RowCount + 1;
        end;
      except
        EDP.Free;
        raise;
      end;
    end;
end;

procedure TfrmExifTimeShift.FormCreate(Sender: TObject);
begin
  FData := TObjectList<TExifDataPatcher>.Create;
  TFileManager.DocumentMode := dmMultiplePerForm;
  if TOSVersion.Platform = pfMacOS then
  begin
    TMacCommands.InitializeAboutAction(actAppAbout);
    TMacCommands.InitializeShowAllAction(actAppShowAll);
    TMacCommands.InitializeToggleFullScreenAction(actToggleFullScreen);
    TMacCommands.InitializeMinimizeAction(actMinimizeWindow);
    TMacCommands.InitializeZoomAction(actZoomWindow);
    TMacCommands.InitializeBringAllToFrontAction(actBringAllToFront);
    MacMenu.Activate;
    TMacCommands.InitializeWindowMenu(itmWindow);
    //toolbars on the Mac tend to be relatively bare, so let's hide a couple of buttons
    btnShowImage.Visible := False;
    btnOptions.Visible := False;
  end;
end;

procedure TfrmExifTimeShift.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

procedure TfrmExifTimeShift.grdFilesGetValue(Sender: TObject; const Col,
  Row: Integer; var Value: TValue);
var
  EDP: TExifDataPatcher;
begin
  if Row >= FData.Count then Exit; //possible if an exception raised while deleting a series of items
  EDP := FData[Row];
  case Col of
    0: Value := ExtractFileName(EDP.FileName);
    1: Value := DateTimeToText(EDP.FileDateTime);
    2: Value := DateTimeToText(EDP.DateTime);
    3: Value := DateTimeToText(EDP.DateTimeOriginal);
    4: Value := DateTimeToText(EDP.DateTimeDigitized);
  end;
end;

procedure TfrmExifTimeShift.grdFilesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkDelete: if actRemove.Execute then Key := 0;
    vkReturn: if actShowImage.Execute then Key := 0;
  end;
end;

procedure TfrmExifTimeShift.spbMinsToShiftKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and actProcess.Enabled then
  begin
    actProcess.Execute;
    Key := 0;
  end;
end;

procedure TfrmExifTimeShift.actAddExecute(Sender: TObject);
var
  S: string;
begin
  Activate;
  if not dlgOpen.Execute then Exit;
  grdFiles.BeginUpdate;
  try
    for S in dlgOpen.Files do
      TFileManager.OpenFile(S);
    grdFiles.Selected := FData.Count - 1;
  finally
    grdFiles.EndUpdate;
  end;
end;

procedure TfrmExifTimeShift.actAddUpdate(Sender: TObject);
begin
  actAdd.Enabled := Active;
end;

procedure TfrmExifTimeShift.actAppPreferencesExecute(Sender: TObject);
begin
  ShowOptionsDialog;
end;

procedure TfrmExifTimeShift.actProcessExecute(Sender: TObject);

  function DoShiftTime(const DateTime: TDateTimeTagValue; var NewValue: TDateTime): Boolean;
  begin
    Result := not DateTime.MissingOrInvalid;
    if Result then NewValue := IncMinute(DateTime, Trunc(spbMinsToShift.Value));
  end;
var
  EDP: TExifDataPatcher;
  I: Integer;
  NewValue: TDateTime;
begin
  Activate;
  try
    for I := FData.Count - 1 downto 0 do
    begin
      EDP := FData[I];
      NewValue := 0;
      if DoShiftTime(EDP.DateTimeOriginal, NewValue) then
        EDP.DateTimeOriginal := NewValue;
      if DoShiftTime(EDP.DateTimeDigitized, NewValue) then
        EDP.DateTimeDigitized := NewValue;
      if DoShiftTime(EDP.DateTime, NewValue) then
        EDP.DateTime := NewValue;
      EDP.PreserveFileDate := (FileTimeBehaviour = ftPreserve);
      EDP.UpdateFile;
      if FileTimeBehaviour = ftMatchExif then
      begin
        if NewValue = 0 then
        begin
          NewValue := EDP.DateTime.Value;
          if NewValue = 0 then
          begin
            NewValue := EDP.DateTimeDigitized.Value;
            if NewValue = 0 then NewValue := EDP.DateTimeOriginal.Value;
          end;
        end;
        if NewValue <> 0 then
          EDP.FileDateTime := NewValue;
      end;
      FData.Delete(I);
    end;
  finally
    grdFiles.RowCount := FData.Count;
  end;
end;

procedure TfrmExifTimeShift.actProcessUpdate(Sender: TObject);
begin
  actProcess.Enabled := Active and (spbMinsToShift.Value <> 0) and (grdFiles.RowCount <> 0);
end;

function TryGetImageForm(EDP: TExifDataPatcher; var Form: TfrmImage): Boolean;
var
  I: Integer;
begin
  for I := EDP.ComponentCount - 1 downto 0 do
    if EDP.Components[I] is TfrmImage then
    begin
      Form := TfrmImage(EDP.Components[I]);
      Exit(True);
    end;
  Exit(False);
end;

procedure TfrmExifTimeShift.actRemoveExecute(Sender: TObject);
begin
  FData.Delete(grdFiles.Selected);
  grdFiles.RowCount := grdFiles.RowCount - 1;
end;

procedure TfrmExifTimeShift.actRemoveUpdate(Sender: TObject);
var
  Form: TCommonCustomForm;
begin
  //actRemove.Enabled := Active and (grdFiles.Selected >= 0);
  if grdFiles.RowCount = 0 then
    actRemove.Enabled := False
  else if Active then
    actRemove.Enabled := True
  else
  begin
    Form := Screen.ActiveForm;
    actRemove.Enabled := (Form is TfrmImage) and not TMacCommands.IsFullScreen(Form);
  end;
end;

procedure TfrmExifTimeShift.actShowImageExecute(Sender: TObject);
var
  EDP: TExifDataPatcher;
  Form: TfrmImage;
begin
  EDP := FData[grdFiles.Selected];
  if TryGetImageForm(EDP, Form) then
  begin
    if Form.WindowState = TWindowState.wsMinimized then
      Form.WindowState := TWindowState.wsNormal;
    Form.Show;
    Exit;
  end;
  Form := TfrmImage.Create(EDP);
  Form.Show;
  Form.OnActivate := ImageFormActivate;
end;

procedure TfrmExifTimeShift.actShowImageUpdate(Sender: TObject);
begin
  actShowImage.Enabled := Active and (grdFiles.RowCount > 0);
end;

procedure TfrmExifTimeShift.ImageFormActivate(Sender: TObject);
begin
  grdFiles.Selected := FData.IndexOf((Sender as TfrmImage).Owner);
end;

end.
