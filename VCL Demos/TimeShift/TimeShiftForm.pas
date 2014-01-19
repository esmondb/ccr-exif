{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.3                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is TimeShiftForm.pas.                                              }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit TimeShiftForm;
{
  Demonstrates use of TExifDataPatcher.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ToolWin, ComCtrls, Menus, ActnList, ImgList, ExtDlgs,
  CCR.Exif.Demos, FileTimeOptsForm;

type
  TListView = class(ComCtrls.TListView) //OK, using an interposer class is a bit lazy,
  protected                             //but this isn't a big app...
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  end;

  TfrmTimeShiftDemo = class(TForm)
    ToolBar: TToolBar;
    ListView: TListView;
    imlColor: TImageList;
    ActionList: TActionList;
    actAdd: TAction;
    actRemove: TAction;
    btnAdd: TToolButton;
    btnRemove: TToolButton;
    sep1: TToolButton;
    panMins: TPanel;
    updMins: TUpDown;
    lblMins: TLabel;
    edtMins: TMemo;
    actProcess: TAction;
    btnProcess: TToolButton;
    dlgOpen: TOpenPictureDialog;
    sep2: TToolButton;
    btnFileTimeOpts: TToolButton;
    actFileTimeOpts: TAction;
    imlDisabled: TImageList;
    procedure actRemoveUpdate(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
    procedure FormActivate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actProcessUpdate(Sender: TObject);
    procedure edtMinsKeyPress(Sender: TObject; var Key: Char);
    procedure actProcessExecute(Sender: TObject);
    procedure actFileTimeOptsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileTimeAction: TFileTimeAction;
    procedure DoOpen;
  end;

var
  frmTimeShiftDemo: TfrmTimeShiftDemo;

implementation

uses
  {$IF CompilerVersion >= 23}System.UITypes,{$IFEND} ShellApi, DateUtils, StrUtils, Registry,
  CCR.Exif.BaseUtils, CCR.Exif;

{$R *.dfm}

resourcestring
  SReadOnlyFileMsg = '%s is marked read only. Attempt to open it for read/write access anyhow?';

const
  RegPath = 'Software\CCR';

{ TListView }

procedure TListView.WMDropFiles(var Message: TWMDropFiles);
var
  I: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  FileName: string;
begin
  for I := 0 to DragQueryFile(HDROP(Message.Drop), $FFFFFFFF, nil, 0) - 1 do
  begin
    SetString(FileName, Buffer, DragQueryFile(HDROP(Message.Drop), I, Buffer,
      Length(Buffer)));
    frmTimeShiftDemo.dlgOpen.Files.Clear;
    frmTimeShiftDemo.dlgOpen.Files.Add(FileName);
    frmTimeShiftDemo.DoOpen;
  end;
  DragFinish(HDROP(Message.Drop));
end;

{ TfrmTimeShiftDemo }

procedure TfrmTimeShiftDemo.FormCreate(Sender: TObject);
var
  IntVal: Integer;
begin
  Application.Title := Caption;
  with TRegIniFile.Create(RegPath) do
  try
    IntVal := ReadInteger(Application.Title, 'FileTimeAction', 0);
    if (IntVal >= Ord(Low(TFileTimeAction))) and (IntVal <= Ord(High(TFileTimeAction))) then
      FFileTimeAction := TFileTimeAction(IntVal);
  finally
    Free;
  end;
  //prevent list view flicker if running on Vista or above
  if Win32MajorVersion >= 6 then ListView.DoubleBuffered := True;
  //fixup the toolbar
  updMins.Align := alLeft;
  updMins.HandleNeeded;
  panMins.Width := updMins.Left + updMins.Width + panMins.BorderWidth;
end;

procedure TfrmTimeShiftDemo.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  OnActivate := nil;
  Application.Title := Caption;
  DragAcceptFiles(ListView.Handle, True);
  Update; //make sure we've been painted properly before any dialog is shown
  for I := 1 to ParamCount do
    dlgOpen.Files.Add(ParamStr(I));
  if dlgOpen.Files.Count <> 0 then DoOpen;
end;

procedure TfrmTimeShiftDemo.ListViewDeletion(Sender: TObject; Item: TListItem);
var
  FileName: string;
begin
  FileName := TExifDataPatcher(Item.Data).FileName;
  TExifDataPatcher(Item.Data).Free;
  if Item.SubItems.Objects[0] <> nil then //see actAddExecute
    FileSetReadOnly(FileName, True);
end;

function DateTimeToText(const DateTime: TDateTimeTagValue): string;
begin
  if DateTime.MissingOrInvalid then
    Result := '<missing>'
  else
    Result := SysUtils.DateTimeToStr(DateTime);
end;

procedure TfrmTimeShiftDemo.DoOpen;
var
  S: string;
  Data: TExifDataPatcher;
  Item: TListItem;
  ForceReadOnly: TModalResult;
  ForceReadOnlyOptions: TMsgDlgButtons;
  ReadOnly: Boolean;
begin
  Data := nil; //prevent compiler warning
  ForceReadOnly := mrNone;
  ForceReadOnlyOptions := [mbYes, mbNo];
  if dlgOpen.Files.Count > 1 then
    ForceReadOnlyOptions := ForceReadOnlyOptions + [mbNoToAll, mbYesToAll];
  for S in dlgOpen.Files do
  begin
    ReadOnly := FileIsReadOnly(S);
    if ReadOnly then //we can't open a read-only file with read/write access obviously
    begin
      if ForceReadOnly = mrNone then
      begin
        ForceReadOnly := MessageDlg(Format(SReadOnlyFileMsg, [ExtractFileName(S)]),
          mtConfirmation, ForceReadOnlyOptions, 0, mbNo);
        if ForceReadOnly = mrNone then ForceReadOnly := mrNo;
      end;
      if IsNegativeResult(ForceReadOnly) then
        Continue;
      if not FileSetReadOnly(S, False) then
      begin
        MessageDlg(SysErrorMessage(GetLastError), mtError, [mbOK], 0);
        Continue;
      end;
    end;
    try
      Data := TExifDataPatcher.Create(S);
    except
      on E: EInvalidJPEGHeader do     //If it's just that a certain file isn't a JPEG or
      begin                           //is corrupt, we still want to continue with any
        Application.ShowException(E); //other filenames we have been requested to process.
        Continue;
      end
      else
        raise;
    end;
    Item := ListView.Items.Add;
    Item.Data := Data;
    Item.Caption := ExtractFileName(S);
    Item.SubItems.AddObject(DateTimeToText(Data.FileDateTime), TObject(ReadOnly));
    Item.SubItems.Add(DateTimeToText(Data.DateTime));
    Item.SubItems.Add(DateTimeToText(Data.DateTimeOriginal));
    Item.SubItems.Add(DateTimeToText(Data.DateTimeDigitized));
  end;
end;

procedure TfrmTimeShiftDemo.actAddExecute(Sender: TObject);
begin
  if dlgOpen.Execute then DoOpen;
end;

procedure TfrmTimeShiftDemo.actRemoveUpdate(Sender: TObject);
begin
  actRemove.Enabled := (ListView.SelCount > 0);
end;

procedure TfrmTimeShiftDemo.actRemoveExecute(Sender: TObject);
begin
  ListView.Selected.Delete;
end;

procedure TfrmTimeShiftDemo.edtMinsKeyPress(Sender: TObject; var Key: Char);
begin
  case UpCase(Key) of
    'A'..'Z', ' ':
    begin
      Key := #0;
      Beep;
    end;
    #13:
    begin
      Key := #0;
      if not actProcess.Execute then
        Beep;
    end;
  end;
end;

procedure TfrmTimeShiftDemo.actFileTimeOptsExecute(Sender: TObject);
var
  Dlg: TfrmFileTimeOptions;
  RegIniFile: TRegIniFile;
begin
  RegIniFile := nil;
  Dlg := TfrmFileTimeOptions.Create(nil);
  try
    if Dlg.ShowModal(FFileTimeAction) then
    begin
      RegIniFile := TRegIniFile.Create(RegPath);
      RegIniFile.WriteInteger(Application.Title, 'FileTimeAction', Ord(FFileTimeAction));
    end;
  finally
    RegIniFile.Free;
    Dlg.Free;
  end;
end;

procedure TfrmTimeShiftDemo.actProcessUpdate(Sender: TObject);
begin
  actProcess.Enabled := (ListView.Items.Count > 0) and
    ((updMins.Position <> 0) or (FFileTimeAction = ftMatchExif));
end;

procedure TfrmTimeShiftDemo.actProcessExecute(Sender: TObject);
var
  NewValue: TDateTime;

  function DoShiftTime(const DateTime: TDateTimeTagValue): Boolean;
  begin
    Result := not DateTime.MissingOrInvalid;
    if Result then NewValue := IncMinute(DateTime, updMins.Position);
  end;
var
  Data: TExifDataPatcher;
  I: Integer;
begin
  for I := ListView.Items.Count - 1 downto 0 do
  begin
    Data := ListView.Items[I].Data;
    NewValue := 0;
    if DoShiftTime(Data.DateTimeOriginal) then
      Data.DateTimeOriginal := NewValue;
    if DoShiftTime(Data.DateTimeDigitized) then
      Data.DateTimeDigitized := NewValue;
    if DoShiftTime(Data.DateTime) then
      Data.DateTime := NewValue;
    Data.PreserveFileDate := (FFileTimeAction = ftPreserve);
    Data.UpdateFile;
    if FFileTimeAction = ftMatchExif then
    begin
      if NewValue = 0 then
      begin
        NewValue := Data.DateTime.Value;
        if NewValue = 0 then
        begin
          NewValue := Data.DateTimeDigitized.Value;
          if NewValue = 0 then NewValue := Data.DateTimeOriginal.Value;
        end;
      end;
      if NewValue <> 0 then           
        Data.FileDateTime := NewValue;
    end;
    ListView.Items.Delete(I);
  end;
end;

end.
