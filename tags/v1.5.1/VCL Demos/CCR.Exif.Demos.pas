{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.1                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.Demos.pas.                                             }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.Exif.Demos;

interface

{$IF CompilerVersion < 18.5}
  {$DEFINE PreVistaVCL}
{$IFEND}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Themes
  {$IFDEF PreVistaVCL}, XPMan{$ENDIF};

type
  {$IFDEF PreVistaVCL}
  TApplicationHelper = class helper for TApplication
  private
    procedure DummySetter(Value: Boolean);
  public
    property MainFormOnTaskbar: Boolean write DummySetter;
  end;
  {$ENDIF}

  TForm = class(Forms.TForm)
  strict private const
    CmdLineTimerID = 999;
  strict private
    FFileName: string;
  protected
    SupportOpeningFiles: Boolean;
    procedure DoCreate; override; //update the font if running on Vista or greater
    procedure DoFileOpen(const FileName, FileToCompare: string); virtual;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WMSetIcon(var Message: TWMSetIcon); message WM_SETICON; //for VCL buglet
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  public
    procedure OpenFile(const FileName: string; const FileToCompare: string = '');
    property FileName: string read FFileName;
  end;

procedure CreateNewExeInstance(const FileName: string);
function IsThemingEnabled: Boolean;
procedure SelectFileInExplorer(const FileName: string);
function TestMode: Boolean;

implementation

uses
  ShellApi, CCR.Exif.Consts, CCR.Exif.BaseUtils;

{$IFDEF PreVistaVCL}
procedure TApplicationHelper.DummySetter(Value: Boolean);
begin
end;
{$ENDIF}

procedure CreateNewExeInstance(const FileName: string);
var
  Params: string;
begin
  if Pos(' ', FileName) > 0 then
    Params := '"' + FileName + '"'
  else
    Params := FileName;
  ShellExecute(0, nil, PChar(Application.ExeName), PChar(Params), PChar(GetCurrentDir),
    SW_SHOWNORMAL);
end;

function IsThemingEnabled: Boolean;
begin
{$IF Declared(StyleServices)}
  Result := StyleServices.Enabled;
{$ELSE}
  Result := ThemeServices.ThemesEnabled;
{$IFEND}
end;

procedure SelectFileInExplorer(const FileName: string);
begin
  ShellExecute(Screen.ActiveForm.Handle, 'open', 'explorer.exe',
    PChar('/select,"' + FileName + '"'), nil, SW_SHOWNORMAL)
end;

{$IFDEF BackfillStyleServices}
function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$ENDIF}

var
  TestModeStatus: (tmDontKnow, tmNo, tmYes);

function TestMode: Boolean;
begin
  if TestModeStatus = tmDontKnow then
  begin
    if FileExists(ParamStr(2)) and FileExists(ParamStr(1)) then
      TestModeStatus := tmYes
    else
      TestModeStatus := tmNo;
  end;
  Result := (TestModeStatus = tmYes);
end;

type
  TControlAccess = class(TControl);

procedure TForm.DoCreate;
  procedure UpdateFont(Root: TControl);
  var
    I: Integer;
  begin
    if TControlAccess(Root).ParentFont then Exit;
    with TControlAccess(Root).Font do
    begin
      if Name = 'Tahoma' then Name := 'Segoe UI';
      if Size = 8 then Size := 9;
    end;
    if Root is TWinControl then
      for I := 0 to TWinControl(Root).ControlCount - 1 do
        UpdateFont(TWinControl(Root).Controls[I]);
  end;
begin
  if Win32MajorVersion >= 6 then //update the font if running on Vista or later
    UpdateFont(Self);
  if Application.MainForm = nil then
  begin
    Caption := Caption + ' v' + CCRExifVersion;
    Application.Title := Caption;
  end;
  inherited; //calls any OnCreate handler
  if SupportOpeningFiles then SetTimer(Handle, CmdLineTimerID, 10, nil);
end;

procedure TForm.DoFileOpen(const FileName, FileToCompare: string);
begin
end;

procedure TForm.OpenFile(const FileName, FileToCompare: string);
var
  SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Caption := ExtractFileName(FileName) + ' - ' + Application.Title;
    FFileName := FileName;
    try
      DoFileOpen(FileName, FileToCompare);
    except
      Caption := Application.Title;
      raise;
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TForm.WMDropFiles(var Message: TWMDropFiles);
var
  I: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  FileName: string;
begin
  for I := 0 to DragQueryFile(HDROP(Message.Drop), $FFFFFFFF, nil, 0) - 1 do
  begin
    SetString(FileName, Buffer, DragQueryFile(HDROP(Message.Drop), I, Buffer,
      Length(Buffer)));
    if I = 0 then
      OpenFile(FileName)
    else
      CreateNewExeInstance(FileName)
  end;
  DragFinish(HDROP(Message.Drop));
end;

procedure TForm.WMSetIcon(var Message: TWMSetIcon);
begin
  if not Application.Terminated then //The VCL pointlessly clears the icon when a form is
    inherited;                       //destroyed, producing an annoying visual effect on Vista.
end;

procedure TForm.WMTimer(var Message: TWMTimer);
var
  S: string;
begin
  if Message.TimerID <> CmdLineTimerID then Exit;
  KillTimer(Handle, Message.TimerID);
  if TestMode then
    OpenFile(ParamStr(1), ParamStr(2))
  else
  begin
    S := ParamStr(1);
    if (S <> '') and not CharInSet(S[1], SwitchChars) then
      OpenFile(S);
    DragAcceptFiles(Handle, True);
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
