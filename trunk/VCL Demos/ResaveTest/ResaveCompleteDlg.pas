{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.2 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.ResaveCompleteDlg.pas.                                      }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ResaveCompleteDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  CCR.Exif.Demos;

type
  TfrmOpenFile = class(TForm)
    btnClose: TButton;
    grpNewFile: TGroupBox;
    grpCompare: TGroupBox;
    btnCompareInExifList: TButton;
    btnCompareInJpegDump: TButton;
    btnCompareInXMPBrowser: TButton;
    btnOpenInDefaultApp: TButton;
    btnOpenInExplorer: TButton;
    btnDeleteFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnDeleteFileClick(Sender: TObject);
    procedure btnOpenInDefaultAppClick(Sender: TObject);
    procedure btnOpenInDemoClick(Sender: TObject);
    procedure btnOpenInExplorerClick(Sender: TObject);
  private
    FLastExecBtn: TButton;
    FSourceFile, FDestFile: string;
  public
    procedure ShowModal(const ASourceFile, ADestFile: string); reintroduce;
  end;

var
  frmOpenFile: TfrmOpenFile;

implementation

uses Masks, ShellApi;

{$R *.dfm}

procedure TfrmOpenFile.ShowModal(const ASourceFile, ADestFile: string);
var
  CanCompare: Boolean;
  I: Integer;
begin
  FSourceFile := ASourceFile;
  FDestFile := ADestFile;
  grpNewFile.Caption := ' ' + ExtractFileName(ADestFile) + ' ';
  CanCompare := not SameFileName(ASourceFile, ADestFile);
  for I := grpCompare.ControlCount - 1 downto 0 do
    grpCompare.Controls[I].Enabled := CanCompare;
  if FLastExecBtn.Enabled then
    ActiveControl := FLastExecBtn
  else
    ActiveControl := btnOpenInDefaultApp;
  inherited ShowModal;
end;

procedure TfrmOpenFile.FormCreate(Sender: TObject);

  procedure CheckDir(const Path: string);
  var
    SearchRec: TSearchRec;
  begin                                                         {$WARN SYMBOL_PLATFORM OFF}
    if FindFirst(Path + '*.exe', faArchive, SearchRec) = 0 then {$WARN SYMBOL_PLATFORM ON}
    begin
      repeat
        if MatchesMask(SearchRec.Name, 'ExifList*.exe') then
          btnCompareInExifList.Hint := Path + SearchRec.Name
        else if MatchesMask(SearchRec.Name, 'JpegDump*.exe') then
          btnCompareInJpegDump.Hint := Path + SearchRec.Name
        else if MatchesMask(SearchRec.Name, 'XMPBrowser*.exe') then
          btnCompareInXMPBrowser.Hint := Path + SearchRec.Name
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;
var
  BasePath: string;
  SearchRec: TSearchRec;
begin
  BasePath := ExtractFilePath(ExtractFileDir(ParamStr(0)));
  if FindFirst(BasePath + '*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if Pos('.', SearchRec.Name) = 0 then CheckDir(BasePath + SearchRec.Name + PathDelim);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  FLastExecBtn := btnCompareInExifList;
end;

procedure TfrmOpenFile.btnOpenInDefaultAppClick(Sender: TObject);
begin
  FLastExecBtn := (Sender as TButton);
  ShellExecute(Handle, nil, PChar(FDestFile), nil, nil, SW_SHOWNORMAL)
end;

procedure TfrmOpenFile.btnOpenInDemoClick(Sender: TObject);
var
  Path: string;
begin
  FLastExecBtn := (Sender as TButton);
  Path := FLastExecBtn.Hint;
  if (Path = '') or not FileExists(Path) then
    MessageDlg('Please compile the other demos first.', mtError, [mbOK], 0)
  else
    ShellExecute(Handle, nil, PChar(Path),
      PChar('"' + FSourceFile + '" "' + FDestFile + '"'), nil, SW_SHOWNORMAL)
end;

procedure TfrmOpenFile.btnOpenInExplorerClick(Sender: TObject);
begin
  FLastExecBtn := (Sender as TButton);
  SelectFileInExplorer(FDestFile);
end;

procedure TfrmOpenFile.btnDeleteFileClick(Sender: TObject);
const
  SConfirmDeletion = 'Are you sure you want to delete "%s"?';
begin
  if MessageDlg(Format(SConfirmDeletion, [FDestFile]), mtWarning, mbYesNo, 0) = mrYes then
  begin
    if not DeleteFile(FDestFile) then RaiseLastOSError;
    Close;
  end;
end;

end.
