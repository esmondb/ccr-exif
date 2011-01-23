{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.2 (2011-01-23)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is ExifListForm.pas.                                               }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ExifListForm;
{
  Demonstrates listing both standard and MakerNote tags - see ExifListFrame.pas for the
  important code. Note that the approach taken to the two tag types is different. In the
  standard tag case, how tags are interpreted is hardcoded at compile time. In the
  MakerNote case, in contrast, tag interpretation is done on the fly using an INI file
  (MakerNotes.ini), albeit with the intial detection of the basic MakerNote type being
  done by CCR.Exif.pas.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, IniFiles, Controls, Forms, Dialogs,
  ExtDlgs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ActnList, CCR.Exif.Demos, ExifListFrame;

type
  TfrmExifList = class(TForm)
    dlgOpen: TOpenPictureDialog;
    ActionList: TActionList;
    actOpen: TAction;
    dlgSave: TSavePictureDialog;
    PageControl: TPageControl;
    tabOriginal: TTabSheet;
    tabResaved: TTabSheet;
    panBtns: TPanel;
    btnOpen: TBitBtn;
    btnExit: TBitBtn;
    btnCopy: TBitBtn;
    actCopy: TAction;
    actSelectAll: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
  private
    FActiveFrame, FOriginalFrame, FResavedFrame: TOutputFrame;
    FMakerNoteValueMap: TMemIniFile;
  protected
    procedure DoFileOpen(const FileName1, FileName2: string); override;
  end;

var
  frmExifList: TfrmExifList;

implementation

uses ShellApi, CCR.Exif;

{$R *.dfm}

procedure TfrmExifList.FormCreate(Sender: TObject);
begin
  PageControl.Visible := TestMode;
  FMakerNoteValueMap := TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'MakerNotes.ini');
  FOriginalFrame := TOutputFrame.Create(Self);
  FOriginalFrame.Align := alClient;
  FOriginalFrame.Name := '';
  if not TestMode then
    FOriginalFrame.Parent := Self
  else
  begin
    actOpen.Enabled := False;
    actOpen.Visible := False;
    ActiveControl := PageControl;
    FOriginalFrame.Parent := tabOriginal;
    FResavedFrame := TOutputFrame.Create(Self);
    FResavedFrame.Align := alClient;
    FResavedFrame.Parent := tabResaved;
  end;
  FActiveFrame := FOriginalFrame;
  SupportOpeningFiles := True;
end;

procedure TfrmExifList.FormDestroy(Sender: TObject);
begin
  FMakerNoteValueMap.Free;
end;

procedure TfrmExifList.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = tabOriginal then
    FActiveFrame := FOriginalFrame
  else
    FActiveFrame := FResavedFrame;
end;

procedure TfrmExifList.actCopyExecute(Sender: TObject);
begin
  FActiveFrame.CopyToClipboard;
end;

procedure TfrmExifList.actCopyUpdate(Sender: TObject);
begin
  actCopy.Enabled := FActiveFrame.CanCopyToClipboard;
end;

procedure TfrmExifList.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

procedure TfrmExifList.actSelectAllExecute(Sender: TObject);
begin
  FActiveFrame.SelectAll;
end;

procedure TfrmExifList.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmExifList.DoFileOpen(const FileName1, FileName2: string);
begin
  FOriginalFrame.LoadFromFile(FileName1, FMakerNoteValueMap);
  if FileName2 = '' then Exit;
  FResavedFrame.LoadFromFile(FileName2, FMakerNoteValueMap);
  tabOriginal.Caption := ExtractFileName(FileName1);
  tabResaved.Caption := ExtractFileName(FileName2);
end;

end.
