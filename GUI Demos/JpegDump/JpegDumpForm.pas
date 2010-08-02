{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.1 (2010-08-02)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is JpegDumpForm.pas.                                               }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit JpegDumpForm;
{
  Demonstrates using the enumerator functions of CCR.Exif.JPEGUtils to parse a JPEG
  file's structure - see JpegDumpOutputFrame.pas for the actual parsing code.
  While the implementation of JPEGHeader may look a bit funky if you're unused to
  the details of Delphi's for/in loop support, its use should be straightforward.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, ActnList, StdActns,
  CCR.Exif.Demos, JpegDumpOutputFrame;


type
  TfrmJpegDump = class(TForm)
    panBtns: TPanel;
    btnOpen: TBitBtn;
    btnExit: TBitBtn;
    dlgOpen: TOpenPictureDialog;
    ActionList: TActionList;
    EditSelectAll1: TEditSelectAll;
    PageControl: TPageControl;
    tabOriginal: TTabSheet;
    tabResaved: TTabSheet;
    actOpen: TAction;
    dlgSave: TSavePictureDialog;
    btnReload: TBitBtn;
    actReload: TAction;
    chkStayOnTop: TCheckBox;
    procedure btnExitClick(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure chkStayOnTopClick(Sender: TObject);
  private
    FOriginalFrame, FResavedFrame: TOutputFrame;
  protected
    procedure DoFileOpen(const FileName1, FileName2: string); override;
  end;

var
  frmJpegDump: TfrmJpegDump;

implementation

uses ShellApi, CCR.Exif;

{$R *.dfm}

procedure TfrmJpegDump.FormCreate(Sender: TObject);
begin
  PageControl.Visible := TestMode;
  FOriginalFrame := TOutputFrame.Create(Self);
  FOriginalFrame.Align := alClient;
  FOriginalFrame.Name := '';
  if not TestMode then
    FOriginalFrame.Parent := Self
  else
  begin
    actOpen.Enabled := False;
    actOpen.Visible := False;
    actReload.Visible := False;
    ActiveControl := PageControl;
    FOriginalFrame.Parent := tabOriginal;
    FResavedFrame := TOutputFrame.Create(Self);
    FResavedFrame.Align := alClient;
    FResavedFrame.Parent := tabResaved;
  end;
  SupportOpeningFiles := True;
end;

procedure TfrmJpegDump.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

procedure TfrmJpegDump.actReloadExecute(Sender: TObject);
begin
  OpenFile(FileName);
end;

procedure TfrmJpegDump.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmJpegDump.chkStayOnTopClick(Sender: TObject);
begin
  if chkStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TfrmJpegDump.DoFileOpen(const FileName1, FileName2: string);
begin
  FOriginalFrame.LoadFromFile(FileName1);
  if actReload.Visible then actReload.Enabled := True;
  if FileName2 = '' then Exit;
  FResavedFrame.LoadFromFile(FileName2);
  tabOriginal.Caption := ExtractFileName(FileName1);
  tabResaved.Caption := ExtractFileName(FileName2);
end;

end.
