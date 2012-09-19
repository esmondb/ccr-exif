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

unit JpegDumpForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl, FMX.Layouts, FMX.Memo,
  CCR.Exif.BaseUtils, CCR.Exif.Consts, CCR.Exif.XMPUtils, CCR.Exif.FMXUtils, JpegDumpController;

type
  TfrmJpegDump = class(TForm, IJpegDocumentForm)
    TabControl: TTabControl;
    tabDetails: TTabItem;
    tabThumbnail: TTabItem;
    lyoButtonBar: TLayout;
    btnOpenFile: TButton;
    btnClose: TButton;
    tabImage: TTabItem;
    memDetails: TMemo;
    imgImage: TImageControl;
    imgThumbnail: TImageControl;
    lblNoExifThumbnail: TLabel;
    procedure TabControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FDoneDragEnter, FHandleDragDirectly: Boolean;
    FFileName: string;
    FLoadedImage: Boolean;
    FRotateImageBy: Single;
  strict protected
    function GetFileName: string;
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    procedure OpenFile(const AFileName: string);
  public
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: Boolean); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DragLeave; override;
  end;

implementation

{$R *.fmx}

uses JpegDumpBuilder;

{ TfrmJpegDump }

procedure TfrmJpegDump.FormCreate(Sender: TObject);
begin
  lyoButtonBar.Visible := (TFileManager.DocumentMode = dmOnePerAppInst);
  TMacCommands.EnableFullScreen(Self);
end;

procedure TfrmJpegDump.FormShow(Sender: TObject);
begin
  OnShow := nil;
  if lyoButtonBar.Visible then
  begin
    btnOpenFile.Action := dtmController.actOpenFile;
    btnClose.Action := dtmController.actClose;
  end;
end;

procedure TfrmJpegDump.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmJpegDump.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  { The default behaviour is to pass the event on to the control being hovered
    over. Handling the drag operation at the form level just means it doesn't
    matter what control files are dropped onto. }
  FDoneDragEnter := True;
  FHandleDragDirectly := (Data.Files <> nil);
  if not FHandleDragDirectly then inherited;
end;

procedure TfrmJpegDump.DragOver(const Data: TDragObject; const Point: TPointF;
  var Accept: Boolean);
begin
  if not FDoneDragEnter then //!!!FMX bug - DragEnter never called on Windows
    DragEnter(Data, Point);
  if FHandleDragDirectly then
    Accept := True
  else
    inherited;
end;

procedure TfrmJpegDump.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  if FHandleDragDirectly then
    TFileManager.OpenDroppedFiles(Data.Files)
  else
    inherited;
  FDoneDragEnter := False;
end;

procedure TfrmJpegDump.DragLeave;
begin
  FDoneDragEnter := False;
  inherited;
end;

function TfrmJpegDump.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TfrmJpegDump.GotoNextTab;
begin
  if TabControl.TabIndex = TabControl.TabCount - 1 then
    TabControl.TabIndex := 0
  else
    TabControl.TabIndex := TabControl.TabIndex + 1;
end;

procedure TfrmJpegDump.GotoPreviousTab;
begin
  if TabControl.TabIndex = 0 then
    TabControl.TabIndex := TabControl.TabCount - 1
  else
    TabControl.TabIndex := TabControl.TabIndex - 1;
end;

procedure TfrmJpegDump.OpenFile(const AFileName: string);
var
  Builder: TJpegDumpBuilder;
begin
  Builder := TJpegDumpBuilder.Create($FFF);
  try
    Builder.Load(AFileName);
    memDetails.Text := Builder.ToString;
    imgThumbnail.Bitmap.Assign(Builder.ExifThumbnail);
    lblNoExifThumbnail.Visible := Builder.ExifThumbnail.IsEmpty;
  finally
    Builder.Free;
  end;
  FFileName := AFileName;
  FLoadedImage := False;
  TabControlChange(nil);
end;

procedure TfrmJpegDump.TabControlChange(Sender: TObject);
begin
  if tabImage.IsSelected and not FLoadedImage and (FFileName <> '') then
  begin
    imgImage.Bitmap.LoadFromFile(FFileName);
    if FRotateImageBy <> 0 then imgImage.Bitmap.Rotate(FRotateImageBy);
    FLoadedImage := True;
  end;
end;

initialization
  TfrmJpegDump.ClassName; //ensure class is linked in!
end.
