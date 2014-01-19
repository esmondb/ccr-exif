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
{ The Original Code is ResaveForm.pas.                                                 }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ResaveForm;
{
  You should compile ExifList.exe, JpegDump.exe and XMPBroswer.exe before running this
  demo-cum-testing app.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs, 
  Buttons, StdCtrls, ExtCtrls, CCR.Exif.Demos;

type
  TfrmSaveTest = class(TForm)
    grpSourceFile: TGroupBox;
    edtSourceFile: TEdit;
    btnChangeSourceImage: TSpeedButton;
    grpExif: TGroupBox;
    chkClearGeneralSection: TCheckBox;
    chkClearDetailsSection: TCheckBox;
    chkRemoveMakerNoteTag: TCheckBox;
    chkSwitchByteOrder: TCheckBox;
    chkRemovePaddingTags: TCheckBox;
    chkRemoveExifSegment: TCheckBox;
    chkRemoveInteropSection: TCheckBox;
    chkRemoveGPSSection: TCheckBox;
    chkRemoveThumbnail: TCheckBox;
    chkSetDummyThumbnail: TCheckBox;
    rdgXMP: TRadioGroup;
    btnCreateNewImage: TButton;
    btnClose: TButton;
    rdgClassToTest: TRadioGroup;
    dlgSelectSource: TOpenPictureDialog;
    dlgDest: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure edtSourceFileChange(Sender: TObject);
    procedure edtSourceFileKeyPress(Sender: TObject; var Key: Char);
    procedure edtSourceFileKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnChangeSourceImageClick(Sender: TObject);
    procedure btnCreateNewImageClick(Sender: TObject);
    procedure chkRemoveExifSegmentClick(Sender: TObject);
    procedure chkRemoveThumbnailClick(Sender: TObject);
    procedure edtSourceFileDblClick(Sender: TObject);
  private
    FClickChangeSourceImageBtn, FSetDummyThumbnailWasChecked: Boolean;
  end;
                        
var
  frmSaveTest: TfrmSaveTest;

implementation

uses StrUtils, CCR.Exif, CCR.Exif.Consts, CCR.Exif.XMPUtils, ResaveCompleteDlg;

{$R *.dfm}

procedure TfrmSaveTest.edtSourceFileChange(Sender: TObject);
begin
  btnCreateNewImage.Enabled := FileExists(edtSourceFile.Text);
end;

procedure TfrmSaveTest.edtSourceFileDblClick(Sender: TObject);
begin
  btnChangeSourceImage.Click;
end;

procedure TfrmSaveTest.edtSourceFileKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FClickChangeSourceImageBtn := (Key = VK_RETURN) and
    ((GetKeyState(VK_CONTROL) < 0) or not btnCreateNewImage.Enabled);
end;

procedure TfrmSaveTest.edtSourceFileKeyPress(Sender: TObject; var Key: Char);
begin
  if FClickChangeSourceImageBtn then
  begin
    FClickChangeSourceImageBtn := False;
    Key := #0;
    btnChangeSourceImage.Click;
  end;
end;

procedure TfrmSaveTest.btnChangeSourceImageClick(Sender: TObject);
begin
  btnChangeSourceImage.Update;
  dlgSelectSource.FileName := edtSourceFile.Text;
  if dlgSelectSource.Execute then edtSourceFile.Text := dlgSelectSource.FileName;
end;

procedure TfrmSaveTest.chkRemoveExifSegmentClick(Sender: TObject);
var
  Control: TControl;
  Enabling: Boolean;
  I: Integer;
begin
  Enabling := not chkRemoveExifSegment.Checked;
  for I := 0 to grpExif.ControlCount - 1 do
  begin
    Control := grpExif.Controls[I];
    if Control <> Sender then
      if Enabling then
        Control.Enabled := Boolean(Control.Tag)
      else
      begin
        Control.Tag := Ord(Control.Enabled);
        Control.Enabled := False;
      end;
  end;
end;

procedure TfrmSaveTest.chkRemoveThumbnailClick(Sender: TObject);
begin
  chkSetDummyThumbnail.Enabled := not chkRemoveThumbnail.Checked;
  if chkSetDummyThumbnail.Enabled then
    chkSetDummyThumbnail.Checked := FSetDummyThumbnailWasChecked
  else
  begin
    FSetDummyThumbnailWasChecked := chkSetDummyThumbnail.Checked;
    chkSetDummyThumbnail.Checked := False;
  end;
end;

procedure TfrmSaveTest.btnCreateNewImageClick(Sender: TObject);
const
  rdgXMP_REMOVE             = 0;
  rdgXMP_PRESERVE           = 1;
  rdgXMP_REWRITETOEDIT      = 2;
  rdgXMP_REWRITETOORIGINAL  = 3;

  procedure DoIt(ExifData: TExifData);
  var
    Bitmap: TBitmap;
    R: TRect;
    S: string;
  begin
    case rdgXMP.ItemIndex of
      rdgXMP_REMOVE:
      begin
        ExifData.XMPPacket.Clear;
        ExifData.XMPWritePolicy := xwUpdateIfExists; //this is in fact the default
      end;
      rdgXMP_REWRITETOORIGINAL:
      begin
        ExifData.XMPWritePolicy := xwAlwaysUpdate;
        ExifData.Rewrite;
      end;
    end;
    if chkRemoveExifSegment.Checked then
    begin
      ExifData.Clear(rdgXMP.ItemIndex = rdgXMP_REWRITETOEDIT);
      Exit;
    end;
    if chkSwitchByteOrder.Checked then
      if ExifData.Endianness = BigEndian then
        ExifData.Endianness := SmallEndian
      else
        ExifData.Endianness := BigEndian;
    ExifData.RemovePaddingTagsOnSave := chkRemovePaddingTags.Checked;
    if chkClearGeneralSection.Checked then ExifData[esGeneral].Clear;
    if chkClearDetailsSection.Checked then
      ExifData[esDetails].Clear
    else if chkRemoveMakerNoteTag.Checked then
      ExifData.RemoveMakerNote;
    if chkRemoveInteropSection.Checked then ExifData[esInterop].Clear;
    if chkRemoveGPSSection.Checked then ExifData[esGPS].Clear;
    if chkRemoveThumbnail.Checked then
      ExifData.Thumbnail := nil
    else if chkSetDummyThumbnail.Checked then
    begin
      Bitmap := TBitmap.Create;
      try
        Bitmap.Canvas.Brush.Color := clYellow;
        Bitmap.SetSize(StandardExifThumbnailWidth, StandardExifThumbnailHeight);
        R := Rect(0, StandardExifThumbnailHeight div 3,
          StandardExifThumbnailWidth, StandardExifThumbnailHeight);
        S := 'Dummy thumbnail created ' + DateTimeToStr(Now);
        DrawText(Bitmap.Canvas.Handle, PChar(S), Length(S), R, DT_CENTER or DT_WORDBREAK);
        ExifData.Thumbnail.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;
    end;
    if rdgXMP.ItemIndex = rdgXMP_REWRITETOEDIT then
      if chkRemoveExifSegment.Checked then
        ExifData.XMPPacket.Clear
      else
      begin
        ExifData.XMPWritePolicy := xwAlwaysUpdate;
        ExifData.Rewrite;
      end;
  end;

  procedure DoExifDataDirectly(const SourceFile, DestFile: string);
  var
    ExifData: TExifData;
  begin
    ExifData := TExifData.Create;
    try
      if not ExifData.LoadFromGraphic(SourceFile) then
        raise EInvalidGraphic.Create(SUnsupportedGraphicFormat);
      DoIt(ExifData);
      if not SameFileName(SourceFile, DestFile) and not CopyFile(PChar(SourceFile),
        PChar(DestFile), False) then RaiseLastOSError;
      ExifData.SaveToGraphic(DestFile);
    finally
      ExifData.Free;
    end;
  end;

  procedure DoJpegImageEx(const SourceFile, DestFile: string);
  var
    Jpeg: TJpegImageEx;
  begin
    Jpeg := TJpegImageEx.Create;
    try
      Jpeg.LoadFromFile(SourceFile);
      DoIt(Jpeg.ExifData);
      Jpeg.SaveToFile(DestFile);
    finally
      Jpeg.Free;
    end;
  end;
const
  Suffix = ' (resaved)';
var
  SourceFile, DestFile, S: string;
begin
  SourceFile := edtSourceFile.Text;
  if not FileExists(SourceFile) then
  begin
    btnCreateNewImage.Enabled := False;
    Exit;
  end;
  S := ChangeFileExt(SourceFile, '');
  if EndsStr(Suffix, S) then
    dlgDest.FileName := SourceFile
  else
    dlgDest.FileName := S + Suffix + LowerCase(ExtractFileExt(SourceFile));
  if not dlgDest.Execute then Exit;
  DestFile := dlgDest.FileName;
  if rdgClassToTest.ItemIndex = 0 then
    DoExifDataDirectly(SourceFile, DestFile)
  else
    DoJpegImageEx(SourceFile, DestFile);
  frmOpenFile.ShowModal(SourceFile, DestFile);
end;

procedure TfrmSaveTest.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
