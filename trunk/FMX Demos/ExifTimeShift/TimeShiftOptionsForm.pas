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

unit TimeShiftOptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs;

type
  TfrmOptions = class(TForm)
    grpOptions: TGroupBox;
    rdoPreserve: TRadioButton;
    rdoMatchExif: TRadioButton;
    rdoSetToNow: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    procedure MakeSetting;
  end;

procedure ShowOptionsDialog;

implementation

{$R *.fmx}

uses TimeShiftOptions;

const
  ImmediateUpdate = {$IFDEF MACOS}True{$ELSE}False{$ENDIF};

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  if ImmediateUpdate then
  begin
    //update text and styling
    Caption := 'Preferences';
    grpOptions.ApplyStyleLookup;
    grpOptions.StyledSettings := [];
    grpOptions.Font.Style := [TFontStyle.fsBold];
    grpOptions.Font.Size := 12;
    //remove buttons
    ClientHeight := Trunc(btnOK.Position.Y);
    FreeAndNil(btnOK);
    FreeAndNil(btnCancel);
  end;
  Left := Application.MainForm.Left + (Application.MainForm.Width - Width) div 2;
  Top := Application.MainForm.Top + (Application.MainForm.Height -  Height) div 2;
  case FileTimeBehaviour of
    ftPreserve: rdoPreserve.IsChecked := True;
    ftMatchExif: rdoMatchExif.IsChecked := True;
    ftSetToNow: rdoSetToNow.IsChecked := True;
  end;
end;

procedure TfrmOptions.MakeSetting;
begin
  if rdoPreserve.IsChecked then
    SetFileTimeBehaviour(ftPreserve)
  else if rdoMatchExif.IsChecked then
    SetFileTimeBehaviour(ftMatchExif)
  else if rdoSetToNow.IsChecked then
    SetFileTimeBehaviour(ftSetToNow)
end;

procedure TfrmOptions.RadioButtonChange(Sender: TObject);
begin
  if ImmediateUpdate and TCheckBox(Sender).IsChecked then MakeSetting;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  MakeSetting;
  Close;
end;

var
  Form: TfrmOptions = nil;

procedure ShowOptionsDialog;
begin
  if Form = nil then Form := TfrmOptions.Create(Application);
  try
    if ImmediateUpdate then
      Form.Show
    else
      Form.ShowModal;
  finally
    if not ImmediateUpdate then FreeAndNil(Form);
  end;
end;

end.
